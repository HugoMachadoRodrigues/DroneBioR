# =============================================================================
# Windowed covariate extraction at field sample points
#
# The performance core of the Field Models tab. The cost of every function
# here is O(points x window^2 x layers) and independent of raster size: the
# full-resolution index stack is never materialised. That is what makes a
# 21 x 21 window over a 400 Mpx orthomosaic a sub-second operation.
#
# The trick is synthesize_pixel_raster(): pack the handful of window pixels
# into a 1-row SpatRaster so compute_spectral_indices() and
# compute_biomass_proxies() can be reused verbatim. No formula in R/indices.R
# uses focal / global / aggregate, so every index is pure per-pixel
# arithmetic and the synthetic route is numerically identical to computing
# the index globally and then extracting.
# =============================================================================

# Windows are odd so the sample point sits at the centre; 21 is the largest
# size terra::adjacent() handles comfortably and already covers a 1.2 m
# footprint at 5.7 cm GSD.
.allowed_windows <- seq(1L, 21L, by = 2L)

# A window in pixels means different things at different ground sampling
# distances: 3 x 3 spans about 17 cm at 5.8 cm GSD but about 1.5 m at 50 cm.
# resolve_window() lets a caller state the support it actually wants in metres
# and converts to the nearest odd pixel count for the raster in hand.
#' @noRd
.resolve_window <- function(window, window_m, reference) {
  if (is.null(window_m)) return(as.integer(window))
  if (length(window_m) != 1L || !is.finite(suppressWarnings(as.numeric(window_m))) ||
      as.numeric(window_m) <= 0) {
    stop("`window_m` must be a single positive number of metres.", call. = FALSE)
  }
  res <- tryCatch(min(terra::res(reference)), error = function(e) NA_real_)
  if (!is.finite(res) || res <= 0) {
    stop("Could not read the resolution of the reference raster, so `window_m` ",
         "cannot be converted to pixels. Pass `window` in pixels instead.",
         call. = FALSE)
  }
  n <- as.numeric(window_m) / res
  # round to the nearest odd integer: a window has to be centred on the pixel
  # holding the sample, so an even count has no centre.
  px <- max(1L, as.integer(round((n - 1) / 2)) * 2L + 1L)
  if (!(px %in% .allowed_windows)) {
    stop("`window_m` = ", window_m, " m is ", round(n, 1), " pixels at a ",
         signif(res, 3), " m resolution, which rounds to ", px,
         ". Supported windows are ", paste(.allowed_windows, collapse = ", "),
         " pixels, i.e. up to ", signif(21 * res, 3), " m here.", call. = FALSE)
  }
  attr(px, "window_m_requested") <- as.numeric(window_m)
  attr(px, "window_m_actual")    <- px * res
  as.integer(px)
}

#' Convert a metric window to the nearest odd pixel window
#'
#' A window expressed in pixels changes physical meaning with the ground
#' sampling distance, so a quadrat-matched support has to be restated for every
#' survey. This converts metres to the nearest odd pixel count for a given
#' raster, and reports the metric support that count actually spans.
#'
#' @param reference A `terra::SpatRaster` whose resolution defines the
#'   conversion.
#' @param window_m Desired support in metres (the side of the square).
#' @return A list with `window` (odd pixel count), `window_m_actual` (what that
#'   count spans) and `resolution`.
#' @examples
#' r <- terra::rast(nrows = 100, ncols = 100, xmin = 0, xmax = 5.76, ymin = 0, ymax = 5.76)
#' window_from_metres(r, 0.52)
#' @export
window_from_metres <- function(reference, window_m) {
  if (!inherits(reference, "SpatRaster")) {
    stop("`reference` must be a terra SpatRaster.", call. = FALSE)
  }
  px  <- .resolve_window(1L, window_m, reference)
  res <- min(terra::res(reference))
  list(window = as.integer(px), window_m_actual = as.integer(px) * res,
       resolution = res)
}

# Coordinates from whatever the caller has: an sf layer, a SpatVector, or a
# plain table with x/y columns.
.points_xy <- function(points) {
  if (inherits(points, "sf") || inherits(points, "sfc")) {
    xy <- sf::st_coordinates(sf::st_geometry(points))
    return(unname(xy[, 1:2, drop = FALSE]))
  }
  if (inherits(points, "SpatVector")) {
    return(unname(terra::crds(points)[, 1:2, drop = FALSE]))
  }
  if (is.matrix(points)) {
    return(unname(points[, 1:2, drop = FALSE]))
  }
  if (is.data.frame(points) && all(c("x", "y") %in% names(points))) {
    return(unname(as.matrix(points[, c("x", "y")])))
  }
  stop("`points` must be an sf layer, a SpatVector, or a table with x/y columns.",
       call. = FALSE)
}

# Row-wise aggregation that does not blow up on all-NA rows. base::max on an
# empty vector returns -Inf with a warning, which would quietly poison an
# edge point's covariates.
.row_aggregate <- function(m, fun, na.rm = TRUE) {
  if (ncol(m) == 1L) {
    return(as.numeric(m[, 1L]))
  }
  switch(
    fun,
    mean   = rowMeans(m, na.rm = na.rm),
    median = apply(m, 1L, function(z) {
      z <- if (na.rm) z[!is.na(z)] else z
      if (length(z) == 0L) NA_real_ else stats::median(z)
    }),
    max    = apply(m, 1L, function(z) {
      z <- if (na.rm) z[!is.na(z)] else z
      if (length(z) == 0L) NA_real_ else max(z)
    }),
    min    = apply(m, 1L, function(z) {
      z <- if (na.rm) z[!is.na(z)] else z
      if (length(z) == 0L) NA_real_ else min(z)
    }),
    sd     = apply(m, 1L, function(z) {
      z <- if (na.rm) z[!is.na(z)] else z
      if (length(z) < 2L) NA_real_ else stats::sd(z)
    }),
    stop("Unsupported aggregation function: ", fun, call. = FALSE)
  )
}

#' Cell numbers of the n x n window around each field point
#'
#' @param reference A `terra::SpatRaster` defining the grid. Only its first
#'   layer's geometry is used.
#' @param points Sample locations: an `sf` layer, a `SpatVector`, a two-column
#'   matrix, or a data frame with `x` / `y`.
#' @param window Odd window size, one of 1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21.
#' @return A list with `cells` (integer matrix, one row per **in-extent**
#'   point and `window^2` columns), `in_extent` (logical, one per input
#'   point) and `index` (the original point index of each row of `cells`).
#' @examples
#' ortho <- system.file("extdata", "micasense_subset.tif", package = "DroneBioR")
#' r <- terra::rast(ortho)
#' pts <- data.frame(x = c(392004, 392012), y = c(3033007, 3033012))
#' wc <- window_cells(r, pts, window = 3)
#' dim(wc$cells)
#' @export
window_cells <- function(reference, points, window = 1L) {
  if (!inherits(reference, "SpatRaster")) {
    stop("`reference` must be a terra SpatRaster.", call. = FALSE)
  }
  if (length(window) != 1L || !is.finite(suppressWarnings(as.numeric(window))) ||
      suppressWarnings(as.numeric(window)) %% 1 != 0 ||
      !(as.integer(window) %in% .allowed_windows)) {
    stop("`window` must be one of: ",
         paste(.allowed_windows, collapse = ", "),
         " (odd, so the sample point stays at the centre).", call. = FALSE)
  }
  window <- as.integer(window)
  ref <- reference[[1L]]
  coords <- .points_xy(points)

  cells <- terra::cellFromXY(ref, coords)
  in_extent <- is.finite(cells)
  idx <- which(in_extent)

  if (length(idx) == 0L) {
    return(list(
      cells     = matrix(numeric(0), nrow = 0L, ncol = window^2L),
      in_extent = in_extent,
      index     = idx
    ))
  }

  if (window == 1L) {
    cm <- matrix(as.numeric(cells[idx]), ncol = 1L)
  } else {
    # Out-of-extent points MUST be dropped before this call: adjacent() on an
    # NA cell number returns a row of NaN whose last element is 1, so an
    # off-raster point would silently absorb cell 1's value.
    cm <- terra::adjacent(ref, cells[idx],
                          directions = matrix(1L, window, window),
                          include = FALSE)
    cm <- matrix(as.numeric(cm), nrow = length(idx), ncol = window^2L)
  }
  list(cells = cm, in_extent = in_extent, index = idx)
}

#' Aggregate raster values over pre-computed window cells
#'
#' One windowed `terra::extract()` read per call, reshaped per layer and
#' collapsed with `fun`. Cells that fall outside the raster come back as
#' `NA`, so an edge point aggregates over its in-raster pixels only.
#'
#' Never use `terra::extract(..., buffer =)` for this: terra accepts and
#' silently ignores `buffer` for point extraction (unlike `raster::extract`),
#' returning single-cell values that look plausible.
#'
#' @param x A `terra::SpatRaster`.
#' @param cells Integer matrix of cell numbers, as returned in the `cells`
#'   element of [window_cells()].
#' @param fun Aggregation: `"mean"`, `"median"`, `"max"`, `"min"` or `"sd"`.
#' @param na.rm Drop missing window pixels before aggregating.
#' @return A data frame with `nrow(cells)` rows, one numeric column per layer
#'   of `x` (named after the layer), plus `.n_valid_px` - the number of window
#'   pixels with data in the first layer.
#' @examples
#' ortho <- system.file("extdata", "micasense_subset.tif", package = "DroneBioR")
#' r <- terra::rast(ortho)[[1:2]]
#' pts <- data.frame(x = 392004, y = 3033007)
#' wc <- window_cells(r, pts, window = 3)
#' extract_window_values(r, wc$cells, fun = "mean")
#' @export
extract_window_values <- function(x, cells,
                                  fun = c("mean", "median", "max", "min", "sd"),
                                  na.rm = TRUE) {
  fun <- match.arg(fun)
  if (!inherits(x, "SpatRaster")) {
    stop("`x` must be a terra SpatRaster.", call. = FALSE)
  }
  cells <- as.matrix(cells)
  n <- nrow(cells)
  w <- ncol(cells)
  layer_names <- names(x)

  if (n == 0L) {
    out <- stats::setNames(
      as.data.frame(matrix(numeric(0), nrow = 0L, ncol = length(layer_names))),
      layer_names
    )
    out$.n_valid_px <- integer(0)
    return(out)
  }

  # Row-major flattening, so matrix(..., byrow = TRUE) reassembles each
  # point's window in the same order.
  flat <- as.vector(t(cells))
  values <- terra::extract(x, flat)
  values <- values[, layer_names, drop = FALSE]

  out <- list()
  for (nm in layer_names) {
    m <- matrix(as.numeric(values[[nm]]), nrow = n, ncol = w, byrow = TRUE)
    out[[nm]] <- .row_aggregate(m, fun, na.rm = na.rm)
  }
  first <- matrix(as.numeric(values[[layer_names[[1L]]]]),
                  nrow = n, ncol = w, byrow = TRUE)
  out$.n_valid_px <- as.integer(rowSums(!is.na(first)))
  as.data.frame(out, check.names = FALSE, stringsAsFactors = FALSE)
}

#' Pack pixel values into a one-row SpatRaster
#'
#' Lets per-pixel raster functions ([compute_spectral_indices()],
#' [compute_biomass_proxies()]) run on a handful of window pixels instead of
#' hundreds of millions of cells. terra fills row-major, so cell *i* is row
#' *i* of `values` and the input order is preserved exactly.
#'
#' @param values Numeric matrix or data frame; one column per layer, one row
#'   per pixel. Column names become layer names.
#' @param crs CRS string for the synthetic grid. Any consistent value works -
#'   it only has to match between stacks that will be compared with
#'   `terra::compareGeom()`.
#' @return A `terra::SpatRaster` with 1 row, `nrow(values)` columns and
#'   `ncol(values)` layers.
#' @examples
#' r <- synthesize_pixel_raster(data.frame(Red = c(0.1, 0.2), NIR = c(0.5, 0.6)))
#' dim(r)
#' terra::values(r)
#' @export
synthesize_pixel_raster <- function(values, crs = "") {
  values <- as.data.frame(values, stringsAsFactors = FALSE)
  if (ncol(values) == 0L) {
    stop("`values` needs at least one column.", call. = FALSE)
  }
  if (nrow(values) == 0L) {
    stop("`values` needs at least one row.", call. = FALSE)
  }
  nms <- names(values)
  m <- as.matrix(values)
  storage.mode(m) <- "double"
  r <- terra::rast(
    nrows = 1L, ncols = nrow(m), nlyrs = ncol(m),
    xmin = 0, xmax = nrow(m), ymin = 0, ymax = 1,
    crs = crs
  )
  terra::values(r) <- m
  names(r) <- nms
  r
}

# Attach a vector / one-column frame as a named column, when it is supplied.
# `force` renames a single incoming column onto the canonical covariate id
# (CHM_m / DSM / DTM); a custom index keeps whatever the caller named it,
# because that name is the covariate id the user ticked.
.add_pixel_column <- function(pool, values, name, n, force = TRUE) {
  if (is.null(values)) return(pool)
  if (is.data.frame(values) || is.matrix(values)) {
    values <- as.data.frame(values, stringsAsFactors = FALSE)
    if (ncol(values) == 1L && isTRUE(force) && !is.null(name)) names(values) <- name
    for (nm in names(values)) {
      if (nrow(values) != n) {
        stop("`", nm, "` has ", nrow(values), " value(s) but ", n,
             " pixel(s) were supplied.", call. = FALSE)
      }
      pool[[nm]] <- as.numeric(values[[nm]])
    }
    return(pool)
  }
  if (length(values) != n) {
    stop("`", name, "` has ", length(values), " value(s) but ", n,
         " pixel(s) were supplied.", call. = FALSE)
  }
  pool[[name]] <- as.numeric(values)
  pool
}

#' Compute a covariate frame from pixel values
#'
#' The single definition of "how a covariate is computed from a pixel".
#' Extraction, map prediction and the full-resolution export all route
#' through this function, so training and prediction can never diverge.
#'
#' Indices are computed by packing the pixels into a one-row raster
#' ([synthesize_pixel_raster()]) and calling [compute_spectral_indices()].
#' Biomass proxies get the CHM as an extra layer on that **same** one-row
#' grid, so `terra::compareGeom()` inside [compute_biomass_proxies()]
#' succeeds and its no-resample branch is taken - resampling a one-row
#' raster would be meaningless.
#'
#' @param band_values Numeric matrix / data frame of reflectance values, one
#'   column per band (`Blue`, `Green`, `Red`, `RedEdge`, `NIR`, ...).
#' @param selected Character vector of covariate ids to return, in order.
#' @param chm_values Optional canopy-height values (one per pixel).
#' @param custom_values Optional custom-index values: a named vector, or a
#'   data frame whose column names are the covariate ids.
#' @param dsm_values,dtm_values Optional terrain values (one per pixel).
#' @param crs CRS for the synthetic grid; must be the same for all stacks
#'   compared inside one call.
#' @return A data frame with `nrow(band_values)` rows and columns named
#'   exactly `selected`, in that order.
#' @examples
#' bands <- data.frame(Green = c(0.2, 0.3), Red = c(0.1, 0.15),
#'                     RedEdge = c(0.3, 0.35), NIR = c(0.6, 0.7))
#' covariate_frame_from_pixels(bands, c("NDVI", "NDRE", "Red"))
#' @export
covariate_frame_from_pixels <- function(band_values, selected,
                                        chm_values = NULL,
                                        custom_values = NULL,
                                        dsm_values = NULL,
                                        dtm_values = NULL,
                                        crs = "") {
  selected <- as.character(selected)
  if (length(selected) == 0L) {
    stop("`selected` must name at least one covariate.", call. = FALSE)
  }
  band_df <- as.data.frame(band_values, stringsAsFactors = FALSE)
  n <- nrow(band_df)
  if (n == 0L) {
    stop("`band_values` has no rows.", call. = FALSE)
  }

  pool <- band_df
  pool <- .add_pixel_column(pool, chm_values, "CHM_m", n)
  pool <- .add_pixel_column(pool, dsm_values, "DSM", n)
  pool <- .add_pixel_column(pool, dtm_values, "DTM", n)
  pool <- .add_pixel_column(pool, custom_values, "custom_index", n, force = FALSE)

  indices <- NULL
  if (length(setdiff(selected, names(pool))) > 0L) {
    synth <- synthesize_pixel_raster(band_df, crs)
    indices <- tryCatch(
      compute_spectral_indices(synth),
      error = function(e) {
        stop("Cannot compute spectral indices for ",
             paste(setdiff(selected, names(pool)), collapse = ", "), ": ",
             conditionMessage(e), call. = FALSE)
      }
    )
    idx_df <- as.data.frame(terra::values(indices), stringsAsFactors = FALSE)
    for (nm in setdiff(names(idx_df), names(pool))) pool[[nm]] <- idx_df[[nm]]
  }

  if (length(setdiff(selected, names(pool))) > 0L && !is.null(indices)) {
    chm_synth <- NULL
    if (!is.null(chm_values)) {
      chm_col <- if (is.data.frame(chm_values) || is.matrix(chm_values)) {
        as.numeric(as.data.frame(chm_values)[[1L]])
      } else {
        as.numeric(chm_values)
      }
      # Same one-row geometry as `indices` so compareGeom() passes and
      # compute_biomass_proxies() keeps every product per-pixel.
      chm_synth <- synthesize_pixel_raster(
        stats::setNames(data.frame(chm_col), "CHM_m"), crs
      )
    }
    proxies <- tryCatch(
      compute_biomass_proxies(indices, chm_synth),
      error = function(e) NULL
    )
    if (!is.null(proxies)) {
      px_df <- as.data.frame(terra::values(proxies), stringsAsFactors = FALSE)
      for (nm in setdiff(names(px_df), names(pool))) pool[[nm]] <- px_df[[nm]]
    }
  }

  missing <- setdiff(selected, names(pool))
  if (length(missing) > 0L) {
    stop("Cannot supply covariate(s): ", paste(missing, collapse = ", "),
         ". Available from these pixels: ",
         paste(names(pool), collapse = ", "), ".", call. = FALSE)
  }
  out <- pool[, selected, drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Extract windowed covariates at field sample points
#'
#' The extraction pipeline for the Field Models tab. A covariate value at a
#' field point is the aggregate (`fun`) over the `window` x `window` block of
#' **per-pixel covariate values computed at native resolution** - indices are
#' never computed from block means during training.
#'
#' Terrain layers and a custom index are sampled bilinearly at the window
#' pixel centres, which is mathematically identical to resample-then-extract
#' and avoids a whole-raster `terra::resample()`.
#'
#' @param points Field samples from [prepare_field_table()] (or any `sf`
#'   POINT layer).
#' @param reflectance Reflectance `SpatRaster` defining the grid.
#' @param selected Covariate ids to extract, in order (see
#'   [field_covariate_catalogue()]).
#' @param window Odd window size in pixels (see [window_cells()]). Ignored
#'   when `window_m` is supplied.
#' @param window_m Support in metres instead of pixels. A window in pixels
#'   spans a different area at every ground sampling distance, so a quadrat
#'   size is better stated in metres and converted per survey: this rounds to
#'   the nearest odd pixel count for `reflectance` and errors when the request
#'   falls outside the supported range. See [window_from_metres()].
#' @param fun Aggregation: `"mean"`, `"median"`, `"max"`, `"min"`, `"sd"`.
#' @param custom_index Optional single-layer `SpatRaster` with a user index.
#' @param chm,dsm,dtm Optional terrain `SpatRaster`s.
#' @return A plain data frame with one row per input point, in input order:
#'   every original attribute, then one numeric column per `selected` id in
#'   that order, then `.n_valid_px`, `.window_px`, `.window_valid_frac` and
#'   `.in_extent`. Out-of-extent points keep their row with all-`NA`
#'   covariates. Carries `window_px`, `window_m`, `window_fun`, `crs` and
#'   `reference_geom` attributes describing the training grid.
#' @examples
#' ortho <- system.file("extdata", "micasense_subset.tif", package = "DroneBioR")
#' refl <- scale_to_reflectance(read_multispectral_orthomosaic(ortho)$bands)
#' pts <- read_field_points(
#'   system.file("extdata", "field_samples.csv", package = "DroneBioR"),
#'   crs = 32617
#' )
#' tab <- extract_field_covariates(pts, refl, c("NDVI", "NDRE"), window = 3)
#' head(tab[, c("sample_id", "NDVI", "NDRE", ".n_valid_px")])
#' @export
extract_field_covariates <- function(points, reflectance, selected,
                                     window = 1L, window_m = NULL, fun = "mean",
                                     custom_index = NULL, chm = NULL,
                                     dsm = NULL, dtm = NULL) {
  if (!inherits(reflectance, "SpatRaster")) {
    stop("`reflectance` must be a terra SpatRaster.", call. = FALSE)
  }
  window <- .resolve_window(window, window_m, reflectance)
  fun <- match.arg(fun, c("mean", "median", "max", "min", "sd"))
  selected <- as.character(selected)
  if (length(selected) == 0L) {
    stop("Select at least one covariate before extracting.", call. = FALSE)
  }

  wc <- window_cells(reflectance, points, window)
  window <- as.integer(window)
  n_pts <- length(wc$in_extent)
  n_ok <- length(wc$index)
  crs_str <- terra::crs(reflectance)

  attrs <- if (inherits(points, "sf")) {
    as.data.frame(sf::st_drop_geometry(points), stringsAsFactors = FALSE)
  } else {
    as.data.frame(points, stringsAsFactors = FALSE)
  }
  rownames(attrs) <- NULL

  cov_cols <- stats::setNames(
    replicate(length(selected), rep(NA_real_, n_pts), simplify = FALSE),
    selected
  )
  n_valid <- rep(NA_integer_, n_pts)

  if (n_ok > 0L) {
    flat <- as.vector(t(wc$cells))
    band_values <- terra::extract(reflectance, flat)
    band_values <- band_values[, names(reflectance), drop = FALSE]

    xy <- terra::xyFromCell(reflectance[[1L]], flat)
    sample_aux <- function(r) {
      if (is.null(r)) return(NULL)
      terra::extract(r[[1L]], xy, method = "bilinear")[[1L]]
    }
    chm_values <- sample_aux(chm)
    dsm_values <- sample_aux(dsm)
    dtm_values <- sample_aux(dtm)
    custom_values <- NULL
    if (!is.null(custom_index)) {
      cv <- terra::extract(custom_index[[1L]], xy, method = "bilinear")
      custom_values <- stats::setNames(
        data.frame(as.numeric(cv[[1L]])), names(custom_index)[[1L]]
      )
    }

    px <- covariate_frame_from_pixels(
      band_values, selected,
      chm_values    = chm_values,
      custom_values = custom_values,
      dsm_values    = dsm_values,
      dtm_values    = dtm_values,
      crs           = crs_str
    )

    w2 <- ncol(wc$cells)
    for (nm in selected) {
      m <- matrix(as.numeric(px[[nm]]), nrow = n_ok, ncol = w2, byrow = TRUE)
      cov_cols[[nm]][wc$index] <- .row_aggregate(m, fun, na.rm = TRUE)
    }
    first_band <- matrix(as.numeric(band_values[[1L]]),
                         nrow = n_ok, ncol = w2, byrow = TRUE)
    n_valid[wc$index] <- as.integer(rowSums(!is.na(first_band)))
  }

  # A field file carrying a column named like a covariate (NDVI, CHM_m, ...)
  # would leave two of that name in the table, and every downstream selector
  # reads the FIRST -- the stale value from the file, not the one just
  # extracted -- so the model would train on the wrong numbers in silence.
  # The extracted values are the authoritative ones; keep them and say so.
  reserved <- c(names(cov_cols), ".n_valid_px", ".window_px",
                ".window_valid_frac", ".in_extent")
  shadowed <- intersect(names(attrs), reserved)
  if (length(shadowed) > 0L) {
    warning("Field-file column(s) ", paste(sprintf("`%s`", shadowed), collapse = ", "),
            " were dropped: they share a name with an extracted covariate, and ",
            "the extracted values are the ones the model must use.",
            call. = FALSE)
    attrs <- attrs[, setdiff(names(attrs), shadowed), drop = FALSE]
  }

  out <- cbind(
    attrs,
    as.data.frame(cov_cols, check.names = FALSE, stringsAsFactors = FALSE),
    data.frame(
      .n_valid_px        = n_valid,
      .window_px         = rep(as.integer(window^2L), n_pts),
      .window_valid_frac = n_valid / (window^2L),
      .in_extent         = wc$in_extent,
      check.names        = FALSE
    ),
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL

  attr(out, "window_px") <- window
  # the metric support the pixel window actually spans on this raster, so a
  # table always states its own footprint in ground units
  attr(out, "window_m") <- tryCatch(
    as.integer(window) * min(terra::res(reflectance)),
    error = function(e) NA_real_)
  attr(out, "window_fun") <- fun
  attr(out, "crs") <- crs_str
  attr(out, "reference_geom") <- list(
    res    = as.numeric(terra::res(reflectance)),
    extent = as.numeric(terra::ext(reflectance)[1:4]),
    nrow   = terra::nrow(reflectance),
    ncol   = terra::ncol(reflectance),
    epsg   = suppressWarnings(as.integer(sf::st_crs(crs_str)$epsg))
  )
  out
}
