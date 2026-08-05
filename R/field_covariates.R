# =============================================================================
# Covariate vocabulary and map-side plumbing for the Field Models tab
#
# field_covariate_catalogue() is pure metadata - it never touches a raster,
# so the picker is instant even when the project holds a 400 Mpx ortho. The
# `id` column is the canonical covariate name and is the contract shared by
# extraction, training, prediction and export.
# =============================================================================

# Greenness x height proxies and the index each one needs. Mirrors the
# `candidates` list in compute_biomass_proxies(); keep the two in step.
.proxy_requirements <- c(
  Biomass_NDVI_x_CHM  = "NDVI",
  Biomass_NDRE_x_CHM  = "NDRE",
  Biomass_SAVI_x_CHM  = "SAVI",
  Biomass_GNDVI_x_CHM = "GNDVI",
  Biomass_VARI_x_CHM  = "VARI",
  Biomass_EXG_x_CHM   = "ExG",
  Biomass_MGRVI_x_CHM = "MGRVI",
  Biomass_RGBVI_x_CHM = "RGBVI"
)

# Layers that are never useful as model covariates: the alpha channel and
# the no-data mask are constant inside the survey footprint, so they add a
# column of zero variance that some caret models refuse outright.
.non_covariate_layers <- c("alpha", "valid_data_mask", "mask")

# With ~30 field plots, offering 40 ticked covariates is p >> n. Only the
# four workhorse indices plus canopy height are pre-ticked.
.recommended_covariates <- c("NDVI", "NDRE", "SAVI", "GNDVI", "CHM_m")

#' Catalogue the covariates available for field modelling
#'
#' Pure metadata: no raster is read, so the covariate picker can be rendered
#' before any product is loaded into memory. Proxy rows appear only when
#' their prerequisites hold, so the user cannot tick a covariate that
#' extraction would then fail to supply.
#'
#' @param band_names Layer names of the reflectance stack.
#' @param index_names Layer names of the spectral-index stack.
#' @param custom_index_name Optional name of a user-defined index layer.
#' @param has_chm,has_dsm,has_dtm Whether those terrain products exist.
#' @return A data frame with `id`, `label`, `group`, `source`, `recommended`
#'   and `note`. `group` is one of `Reflectance bands`, `Spectral indices`,
#'   `Biomass proxies`, `Terrain`.
#' @examples
#' field_covariate_catalogue(
#'   band_names = c("Red", "NIR"),
#'   index_names = c("NDVI", "SAVI", "NDRE"),
#'   has_chm = TRUE
#' )
#' @export
field_covariate_catalogue <- function(band_names = character(),
                                      index_names = character(),
                                      custom_index_name = NULL,
                                      has_chm = FALSE,
                                      has_dsm = FALSE,
                                      has_dtm = FALSE) {
  band_names <- setdiff(as.character(band_names), .non_covariate_layers)
  index_names <- setdiff(as.character(index_names), .non_covariate_layers)

  rows <- list()
  add <- function(id, label, group, source, note = "") {
    rows[[length(rows) + 1L]] <<- data.frame(
      id = id, label = label, group = group, source = source,
      recommended = id %in% .recommended_covariates,
      note = note, stringsAsFactors = FALSE
    )
  }

  for (b in band_names) {
    add(b, sprintf("%s reflectance", b), "Reflectance bands", "Orthomosaic",
        "Per-pixel surface reflectance.")
  }
  for (ix in index_names) {
    add(ix, ix, "Spectral indices", "Spectral Analytics",
        "Computed per pixel at native resolution.")
  }
  if (!is.null(custom_index_name) && nzchar(custom_index_name)) {
    add(custom_index_name, custom_index_name, "Spectral indices",
        "Custom index", "User-defined band-maths layer.")
  }

  # Biomass_Spectral has two routes inside compute_biomass_proxies(): the
  # NDVI/SAVI/NDRE mean, or a VARI fallback for RGB-only surveys.
  if (all(c("NDVI", "SAVI", "NDRE") %in% index_names)) {
    add("Biomass_Spectral", "Biomass_Spectral (mean NDVI/SAVI/NDRE)",
        "Biomass proxies", "Biomass proxies",
        "Mean of NDVI, SAVI and NDRE clipped to [-1, 1].")
  } else if ("VARI" %in% index_names) {
    add("Biomass_Spectral", "Biomass_Spectral (VARI fallback)",
        "Biomass proxies", "Biomass proxies",
        "RGB-only fallback: clipped VARI.")
  }
  if (isTRUE(has_chm)) {
    for (id in names(.proxy_requirements)) {
      need <- .proxy_requirements[[id]]
      if (need %in% index_names) {
        add(id, sprintf("%s (%s x canopy height)", id, need),
            "Biomass proxies", "Biomass proxies",
            sprintf("%s multiplied by the CHM in metres.", need))
      }
    }
  }

  if (isTRUE(has_chm)) {
    add("CHM_m", "Canopy height (m)", "Terrain", "3D Modeling (CHM)",
        "Canopy Height Model, metres above ground.")
  }
  if (isTRUE(has_dsm)) {
    add("DSM", "Digital surface model (m)", "Terrain", "3D Modeling (DSM)",
        "Absolute surface elevation; usually only useful with a DTM.")
  }
  if (isTRUE(has_dtm)) {
    add("DTM", "Digital terrain model (m)", "Terrain", "3D Modeling (DTM)",
        "Bare-earth elevation.")
  }

  if (length(rows) == 0L) {
    return(data.frame(id = character(), label = character(), group = character(),
                      source = character(), recommended = logical(),
                      note = character(), stringsAsFactors = FALSE))
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# Canonical spellings for the layers that arrive under several names across
# the package: products.R writes "CHM", the studio labels it "CHM_m",
# biomass_mapping.R uses "chm", and GDAL hands back lowercase names off disk.
.canonical_covariate_name <- function(x) {
  key <- tolower(trimws(x))
  out <- x
  out[key %in% c("chm", "chm_m", "canopy_height", "canopy_height_m")] <- "CHM_m"
  out[key %in% c("dsm", "dsm_m", "surface", "surface_m")] <- "DSM"
  out[key %in% c("dtm", "dtm_m", "dem", "terrain", "terrain_m")] <- "DTM"
  out
}

#' Canonicalise covariate / layer names
#'
#' Applied identically to the training frame and to every prediction stack,
#' so a CHM that arrived as `CHM`, `chm` or `CHM_m` becomes one name and
#' `terra::predict()` never fails on a mismatch.
#'
#' True duplicates are an error rather than a silent rename: a stack with two
#' layers called `CHM_m` would make `terra::predict()` fail much later with
#' `duplicate names in SpatRaster`.
#'
#' @param x Character vector of names.
#' @param warn Emit a warning listing the renames.
#' @return A named character vector mapping each original name to its
#'   normalised form.
#' @examples
#' normalize_covariate_names(c("NDVI", "chm", "dsm"), warn = FALSE)
#' @export
normalize_covariate_names <- function(x, warn = TRUE) {
  x <- as.character(x)
  if (length(x) == 0L) return(stats::setNames(character(), character()))
  canon <- .canonical_covariate_name(x)
  dup <- unique(canon[duplicated(canon)])
  if (length(dup) > 0L) {
    stop("Duplicate covariate name(s) after normalisation: ",
         paste(dup, collapse = ", "),
         ". Rename or drop one of the layers before modelling.", call. = FALSE)
  }
  out <- make.names(canon, unique = TRUE)
  changed <- out != x
  if (isTRUE(warn) && any(changed)) {
    warning("Renamed covariate(s): ",
            paste(sprintf("%s -> %s", x[changed], out[changed]), collapse = ", "),
            call. = FALSE)
  }
  stats::setNames(out, x)
}

#' Build an aggregated covariate stack for map prediction
#'
#' The fast map path: aggregate the reflectance first, then compute indices,
#' proxies and terrain layers on the small grid. `fact` defaults to the
#' training window whenever the cell budget allows, so one map cell matches
#' the support the model was trained on.
#'
#' **Approximation:** indices here come from block-mean reflectance, whereas
#' training used the mean of native-resolution indices. The two differ
#' slightly because the index formulas are nonlinear. Use
#' [export_field_biomass_map()] for the exact surface.
#'
#' @param reflectance Reflectance `SpatRaster`.
#' @param covariates Covariate ids to supply, in order (`model$predictors`).
#' @param max_cells Approximate cell budget for the aggregated grid.
#' @param window Extraction window in pixels the model was trained with.
#'   Ignored when `window_m` is supplied.
#' @param window_m The same support expressed in metres, converted to the
#'   nearest odd pixel count for `reflectance`. Prefer this when the
#'   calibration table was extracted with `window_m`, so the prediction stack
#'   matches the support the model saw. See [window_from_metres()].
#' @param custom_index Optional custom index `SpatRaster`.
#' @param chm,dsm,dtm Optional terrain `SpatRaster`s.
#' @return A `SpatRaster` whose `names()` are exactly `covariates`, in order,
#'   with `fact` and `cell_size_m` attributes.
#' @examples
#' ortho <- system.file("extdata", "micasense_subset.tif", package = "DroneBioR")
#' refl <- scale_to_reflectance(read_multispectral_orthomosaic(ortho)$bands)
#' stack <- build_prediction_stack(refl, c("NDVI", "NDRE"), max_cells = 500)
#' names(stack)
#' @export
build_prediction_stack <- function(reflectance, covariates, max_cells = 1e6,
                                   window = 1L, window_m = NULL,
                                   custom_index = NULL,
                                   chm = NULL, dsm = NULL, dtm = NULL) {
  if (!inherits(reflectance, "SpatRaster")) {
    stop("`reflectance` must be a terra SpatRaster.", call. = FALSE)
  }
  # A prediction stack must be aggregated to the same support the model was
  # calibrated on, so this accepts the metric form too.
  window <- .resolve_window(window, window_m, reflectance)
  covariates <- as.character(covariates)
  if (length(covariates) == 0L) {
    stop("`covariates` must name at least one layer.", call. = FALSE)
  }
  window <- as.integer(window)
  n_cell <- terra::ncell(reflectance)

  fact <- if (n_cell / window^2 <= max_cells) {
    window
  } else {
    max(1L, floor(sqrt(n_cell / max_cells)))
  }
  small <- if (fact > 1L) {
    terra::aggregate(reflectance, fact = fact, fun = "mean", na.rm = TRUE)
  } else {
    reflectance
  }

  pool <- small
  add_layer <- function(pool, r, nm) {
    if (is.null(r)) return(pool)
    if (nm %in% names(pool)) return(pool)
    rr <- terra::resample(r[[1L]], small[[1L]], method = "bilinear")
    names(rr) <- nm
    c(pool, rr)
  }
  pool <- add_layer(pool, chm, "CHM_m")
  pool <- add_layer(pool, dsm, "DSM")
  pool <- add_layer(pool, dtm, "DTM")
  if (!is.null(custom_index)) {
    pool <- add_layer(pool, custom_index, names(custom_index)[[1L]])
  }

  indices <- NULL
  if (length(setdiff(covariates, names(pool))) > 0L) {
    indices <- tryCatch(compute_spectral_indices(small), error = function(e) NULL)
    if (!is.null(indices)) {
      keep <- setdiff(names(indices), names(pool))
      if (length(keep) > 0L) pool <- c(pool, indices[[keep]])
    }
  }
  if (length(setdiff(covariates, names(pool))) > 0L && !is.null(indices)) {
    chm_small <- if ("CHM_m" %in% names(pool)) pool[["CHM_m"]] else NULL
    proxies <- tryCatch(compute_biomass_proxies(indices, chm_small),
                        error = function(e) NULL)
    if (!is.null(proxies)) {
      keep <- setdiff(names(proxies), names(pool))
      if (length(keep) > 0L) pool <- c(pool, proxies[[keep]])
    }
  }

  missing <- setdiff(covariates, names(pool))
  if (length(missing) > 0L) {
    stop("Cannot build prediction layer(s): ", paste(missing, collapse = ", "),
         ". Load the products they depend on (a CHM for the *_x_CHM proxies) ",
         "and try again.", call. = FALSE)
  }

  out <- pool[[covariates]]
  names(out) <- covariates
  attr(out, "fact") <- as.integer(fact)
  attr(out, "cell_size_m") <- as.numeric(terra::res(out)[1L])
  out
}

#' Quantile breaks and a robust stretch for a biomass map
#'
#' Large maps are sampled rather than read in full; regular sampling
#' reproduces the full-raster quantiles closely enough for a legend and
#' costs a fraction of the I/O.
#'
#' @param map Single-layer biomass `SpatRaster`.
#' @param n Number of classes.
#' @param sample_size Cell budget for `terra::spatSample()`.
#' @return A list with `breaks` (length `n + 1`), `quartiles`, `p01`, `p99`
#'   and `labels`.
#' @examples
#' r <- terra::rast(nrows = 20, ncols = 20)
#' terra::values(r) <- seq_len(terra::ncell(r))
#' biomass_map_breaks(r, n = 4)$breaks
#' @export
biomass_map_breaks <- function(map, n = 4L, sample_size = 1e5) {
  if (!inherits(map, "SpatRaster")) {
    stop("`map` must be a terra SpatRaster.", call. = FALSE)
  }
  n <- as.integer(n)
  if (n < 2L) stop("`n` must be at least 2.", call. = FALSE)
  layer <- map[[1L]]

  v <- if (terra::ncell(layer) > sample_size) {
    terra::spatSample(layer, size = sample_size, method = "regular",
                      na.rm = TRUE, as.df = TRUE)[[1L]]
  } else {
    terra::values(layer)[, 1L]
  }
  v <- v[is.finite(v)]
  if (length(v) < 2L) {
    stop("The biomass map has fewer than two finite cells.", call. = FALSE)
  }

  breaks <- unname(stats::quantile(v, probs = seq(0, 1, length.out = n + 1L)))
  # Ties (a flat map, or a heavily clamped one) collapse classes; keep the
  # break vector strictly increasing so terra::classify() stays valid.
  breaks <- unique(breaks)
  if (length(breaks) < 2L) {
    stop("The biomass map is constant; there is nothing to classify.",
         call. = FALSE)
  }
  labels <- sprintf("%.0f - %.0f", breaks[-length(breaks)], breaks[-1L])
  list(
    breaks    = breaks,
    quartiles = unname(stats::quantile(v, c(0.25, 0.5, 0.75))),
    p01       = unname(stats::quantile(v, 0.01)),
    p99       = unname(stats::quantile(v, 0.99)),
    labels    = labels
  )
}

#' Classify a biomass map into labelled classes
#'
#' @param map Single-layer biomass `SpatRaster`.
#' @param breaks Cut values, as returned by [biomass_map_breaks()].
#' @param labels Optional class labels; defaults to `"lo - hi"` ranges.
#' @return A categorical `SpatRaster` named `biomass_class`.
#' @examples
#' r <- terra::rast(nrows = 20, ncols = 20)
#' terra::values(r) <- seq_len(terra::ncell(r))
#' brk <- biomass_map_breaks(r, n = 4)
#' terra::freq(classify_biomass_map(r, brk$breaks))
#' @export
classify_biomass_map <- function(map, breaks, labels = NULL) {
  if (!inherits(map, "SpatRaster")) {
    stop("`map` must be a terra SpatRaster.", call. = FALSE)
  }
  breaks <- sort(unique(as.numeric(breaks)))
  if (length(breaks) < 2L) {
    stop("`breaks` needs at least two distinct cut values.", call. = FALSE)
  }
  if (is.null(labels)) {
    labels <- sprintf("%.0f - %.0f", breaks[-length(breaks)], breaks[-1L])
  }
  n_class <- length(breaks) - 1L
  if (length(labels) != n_class) {
    stop("`labels` must have ", n_class, " entries for ", length(breaks),
         " breaks.", call. = FALSE)
  }

  out <- terra::classify(map[[1L]], rcl = breaks, include.lowest = TRUE,
                         brackets = TRUE)
  levels(out) <- data.frame(ID = seq_len(n_class) - 1L, biomass_class = labels)
  names(out) <- "biomass_class"
  out
}
