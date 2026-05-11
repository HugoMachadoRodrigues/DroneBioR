#' Survey-grade volume calculations over a region of interest
#'
#' Computes the volume between a "top" surface (a DSM, a CHM, or any other
#' height raster) and a chosen base reference, broken into cut (height
#' above the base), fill (height below the base) and net volumes. The
#' base reference is chosen with `method`, and matches the conventions
#' used by photogrammetric surveyors for stockpiles, earthworks and
#' biomass calculations.
#'
#' @details
#'
#' The volume math is the standard raster integral
#' \deqn{V = \sum_i (z_i^{top} - z_i^{base}) \cdot A_{cell}}
#' clipped to positive (cut) and negative (fill) contributions, where
#' the sum runs over every cell of `top` whose centre lies inside the
#' ROI polygon. The base surface is built on the same grid as the
#' clipped `top`, so cut and fill are computed cell-by-cell with no
#' resampling artefact between top and base.
#'
#' Base-reference methods:
#'
#' \describe{
#'   \item{`"dtm"`}{Use a separate DTM (bare-earth model) as the base.
#'     This is the correct method for canopy biomass when a DTM is
#'     available: the cut volume reduces to \eqn{\int (DSM - DTM) dA},
#'     i.e. the canopy height model integrated over the ROI.}
#'   \item{`"min_z"`}{Constant plane at the minimum top-surface value
#'     inside the ROI. The classic stockpile method when no separate
#'     bare-earth model exists.}
#'   \item{`"mean_z"`}{Constant plane at the mean top-surface value.}
#'   \item{`"ground_quantile"`}{Constant plane at a low quantile of the
#'     top surface (default 5th percentile). A robust proxy for the
#'     ground when there is no DTM and `"min_z"` is too sensitive to a
#'     single dark pixel.}
#'   \item{`"user_plane"`}{Constant plane at the user-supplied `base_z`.}
#'   \item{`"perimeter_tin"`}{Triangulated irregular network built from
#'     the top-surface values sampled at the ROI's perimeter vertices.
#'     This is the industry standard for stockpile surveys (used by
#'     Pix4D Mapper, Bentley ContextCapture, Trimble Business Center):
#'     the perimeter rests on the surveyed "natural ground line", a
#'     Delaunay triangulation interpolates linearly between those
#'     vertices, and the volume is the integral of `top - TIN` inside
#'     the ROI. Requires the optional `interp` package.}
#' }
#'
#' Both planimetric and 3D draped surface areas are reported. The
#' draped area is the sum of cell areas inflated by the secant of the
#' local slope, \eqn{A_{drape} = \sum_i A_{cell} \cdot \sec(\theta_i)},
#' which is the standard formula in
#' photogrammetric reporting.
#'
#' @param top A `terra::SpatRaster` containing the top surface (DSM,
#'   CHM, etc.). Must be in a projected CRS with linear units;
#'   otherwise volumes are not meaningful.
#' @param roi A data frame with `x` and `y` columns describing the ROI
#'   polygon in the same CRS as `top`. At least three vertices. The
#'   ring is closed automatically.
#' @param method Base-reference method. One of `"dtm"`, `"min_z"`,
#'   `"mean_z"`, `"ground_quantile"`, `"user_plane"`, `"perimeter_tin"`.
#' @param dtm Required when `method = "dtm"`. A `terra::SpatRaster`
#'   bare-earth DTM. Resampled to the `top` grid if geometries differ.
#' @param base_z Required when `method = "user_plane"`. Numeric.
#' @param ground_quantile Quantile in `[0, 1]` used by
#'   `method = "ground_quantile"`. Default `0.05`.
#'
#' @return A list with class `"dronebio_survey_volume"`:
#'
#' \describe{
#'   \item{`method`}{Method used.}
#'   \item{`cut_volume_m3`}{Volume above the base (m^3).}
#'   \item{`fill_volume_m3`}{Volume below the base (m^3, positive).}
#'   \item{`net_volume_m3`}{`cut - fill` (m^3).}
#'   \item{`surface_area_planar_m2`}{Projected (planimetric) area of
#'     valid cells inside the ROI.}
#'   \item{`surface_area_draped_m2`}{3D surface area via the cell-wise
#'     slope tangent; `NA` if `terra::terrain()` cannot be computed
#'     (e.g. ROI < 3x3 cells).}
#'   \item{`perimeter_m`}{Planimetric polygon perimeter.}
#'   \item{`cell_count`}{Number of valid `top`-cells inside the ROI.}
#'   \item{`cell_area_m2`}{Grid cell area, m^2.}
#'   \item{`top_z_summary`}{Named vector `min/median/mean/max` of the
#'     top surface inside the ROI.}
#'   \item{`base_z_summary`}{Same for the base surface.}
#'   \item{`height_summary`}{Same for `(top - base)`.}
#'   \item{`base_reference_text`}{Human-readable description of the
#'     base method.}
#' }
#'
#' @examples
#' dsm <- terra::rast(system.file("extdata", "dsm_subset.tif", package = "DroneBioR"))
#' dtm <- terra::rast(system.file("extdata", "dtm_subset.tif", package = "DroneBioR"))
#' roi <- data.frame(
#'   x = c(392004, 392012, 392012, 392004),
#'   y = c(3033004, 3033004, 3033012, 3033012)
#' )
#' result <- compute_survey_volumes(top = dsm, roi = roi, method = "dtm", dtm = dtm)
#' result$cut_volume_m3
#'
#' # Constant-plane stockpile-style:
#' result_min <- compute_survey_volumes(top = dsm, roi = roi, method = "min_z")
#' result_min$cut_volume_m3
#' @export
compute_survey_volumes <- function(top, roi,
                                   method = c("dtm", "min_z", "mean_z",
                                              "ground_quantile",
                                              "user_plane",
                                              "perimeter_tin"),
                                   dtm = NULL,
                                   base_z = NULL,
                                   ground_quantile = 0.05) {
  method <- match.arg(method)

  if (!inherits(top, "SpatRaster")) {
    stop("`top` must be a terra SpatRaster.", call. = FALSE)
  }
  if (!is.data.frame(roi) || !all(c("x", "y") %in% names(roi)) || nrow(roi) < 3) {
    stop("`roi` must be a data frame with `x` and `y` columns and at least 3 vertices.",
         call. = FALSE)
  }
  if (identical(method, "dtm") && !inherits(dtm, "SpatRaster")) {
    stop("`method = 'dtm'` requires a SpatRaster `dtm`.", call. = FALSE)
  }
  if (identical(method, "user_plane") && (is.null(base_z) || !is.finite(base_z))) {
    stop("`method = 'user_plane'` requires a numeric `base_z`.", call. = FALSE)
  }
  if (!is.finite(ground_quantile) || ground_quantile < 0 || ground_quantile > 1) {
    stop("`ground_quantile` must be in [0, 1].", call. = FALSE)
  }

  # Close the ring if needed.
  first <- as.numeric(roi[1L, c("x", "y")])
  last  <- as.numeric(roi[nrow(roi), c("x", "y")])
  if (!isTRUE(all.equal(first, last, tolerance = 1e-9))) {
    roi <- rbind(roi[, c("x", "y")], roi[1L, c("x", "y")])
  }
  coords <- as.matrix(roi[, c("x", "y")])

  poly <- terra::vect(list(coords), type = "polygons", crs = terra::crs(top))

  # terra::crop errors when the polygon does not intersect the raster
  # at all, rather than returning an empty raster. We translate that
  # into the same NA-filled return value as "ROI inside raster but no
  # finite cells", so callers do not have to special-case it.
  top_clip <- tryCatch(
    terra::mask(terra::crop(top, poly), poly),
    error = function(e) NULL
  )
  if (is.null(top_clip)) {
    return(.empty_survey_volume(method))
  }
  top_values <- terra::values(top_clip, mat = FALSE)
  finite_mask <- is.finite(top_values)
  if (sum(finite_mask) == 0L) {
    return(.empty_survey_volume(method))
  }

  cell_area <- prod(abs(terra::res(top_clip)))

  base_clip <- switch(
    method,
    "dtm" = {
      d <- if (terra::compareGeom(top_clip, dtm, stopOnError = FALSE)) dtm
           else terra::resample(dtm, top_clip, method = "bilinear")
      terra::mask(terra::crop(d, poly), poly)
    },
    "min_z" = .raster_of_constant(top_clip, min(top_values[finite_mask])),
    "mean_z" = .raster_of_constant(top_clip, mean(top_values[finite_mask])),
    "ground_quantile" = .raster_of_constant(
      top_clip,
      as.numeric(stats::quantile(top_values[finite_mask],
                                 probs = ground_quantile,
                                 na.rm = TRUE, names = FALSE))
    ),
    "user_plane"    = .raster_of_constant(top_clip, base_z),
    "perimeter_tin" = .perimeter_tin_raster(top_clip, coords)
  )
  base_values <- terra::values(base_clip, mat = FALSE)

  delta <- top_values - base_values
  cut_per_cell  <- ifelse(is.finite(delta) & delta > 0,  delta, 0)
  fill_per_cell <- ifelse(is.finite(delta) & delta < 0, -delta, 0)

  cut_v  <- sum(cut_per_cell)  * cell_area
  fill_v <- sum(fill_per_cell) * cell_area
  net_v  <- cut_v - fill_v

  drape_area <- tryCatch({
    s <- terra::terrain(top_clip, v = "slope", unit = "radians", neighbors = 8)
    sec_slope <- 1 / cos(terra::values(s, mat = FALSE))
    sec_slope[!is.finite(sec_slope) | sec_slope < 1] <- 1
    sum(sec_slope, na.rm = TRUE) * cell_area
  }, error = function(e) NA_real_)

  list_summary <- function(v) {
    v <- v[is.finite(v)]
    if (length(v) == 0L) {
      return(c(min = NA_real_, median = NA_real_, mean = NA_real_, max = NA_real_))
    }
    c(min    = min(v),
      median = stats::median(v),
      mean   = mean(v),
      max    = max(v))
  }

  cell_count <- sum(finite_mask)

  out <- list(
    method                  = method,
    cut_volume_m3           = cut_v,
    fill_volume_m3          = fill_v,
    net_volume_m3           = net_v,
    surface_area_planar_m2  = cell_count * cell_area,
    surface_area_draped_m2  = drape_area,
    perimeter_m             = .polygon_perimeter(coords),
    cell_count              = cell_count,
    cell_area_m2            = cell_area,
    top_z_summary           = list_summary(top_values),
    base_z_summary          = list_summary(base_values),
    height_summary          = list_summary(delta),
    base_reference_text     = .method_description(method, ground_quantile, base_z)
  )
  class(out) <- "dronebio_survey_volume"
  out
}

#' @export
print.dronebio_survey_volume <- function(x, ...) {
  cat("DroneBioR survey volume\n")
  cat("Method:        ", x$method, " - ", x$base_reference_text, "\n", sep = "")
  cat(sprintf("Cut volume:    %.3f m^3\n", x$cut_volume_m3))
  cat(sprintf("Fill volume:   %.3f m^3\n", x$fill_volume_m3))
  cat(sprintf("Net volume:    %.3f m^3\n", x$net_volume_m3))
  cat(sprintf("Planar area:   %.3f m^2\n", x$surface_area_planar_m2))
  cat(sprintf("Draped area:   %.3f m^2\n", x$surface_area_draped_m2))
  cat(sprintf("Perimeter:     %.3f m\n", x$perimeter_m))
  cat(sprintf("Cell area:     %.4f m^2 (%d cells inside ROI)\n",
              x$cell_area_m2, x$cell_count))
  invisible(x)
}

.raster_of_constant <- function(template, value) {
  r <- terra::rast(template)
  terra::values(r) <- as.numeric(value)
  terra::mask(r, template)
}

.polygon_perimeter <- function(coords) {
  sum(sqrt(diff(coords[, 1L])^2 + diff(coords[, 2L])^2))
}

.method_description <- function(method, ground_quantile, base_z) {
  switch(method,
    "dtm"             = "External DTM (bare earth)",
    "min_z"           = "Minimum top-surface value inside ROI",
    "mean_z"          = "Mean top-surface value inside ROI",
    "ground_quantile" = sprintf("Quantile %.2f of top-surface inside ROI",
                                ground_quantile),
    "user_plane"      = sprintf("Constant plane at z = %.3f m", base_z),
    "perimeter_tin"   = "TIN through perimeter vertices (linear interpolation)"
  )
}

.empty_survey_volume <- function(method) {
  out <- list(
    method                  = method,
    cut_volume_m3           = NA_real_,
    fill_volume_m3          = NA_real_,
    net_volume_m3           = NA_real_,
    surface_area_planar_m2  = 0,
    surface_area_draped_m2  = NA_real_,
    perimeter_m             = NA_real_,
    cell_count              = 0L,
    cell_area_m2            = NA_real_,
    top_z_summary           = c(min = NA_real_, median = NA_real_,
                                mean = NA_real_, max = NA_real_),
    base_z_summary          = c(min = NA_real_, median = NA_real_,
                                mean = NA_real_, max = NA_real_),
    height_summary          = c(min = NA_real_, median = NA_real_,
                                mean = NA_real_, max = NA_real_),
    base_reference_text     = paste(method, "(no data in ROI)")
  )
  class(out) <- "dronebio_survey_volume"
  out
}

.perimeter_tin_raster <- function(template, coords) {
  if (!requireNamespace("interp", quietly = TRUE)) {
    stop(
      "`method = 'perimeter_tin'` requires the 'interp' package. ",
      "Install with install.packages('interp').",
      call. = FALSE
    )
  }
  # terra::extract() on a matrix returns a single-column data frame without
  # an ID column; the ID argument is only valid for SpatVector inputs.
  vertex_z <- as.numeric(terra::extract(template, coords)[[1L]])
  finite   <- is.finite(vertex_z)
  if (sum(finite) < 3L) {
    stop(
      "Could not build perimeter TIN: fewer than 3 perimeter vertices ",
      "fall on finite top-surface cells.",
      call. = FALSE
    )
  }

  ext  <- terra::ext(template)
  ncol <- terra::ncol(template)
  nrow <- terra::nrow(template)
  dx   <- terra::xres(template)
  dy   <- terra::yres(template)

  xs <- seq(ext[1L] + dx / 2, ext[2L] - dx / 2, length.out = ncol)
  ys <- seq(ext[4L] - dy / 2, ext[3L] + dy / 2, length.out = nrow)

  tin <- interp::interp(
    x         = coords[finite, 1L],
    y         = coords[finite, 2L],
    z         = vertex_z[finite],
    xo        = xs,
    yo        = ys,
    linear    = TRUE,
    extrap    = TRUE,
    duplicate = "mean"
  )

  # interp::interp returns z as matrix [length(xo), length(yo)] = [ncol, nrow].
  # SpatRaster expects values in row-major order, top row first (=> ys[1]).
  vals_matrix <- t(tin$z)

  base_raster <- terra::rast(
    extent = ext,
    nrows  = nrow,
    ncols  = ncol,
    crs    = terra::crs(template),
    vals   = as.vector(vals_matrix)
  )
  terra::mask(base_raster, template)
}
