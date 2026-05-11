#' Rule-based ground / vegetation classification from NDVI (and CHM)
#'
#' Applies a small ladder of thresholds to an NDVI raster (and, optionally, a
#' CHM raster) to produce a five-class categorical raster. Designed as a
#' first-pass label layer for visualization and Shiny app legends; for
#' research-grade classification, train a supervised classifier on the
#' index stack instead.
#'
#' Classes:
#' \describe{
#'   \item{1}{Bare / soil (low NDVI).}
#'   \item{2}{Stressed or sparse vegetation.}
#'   \item{3}{Moderate vigor.}
#'   \item{4}{Vigorous vegetation (high NDVI, short / unknown height).}
#'   \item{5}{Tall vegetation (CHM above `chm_tall_min`).}
#' }
#' When `chm` is `NULL`, the output uses classes 1-4 only.
#'
#' @param ndvi A `terra::SpatRaster` of NDVI (typically -1..1).
#' @param chm Optional `terra::SpatRaster` canopy height model in meters.
#' @param ndvi_bare_max Upper bound for "bare / soil" (default 0.20).
#' @param ndvi_stress_max Upper bound for "stressed / sparse" (default 0.40).
#' @param ndvi_vigorous_min Lower bound for "vigorous" (default 0.65).
#' @param chm_tall_min Lower bound (m) for "tall vegetation" (default 2.0).
#' @return A single-layer `terra::SpatRaster` named `class` with integer codes.
#' @examples
#' ortho_path <- system.file("extdata", "micasense_subset.tif", package = "DroneBioR")
#' refl <- scale_to_reflectance(read_multispectral_orthomosaic(ortho_path)$bands)
#' ix   <- compute_spectral_indices(refl)
#' classes <- classify_ground_vegetation(ix[["NDVI"]])
#' names(classes)
#' @export
classify_ground_vegetation <- function(ndvi,
                                       chm = NULL,
                                       ndvi_bare_max     = 0.20,
                                       ndvi_stress_max   = 0.40,
                                       ndvi_vigorous_min = 0.65,
                                       chm_tall_min      = 2.0) {
  if (!inherits(ndvi, "SpatRaster")) {
    stop("`ndvi` must be a terra SpatRaster.", call. = FALSE)
  }
  if (!(ndvi_bare_max <= ndvi_stress_max && ndvi_stress_max <= ndvi_vigorous_min)) {
    stop("Thresholds must satisfy ndvi_bare_max <= ndvi_stress_max <= ndvi_vigorous_min.",
         call. = FALSE)
  }

  classes <- terra::ifel(
    is.na(ndvi), NA,
    terra::ifel(ndvi < ndvi_bare_max,     1L,
      terra::ifel(ndvi < ndvi_stress_max, 2L,
        terra::ifel(ndvi < ndvi_vigorous_min, 3L,
          4L
        )
      )
    )
  )

  if (!is.null(chm)) {
    if (!inherits(chm, "SpatRaster")) {
      stop("`chm` must be a terra SpatRaster.", call. = FALSE)
    }
    if (!terra::compareGeom(ndvi, chm, stopOnError = FALSE)) {
      chm <- terra::resample(chm, ndvi, method = "bilinear")
    }
    # Promote pixels above the tall-vegetation height to class 5.
    classes <- terra::ifel(
      !is.na(chm) & chm > chm_tall_min,
      5L,
      classes
    )
  }

  names(classes) <- "class"
  classes
}

#' Classify ground points in a LAS file using lidR's CSF algorithm
#'
#' Bridge to `lidR::classify_ground()` with the Cloth Simulation Filter
#' (Zhang et al., 2016). Returns a `lidR::LAS` object whose `Classification`
#' attribute marks ground points as 2 and the rest as 1.
#'
#' Requires the optional `lidR` package (a Suggests dependency). The
#' function does not implement CSF in R itself - it just wraps the
#' upstream algorithm so users can stay inside the DroneBioR API surface.
#'
#' @param las_path Path to a LAS or LAZ file.
#' @param sloop_smooth Logical, passed to `lidR::csf()`. Smooth before
#'   building the simulated cloth.
#' @param class_threshold Numeric distance in meters from cloth to point;
#'   below this, the point is ground.
#' @param cloth_resolution Numeric cloth grid resolution in meters.
#' @param rigidness Integer 1, 2, or 3. Cloth rigidness.
#' @param ... Additional arguments passed to `lidR::csf()`.
#' @return A classified `lidR::LAS` object.
#' @examples
#' \dontrun{
#' las <- classify_ground_csf(
#'   "outputs/odm_micasense_dataset/micasense/odm_georeferencing/odm_georeferenced_model.las",
#'   class_threshold = 0.5,
#'   cloth_resolution = 0.5
#' )
#' }
#' @export
classify_ground_csf <- function(las_path,
                                sloop_smooth     = FALSE,
                                class_threshold  = 0.5,
                                cloth_resolution = 0.5,
                                rigidness        = 1L,
                                ...) {
  if (!requireNamespace("lidR", quietly = TRUE)) {
    stop(
      "The 'lidR' package is required for CSF ground classification. ",
      "Install it with install.packages('lidR').",
      call. = FALSE
    )
  }
  if (!file.exists(las_path)) {
    stop("LAS file not found: ", las_path, call. = FALSE)
  }
  las <- lidR::readLAS(las_path)
  if (is.null(las)) {
    return(NULL)
  }
  csf <- lidR::csf(
    sloop_smooth     = isTRUE(sloop_smooth),
    class_threshold  = class_threshold,
    cloth_resolution = cloth_resolution,
    rigidness        = as.integer(rigidness),
    ...
  )
  lidR::classify_ground(las, algorithm = csf)
}
