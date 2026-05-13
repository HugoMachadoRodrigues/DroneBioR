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

#' Re-classify ground via CSF and rebuild the DTM (and optionally the CHM)
#'
#' ODM's default SMRF ground classification with `--smrf-threshold 0.5`
#' is conservative on dense canopy and often labels the entire scene
#' as ground, producing a DTM nearly identical to the DSM and a CHM
#' around zero. Cloth Simulation Filter (CSF) from lidR handles dense
#' vegetation more reliably. This helper:
#'
#' 1. Reads the LAZ / LAS point cloud (preferring the local cache when
#'    migration has run).
#' 2. Runs CSF via [classify_ground_csf()].
#' 3. Rasterises the ground points to a new DTM at the requested
#'    resolution.
#' 4. Writes the DTM into the project's cache directory (or, when no
#'    cache is set up, alongside the original DTM).
#' 5. Optionally regenerates the CHM via [build_chm_raster()].
#'
#' @param project A `dronebio_project` object.
#' @param resolution DTM grid spacing in metres. Default 0.5 m, plenty
#'   of detail for vegetation work while keeping the rasterisation fast.
#' @param class_threshold,cloth_resolution,rigidness Passed straight
#'   through to [classify_ground_csf()]. Defaults are sensible for
#'   moderate-to-dense canopy; lower `class_threshold` (0.1-0.3) and
#'   smaller `cloth_resolution` (0.3) for sparser vegetation.
#' @param rebuild_chm Logical. Run [build_chm_raster()] after writing
#'   the new DTM so the canopy height model picks up the improvement.
#' @param dtm_filename Output filename. Default `"dtm.tif"` replaces
#'   the existing cached DTM in place.
#' @return Absolute path to the new DTM (invisibly returns
#'   `list(dtm = ..., chm = ...)` when `rebuild_chm = TRUE`).
#' @examples
#' \dontrun{
#'   project <- dronebio_project("~/aerial_geoscan_project")
#'   improve_dtm_csf(project, resolution = 0.5, rebuild_chm = TRUE)
#' }
#' @export
improve_dtm_csf <- function(project,
                            resolution       = 0.5,
                            class_threshold  = 0.5,
                            cloth_resolution = 0.5,
                            rigidness        = 1L,
                            rebuild_chm      = TRUE,
                            dtm_filename     = "dtm.tif") {
  if (!requireNamespace("lidR", quietly = TRUE)) {
    stop("The 'lidR' package is required. install.packages('lidR').",
         call. = FALSE)
  }

  paths <- odm_product_paths(project)
  cache <- local_cache_dir(project)
  cache_exists <- dir.exists(cache)
  candidates <- character()
  for (k in c("point_cloud_laz", "point_cloud_las", "point_cloud_copc")) {
    if (cache_exists) {
      candidates <- c(candidates, file.path(cache, basename(paths[[k]])))
    }
    candidates <- c(candidates, unname(paths[[k]]))
  }
  laz_path <- NULL
  for (p in candidates) {
    if (file.exists(p)) { laz_path <- p; break }
  }
  if (is.null(laz_path)) {
    stop("No point cloud (LAZ / LAS / COPC) was found in the project.",
         call. = FALSE)
  }

  message("[DroneBioR] Reading point cloud: ", laz_path)
  las <- classify_ground_csf(
    laz_path,
    class_threshold  = class_threshold,
    cloth_resolution = cloth_resolution,
    rigidness        = rigidness
  )
  if (is.null(las)) {
    stop("CSF classification produced no points; check the LAS file.",
         call. = FALSE)
  }

  message("[DroneBioR] Rasterising ground points to DTM @ ", resolution, " m")
  # knnidw is much faster than tin() on dense clouds and visually
  # equivalent for sub-metre DTMs. Override by switching algorithm
  # in a future refactor if needed.
  dtm <- lidR::rasterize_terrain(las, res = resolution,
                                 algorithm = lidR::knnidw(k = 10L, p = 2))

  out_path <- if (cache_exists) {
    file.path(cache, dtm_filename)
  } else {
    file.path(dirname(unname(paths[["dtm"]])), dtm_filename)
  }
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  terra::writeRaster(
    dtm, out_path,
    overwrite = TRUE, datatype = "FLT4S",
    gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2", "BIGTIFF=IF_SAFER")
  )
  message("[DroneBioR] DTM written: ", out_path)

  if (isTRUE(rebuild_chm)) {
    chm_path <- build_chm_raster(project, force = TRUE, cache_aware = TRUE)
    return(invisible(list(dtm = out_path, chm = chm_path)))
  }
  invisible(out_path)
}
