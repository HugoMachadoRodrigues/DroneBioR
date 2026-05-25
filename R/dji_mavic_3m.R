# DJI Mavic 3M per-band ODM orchestrator + 7-band ortho stacker.
#
# ODM cannot reconstruct the Mavic 3M's 5-image-per-capture bursts in a
# single pass (1 RGB JPG + 4 single-band MS TIFFs share a capture index
# but are independent cameras to ODM's bundle adjuster). We work around
# that by running ODM five times — once on the RGB JPGs to derive the
# geometric products (orthomosaic, DSM, DTM, point cloud), then once
# per MS band with --fast-orthophoto on top of the RGB-derived
# reconstruction grid. The five resulting orthos get resampled onto a
# common grid (the RGB ortho's) and stacked into a 7-band GeoTIFF that
# downstream functions (read_multispectral_orthomosaic /
# compute_spectral_indices) consume via default_dji_mavic_3m_band_map().

dji_band_project_name <- function(project, band_label) {
  # The RGB run lands at the project's canonical ODM project dir so
  # everything downstream (`odm_product_paths()`, `build_chm_raster()`,
  # `project$odm_orthomosaic`) keeps working without overrides.
  # MS-band runs become siblings: `<project_name>_ms_<band>`.
  if (identical(band_label, "rgb")) {
    project$odm_project_name
  } else {
    paste0(project$odm_project_name, "_", band_label)
  }
}

dji_band_dataset_subdir <- function(project, band_label) {
  # ODM expects images under `<dataset_dir>/<project_name>/images/`. The
  # dataset_dir is shared across the five per-band runs; only the
  # project_name differs.
  file.path(project$odm_dataset_dir, dji_band_project_name(project, band_label))
}

dji_band_ortho_path <- function(project, band_label) {
  file.path(dji_band_dataset_subdir(project, band_label),
            "odm_orthophoto", "odm_orthophoto.tif")
}

# Hardlink (or copy, on cross-filesystem setups) a manifest of images
# into the ODM `images/` subfolder of a per-band run. We use hardlinks
# wherever possible — on a single-filesystem setup that is essentially
# free, and matches the strategy in `process_flyover_1.R`.
populate_band_images_dir <- function(manifest, dest_dir) {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  for (i in seq_len(nrow(manifest))) {
    dest <- file.path(dest_dir, manifest$filename[i])
    if (file.exists(dest)) next
    ok <- suppressWarnings(file.link(manifest$file[i], dest))
    if (!isTRUE(ok)) {
      file.copy(manifest$file[i], dest)
    }
  }
  invisible(file.path(dest_dir, manifest$filename))
}

run_one_dji_band <- function(project,
                             band,
                             band_label,
                             images_manifest,
                             odm_image,
                             force,
                             rgb_extra_args = character(),
                             ms_extra_args  = character(),
                             orthophoto_resolution_cm = 5,
                             max_concurrency = 4) {
  proj_name <- dji_band_project_name(project, band_label)
  band_proj <- dji_band_dataset_subdir(project, band_label)  # dataset_dir/project_name
  band_imgs <- file.path(band_proj, "images")
  ortho_path <- dji_band_ortho_path(project, band_label)

  if (file.exists(ortho_path) && isFALSE(force)) {
    message(sprintf("[%s] orthomosaic already present, skipping ODM run.", band))
    return(ortho_path)
  }

  message(sprintf("[%s] linking %d images into %s",
                  band, nrow(images_manifest), band_imgs))
  populate_band_images_dir(images_manifest, band_imgs)

  is_rgb <- identical(band, "RGB")
  args <- build_odm_args(
    dataset_dir              = project$odm_dataset_dir,
    project_name             = proj_name,
    image                    = odm_image,
    camera_type              = "rgb",
    radiometric_calibration  = if (is_rgb) NULL else "camera+sun",
    orthophoto_resolution_cm = orthophoto_resolution_cm,
    max_concurrency          = max_concurrency,
    # RGB run: full pipeline (we need DSM + DTM + point cloud).
    # MS runs: fast-orthophoto — the geometric products already exist
    # from the RGB run, here we only want a calibrated radiance ortho.
    fast_orthophoto = !is_rgb,
    build_dsm       = is_rgb,
    build_dtm       = is_rgb,
    pc_las          = is_rgb,
    extra_args      = if (is_rgb) rgb_extra_args else ms_extra_args
  )

  message(sprintf("[%s] running ODM (this is the slow step)...", band))
  status <- system2("docker", args = args)
  if (!identical(status, 0L) && !file.exists(ortho_path)) {
    # The MVS-Texturing float-tiff workaround pattern that lives in
    # run_odm_project() is also relevant here when the per-band TIF
    # input upsets MVS; retry once after converting any float tiffs.
    converted <- convert_undistorted_tiffs_for_texturing(band_proj)
    if (converted > 0) {
      retry_args <- build_odm_args(
        dataset_dir              = project$odm_dataset_dir,
        project_name             = proj_name,
        image                    = odm_image,
        camera_type              = "rgb",
        radiometric_calibration  = if (is_rgb) NULL else "camera+sun",
        orthophoto_resolution_cm = orthophoto_resolution_cm,
        max_concurrency          = max_concurrency,
        fast_orthophoto          = !is_rgb,
        build_dsm                = is_rgb,
        build_dtm                = is_rgb,
        pc_las                   = is_rgb,
        rerun_from               = "mvs_texturing"
      )
      status <- system2("docker", args = retry_args)
    }
  }
  if (!identical(status, 0L)) {
    stop(sprintf("ODM failed on band %s (exit status %s).", band, status),
         call. = FALSE)
  }
  ortho_path
}

#' Stack RGB + per-band MS orthomosaics into a single GeoTIFF
#'
#' Reads the 3-band RGB ortho plus up to four single-band MS orthos
#' and resamples each MS layer onto the RGB grid (bilinear). The
#' output band order is `Red, Green, Blue, MS_G, MS_R, MS_RE, MS_NIR`
#' — matching [default_dji_mavic_3m_band_map()] — so downstream
#' [read_multispectral_orthomosaic()] auto-detects it.
#'
#' @param rgb_ortho Path to the RGB ortho (3 bands; ODM convention is
#'   R/G/B).
#' @param ms_orthos Named character vector of paths to MS-band orthos.
#'   Names must be among `MS_G`, `MS_R`, `MS_RE`, `MS_NIR`. Missing
#'   bands are silently skipped (the resulting stack still works, it
#'   just exposes fewer indices).
#' @param out_path Destination GeoTIFF path.
#' @return The `out_path`, invisibly.
#' @keywords internal
stack_dji_mavic_3m_ortho <- function(rgb_ortho, ms_orthos, out_path) {
  if (!file.exists(rgb_ortho)) {
    stop("RGB orthomosaic not found: ", rgb_ortho, call. = FALSE)
  }
  rgb <- terra::rast(rgb_ortho)
  # ODM writes the RGB ortho as Red, Green, Blue (sometimes plus alpha
  # at layer 4). Keep the first 3 layers and force the names so that
  # downstream band_maps line up regardless of TIFF metadata.
  rgb_rgb <- rgb[[1:3]]
  names(rgb_rgb) <- c("Red", "Green", "Blue")

  out_layers <- list(rgb_rgb)
  for (band in c("MS_G", "MS_R", "MS_RE", "MS_NIR")) {
    p <- ms_orthos[[band]]
    if (is.null(p) || !file.exists(p)) next
    ms <- terra::rast(p)[[1L]]
    names(ms) <- band
    if (!terra::compareGeom(rgb_rgb, ms, stopOnError = FALSE,
                            lyrs = FALSE, messages = FALSE)) {
      ms <- terra::resample(ms, rgb_rgb, method = "bilinear")
    }
    out_layers[[length(out_layers) + 1L]] <- ms
  }

  # `do.call(c, list_of_rasters)` falls through to base::c and returns
  # a list, not a SpatRaster. Reduce with binary `c()` so the terra S4
  # method dispatches.
  stacked <- out_layers[[1L]]
  for (i in seq_along(out_layers)[-1L]) {
    stacked <- c(stacked, out_layers[[i]])
  }

  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  terra::writeRaster(
    stacked, out_path, overwrite = TRUE, datatype = "FLT4S",
    gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2", "BIGTIFF=IF_SAFER",
             "TILED=YES")
  )
  invisible(out_path)
}

#' Run OpenDroneMap on a DJI Mavic 3M flight, producing a 7-band ortho
#'
#' DJI Mavic 3M captures 1 RGB (`_D.JPG`) and 4 single-band
#' multispectral TIFFs (`_MS_G/R/RE/NIR.TIF`) per shot — five
#' independent cameras from ODM's perspective. ODM cannot bundle-adjust
#' the burst as one capture, so this function orchestrates **five
#' separate ODM runs**: one on the RGB JPGs (full pipeline -
#' orthomosaic, DSM, DTM, point cloud) and one per MS band with
#' `--fast-orthophoto` (orthomosaic only, calibrated to reflectance via
#' the DLS camera+sun flag). All five resulting orthos are then
#' resampled onto the RGB ortho's grid and stacked into a single
#' 7-band GeoTIFF (`Red, Green, Blue, MS_G, MS_R, MS_RE, MS_NIR`) at
#' `<project_dir>/<odm_dataset_subdir>/odm_orthophoto_dji.tif`.
#'
#' Downstream [run_dronebio_workflow()] / [read_multispectral_orthomosaic()]
#' auto-detects the 7-layer stack and uses [default_dji_mavic_3m_band_map()]
#' to expose Blue, Green, Red, RedEdge and NIR — Green/Red/RedEdge/NIR
#' are pulled from the calibrated MS bands, Blue from the RGB JPG
#' channel (the Mavic 3M does not capture a calibrated blue MS band).
#'
#' @param project A `dronebio_project` object whose `images_dir`
#'   contains DJI Mavic 3M raw images.
#' @param force Logical. Re-run every band even if outputs already
#'   exist. Useful after changing camera or radiometric parameters.
#' @param odm_image Docker image tag for the ODM container.
#' @param orthophoto_resolution_cm Orthophoto ground sampling distance.
#' @param max_concurrency Concurrent ODM workers per band.
#' @param rgb_extra_args Extra arguments appended to the **RGB** ODM
#'   run (`build_odm_args(..., extra_args = ...)`).
#' @param ms_extra_args Extra arguments appended to **each MS** ODM
#'   run.
#' @return A list with paths to the per-band orthos, the RGB DSM /
#'   DTM, and the stacked 7-band orthomosaic.
#' @examples
#' \dontrun{
#'   project <- dronebio_project("/path/to/flight",
#'                               images_subdir      = ".",
#'                               odm_dataset_subdir = "odm_dji_dataset",
#'                               odm_project_name   = "dji")
#'   project$images_dir <- "/path/to/raw/images"
#'   result <- run_odm_dji_mavic_3m(project)
#'   result$stacked_orthomosaic
#' }
#' @export
run_odm_dji_mavic_3m <- function(project,
                                 force = FALSE,
                                 odm_image = "opendronemap/odm",
                                 orthophoto_resolution_cm = 5,
                                 max_concurrency = 4,
                                 rgb_extra_args = character(),
                                 ms_extra_args  = character()) {
  if (!nzchar(Sys.which("docker"))) {
    stop("Docker was not found. Install / start Docker first.", call. = FALSE)
  }
  manifests <- list_dji_mavic_3m_images(project$images_dir)
  if (!"D" %in% names(manifests)) {
    stop(
      "DJI Mavic 3M dataset is missing the RGB visible (_D.JPG) images; ",
      "they are required to drive the SfM reconstruction.",
      call. = FALSE
    )
  }

  # Five (RGB, MS_G, MS_R, MS_RE, MS_NIR) per-band ODM runs.
  run_specs <- list(
    list(band = "RGB",    label = "rgb",    manifest = manifests[["D"]]),
    list(band = "MS_G",   label = "ms_g",   manifest = manifests[["MS_G"]]),
    list(band = "MS_R",   label = "ms_r",   manifest = manifests[["MS_R"]]),
    list(band = "MS_RE",  label = "ms_re",  manifest = manifests[["MS_RE"]]),
    list(band = "MS_NIR", label = "ms_nir", manifest = manifests[["MS_NIR"]])
  )
  ortho_paths <- list()
  t0 <- Sys.time()
  for (spec in run_specs) {
    if (is.null(spec$manifest)) {
      message(sprintf("[%s] no images present in the dataset, skipping.",
                      spec$band))
      next
    }
    ortho_paths[[spec$band]] <- run_one_dji_band(
      project          = project,
      band             = spec$band,
      band_label       = spec$label,
      images_manifest  = spec$manifest,
      odm_image        = odm_image,
      force            = force,
      rgb_extra_args   = rgb_extra_args,
      ms_extra_args    = ms_extra_args,
      orthophoto_resolution_cm = orthophoto_resolution_cm,
      max_concurrency  = max_concurrency
    )
  }

  # The RGB run owns the geometric products. Because we ran it under
  # the project's canonical odm_project_name, `odm_product_paths()` and
  # `build_chm_raster()` already point at the right DSM/DTM/CHM.
  rgb_proj <- project$odm_project_dir
  dsm_path <- file.path(rgb_proj, "odm_dem", "dsm.tif")
  dtm_path <- file.path(rgb_proj, "odm_dem", "dtm.tif")

  # Stack RGB + MS bands into the canonical 7-band ortho. We write it
  # alongside the RGB ortho so it lives in the project's odm_project_dir
  # and is easy to find via odm_product_paths(); downstream
  # `run_dronebio_workflow()` should be invoked with this file.
  stacked_path <- file.path(rgb_proj, "odm_orthophoto",
                            "odm_orthophoto_dji.tif")
  ms_paths <- ortho_paths[c("MS_G", "MS_R", "MS_RE", "MS_NIR")]
  ms_paths <- ms_paths[!vapply(ms_paths, is.null, logical(1))]
  stack_dji_mavic_3m_ortho(
    rgb_ortho = ortho_paths[["RGB"]],
    ms_orthos = ms_paths,
    out_path  = stacked_path
  )

  message(sprintf("DJI Mavic 3M workflow done in %.1f min.",
                  as.numeric(difftime(Sys.time(), t0, units = "mins"))))

  list(
    rgb_orthomosaic      = ortho_paths[["RGB"]],
    ms_orthomosaics      = ms_paths,
    dsm                  = dsm_path,
    dtm                  = dtm_path,
    stacked_orthomosaic  = stacked_path,
    rgb_project_dir      = rgb_proj
  )
}
