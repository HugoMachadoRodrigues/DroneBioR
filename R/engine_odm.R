#' Build an ODM Docker command
#'
#' @param dataset_dir Host folder mounted to `/datasets`.
#' @param project_name ODM project name inside `dataset_dir`.
#' @param image Docker image name.
#' @param radiometric_calibration ODM radiometric calibration option.
#' @param orthophoto_resolution_cm Orthophoto resolution in centimeters.
#' @param max_concurrency Maximum concurrent ODM workers.
#' @param fast_orthophoto Logical. Add `--fast-orthophoto`.
#' @param build_dsm Logical. Add DSM generation.
#' @param build_dtm Logical. Add DTM generation options.
#' @param pc_las Logical. Export LAS point cloud.
#' @param pc_csv Logical. Export CSV point cloud.
#' @param pc_copc Logical. Export COPC point cloud.
#' @param tiles Logical. Export web map tiles.
#' @param three_d_tiles Logical. Export 3D tiles.
#' @param gltf Logical. Export glTF model.
#' @param rerun_from Optional ODM stage.
#' @param end_with Optional ODM end stage.
#' @param extra_args Additional ODM arguments.
#' @return Character vector of arguments for `system2("docker", args)`.
#' @export
build_odm_args <- function(dataset_dir,
                           project_name = "micasense",
                           image = "opendronemap/odm",
                           radiometric_calibration = "camera+sun",
                           orthophoto_resolution_cm = 5,
                           max_concurrency = 4,
                           fast_orthophoto = TRUE,
                           build_dsm = FALSE,
                           build_dtm = FALSE,
                           pc_las = FALSE,
                           pc_csv = FALSE,
                           pc_copc = FALSE,
                           tiles = FALSE,
                           three_d_tiles = FALSE,
                           gltf = FALSE,
                           rerun_from = NULL,
                           end_with = NULL,
                           extra_args = character()) {
  dataset_dir <- normalizePath(dataset_dir, mustWork = FALSE)
  odm_args <- c(
    "--project-path", "/datasets",
    "--radiometric-calibration", radiometric_calibration,
    "--orthophoto-resolution", as.character(orthophoto_resolution_cm),
    "--max-concurrency", as.character(max_concurrency)
  )

  if (isTRUE(fast_orthophoto)) {
    odm_args <- c(odm_args, "--fast-orthophoto")
  }
  if (isTRUE(build_dsm)) {
    odm_args <- c(odm_args, "--dsm")
  }
  if (isTRUE(build_dtm)) {
    odm_args <- c(odm_args, "--dtm", "--smrf-threshold", "0.5")
  }
  if (isTRUE(pc_las)) {
    odm_args <- c(odm_args, "--pc-las")
  }
  if (isTRUE(pc_csv)) {
    odm_args <- c(odm_args, "--pc-csv")
  }
  if (isTRUE(pc_copc)) {
    odm_args <- c(odm_args, "--pc-copc")
  }
  if (isTRUE(tiles)) {
    odm_args <- c(odm_args, "--tiles")
  }
  if (isTRUE(three_d_tiles)) {
    odm_args <- c(odm_args, "--3d-tiles")
  }
  if (isTRUE(gltf)) {
    odm_args <- c(odm_args, "--gltf")
  }
  if (!is.null(rerun_from) && nzchar(rerun_from)) {
    odm_args <- c(odm_args, "--rerun-from", rerun_from)
  }
  if (!is.null(end_with) && nzchar(end_with)) {
    odm_args <- c(odm_args, "--end-with", end_with)
  }
  if (length(extra_args) > 0) {
    odm_args <- c(odm_args, extra_args)
  }

  c(
    "run", "--rm",
    "-v", paste0(dataset_dir, ":/datasets"),
    image,
    odm_args,
    project_name
  )
}

#' Convert ODM undistorted Float TIFFs to UInt16 for texturing
#'
#' MVS-Texturing inside ODM occasionally fails on float undistorted images
#' produced from MicaSense reflectance TIFFs. This helper rewrites the
#' undistorted TIFFs as UInt16 (0-65535) so MVS-Texturing can consume them
#' on a re-run from the `mvs_texturing` stage.
#'
#' @param odm_project_dir ODM project folder.
#' @return Number of files converted.
#' @export
convert_undistorted_tiffs_for_texturing <- function(odm_project_dir) {
  undistorted_dir <- file.path(odm_project_dir, "opensfm", "undistorted", "images")
  files <- list.files(undistorted_dir, pattern = "\\.tif$", full.names = TRUE, ignore.case = TRUE)
  if (length(files) == 0) {
    return(0L)
  }

  converted <- 0L
  for (f in files) {
    r <- suppressWarnings(terra::rast(f))
    if (!terra::datatype(r)[[1]] %in% c("FLT8S", "FLT4S")) {
      next
    }

    tmp <- paste0(f, ".uint16.tif")
    r_uint16 <- terra::clamp(r, lower = 0, upper = 1, values = TRUE) * 65535
    terra::writeRaster(
      r_uint16,
      tmp,
      overwrite = TRUE,
      datatype = "INT2U",
      gdal = c("COMPRESS=LZW", "PREDICTOR=2")
    )
    ok <- file.copy(tmp, f, overwrite = TRUE)
    unlink(tmp)
    if (!ok) {
      stop("Failed to overwrite undistorted image after UInt16 conversion: ", f, call. = FALSE)
    }
    converted <- converted + 1L
  }

  converted
}

#' Run ODM through Docker for a DroneBioR project
#'
#' @param project A `dronebio_project` object.
#' @param run Logical. If `FALSE`, only return the Docker command.
#' @param force Logical. Remove the existing orthomosaic before running.
#' @param ... Additional arguments passed to `build_odm_args()`.
#' @return A list with command, status and output orthomosaic path.
#' @export
run_odm_project <- function(project,
                            run = TRUE,
                            force = FALSE,
                            ...) {
  dir.create(project$odm_images_dir, recursive = TRUE, showWarnings = FALSE)
  manifest <- list_micasense_images(project$images_dir)
  copy_images_for_odm(manifest, project$odm_images_dir)

  if (isTRUE(force) && file.exists(project$odm_orthomosaic)) {
    unlink(project$odm_orthomosaic)
  }

  args <- build_odm_args(
    dataset_dir = project$odm_dataset_dir,
    project_name = project$odm_project_name,
    ...
  )
  command <- paste("docker", paste(shQuote(args), collapse = " "))

  if (!isTRUE(run)) {
    return(list(command = command, status = NA_integer_, orthomosaic = project$odm_orthomosaic))
  }
  if (!nzchar(Sys.which("docker"))) {
    stop("Docker was not found. Install/start Docker or run an external engine first.", call. = FALSE)
  }

  status <- system2("docker", args = args)
  if (!identical(status, 0L) && !file.exists(project$odm_orthomosaic)) {
    converted <- convert_undistorted_tiffs_for_texturing(project$odm_project_dir)
    if (converted > 0) {
      retry_args <- build_odm_args(
        dataset_dir = project$odm_dataset_dir,
        project_name = project$odm_project_name,
        rerun_from = "mvs_texturing",
        ...
      )
      status <- system2("docker", args = retry_args)
    }
  }

  if (!identical(status, 0L)) {
    stop("ODM failed with exit status ", status, call. = FALSE)
  }

  list(command = command, status = status, orthomosaic = project$odm_orthomosaic)
}
