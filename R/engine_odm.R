#' Clean an incomplete ODM project directory before re-launching
#'
#' ODM's stage runner has aggressive resume detection: when it sees an
#' existing `opensfm/features/` directory it skips feature extraction.
#' If the previous run was interrupted between extraction and matching
#' (a crash, Ctrl-C, processx misuse, OOM, etc.), `features/` exists
#' but is missing one or more `*.features.npz` files. The next run
#' then crashes deep inside OpenSfM's joblib worker with
#'
#'   FileNotFoundError: '/datasets/.../features/...features.npz'
#'
#' This helper detects that state — `opensfm/features/` exists but the
#' completion marker `opensfm/reconstruction.json` does not — and wipes
#' the partial `opensfm/` directory together with every downstream
#' stage directory. The next ODM invocation then starts OpenSfM
#' cleanly. `images/` is left untouched.
#'
#' Called automatically by [run_odm_project()] and the per-band DJI
#' Mavic 3M orchestrator before they invoke docker; users typically
#' do not need to call it directly.
#'
#' @param project_dir ODM project root (the folder containing
#'   `images/`, `opensfm/`, `odm_dem/`, ...).
#' @return Invisibly returns `TRUE` when a clean-up actually happened,
#'   `FALSE` otherwise.
#' @noRd
clean_incomplete_odm_state <- function(project_dir) {
  if (!dir.exists(project_dir)) return(invisible(FALSE))
  opensfm_dir   <- file.path(project_dir, "opensfm")
  features_dir  <- file.path(opensfm_dir, "features")
  recon_marker  <- file.path(opensfm_dir, "reconstruction.json")
  if (!dir.exists(features_dir) || file.exists(recon_marker)) {
    return(invisible(FALSE))
  }
  message(sprintf(
    "[clean] Incomplete OpenSfM state detected at %s (features/ present, reconstruction.json missing). Removing partial state so ODM can start fresh.",
    opensfm_dir
  ))
  unlink(opensfm_dir, recursive = TRUE, force = TRUE)
  # Downstream stages cannot be valid without a clean OpenSfM, so wipe
  # them too. Anything outside this list (e.g., `images/`,
  # `cameras.json`, `geo.txt`, the ODM CLI logs) is preserved.
  downstream_stages <- c(
    "odm_filterpoints", "odm_meshing",
    "mvs_texturing", "odm_texturing", "odm_texturing_25d",
    "odm_georeferencing", "odm_dem", "odm_orthophoto",
    "odm_report", "odm_postprocess", "3d_tiles"
  )
  for (s in downstream_stages) {
    p <- file.path(project_dir, s)
    if (dir.exists(p)) unlink(p, recursive = TRUE, force = TRUE)
  }
  invisible(TRUE)
}

#' Build an ODM Docker command
#'
#' @param dataset_dir Host folder mounted to `/datasets`.
#' @param project_name ODM project name inside `dataset_dir`.
#' @param image Docker image name.
#' @param camera_type One of `"multispectral"` (MicaSense / Sequoia-style 5-band
#'   cameras, applies `--radiometric-calibration camera+sun` by default) or
#'   `"rgb"` (Sony, DJI, Phantom, generic RGB - skips the radiometric flag
#'   because it does not apply).
#' @param radiometric_calibration ODM radiometric-calibration value. When
#'   `NULL`, the default is chosen by `camera_type`: `"camera+sun"` for
#'   multispectral, omitted for RGB. Set to `"none"` to skip explicitly.
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
#' @param skip_3dmodel Logical. Add `--skip-3dmodel` to skip the
#'   ODM `odm_meshing` and `mvs_texturing` stages. Saves 10-30 min
#'   on a typical 300-image flight when the consumer only needs
#'   DSM / DTM / orthomosaic. The skipped artifacts are the textured
#'   3D `.obj` / `.glb` files.
#' @param skip_report Logical. Add `--skip-report` to skip ODM's
#'   PDF report generation stage. Saves ~1-2 min and avoids the
#'   known `gdal_translate` / numpy ABI crash inside some
#'   `opendronemap/odm` Docker images.
#' @param rerun_from Optional ODM stage.
#' @param end_with Optional ODM end stage.
#' @param extra_args Additional ODM arguments.
#' @return Character vector of arguments for `system2("docker", args)`.
#' @examples
#' args <- build_odm_args(
#'   dataset_dir = tempdir(),
#'   project_name = "demo",
#'   build_dsm = TRUE,
#'   build_dtm = TRUE
#' )
#' head(args)
#'
#' # RGB camera (Sony / DJI / Phantom): no radiometric calibration flag.
#' rgb_args <- build_odm_args(
#'   dataset_dir  = tempdir(),
#'   project_name = "rgb_flight",
#'   camera_type  = "rgb",
#'   build_dsm    = TRUE
#' )
#' "--radiometric-calibration" %in% rgb_args
#' @export
build_odm_args <- function(dataset_dir,
                           project_name = "micasense",
                           image = "opendronemap/odm",
                           camera_type = c("multispectral", "rgb"),
                           radiometric_calibration = NULL,
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
                           skip_3dmodel = FALSE,
                           skip_report = FALSE,
                           rerun_from = NULL,
                           end_with = NULL,
                           extra_args = character()) {
  camera_type <- match.arg(camera_type)
  if (is.null(radiometric_calibration)) {
    radiometric_calibration <- switch(camera_type,
                                      multispectral = "camera+sun",
                                      rgm           = NULL,
                                      rgb           = NULL)
  }
  dataset_dir <- normalizePath(dataset_dir, mustWork = FALSE)
  odm_args <- c(
    "--project-path", "/datasets",
    "--orthophoto-resolution", as.character(orthophoto_resolution_cm),
    "--max-concurrency", as.character(max_concurrency)
  )
  if (!is.null(radiometric_calibration) &&
      nzchar(radiometric_calibration) &&
      !identical(tolower(radiometric_calibration), "none")) {
    odm_args <- c(odm_args, "--radiometric-calibration", radiometric_calibration)
  }

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
  # --skip-3dmodel skips the odm_meshing + mvs_texturing stages, which
  # together can take 10-30 min on a 300-image flight and produce only
  # the textured 3D model (.obj / .glb) — irrelevant when the
  # downstream consumer needs DSM/DTM/CHM/Ortho and spectral indices.
  if (isTRUE(skip_3dmodel)) {
    odm_args <- c(odm_args, "--skip-3dmodel")
  }
  # --skip-report skips the PDF report stage. Saves a minute or two
  # and dodges the well-known gdal_translate / numpy ABI mismatch
  # crash in some opendronemap/odm builds.
  if (isTRUE(skip_report)) {
    odm_args <- c(odm_args, "--skip-report")
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
#' @examples
#' \dontrun{
#' n <- convert_undistorted_tiffs_for_texturing("/path/to/odm_project")
#' }
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
#' @param camera_type `"multispectral"` (default; expects the
#'   MicaSense filename pattern via [list_micasense_images()]) or `"rgb"`
#'   (uses [list_aerial_images()] which accepts any JPG/PNG/TIF without
#'   a band-id suffix).
#' @param auto_geoscan When `TRUE` (default) and `camera_type = "rgb"`,
#'   look for a `Metadata/Cameras_WGS84.txt` sibling of the source images
#'   folder (via [detect_geoscan_metadata()]); if found, generate
#'   `<project_dir>/geo.txt` and append `--geo` + `--matcher-neighbors 8`
#'   to the ODM args. Sony RX1R / GeoScan datasets ship with empty EXIF
#'   GPS but per-image WGS84 records in this sidecar — without it, ODM
#'   reconstructs at scene origin.
#' @param ... Additional arguments passed to `build_odm_args()`.
#' @return A list with command, status and output orthomosaic path.
#' @examples
#' \dontrun{
#' project <- dronebio_project("/path/to/Drone_Biomass")
#' run_odm_project(project, build_dsm = TRUE, build_dtm = TRUE)
#' # Sony RX1R / DJI RGB flight - no radiometric calibration, permissive
#' # image lister:
#' run_odm_project(project, camera_type = "rgb")
#' }
#' @export
run_odm_project <- function(project,
                            run = TRUE,
                            force = FALSE,
                            camera_type = c("multispectral", "rgb"),
                            auto_geoscan = TRUE,
                            ...) {
  camera_type <- match.arg(camera_type)
  dir.create(project$odm_images_dir, recursive = TRUE, showWarnings = FALSE)
  manifest <- switch(camera_type,
                     multispectral = list_micasense_images(project$images_dir),
                     rgb           = list_aerial_images(project$images_dir))
  copy_images_for_odm(manifest, project$odm_images_dir)

  if (isTRUE(force) && file.exists(project$odm_orthomosaic)) {
    unlink(project$odm_orthomosaic)
  }

  # Auto-detect GeoScan camera metadata (RGB datasets only) and inject
  # --geo + --matcher-neighbors 8. The user can still pass extra_args
  # via ...; we merge so explicit user args win when they conflict.
  passthrough <- list(...)
  user_extra <- passthrough$extra_args %||% character()
  passthrough$extra_args <- NULL
  auto_extra <- character()
  if (isTRUE(auto_geoscan) && identical(camera_type, "rgb")) {
    geo_meta <- detect_geoscan_metadata(project$images_dir)
    if (!is.null(geo_meta)) {
      geo_path <- file.path(project$odm_project_dir, "geo.txt")
      dir.create(project$odm_project_dir, recursive = TRUE, showWarnings = FALSE)
      convert_geoscan_to_odm_geo(
        cameras_path = geo_meta$cameras_path,
        geo_txt_path = geo_path,
        gnss_offset  = geo_meta$gnss_offset_path
      )
      auto_extra <- c(
        "--geo", paste0("/datasets/", project$odm_project_name, "/geo.txt"),
        "--matcher-neighbors", "8"
      )
      message("[DroneBioR] GeoScan camera metadata detected at ",
              geo_meta$cameras_path,
              "\n              -> wrote ", geo_path,
              "\n              -> appending --geo and --matcher-neighbors 8 to ODM args")
    }
  }

  args <- do.call(build_odm_args, c(list(
    dataset_dir  = project$odm_dataset_dir,
    project_name = project$odm_project_name,
    camera_type  = camera_type,
    extra_args   = c(auto_extra, user_extra)
  ), passthrough))
  command <- paste("docker", paste(shQuote(args), collapse = " "))

  if (!isTRUE(run)) {
    return(list(command = command, status = NA_integer_, orthomosaic = project$odm_orthomosaic))
  }
  if (!nzchar(Sys.which("docker"))) {
    stop("Docker was not found. Install/start Docker or run an external engine first.", call. = FALSE)
  }

  # Heal any orphan OpenSfM state from a previous interrupted run
  # before invoking docker — see clean_incomplete_odm_state() for the
  # failure mode this protects against.
  clean_incomplete_odm_state(project$odm_project_dir)
  status <- run_docker_with_progress(
    args         = args,
    project_dir  = project$odm_project_dir,
    image_count  = nrow(manifest),
    band_label   = NULL,
    camera       = camera_type
  )

  # Exit 137 = OOM kill inside the Docker container. Retry once with
  # --max-concurrency 1 --feature-quality medium so the container fits
  # inside Docker Desktop's typical 8 GB memory cap. See the matching
  # logic in run_one_dji_band() for the rationale.
  if (identical(as.integer(status), 137L) &&
      !file.exists(project$odm_orthomosaic)) {
    message(sprintf(
      "[%s] ODM exit status 137 - the Docker container was killed by the OS, almost certainly out-of-memory. Retrying once with --max-concurrency 1 --feature-quality medium...",
      project$odm_project_name
    ))
    clean_incomplete_odm_state(project$odm_project_dir)
    oom_args <- do.call(build_odm_args, c(list(
      dataset_dir  = project$odm_dataset_dir,
      project_name = project$odm_project_name,
      camera_type  = camera_type,
      extra_args   = c(auto_extra, user_extra,
                       "--feature-quality", "medium")
    ), modifyList(passthrough, list(max_concurrency = 1L))))
    status <- run_docker_with_progress(
      args        = oom_args,
      project_dir = project$odm_project_dir,
      image_count = nrow(manifest),
      band_label  = "oom-retry",
      camera      = camera_type
    )
  }

  if (!identical(status, 0L) && !file.exists(project$odm_orthomosaic)) {
    converted <- convert_undistorted_tiffs_for_texturing(project$odm_project_dir)
    if (converted > 0) {
      retry_args <- build_odm_args(
        dataset_dir = project$odm_dataset_dir,
        project_name = project$odm_project_name,
        camera_type = camera_type,
        rerun_from = "mvs_texturing",
        ...
      )
      status <- run_docker_with_progress(
        args        = retry_args,
        project_dir = project$odm_project_dir,
        image_count = nrow(manifest),
        band_label  = "retry",
        camera      = camera_type
      )
    }
  }

  # ODM sometimes exits non-zero even after writing the orthomosaic and
  # every other geospatial product — the most common cause is the
  # `odm_report` stage failing on a `gdal_translate` invocation because
  # the `gdal_array` Python binding inside the Docker image hits a numpy
  # ABI mismatch. The PDF report dies, the rest of the pipeline is fine.
  # Treat "ortho on disk" as success-with-warning instead of erroring,
  # so downstream callers (run_odm_dji_mavic_3m, batch scripts) can
  # continue to the next band / next flight.
  if (!identical(status, 0L) && file.exists(project$odm_orthomosaic)) {
    warning(sprintf(
      "ODM exited with status %s but %s is present. This usually means a ",
      "post-processing stage (PDF report, hillshade preview) failed; the ",
      "orthomosaic, DSM/DTM and point cloud should still be valid. ",
      "Treating as success.",
      status, basename(project$odm_orthomosaic)
    ), call. = FALSE)
    status <- 0L
  }

  if (!identical(status, 0L)) {
    if (identical(as.integer(status), 137L)) {
      stop(
        "ODM was killed by the OS twice in a row (exit status 137 = SIGKILL). Two distinct failure modes share this exit code, and the right remedy depends on which one you hit:\n\n  1) The SfM stages (OpenSfM, OpenMVS) ran out of memory. Open Docker Desktop -> Settings -> Resources -> Memory and confirm the allocation is >= 16 GB. The first failure already triggered an automatic retry with --max-concurrency 1 --feature-quality medium, so further reducing concurrency is not the remedy here.\n\n  2) The reconstruction diverged and `odm_orthophoto` then tried to write a multi-kilometre orthomosaic at centimetre resolution, exhausting memory regardless of cap. Check `<project>/log.json` for `Model bounds x` / `Model area` lines; if the area is many orders of magnitude larger than the actual flight footprint, the SfM is the problem, not the cap. This is common when the raw EXIF GPS has negative or wrong altitudes ('Altitude is negative ...: viewing directions are probably divergent'). Two ways out:\n      - Best: clean up the EXIF GPS (apply PPK corrections if you have them) before processing.\n      - Without clean GPS: pass tighter SfM constraints via extra_args, e.g.:\n          c(\"--gps-accuracy\", \"3\",\n            \"--matcher-neighbors\", \"8\",\n            \"--feature-quality\", \"medium\")",
        call. = FALSE
      )
    }
    stop("ODM failed with exit status ", status, call. = FALSE)
  }

  list(command = command, status = status, orthomosaic = project$odm_orthomosaic)
}
