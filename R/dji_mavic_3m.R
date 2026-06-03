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

#' Trim an ODM project directory down to its final products
#'
#' ODM emits a verbose project tree: `images/` (input), `opensfm/`,
#' `openmvs/`, `odm_filterpoints/` (intermediates needed only during
#' the run), `odm_georeferencing/` (georef info that gets baked into
#' the final GeoTIFFs anyway), plus a stack of small JSON/TXT
#' bookkeeping files. After a successful run none of that is read
#' again — DroneBioR's downstream pipeline (CHM, indices, biomass
#' proxy) only consumes `odm_dem/dsm.tif`, `odm_dem/dtm.tif` and
#' `odm_orthophoto/odm_orthophoto.tif` (plus the 7-band stack we
#' write into the same folder).
#'
#' This helper removes everything from `project_dir` except an
#' explicit allowlist. The defaults preserve the DEM + orthomosaic
#' folders and ODM's two log files (small, useful for forensic
#' debugging of past runs). Pass `keep_extra = ...` to extend the
#' allowlist.
#'
#' @param project_dir ODM project root.
#' @param keep_extra Optional character vector of additional
#'   top-level basenames to preserve.
#' @return Invisibly, the character vector of basenames that were
#'   removed.
#' @noRd
keep_only_final_odm_products <- function(project_dir, keep_extra = character()) {
  if (!dir.exists(project_dir)) return(invisible(character()))
  keep <- c(
    "odm_dem",            # DSM / DTM (and CHM written later by build_chm_raster)
    "odm_orthophoto",     # RGB ortho + DJI 7-band stack
    "log.json",           # ODM's structured run log
    "dronebior_odm.log",  # our redirected docker output
    keep_extra
  )
  entries <- list.files(project_dir, include.dirs = TRUE,
                        recursive = FALSE, all.files = FALSE,
                        no.. = TRUE)
  removed <- character()
  for (entry in entries) {
    if (entry %in% keep) next
    p <- file.path(project_dir, entry)
    unlink(p, recursive = TRUE, force = TRUE)
    if (!file.exists(p) && !dir.exists(p)) removed <- c(removed, entry)
  }
  if (length(removed)) {
    message(sprintf(
      "[clean] Removed ODM intermediates from %s: %s",
      project_dir, paste(removed, collapse = ", ")
    ))
  }
  invisible(removed)
}

#' Default ODM worker concurrency for this machine
#'
#' ODM's per-stage parallelism scales with `--max-concurrency`. The old
#' hardcoded default of 4 left most of a modern multi-core machine
#' idle (an Apple M1 Max has 10 cores; 4 workers used ~2 of them in
#' practice). This returns the physical core count, capped at 16 to
#' avoid pathological memory pressure on very large core counts. Each
#' OpenSfM / OpenMVS worker uses on the order of 1-2 GB, so on a 16 GB
#' machine you may want to pass a smaller explicit `max_concurrency`.
#'
#' @noRd
default_odm_concurrency <- function() {
  n <- tryCatch(parallel::detectCores(logical = FALSE),
                error = function(e) NA_integer_)
  if (is.na(n) || n < 1L) {
    n <- tryCatch(parallel::detectCores(), error = function(e) 4L)
  }
  if (is.na(n) || n < 1L) n <- 4L
  as.integer(max(1L, min(n, 16L)))
}

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
#
# When `sanitize_exif = TRUE` (set by the DJI pipeline once an exifread
# crash has been detected, or proactively when exiftool is present) we
# must COPY rather than hardlink: a hardlink shares the inode with the
# source image, so running `exiftool -overwrite_original` on it would
# corrupt the user's original photo (potentially syncing the damage
# back to OneDrive / Google Drive). After copying we strip the DJI
# MakerNote, which is what crashes ODM's bundled `exifread`.
populate_band_images_dir <- function(manifest, dest_dir, sanitize_exif = FALSE) {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  for (i in seq_len(nrow(manifest))) {
    dest <- file.path(dest_dir, manifest$filename[i])
    if (file.exists(dest)) next
    if (isTRUE(sanitize_exif)) {
      file.copy(manifest$file[i], dest)         # real copy, never a hardlink
    } else {
      ok <- suppressWarnings(file.link(manifest$file[i], dest))
      if (!isTRUE(ok)) file.copy(manifest$file[i], dest)
    }
  }
  if (isTRUE(sanitize_exif)) {
    sanitize_dji_exif_makernotes(file.path(dest_dir, manifest$filename))
  }
  invisible(file.path(dest_dir, manifest$filename))
}

#' Strip the DJI MakerNote EXIF tag from a set of image copies
#'
#' ODM 3.6.0 bundles a version of the `exifread` Python library that
#' raises `IndexError: list index out of range` inside
#' `decode_maker_note()` on certain DJI Mavic 3M MakerNote tags. The
#' crash happens in the very first (`dataset`) stage, so the whole
#' run dies in seconds. The DJI MakerNote holds proprietary metadata
#' ODM does not use for reconstruction — standard EXIF (camera model,
#' focal length, image dimensions, GPS) plus our `geo.txt` cover
#' everything ODM needs — so stripping just the MakerNote is a safe,
#' targeted fix.
#'
#' Requires `exiftool` on PATH. The caller is responsible for only
#' passing **copies** (never hardlinks to the originals), because
#' `-overwrite_original` rewrites the files in place.
#'
#' @param image_paths Character vector of image copies to sanitize.
#' @return Invisibly, the number of files exiftool reported updating.
#' @noRd
sanitize_dji_exif_makernotes <- function(image_paths) {
  image_paths <- image_paths[file.exists(image_paths)]
  if (!length(image_paths)) return(invisible(0L))
  if (!nzchar(Sys.which("exiftool"))) {
    stop(
      "exiftool is required to strip the DJI MakerNote EXIF that crashes ",
      "ODM's exifread, but it was not found on PATH. Install it with ",
      "`brew install exiftool` (macOS) or your platform's package manager, ",
      "then re-run.",
      call. = FALSE
    )
  }
  # -MakerNotes= removes the tag; -overwrite_original avoids the
  # `_original` backup files exiftool writes by default; -P preserves
  # filesystem timestamps; -q keeps the output quiet. system2() execs
  # exiftool directly (no shell), so each path is its own argv element
  # and must NOT be shQuote()'d — spaces in paths are handled by the
  # argv boundary. The `--` ends option parsing so filenames that
  # start with `-` are not mistaken for flags.
  invisible(suppressWarnings(system2(
    "exiftool",
    args = c("-q", "-P", "-overwrite_original", "-MakerNotes=", "--",
             image_paths),
    stdout = TRUE, stderr = TRUE
  )))
  message(sprintf("[exif] Stripped DJI MakerNote from %d image(s).",
                  length(image_paths)))
  invisible(length(image_paths))
}

#' Did this ODM run die on the exifread / DJI MakerNote crash?
#'
#' Scans a `dronebior_odm.log` for the signature of the bundled
#' exifread library choking on a DJI MakerNote tag. Used to turn an
#' opaque `exit status 1` into an actionable "install exiftool" error
#' and to trigger the auto-sanitize retry.
#'
#' @param log_path Path to the docker output log.
#' @return `TRUE` when the exifread MakerNote crash signature is found.
#' @noRd
odm_log_has_exifread_crash <- function(log_path) {
  if (!is.character(log_path) || !length(log_path) ||
      !file.exists(log_path)) {
    return(FALSE)
  }
  lines <- tryCatch(readLines(log_path, warn = FALSE),
                    error = function(e) character())
  if (!length(lines)) return(FALSE)
  has_exifread <- any(grepl("exifread", lines, ignore.case = TRUE))
  has_makernote <- any(grepl("decode_maker_note|MakerNote", lines,
                             ignore.case = TRUE))
  has_indexerr <- any(grepl("IndexError", lines))
  has_exifread && (has_makernote || has_indexerr)
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
                             max_concurrency = 4,
                             build_dsm    = TRUE,
                             build_dtm    = TRUE,
                             fast_orthophoto = FALSE,
                             pc_las       = FALSE,
                             skip_3dmodel = TRUE,
                             skip_report  = TRUE,
                             use_ppk_mrk  = TRUE,
                             ppk_min_fix_quality = 4L,
                             ppk_cli      = "auto") {
  proj_name <- dji_band_project_name(project, band_label)
  band_proj <- dji_band_dataset_subdir(project, band_label)  # dataset_dir/project_name
  band_imgs <- file.path(band_proj, "images")
  ortho_path <- dji_band_ortho_path(project, band_label)

  if (file.exists(ortho_path) && isFALSE(force)) {
    message(sprintf("[%s] orthomosaic already present, skipping ODM run.", band))
    return(ortho_path)
  }

  # Proactively strip the DJI MakerNote EXIF when exiftool is present:
  # ODM 3.6.0's bundled exifread crashes on certain DJI MakerNote tags
  # (IndexError in decode_maker_note) during the very first stage. When
  # exiftool is missing we hardlink as usual and only react if the
  # crash actually happens (see the retry block after the docker run).
  have_exiftool <- nzchar(Sys.which("exiftool"))
  message(sprintf("[%s] %s %d images into %s%s",
                  band,
                  if (have_exiftool) "copying + EXIF-sanitizing" else "linking",
                  nrow(images_manifest), band_imgs,
                  if (have_exiftool) " (stripping DJI MakerNote)" else ""))
  populate_band_images_dir(images_manifest, band_imgs,
                           sanitize_exif = have_exiftool)

  # ----- PPK / RTK geo.txt -------------------------------------------------
  # The DJI Mavic 3M EXIF GPS has a documented altitude bug that makes
  # OpenSfM diverge. The .MRK files shipped beside the photos carry
  # the RTK-quality positions for every trigger event. When a .MRK is
  # available we resolve each per-band filename to its row and write
  # an ODM geo.txt right next to the project root, then tell ODM to
  # honour it via --geo + a tight --gps-accuracy. `ppk_cli` lets users
  # hook an external PPK CLI (rtklib etc.) that takes the .bin/.nav
  # rover files plus their own base RINEX and improves the .MRK
  # before we read it.
  ppk_geo_args <- character()
  if (isTRUE(use_ppk_mrk)) {
    ppk_files <- detect_djim3m_ppk_files(project$images_dir)
    # `ppk_cli = "auto"` (the default) probes the system for rtklib +
    # a DJI .bin converter + a base RINEX in standard locations. If
    # every piece is on disk we get a ready hook; otherwise NULL and
    # we proceed with the .MRK-as-shipped path. Passing NULL / FALSE
    # disables PPK CLI explicitly; a function value is used as-is.
    if (identical(ppk_cli, "auto") && ppk_files$has_ppk_inputs) {
      ppk_cli <- resolve_ppk_cli_auto(project$images_dir)
    } else if (identical(ppk_cli, "auto")) {
      # No .bin/.nav rover files -> nothing for a CLI to refine.
      ppk_cli <- NULL
    }
    if (is.function(ppk_cli) && ppk_files$has_ppk_inputs) {
      message(sprintf("[%s] Running user-supplied PPK CLI hook...", band))
      tryCatch(
        ppk_cli(
          images_dir = project$images_dir,
          bin_paths  = ppk_files$bin,
          nav_paths  = ppk_files$nav,
          mrk_paths  = ppk_files$mrk
        ),
        error = function(e) {
          warning("ppk_cli hook failed: ", conditionMessage(e),
                  ". Falling back to the raw .MRK.", call. = FALSE)
        }
      )
      # Re-detect in case the hook rewrote the .MRK files in place.
      ppk_files <- detect_djim3m_ppk_files(project$images_dir)
    }
    if (ppk_files$has_mrk) {
      geo_txt <- file.path(band_proj, "geo.txt")
      result <- tryCatch(
        write_djim3m_geo_txt(
          images_dir       = project$images_dir,
          image_filenames  = images_manifest$filename,
          geo_txt_path     = geo_txt,
          min_fix_quality  = ppk_min_fix_quality
        ),
        error = function(e) {
          warning(sprintf(
            "[%s] Could not build geo.txt from .MRK: %s. Proceeding without --geo.",
            band, conditionMessage(e)
          ), call. = FALSE)
          NULL
        }
      )
      if (!is.null(result)) {
        # ODM consumes the file from inside the container — translate
        # the host path to the in-container `/datasets/<project>/geo.txt`.
        ppk_geo_args <- c(
          "--geo", paste0("/datasets/", proj_name, "/geo.txt"),
          # 0.10 m horizontal accuracy is a conservative cap when the
          # .MRK had RTK Fix; ODM will use the per-row std deviations
          # we wrote out anyway, this is just the global bound.
          "--gps-accuracy", "0.10"
        )
      }
    } else {
      message(sprintf("[%s] No .MRK PPK sidecar found in %s; ",
                      band, project$images_dir),
              "ODM will fall back to EXIF GPS (often bad on Mavic 3M).")
    }
  }

  is_rgb <- identical(band, "RGB")
  args <- build_odm_args(
    dataset_dir              = project$odm_dataset_dir,
    project_name             = proj_name,
    image                    = odm_image,
    camera_type              = "rgb",
    radiometric_calibration  = if (is_rgb) NULL else "camera+sun",
    orthophoto_resolution_cm = orthophoto_resolution_cm,
    max_concurrency          = max_concurrency,
    # RGB run: full pipeline for DSM + DTM, unless the caller asked
    # for fast_orthophoto (skips the dense MVS reconstruction — much
    # faster, lower-quality DEMs; good when only the orthomosaic +
    # spectral indices are needed).
    # MS runs: always fast-orthophoto with no DEMs — they only
    # contribute their calibrated radiance band; the geometric
    # products come from the RGB run.
    fast_orthophoto = if (is_rgb) isTRUE(fast_orthophoto) else TRUE,
    build_dsm       = if (is_rgb) isTRUE(build_dsm) else FALSE,
    build_dtm       = if (is_rgb) isTRUE(build_dtm) else FALSE,
    # By default skip everything that DroneBioR's downstream pipeline
    # does not need (textured 3D model, PDF report, LAS point cloud).
    # Users who want the LAS for `improve_dtm_csf()` or the 3D model
    # for visualization can opt back in via the run_odm_dji_mavic_3m()
    # parameters. MS runs always skip the 3D model and LAS export
    # because they have no geometric output to contribute.
    pc_las          = is_rgb && isTRUE(pc_las),
    skip_3dmodel    = isTRUE(skip_3dmodel),
    skip_report     = isTRUE(skip_report),
    extra_args      = c(if (is_rgb) rgb_extra_args else ms_extra_args,
                        ppk_geo_args)
  )

  # Heal any orphan OpenSfM state from a previous interrupted run
  # before invoking docker — see clean_incomplete_odm_state() for the
  # failure mode this protects against.
  clean_incomplete_odm_state(band_proj)
  band_log <- file.path(band_proj, "dronebior_odm.log")
  status <- run_docker_with_progress(
    args        = args,
    project_dir = band_proj,
    image_count = nrow(images_manifest),
    band_label  = band
  )

  # exifread / DJI MakerNote crash: ODM dies in the `dataset` stage
  # with IndexError inside exifread. If we had NOT already sanitized
  # (exiftool was absent on the first pass) we cannot self-heal, so
  # raise a clear, actionable error. If exiftool has SINCE been
  # installed, re-populate with sanitized copies and retry once.
  if (!identical(status, 0L) && !file.exists(ortho_path) &&
      odm_log_has_exifread_crash(band_log)) {
    if (nzchar(Sys.which("exiftool"))) {
      message(sprintf(
        "[%s] ODM crashed reading DJI MakerNote EXIF. Re-copying images with the MakerNote stripped and retrying once...",
        band
      ))
      unlink(band_imgs, recursive = TRUE, force = TRUE)
      clean_incomplete_odm_state(band_proj)
      populate_band_images_dir(images_manifest, band_imgs,
                               sanitize_exif = TRUE)
      status <- run_docker_with_progress(
        args        = args,
        project_dir = band_proj,
        image_count = nrow(images_manifest),
        band_label  = paste0(band, "/exif-retry")
      )
    } else {
      stop(sprintf(
        "ODM crashed on band %s reading the DJI Mavic 3M MakerNote EXIF (a known bug in ODM's bundled exifread). DroneBioR can strip the offending MakerNote automatically, but that needs exiftool, which is not installed. Install it with `brew install exiftool` (macOS) and re-run.",
        band
      ), call. = FALSE)
    }
  }

  # Exit 137 = 128 + SIGKILL(9): the Docker container was killed by
  # the host OS. By far the most common cause is Docker Desktop's
  # memory cap being lower than what ODM peaks at (OpenSfM feature
  # matching and OpenMVS dense reconstruction both spike memory on
  # 300+ image flights). Retry once with --max-concurrency 1 and
  # --feature-quality medium, which together cut peak memory roughly
  # in half. If even that does not fit, the user has to raise
  # Docker's memory allocation manually.
  if (identical(as.integer(status), 137L) && !file.exists(ortho_path)) {
    message(sprintf(
      "[%s] ODM exit status 137 — the Docker container was killed by the OS, almost certainly out-of-memory. Retrying once with --max-concurrency 1 --feature-quality medium...",
      band
    ))
    clean_incomplete_odm_state(band_proj)
    oom_retry_args <- build_odm_args(
      dataset_dir              = project$odm_dataset_dir,
      project_name             = proj_name,
      image                    = odm_image,
      camera_type              = "rgb",
      radiometric_calibration  = if (is_rgb) NULL else "camera+sun",
      orthophoto_resolution_cm = orthophoto_resolution_cm,
      max_concurrency          = 1L,
      fast_orthophoto          = if (is_rgb) isTRUE(fast_orthophoto) else TRUE,
      build_dsm                = if (is_rgb) isTRUE(build_dsm) else FALSE,
      build_dtm                = if (is_rgb) isTRUE(build_dtm) else FALSE,
      pc_las                   = is_rgb && isTRUE(pc_las),
      skip_3dmodel             = isTRUE(skip_3dmodel),
      skip_report              = isTRUE(skip_report),
      extra_args               = c(
        if (is_rgb) rgb_extra_args else ms_extra_args,
        ppk_geo_args,
        "--feature-quality", "medium"
      )
    )
    status <- run_docker_with_progress(
      args        = oom_retry_args,
      project_dir = band_proj,
      image_count = nrow(images_manifest),
      band_label  = paste0(band, "/oom-retry")
    )
  }

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
        fast_orthophoto          = if (is_rgb) isTRUE(fast_orthophoto) else TRUE,
        build_dsm                = if (is_rgb) isTRUE(build_dsm) else FALSE,
        build_dtm                = if (is_rgb) isTRUE(build_dtm) else FALSE,
        pc_las                   = is_rgb && isTRUE(pc_las),
        skip_3dmodel             = isTRUE(skip_3dmodel),
        skip_report              = isTRUE(skip_report),
        rerun_from               = "mvs_texturing",
        extra_args               = ppk_geo_args
      )
      status <- run_docker_with_progress(
        args        = retry_args,
        project_dir = band_proj,
        image_count = nrow(images_manifest),
        band_label  = paste0(band, "/retry")
      )
    }
  }
  # ODM sometimes exits non-zero even after writing the orthomosaic —
  # most commonly when the `odm_report` stage's `gdal_translate` call
  # trips over a numpy ABI mismatch in the container's `gdal_array`
  # Python binding. The PDF report dies, every geospatial product
  # (ortho, DSM, DTM, point cloud) is intact. Treat "ortho on disk"
  # as success so the orchestrator can move on to the next band.
  if (!identical(status, 0L) && file.exists(ortho_path)) {
    warning(sprintf(
      "ODM exited with status %s on band %s but the orthomosaic is on disk. ",
      "This usually means a post-processing stage (PDF report, hillshade ",
      "preview) failed; the orthomosaic, DSM/DTM and point cloud should ",
      "still be valid. Treating as success.",
      status, band
    ), call. = FALSE)
    status <- 0L
  }
  if (!identical(status, 0L)) {
    if (identical(as.integer(status), 137L)) {
      stop(sprintf(
        "ODM on band %s was killed by the OS twice in a row (exit status 137 = SIGKILL). Two distinct failure modes share this exit code, and the right remedy depends on which one you hit:\n\n  1) The SfM stages (OpenSfM, OpenMVS) ran out of memory. Open Docker Desktop -> Settings -> Resources -> Memory and confirm the allocation is >= 16 GB. The first failure already triggered an automatic retry with --max-concurrency 1 --feature-quality medium, so further reducing concurrency is not the remedy here.\n\n  2) The reconstruction diverged and `odm_orthophoto` then tried to write a multi-kilometre orthomosaic at centimetre resolution, exhausting memory regardless of cap. Check `<project>/log.json` for `Model bounds x` / `Model area` lines; if the area is many orders of magnitude larger than the actual flight footprint, the SfM is the problem, not the cap. This is common on DJI Mavic 3M flights where the raw EXIF GPS has negative or wrong altitudes ('Altitude is negative ...: viewing directions are probably divergent'). Two ways out:\n      - Best: apply PPK corrections (the DJI .bin / .nav / .MRK sidecars) in DJI Terra before processing, so EXIF GPS is clean.\n      - Without PPK: pass tighter SfM constraints via rgb_extra_args / ms_extra_args, e.g.:\n          c(\"--gps-accuracy\", \"3\",\n            \"--matcher-neighbors\", \"8\",\n            \"--feature-quality\", \"medium\")",
        band
      ), call. = FALSE)
    }
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
#' @param build_dsm,build_dtm Logical, default `TRUE`. Build the DSM /
#'   DTM on the RGB run. Set both to `FALSE` when you only need the
#'   orthomosaic + spectral indices — combined with
#'   `fast_orthophoto = TRUE` this is the fastest path.
#' @param fast_orthophoto Logical, default `FALSE`. When `TRUE`, the
#'   RGB run adds ODM's `--fast-orthophoto`, which skips the dense
#'   MVS reconstruction (often the single longest stage). The
#'   orthomosaic is built from the 2.5D mesh instead — much faster,
#'   but any DSM / DTM produced alongside are lower quality. Leave
#'   `FALSE` for scientifically defensible canopy heights.
#' @param pc_las Logical. When `TRUE`, export the dense point cloud
#'   from the RGB run as a `.las` file (~640 MB for a 300-image
#'   flight). Default `FALSE` — DroneBioR's DSM/DTM/CHM pipeline
#'   does not need the LAS. Set to `TRUE` if you plan to run
#'   `improve_dtm_csf()` afterwards (which reads the LAS).
#' @param skip_3dmodel Logical. Default `TRUE`. Adds `--skip-3dmodel`
#'   to every ODM invocation so the `odm_meshing` and `mvs_texturing`
#'   stages — which together cost 10-30 min per flight and only
#'   produce a textured `.obj` / `.glb` 3D model that DroneBioR's
#'   spectral pipeline never reads — are skipped. Set to `FALSE` if
#'   you want the textured 3D model for visualization.
#' @param skip_report Logical. Default `TRUE`. Adds `--skip-report`
#'   so ODM does not generate its PDF run report. Saves ~1-2 min
#'   per band and avoids the well-known `gdal_translate` / numpy
#'   ABI crash inside some `opendronemap/odm` Docker images.
#' @param cleanup_intermediates Logical. Default `TRUE`. After the
#'   7-band stacked orthomosaic is written, perform two cleanups
#'   so the user is left with only the products DroneBioR's
#'   downstream pipeline actually consumes:
#'   \itemize{
#'     \item Delete the per-MS-band ODM project directories
#'       (`dji_ms_g/`, `dji_ms_r/`, `dji_ms_re/`, `dji_ms_nir/`).
#'       The per-band orthos already live in the 7-band stack.
#'     \item Inside the canonical RGB project folder (`dji/`),
#'       strip every directory and file except `odm_dem/`
#'       (DSM/DTM, plus CHM later), `odm_orthophoto/` (RGB ortho
#'       + 7-band DJI stack), `log.json` (ODM's log) and
#'       `dronebior_odm.log` (our docker output). The discarded
#'       intermediates — `images/`, `opensfm/`, `openmvs/`,
#'       `odm_filterpoints/`, `odm_georeferencing/`,
#'       `odm_postprocess/` and the small JSON/TXT bookkeeping
#'       files — are never read by the downstream R pipeline.
#'   }
#'   Set `FALSE` to keep everything for debugging.
#' @param use_ppk_mrk Logical. Default `TRUE`. When the source folder
#'   carries the DJI `_Timestamp.MRK` sidecar(s), parse them with
#'   [parse_djim3m_mrk_folder()], resolve each per-band filename to
#'   its photo number, and write an ODM `geo.txt` so ODM uses the
#'   RTK / PPK positions instead of the EXIF GPS (which the Mavic 3M
#'   notoriously corrupts on altitude — that is the bug that makes
#'   OpenSfM diverge and `odm_orthophoto` OOM). ODM then runs with
#'   `--geo /datasets/<proj>/geo.txt --gps-accuracy 0.10`.
#' @param ppk_min_fix_quality Integer. Default `4` (RTK Float).
#'   Photos whose .MRK row reports a lower fix quality are dropped
#'   from the geo.txt — including them would let degraded positions
#'   destabilise the bundle adjustment. Set to `50` to demand
#'   RTK-Fixed-only, or `0` to keep everything.
#' @param ppk_cli Controls the **PPK CLI step that runs before ODM**
#'   when the source folder ships the `.bin` / `.nav` rover files.
#'   Three forms are accepted:
#'   \describe{
#'     \item{`"auto"` (default)}{Probe the system for everything
#'       `ppk_cli_rtklib_dji()` needs: `rnx2rtkp` on PATH (e.g.
#'       `brew install rtklib`), a DJI `.bin` -> RINEX converter
#'       on PATH (tried in order: `klauppk_dji_to_rinex`,
#'       `klauppk`, `dji_to_rinex`, `djiparsekit`,
#'       `djirinexconverter`, `convbin`), and a base-station
#'       RINEX observation file located via (a) the
#'       `DRONEBIOR_PPK_BASE_OBS` environment variable, (b) the
#'       `dronebior.ppk_base_obs` R option, or (c)
#'       `<images_dir>/base/*.obs|*.YYo`. When every piece is in
#'       place, run full PPK before ODM. When anything is missing,
#'       emit a clear message naming what is missing and fall back
#'       to the .MRK-as-shipped path.}
#'     \item{`NULL` / `FALSE`}{Skip the CLI step. Use the .MRK as
#'       it ships from the drone (still better than EXIF GPS
#'       because the .MRK already holds RTK-quality positions when
#'       the drone had an RTK Fix in flight).}
#'     \item{A function}{Custom hook with signature
#'       `function(images_dir, bin_paths, nav_paths, mrk_paths)`.
#'       Use [ppk_cli_rtklib_dji()] to build one with explicit
#'       paths if the auto-detect probes need an override.}
#'   }
#'   In every case DroneBioR then reads the (possibly improved)
#'   .MRK and writes an ODM `geo.txt` consumed via `--geo`.
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
                                 max_concurrency = NULL,
                                 build_dsm    = TRUE,
                                 build_dtm    = TRUE,
                                 fast_orthophoto = FALSE,
                                 pc_las       = FALSE,
                                 skip_3dmodel = TRUE,
                                 skip_report  = TRUE,
                                 cleanup_intermediates = TRUE,
                                 use_ppk_mrk  = TRUE,
                                 ppk_min_fix_quality = 4L,
                                 ppk_cli      = "auto",
                                 rgb_extra_args = character(),
                                 ms_extra_args  = character()) {
  if (is.null(max_concurrency)) {
    max_concurrency <- default_odm_concurrency()
    message(sprintf(
      "Using --max-concurrency %d (auto-detected physical cores). Pass max_concurrency = N to override.",
      max_concurrency
    ))
  }
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
  # Use historical per-stage durations to estimate the up-front total.
  # MS runs use --fast-orthophoto so they only execute the stages up to
  # odm_orthophoto; the RGB run executes the full pipeline. We sum
  # estimate_remaining_seconds() with no active stage (i.e., from scratch)
  # for each band to seed the batch ETA.
  full_stages <- odm_stage_order()
  fast_stages <- full_stages[seq_len(which(full_stages == "odm_orthophoto"))]
  est_per_band <- vapply(run_specs, function(spec) {
    if (is.null(spec$manifest)) return(0)
    stages <- if (identical(spec$band, "RGB")) full_stages else fast_stages
    estimate_remaining_seconds(
      active_stage           = NULL,
      pending_stages         = stages,
      active_elapsed_seconds = 0,
      image_count            = nrow(spec$manifest)
    )
  }, numeric(1))
  total_estimate_secs <- sum(est_per_band)
  # The estimate is a coarse extrapolation: it scales recorded per-stage
  # durations linearly by image count, and it does NOT know about the
  # feature-quality / pc-quality settings or the host's core count. When
  # the only history is from small runs at default quality, a large
  # low-quality run is badly over-estimated. We flag that so users do
  # not panic at the headline number; durations recorded by this run
  # tighten the estimate for the next run at the same image count.
  hist_counts <- unique(read_odm_stage_history()$image_count)
  hist_counts <- hist_counts[is.finite(hist_counts)]
  est_caveat <- if (!length(hist_counts)) {
    " (rough: no timing history yet — first run uses built-in baselines)"
  } else {
    sprintf(" (rough extrapolation from history at %s images; actual is usually faster at lower quality)",
            paste(sort(hist_counts), collapse = "/"))
  }
  message(sprintf(
    "Pipeline estimate: %d bands, total ~%s%s",
    sum(!vapply(run_specs, function(s) is.null(s$manifest), logical(1))),
    format_seconds_human(total_estimate_secs),
    est_caveat
  ))

  ortho_paths <- list()
  t0 <- Sys.time()
  bands_done <- 0L
  for (idx in seq_along(run_specs)) {
    spec <- run_specs[[idx]]
    if (is.null(spec$manifest)) {
      message(sprintf("[%s] no images present in the dataset, skipping.",
                      spec$band))
      next
    }
    elapsed_so_far <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    remaining_bands_est <- sum(est_per_band[idx:length(est_per_band)])
    batch_pct <- if (total_estimate_secs > 0) {
      min(99, 100 * elapsed_so_far / total_estimate_secs)
    } else 0
    message(sprintf(
      ">>> Band %d/%d (%s): %d images, est ~%s | batch %d%% done, ETA ~%s",
      idx, length(run_specs), spec$band, nrow(spec$manifest),
      format_seconds_human(est_per_band[idx]),
      as.integer(round(batch_pct)),
      format_seconds_human(remaining_bands_est)
    ))
    band_t0 <- Sys.time()

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
      max_concurrency  = max_concurrency,
      build_dsm        = build_dsm,
      build_dtm        = build_dtm,
      fast_orthophoto  = fast_orthophoto,
      pc_las           = pc_las,
      skip_3dmodel     = skip_3dmodel,
      skip_report      = skip_report,
      use_ppk_mrk      = use_ppk_mrk,
      ppk_min_fix_quality = ppk_min_fix_quality,
      ppk_cli          = ppk_cli
    )

    band_secs <- as.numeric(difftime(Sys.time(), band_t0, units = "secs"))
    bands_done <- bands_done + 1L
    # Recompute remaining: bands after this one carry their original
    # estimate; the just-finished band's actual swap-in shifts the
    # total. This is intentionally coarse but enough to see when a
    # band ran faster / slower than the historical baseline.
    est_per_band[idx] <- band_secs
    elapsed_after <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    remaining_after <- if (idx < length(run_specs)) {
      sum(est_per_band[(idx + 1L):length(est_per_band)])
    } else 0
    message(sprintf(
      "<<< Band %d/%d (%s) done in %s | batch %d%% done, ETA ~%s",
      idx, length(run_specs), spec$band,
      format_seconds_human(band_secs),
      as.integer(round(min(100, 100 * elapsed_after /
                             max(elapsed_after + remaining_after, 1)))),
      format_seconds_human(remaining_after)
    ))
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

  # Two-step post-stack cleanup, both gated by `cleanup_intermediates`:
  #   1. The per-band MS workspaces (`dji_ms_g/`, `dji_ms_r/`,
  #      `dji_ms_re/`, `dji_ms_nir/`) are pure intermediates whose
  #      only useful output (the single-band ortho) is already in
  #      the 7-band stack written to `dji/odm_orthophoto/`. Delete
  #      them whole.
  #   2. Inside the canonical RGB project folder (`dji/`), strip
  #      everything that is not part of the final product set
  #      (`odm_dem/`, `odm_orthophoto/`, the two log files). That
  #      removes `images/`, `opensfm/`, `openmvs/`,
  #      `odm_filterpoints/`, `odm_georeferencing/`,
  #      `odm_postprocess/` and the small JSON / TXT bookkeeping
  #      files — none of which the downstream R workflow ever reads
  #      again.
  if (isTRUE(cleanup_intermediates)) {
    removed_workspaces <- character()
    for (band_label in c("ms_g", "ms_r", "ms_re", "ms_nir")) {
      ws <- dji_band_dataset_subdir(project, band_label)
      if (dir.exists(ws)) {
        unlink(ws, recursive = TRUE, force = TRUE)
        if (!dir.exists(ws)) removed_workspaces <- c(removed_workspaces, basename(ws))
      }
    }
    if (length(removed_workspaces)) {
      message(sprintf(
        "Cleaned MS workspaces (intermediate, no longer needed): %s",
        paste(removed_workspaces, collapse = ", ")
      ))
    }
    keep_only_final_odm_products(project$odm_project_dir)
  }

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
