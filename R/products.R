#' Return expected ODM product paths
#'
#' Covers both the 3D texturing folder (`odm_texturing/`, produced when
#' `--fast-orthophoto` is off) and the 2.5D fallback (`odm_texturing_25d/`).
#' Use [pick_best_textured_obj()] / [pick_best_textured_glb()] to choose
#' whichever variant actually exists on disk.
#'
#' @param project A `dronebio_project` object.
#' @return Named character vector of expected product paths.
#' @examples
#' project <- dronebio_project(project_dir = tempdir())
#' odm_product_paths(project)
#' @export
odm_product_paths <- function(project) {
  d <- project$odm_project_dir
  # A DJI Mavic 3M run reconstructs each band separately and stacks them into
  # odm_orthophoto_dji.tif beside the RGB orthomosaic. That stack is the
  # multispectral product; the plain odm_orthophoto.tif next to it carries only
  # R/G/B, so returning it hides NIR and RedEdge -- and with them NDVI, NDRE,
  # EVI and every other multispectral index -- from a multispectral flight.
  ortho <- project$odm_orthomosaic
  dji_stack <- file.path(dirname(ortho), "odm_orthophoto_dji.tif")
  if (file.exists(dji_stack)) ortho <- dji_stack
  c(
    orthomosaic        = ortho,
    dsm                = file.path(d, "odm_dem", "dsm.tif"),
    dtm                = file.path(d, "odm_dem", "dtm.tif"),
    chm                = file.path(d, "odm_dem", "chm.tif"),
    # CSF-refined terrain products produced by improve_dtm_csf().
    # Kept side-by-side with the ODM SMRF DTM/CHM so users can
    # compare both methods without losing either.
    dtm_csf            = file.path(d, "odm_dem", "dtm_csf.tif"),
    chm_csf            = file.path(d, "odm_dem", "chm_csf.tif"),
    point_cloud_las    = file.path(d, "odm_georeferencing", "odm_georeferenced_model.las"),
    point_cloud_laz    = file.path(d, "odm_georeferencing", "odm_georeferenced_model.laz"),
    point_cloud_copc   = file.path(d, "odm_georeferencing", "odm_georeferenced_model.copc.laz"),
    point_cloud_ply    = file.path(d, "odm_filterpoints",   "point_cloud.ply"),
    mesh_ply           = file.path(d, "odm_meshing",        "odm_25dmesh.ply"),
    textured_obj       = file.path(d, "odm_texturing",      "odm_textured_model_geo.obj"),
    textured_obj_25d   = file.path(d, "odm_texturing_25d",  "odm_textured_model_geo.obj"),
    textured_glb       = file.path(d, "odm_texturing",      "odm_textured_model_geo.glb"),
    textured_glb_25d   = file.path(d, "odm_texturing_25d",  "odm_textured_model_geo.glb"),
    tiles_3d           = file.path(d, "3d_tiles",           "tileset.json"),
    map_tiles_dir      = file.path(d, "odm_orthophoto",     "odm_orthophoto_tiles"),
    report             = file.path(d, "odm_report",         "report.pdf")
  )
}

#' Pick whichever textured mesh actually exists, preferring full 3D over 2.5D.
#'
#' @param project A `dronebio_project` object.
#' @return Absolute path to an existing `.obj`, or the 3D path (which may not
#'   exist yet) as a sensible default.
#' @examples
#' project <- dronebio_project(project_dir = tempdir())
#' pick_best_textured_obj(project)
#' @export
pick_best_textured_obj <- function(project) {
  paths <- odm_product_paths(project)
  if (file.exists(paths[["textured_obj"]]))     return(unname(paths[["textured_obj"]]))
  if (file.exists(paths[["textured_obj_25d"]])) return(unname(paths[["textured_obj_25d"]]))
  unname(paths[["textured_obj"]])
}

#' Pick whichever glTF binary actually exists, preferring full 3D over 2.5D.
#'
#' @param project A `dronebio_project` object.
#' @return Absolute path to an existing `.glb`, or the 3D path as default.
#' @examples
#' project <- dronebio_project(project_dir = tempdir())
#' pick_best_textured_glb(project)
#' @export
pick_best_textured_glb <- function(project) {
  paths <- odm_product_paths(project)
  if (file.exists(paths[["textured_glb"]]))     return(unname(paths[["textured_glb"]]))
  if (file.exists(paths[["textured_glb_25d"]])) return(unname(paths[["textured_glb_25d"]]))
  unname(paths[["textured_glb"]])
}

#' Pick the best available point cloud, in order: COPC > LAZ > LAS > PLY.
#'
#' @param project A `dronebio_project` object.
#' @return Absolute path to the best point cloud found, or the COPC path
#'   as default (typical preference) when nothing exists yet.
#' @examples
#' project <- dronebio_project(project_dir = tempdir())
#' pick_best_point_cloud(project)
#' @export
pick_best_point_cloud <- function(project) {
  paths <- odm_product_paths(project)
  for (k in c("point_cloud_copc", "point_cloud_laz",
              "point_cloud_las",  "point_cloud_ply")) {
    if (file.exists(paths[[k]])) return(unname(paths[[k]]))
  }
  unname(paths[["point_cloud_copc"]])
}

#' Detect whether a path lives inside a cloud-sync provider folder
#'
#' OneDrive / iCloud Drive / Google Drive paths on macOS live under
#' `~/Library/CloudStorage/`. Reads against those folders can trigger
#' background up/downloads ("Files On-Demand"), which is painful for
#' a GeoTIFF-heavy app. This helper returns the provider name so the
#' Shiny UI can warn the user and offer a local-cache migration.
#'
#' @param path Filesystem path to inspect.
#' @return `NA_character_` when not under a cloud-sync folder,
#'   otherwise the provider name (e.g. "OneDrive", "GoogleDrive").
#' @examples
#' is_cloud_sync_path("/Users/me/Documents")
#' is_cloud_sync_path("/Users/me/Library/CloudStorage/OneDrive-Acme/proj")
#' @export
is_cloud_sync_path <- function(path) {
  if (!is.character(path) || !length(path) || !nzchar(path)) {
    return(NA_character_)
  }
  norm <- tryCatch(normalizePath(path, mustWork = FALSE),
                   error = function(e) path)
  m <- regmatches(norm, regexec("CloudStorage/([A-Za-z]+)", norm))[[1L]]
  if (length(m) >= 2L) return(m[2L])
  if (grepl("Dropbox", norm, ignore.case = TRUE))     return("Dropbox")
  if (grepl("iCloud Drive", norm, ignore.case = TRUE)) return("iCloud")
  NA_character_
}

#' Path to the run-history manifest for a project.
#'
#' Returns `<project_dir>/dronebio_runs.csv`, the per-project audit
#' log of every Processing Engine / workflow execution. Each row
#' captures who/what produced the products currently on disk:
#' timestamp, engine, preset, resolution, image count, bands
#' detected, CRS, written products, run parameters. The path is
#' returned even when the file does not exist yet so callers can
#' append cleanly.
#'
#' @param project A `dronebio_project` object (or anything with a
#'   `project_dir` field).
#' @return Absolute path to the runs manifest. `NA_character_` when
#'   the project has no `project_dir`.
#' @noRd
dronebio_runs_path <- function(project) {
  pd <- tryCatch(project$project_dir, error = function(e) NULL)
  if (is.null(pd) || !nzchar(pd)) return(NA_character_)
  file.path(pd, "dronebio_runs.csv")
}

#' Append a row to the project's run-history manifest.
#'
#' Idempotent helper: creates the CSV on the first call, appends
#' subsequent rows. Designed so any pipeline step that produces
#' user-visible artefacts can call it without worrying about file
#' existence or header consistency. Fields outside the canonical
#' schema (`timestamp`, `engine`, `preset`, `resolution_cm`,
#' `image_count`, `bands`, `crs`, `orthomosaic`, `dsm`, `dtm`,
#' `chm`, `point_cloud`, `textured_mesh`, `runtime_seconds`,
#' `notes`, `extras`) are JSON-encoded into the `extras` column so
#' the schema stays stable across versions.
#'
#' @param project A `dronebio_project` object.
#' @param record Named list of fields to record.
#' @return Invisibly, the path written to.
#' @noRd
record_dronebio_run <- function(project, record = list()) {
  path <- dronebio_runs_path(project)
  if (is.na(path)) return(invisible(NULL))
  canonical <- c("timestamp", "engine", "preset", "resolution_cm",
                 "image_count", "bands", "crs",
                 "orthomosaic", "dsm", "dtm", "chm",
                 "point_cloud", "textured_mesh",
                 "runtime_seconds", "notes")
  row <- as.list(stats::setNames(rep(NA_character_, length(canonical)), canonical))
  if (is.null(record$timestamp))
    record$timestamp <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  for (k in intersect(names(record), canonical)) {
    v <- record[[k]]
    row[[k]] <- if (is.null(v)) NA_character_ else as.character(v)
  }
  extras <- record[setdiff(names(record), canonical)]
  row[["extras"]] <- if (length(extras) &&
                         requireNamespace("jsonlite", quietly = TRUE))
    tryCatch(jsonlite::toJSON(extras, auto_unbox = TRUE),
             error = function(e) NA_character_) else NA_character_
  df <- as.data.frame(row, stringsAsFactors = FALSE)
  tryCatch({
    if (file.exists(path)) {
      utils::write.table(df, path, append = TRUE, sep = ",", row.names = FALSE,
                         col.names = FALSE, quote = TRUE)
    } else {
      utils::write.csv(df, path, row.names = FALSE)
    }
  }, error = function(e) NULL)
  invisible(path)
}

#' Read the project's run-history manifest as a data.frame.
#'
#' @param project A `dronebio_project` object.
#' @return Data frame with one row per recorded run, sorted newest
#'   first. Empty data frame when the manifest does not exist.
#' @noRd
read_dronebio_runs <- function(project) {
  path <- dronebio_runs_path(project)
  if (is.na(path) || !file.exists(path)) {
    return(data.frame())
  }
  df <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE),
                 error = function(e) data.frame())
  if (nrow(df) == 0) return(df)
  if ("timestamp" %in% names(df)) {
    df$timestamp_parsed <- tryCatch(as.POSIXct(df$timestamp,
                                               format = "%Y-%m-%dT%H:%M:%S",
                                               tz = "UTC"),
                                    error = function(e) NULL)
    df <- df[order(df$timestamp_parsed, decreasing = TRUE), , drop = FALSE]
    df$timestamp_parsed <- NULL
  }
  df
}


#' Lightweight existence + size check on ODM outputs
#'
#' Cheap counterpart to [validate_odm_outputs()] that never opens a
#' raster file (so it doesn't trigger OneDrive Files-On-Demand
#' downloads on cloud-synced project folders). Just checks that the
#' orthomosaic and DSM exist and are bigger than `min_size_mb` MB.
#'
#' @param project A `dronebio_project` object.
#' @param min_size_mb Numeric size threshold (MB) below which a file
#'   is treated as a placeholder / aborted artifact. Default 1 MB.
#' @return Named logical vector with `orthomosaic`, `dsm`, `dtm`,
#'   `point_cloud`, plus a top-level `outputs_complete` summary that
#'   is TRUE iff the orthomosaic and DSM both pass.
#' @examples
#' p <- dronebio_project(project_dir = tempdir())
#' quick_outputs_check(p)
#' @export
quick_outputs_check <- function(project, min_size_mb = 1) {
  paths <- odm_product_paths(project)
  size_ok <- function(p) {
    if (!file.exists(p)) return(FALSE)
    info <- tryCatch(file.info(p), error = function(e) NULL)
    if (is.null(info) || !is.finite(info$size)) return(FALSE)
    info$size / 1e6 >= min_size_mb
  }
  ortho <- size_ok(paths[["orthomosaic"]])
  dsm   <- size_ok(paths[["dsm"]])
  dtm   <- size_ok(paths[["dtm"]])
  chm   <- size_ok(paths[["chm"]])
  # Include the odm_filterpoints/point_cloud.ply that the staged flow
  # (build_point_cloud_only / Process step 2) produces. The LAS/LAZ/COPC
  # exports only exist after a full ODM run, so without the .ply the
  # "Point cloud" status pill never turned green for the staged path even
  # with a cloud sitting on disk.
  pc    <- any(c(size_ok(paths[["point_cloud_copc"]]),
                 size_ok(paths[["point_cloud_laz"]]),
                 size_ok(paths[["point_cloud_las"]]),
                 size_ok(paths[["point_cloud_ply"]])))
  c(orthomosaic = ortho, dsm = dsm, dtm = dtm, chm = chm,
    point_cloud = pc, outputs_complete = (ortho && dsm))
}

#' Validate ODM output products against sanity thresholds
#'
#' Inspects each rasterizable product (orthomosaic, DSM, DTM) and the
#' point-cloud / mesh files for the obvious failure modes that bit us
#' on the GeoScan dataset: degenerate extents (<50 m wide), pixel
#' grids under 100x100, missing or tiny binary files. The `valid`
#' flag is the simple bottom-line; `notes` tells the user *why*.
#'
#' @param project A `dronebio_project` object.
#' @return A data frame with one row per product and columns
#'   `product`, `path`, `exists`, `size_mb`, `dimensions`, `extent_m`,
#'   `crs`, `valid`, `notes`.
#' @examples
#' project <- dronebio_project(project_dir = tempdir())
#' validate_odm_outputs(project)
#' @export
validate_odm_outputs <- function(project) {
  paths <- odm_product_paths(project)

  empty_row <- function(name, key, type, msg) {
    p <- unname(paths[[key]])
    data.frame(
      product    = name,
      path       = p,
      exists     = FALSE,
      size_mb    = NA_real_,
      dimensions = NA_character_,
      extent_m   = NA_character_,
      crs        = NA_character_,
      valid      = FALSE,
      notes      = msg,
      stringsAsFactors = FALSE
    )
  }

  validate_raster <- function(name, key) {
    p <- unname(paths[[key]])
    if (!file.exists(p)) return(empty_row(name, key, "raster", "missing"))
    size_mb <- round(file.info(p)$size / 1e6, 2)
    r <- tryCatch(terra::rast(p), error = function(e) NULL)
    if (is.null(r)) {
      return(data.frame(
        product = name, path = p, exists = TRUE, size_mb = size_mb,
        dimensions = NA_character_, extent_m = NA_character_,
        crs = NA_character_, valid = FALSE,
        notes = "cannot read with terra::rast",
        stringsAsFactors = FALSE
      ))
    }
    nc <- terra::ncol(r); nr <- terra::nrow(r); nl <- terra::nlyr(r)
    e <- terra::ext(r)
    width  <- e$xmax - e$xmin
    height <- e$ymax - e$ymin
    crs_name <- tryCatch(terra::crs(r, describe = TRUE)$name,
                         error = function(e) NA_character_)
    if (is.null(crs_name) || !length(crs_name)) crs_name <- NA_character_
    degenerate <- (!is.finite(width) || !is.finite(height) ||
                   width < 50 || height < 50 || nc < 100 || nr < 100)
    notes <- if (degenerate) "degenerate extent or pixel grid (<50 m or <100 px)" else "ok"
    data.frame(
      product    = name,
      path       = p,
      exists     = TRUE,
      size_mb    = size_mb,
      dimensions = sprintf("%d x %d, %d layer(s)", nc, nr, nl),
      extent_m   = sprintf("%.1f x %.1f m", width, height),
      crs        = crs_name,
      valid      = !degenerate,
      notes      = notes,
      stringsAsFactors = FALSE
    )
  }

  validate_binary <- function(name, key, min_size_mb = 0.1) {
    p <- unname(paths[[key]])
    if (!file.exists(p)) return(empty_row(name, key, "binary", "missing"))
    size_mb <- round(file.info(p)$size / 1e6, 2)
    too_small <- size_mb < min_size_mb
    data.frame(
      product    = name,
      path       = p,
      exists     = TRUE,
      size_mb    = size_mb,
      dimensions = NA_character_,
      extent_m   = NA_character_,
      crs        = NA_character_,
      valid      = !too_small,
      notes      = if (too_small) sprintf("size %.2f MB below %.2f MB threshold", size_mb, min_size_mb) else "ok",
      stringsAsFactors = FALSE
    )
  }

  # Detect whether the ortho is RGB (3-4 layers) or Multispectral
  # (5+ layers). We label the row accordingly so users see exactly
  # the canonical product list their boss expects.
  ortho_label <- "Orthomosaic"
  ortho_p <- unname(paths[["orthomosaic"]])
  if (file.exists(ortho_p)) {
    nl <- tryCatch(terra::nlyr(terra::rast(ortho_p)),
                   error = function(e) NA_integer_)
    if (!is.na(nl)) {
      ortho_label <- if (nl >= 5L) "Multispectral Orthomosaic" else "RGB Orthomosaic"
    }
  }

  rows <- list(
    validate_raster(ortho_label, "orthomosaic"),
    validate_raster("DSM", "dsm"),
    validate_raster("DTM", "dtm"),
    validate_raster("CHM", "chm"),
    validate_binary("Point cloud (COPC)", "point_cloud_copc"),
    validate_binary("Point cloud (LAZ)",  "point_cloud_laz"),
    validate_binary("Point cloud (LAS)",  "point_cloud_las"),
    validate_binary("Point cloud (PLY)",  "point_cloud_ply"),
    validate_binary("Textured mesh (OBJ)", "textured_obj"),
    validate_binary("Textured mesh (glTF)", "textured_glb"),
    validate_binary("3D tiles tileset",   "tiles_3d", min_size_mb = 0.001),
    validate_binary("ODM report PDF",     "report",  min_size_mb = 0.01)
  )
  do.call(rbind, rows)
}

#' Build a Canopy Height Model (CHM) from the DSM and DTM
#'
#' Computes `CHM = DSM - DTM`, clamps negatives to zero (small noise from
#' SMRF ground classification), and writes the result as a COG-style
#' GeoTIFF into the project's `odm_dem/` directory, alongside the DSM.
#'
#' @param project A `dronebio_project` object.
#' @param force Logical. Recompute even when `chm.tif` already exists.
#' @param outlier_percentile Numeric in (0, 100], default `99.5`.
#'   After differencing, canopy-height pixels strictly above this
#'   percentile are set to `NA` and a message reports how many were
#'   dropped. Photogrammetric reconstructions routinely leave a thin
#'   tail of physically impossible spikes (CHM pixels of tens to
#'   hundreds of metres over short pasture) from mis-reconstructed
#'   points at edges, water and low-texture areas. Even when they are
#'   well under 1% of pixels they wreck colour ramps and contaminate
#'   downstream biomass statistics. Clipping the extreme tail at a
#'   high percentile removes them while preserving genuine tall
#'   features (the percentile adapts to each survey). Set to `100`
#'   (or `NULL`) to disable and keep every pixel.
#' @return Absolute path to the written `chm.tif`.
#' @examples
#' \dontrun{
#'   project <- dronebio_project("~/my_project")
#'   build_chm_raster(project)
#'   # keep every pixel, no outlier clipping:
#'   build_chm_raster(project, outlier_percentile = 100)
#' }
#' @export
build_chm_raster <- function(project, force = FALSE,
                             outlier_percentile = 99.5) {
  paths <- odm_product_paths(project)
  dsm_path <- unname(paths[["dsm"]])
  dtm_path <- unname(paths[["dtm"]])

  if (!file.exists(dsm_path) || !file.exists(dtm_path)) {
    stop("CHM needs DSM + DTM on disk. Missing: ",
         paste(c(if (!file.exists(dsm_path)) "DSM", if (!file.exists(dtm_path)) "DTM"),
               collapse = ", "),
         call. = FALSE)
  }

  # Default output: alongside the DSM, in the project's odm_dem/ folder.
  chm_path <- file.path(dirname(dsm_path), "chm.tif")

  if (!isTRUE(force) && file.exists(chm_path)) {
    return(chm_path)
  }

  dsm <- terra::rast(dsm_path)[[1L]]
  dtm <- terra::rast(dtm_path)[[1L]]
  # Resample DTM to match DSM grid if they differ (ODM usually keeps
  # them aligned but be defensive).
  if (!terra::compareGeom(dsm, dtm, stopOnError = FALSE, lyrs = FALSE,
                          messages = FALSE)) {
    dtm <- terra::resample(dtm, dsm, method = "bilinear")
  }
  chm <- dsm - dtm
  chm <- terra::clamp(chm, lower = 0, upper = Inf, values = TRUE)

  # Clip the extreme upper tail of physically implausible spikes. We
  # compute the cut from the cell values (na.rm) rather than a spatial
  # sample so the threshold is exact, then set everything strictly
  # above it to NA via clamp(values = FALSE).
  if (!is.null(outlier_percentile) && is.finite(outlier_percentile) &&
      outlier_percentile > 0 && outlier_percentile < 100) {
    vals <- terra::values(chm, mat = FALSE)
    vals <- vals[is.finite(vals)]
    if (length(vals)) {
      thr <- stats::quantile(vals, probs = outlier_percentile / 100,
                             na.rm = TRUE, names = FALSE)
      n_above <- sum(vals > thr)
      if (is.finite(thr) && n_above > 0) {
        chm <- terra::clamp(chm, lower = 0, upper = thr, values = FALSE)
        message(sprintf(
          "[chm] Clipped %d outlier pixel(s) above P%.1f = %.2f m to NA (max was %.2f m).",
          n_above, outlier_percentile, thr, max(vals)
        ))
      }
    }
  }

  names(chm) <- "CHM"
  terra::writeRaster(chm, chm_path, overwrite = TRUE, datatype = "FLT4S",
                     gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2", "BIGTIFF=IF_SAFER"))
  chm_path
}

#' Remove isolated spikes from a DSM / DTM / DEM
#'
#' Photogrammetric surface models routinely contain a handful of
#' isolated "needle" spikes — single pixels (or tiny clusters) that
#' jut tens of metres above an otherwise locally-smooth surface,
#' caused by mis-reconstructed dense-cloud points where the imagery
#' was blurry, low-texture or reflective. They are devastating for 3D
#' visualisation (the surface sprouts towers) and for any slope /
#' volume statistic, yet they are a vanishing fraction of pixels.
#'
#' Two complementary detectors run, and a cell flagged by either is
#' cleaned:
#'
#' 1. **Local needle filter** (always on): compares each cell to the
#'    median of its `window`x`window` neighbourhood and flags cells
#'    whose absolute deviation exceeds `max_deviation` metres. This
#'    catches single-pixel / tiny-cluster spikes that are taller than
#'    their immediate surroundings.
#' 2. **Height-above-ground filter** (opt-in via
#'    `max_height_above_ground`): some reconstruction artifacts are not
#'    needles but *wide towers* — coherent blobs several metres across
#'    where a blurry / low-texture patch ballooned upward. A small
#'    neighbourhood median cannot see those (the tower's own pixels
#'    dominate the window), so this second pass measures each cell's
#'    height above the ground and flags anything taller than
#'    `max_height_above_ground` metres. The ground reference is the
#'    `ground` raster (pass the DTM) when supplied, otherwise a coarse
#'    trend surface built by aggregating the DEM to ~`trend_cell_m`-metre
#'    cells (large enough to average over the towers). Use this for a
#'    survey where you know the real surface ceiling — e.g. a pasture
#'    whose canopy tops out near 15 m but whose DSM sprouts 50-130 m
#'    towers.
#'
#' Because real terrain and vegetation are spatially coherent, both
#' detectors leave genuine features intact; a global percentile clip
#' could not make that distinction.
#'
#' @param dem A `terra::SpatRaster` (first layer used) or a path to a
#'   DEM GeoTIFF.
#' @param window Odd integer neighbourhood size in pixels for the
#'   local median. Default `5`.
#' @param max_deviation Maximum allowed absolute deviation (metres)
#'   from the local median before a cell is treated as a needle spike.
#'   Default `3`.
#' @param max_height_above_ground Optional numeric (metres). When set,
#'   also flag cells whose height above the ground reference exceeds
#'   this — the wide-tower detector. `NULL` (default) disables it.
#' @param ground Optional ground reference for the height-above-ground
#'   filter: a `terra::SpatRaster` or path (typically the DTM). When
#'   `NULL`, a coarse trend is built from the DEM itself.
#' @param max_depth_below_ground Numeric (metres), default `2`. Used
#'   only when the height-above-ground filter is active. A DSM is the
#'   *top* surface, so a cell more than this far **below** the ground
#'   is impossible — a downward spike. Such cells are flagged and
#'   filled from the ground surface too. Set `NULL` to ignore
#'   downward spikes.
#' @param trend_cell_m Coarse-trend cell size in metres used when
#'   `ground` is not supplied. Default `15` — should comfortably
#'   exceed the width of the towers you want removed.
#' @param iterations Integer, default `2`. Number of detect-and-fill
#'   passes. A single pass cannot fully clean a wide blob (while it is
#'   present it drags the local trend toward itself, hiding its deepest
#'   core); a second pass over the now-mostly-cleaned surface removes
#'   the residual. The loop stops early once a pass changes nothing.
#' @param fill One of `"median"` (replace flagged cells with the local
#'   median / ground — keeps a continuous surface, best for 3D viz) or
#'   `"NA"` (drop them to NoData). Default `"median"`.
#' @param out_path Optional path to write the cleaned DEM. When
#'   `NULL` (default) nothing is written and the cleaned raster is
#'   returned in memory.
#' @return The cleaned `terra::SpatRaster` (invisibly when `out_path`
#'   is written).
#' @examples
#' \dontrun{
#'   # Needles only:
#'   despike_dem("odm_dem/dsm.tif", out_path = "odm_dem/dsm_clean.tif")
#'   # Wide towers too, using the DTM as ground (pasture canopy <= 20 m):
#'   despike_dem("odm_dem/dsm.tif", ground = "odm_dem/dtm.tif",
#'               max_height_above_ground = 20,
#'               out_path = "odm_dem/dsm_clean.tif")
#' }
#' @export
despike_dem <- function(dem, window = 5, max_deviation = 3,
                        max_height_above_ground = NULL, ground = NULL,
                        max_depth_below_ground = 2,
                        trend_cell_m = 15, iterations = 2L,
                        fill = c("median", "NA"), out_path = NULL) {
  fill <- match.arg(fill)
  r0 <- if (is.character(dem)) terra::rast(dem)[[1L]] else dem[[1L]]
  if (window %% 2L == 0L) window <- window + 1L

  # A single pass cannot fully clean a wide pit/tower: while the blob is
  # present it drags the local trend toward itself, so its deepest core
  # hides (DEM - trend stays within threshold there). Iterating fixes
  # this — once the first pass replaces the bulk of the blob with the
  # surrounding ground, the recomputed trend is clean and the residual
  # core stands out and is removed on the next pass. Convergence is fast
  # (typically 2 passes); the loop stops early when a pass changes
  # nothing.
  iterations <- max(1L, as.integer(iterations))
  cleaned <- r0
  for (it in seq_len(iterations)) {
    res <- despike_one_pass(cleaned, window, max_deviation,
                            max_height_above_ground, ground,
                            max_depth_below_ground, trend_cell_m, fill)
    cleaned <- res$cleaned
    if (is.na(res$n_spikes) || res$n_spikes == 0L) break
  }
  names(cleaned) <- names(r0)

  if (!is.null(out_path)) {
    dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
    terra::writeRaster(cleaned, out_path, overwrite = TRUE, datatype = "FLT4S",
                       gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2",
                                "BIGTIFF=IF_SAFER"))
    return(invisible(cleaned))
  }
  cleaned
}

# One despike pass: needle (local-median) + tower/pit (height-above-
# ground) detection, returning the cleaned raster and how many cells
# it replaced. Called repeatedly by despike_dem() to converge on wide
# blobs.
#' @noRd
despike_one_pass <- function(r, window, max_deviation,
                             max_height_above_ground, ground,
                             max_depth_below_ground, trend_cell_m, fill) {
  # --- pass 1: local needle detector ---
  local_med <- terra::focal(r, w = window, fun = "median",
                            na.policy = "omit", na.rm = TRUE)
  is_spike  <- abs(r - local_med) > max_deviation

  # --- pass 2: wide-tower / pit detector (height above ground) ---
  n_towers <- 0L
  ground_surface <- NULL
  if (!is.null(max_height_above_ground)) {
    # A coarse median trend of the DEM is robust to spikes by
    # construction (a few spike pixels never move a wide-cell median),
    # so it always seeds the ground reference.
    fact <- max(1L, as.integer(round(trend_cell_m / terra::res(r)[1L])))
    coarse <- terra::aggregate(r, fact = fact, fun = "median", na.rm = TRUE)
    trend  <- terra::resample(coarse, r, method = "bilinear")
    if (!is.null(ground)) {
      # Use the supplied ground (typically the DTM) where it is
      # trustworthy, but the DTM itself often carries the SAME spikes
      # as the DSM (the user confirmed downward spikes in both). Using
      # a spiked DTM as the reference makes shared spikes invisible
      # (DSM - DTM looks normal where both dip). So we robustify the
      # ground first: wherever it departs from its own coarse trend by
      # more than 5 m it is replaced by the trend.
      g <- if (is.character(ground)) terra::rast(ground)[[1L]] else ground[[1L]]
      if (!terra::compareGeom(r, g, stopOnError = FALSE, lyrs = FALSE,
                              messages = FALSE)) {
        g <- terra::resample(g, r, method = "bilinear")
      }
      g_fact   <- max(1L, as.integer(round(trend_cell_m / terra::res(g)[1L])))
      g_coarse <- terra::aggregate(g, fact = g_fact, fun = "median", na.rm = TRUE)
      g_trend  <- terra::resample(g_coarse, r, method = "bilinear")
      robust_g <- terra::ifel(abs(g - g_trend) > 5, g_trend, g)
      # Fill any gap where the ground has no data (the DEM often extends
      # a little past the DTM at the boundary, and uncovered pits there
      # would otherwise have no reference and survive) with the DEM's own
      # coarse trend.
      ground_surface <- terra::cover(robust_g, trend)
    } else {
      ground_surface <- trend
    }
    height <- r - ground_surface
    # Towers (too far above the surface) AND pits (below the ground —
    # a DSM is the *top* surface, so anything more than
    # max_depth_below_ground metres below the terrain is an artifact,
    # the downward spikes seen in 3D).
    is_tower <- height > max_height_above_ground
    if (!is.null(max_depth_below_ground)) {
      is_tower <- is_tower | (height < -abs(max_depth_below_ground))
    }
    is_tower <- terra::ifel(is.na(is_tower), FALSE, is_tower)
    n_towers <- tryCatch(
      as.integer(terra::global(is_tower, "sum", na.rm = TRUE)[[1L]]),
      error = function(e) NA_integer_)
    is_spike <- is_spike | is_tower
  }

  n_spikes <- tryCatch(
    as.integer(terra::global(is_spike, "sum", na.rm = TRUE)[[1L]]),
    error = function(e) NA_integer_
  )

  # Fill: needles -> local median; towers/pits -> ground surface (so the
  # surface returns to terrain level, not to a still-distorted local
  # median). A cell that is far from the ground (either way) is filled
  # from the ground surface; the rest from the local median.
  replacement <- if (!is.null(ground_surface)) {
    height_now <- r - ground_surface
    far_from_ground <- height_now > max_height_above_ground
    if (!is.null(max_depth_below_ground)) {
      far_from_ground <- far_from_ground |
        (height_now < -abs(max_depth_below_ground))
    }
    terra::ifel(far_from_ground, ground_surface, local_med)
  } else {
    local_med
  }
  cleaned <- if (identical(fill, "median")) {
    terra::ifel(is_spike, replacement, r)
  } else {
    terra::ifel(is_spike, NA, r)
  }
  names(cleaned) <- names(r)

  if (!is.na(n_spikes) && n_spikes > 0L) {
    extra <- if (!is.null(max_height_above_ground) && !is.na(n_towers)) {
      sprintf(" (incl. %d tower/pit pixel(s) outside [-%g, %g] m of ground)",
              n_towers, abs(max_depth_below_ground %||% 0),
              max_height_above_ground)
    } else ""
    message(sprintf(
      "[despike] Replaced %d cell(s)%s (fill = %s).",
      n_spikes, extra, fill
    ))
  }

  list(cleaned = cleaned, n_spikes = n_spikes)
}

#' Produce physically consistent DSM, DTM and CHM
#'
#' ODM generates the DSM (top of the dense cloud) and the DTM
#' (SMRF-classified ground, interpolated) by independent processes, so
#' they are not pixel-consistent: on bare ground the interpolated DTM
#' routinely sits a few centimetres *above* the DSM, which makes
#' `DSM - DTM` (the canopy height) negative over a large fraction of a
#' short-canopy survey. Despiking the two rasters separately can widen
#' that gap further. This helper rebuilds all three products so they
#' obey the physical constraints `CHM >= 0` and `DSM >= DTM`
#' everywhere, by construction:
#'
#' \enumerate{
#'   \item Despike the DTM (the ground) with [despike_dem()].
#'   \item Form `CHM = DSM - DTM_clean`, clamp it to `>= 0` (which
#'     turns the DSM's downward pits — where the surface dipped below
#'     ground — back into bare ground) and despike the result to
#'     remove canopy-height towers / needles.
#'   \item Rebuild `DSM = DTM_clean + CHM_clean`. Because the cleaned
#'     CHM is non-negative, the rebuilt DSM is never below the DTM.
#' }
#'
#' @param project Optional `dronebio_project`; when supplied the DSM
#'   and DTM are taken from [odm_product_paths()] and outputs default
#'   to the same `odm_dem/` folder.
#' @param dsm,dtm Raster paths or `terra::SpatRaster`s. Required when
#'   `project` is not given.
#' @param out_dir Output directory. Defaults to the DSM's folder.
#'   The function writes `dsm_consistent.tif`, `dtm_consistent.tif`
#'   and `chm_consistent.tif` there when `write = TRUE`.
#' @param canopy_ceiling Height (m) above the local canopy trend
#'   beyond which a CHM cell is treated as a tower spike and removed.
#'   Default `30` — keeps genuine tall trees, drops the reconstruction
#'   towers.
#' @param trend_cell_m,max_depth_below_ground,iterations Passed to
#'   [despike_dem()] for the DTM and CHM cleaning. Defaults `30`, `2`,
#'   `2`.
#' @param dtm_max_bump Height (m) above its own trend beyond which a
#'   DTM cell is treated as a spike. Default `5`.
#' @param spike_min_height,max_spike_area_m2,spike_dilate_cells Area-opening
#'   that removes isolated SfM "cone"/needle spikes the height-based
#'   `canopy_ceiling` filter misses (they are shorter than the ceiling). A
#'   CHM cell taller than `spike_min_height` m (default `1.5`) is "tall";
#'   contiguous tall patches of at most `max_spike_area_m2` m^2 (default `10`)
#'   are flattened to ground, larger patches (real canopy) are kept. The
#'   spike mask is grown `spike_dilate_cells` cells (default `15`) to also
#'   catch the cone skirt. Set `max_spike_area_m2 = 0` (or `NULL`) to disable.
#' @param write Logical. Write the three GeoTIFFs. Default `TRUE`.
#' @return Invisibly, a list with the cleaned `dsm`, `dtm`, `chm`
#'   `terra::SpatRaster`s and, when written, their `paths`.
#' @examples
#' \dontrun{
#'   project <- dronebio_project("~/flight")
#'   harmonize_dem_products(project)            # writes *_consistent.tif
#' }
#' @export
harmonize_dem_products <- function(project = NULL, dsm = NULL, dtm = NULL,
                                   out_dir = NULL,
                                   canopy_ceiling = 30,
                                   trend_cell_m = 30,
                                   max_depth_below_ground = 2,
                                   iterations = 2L,
                                   dtm_max_bump = 5,
                                   spike_min_height = 1.5,
                                   max_spike_area_m2 = 10,
                                   spike_dilate_cells = 15L,
                                   write = TRUE) {
  if (!is.null(project)) {
    paths <- odm_product_paths(project)
    if (is.null(dsm)) dsm <- unname(paths[["dsm"]])
    if (is.null(dtm)) dtm <- unname(paths[["dtm"]])
  }
  if (is.null(dsm) || is.null(dtm)) {
    stop("Provide either a project or both dsm and dtm.", call. = FALSE)
  }
  dsm_r <- if (is.character(dsm)) terra::rast(dsm)[[1L]] else dsm[[1L]]
  dtm_r <- if (is.character(dtm)) terra::rast(dtm)[[1L]] else dtm[[1L]]
  if (is.null(out_dir)) {
    out_dir <- if (is.character(dsm)) dirname(dsm) else getwd()
  }
  if (!terra::compareGeom(dsm_r, dtm_r, stopOnError = FALSE, lyrs = FALSE,
                          messages = FALSE)) {
    dtm_r <- terra::resample(dtm_r, dsm_r, method = "bilinear")
  }

  # 1. Clean the ground.
  message("[harmonize] Cleaning DTM (ground)...")
  dtm_clean <- despike_dem(dtm_r, max_height_above_ground = dtm_max_bump,
                           max_depth_below_ground = max_depth_below_ground,
                           trend_cell_m = trend_cell_m, iterations = iterations)

  # 2. Canopy height from the cleaned ground, non-negative, despiked.
  message("[harmonize] Building + cleaning CHM (canopy height)...")
  chm <- terra::clamp(dsm_r - dtm_clean, lower = 0, upper = Inf, values = TRUE)
  chm_clean <- despike_dem(chm, max_height_above_ground = canopy_ceiling,
                           max_depth_below_ground = NULL,
                           trend_cell_m = trend_cell_m, iterations = iterations)
  chm_clean <- terra::clamp(chm_clean, lower = 0, upper = Inf, values = TRUE)

  # 2b. Area-opening: flatten small isolated CHM spikes (SfM "cones") to
  #     ground while preserving large contiguous canopy. The height-based
  #     despike above keeps anything shorter than canopy_ceiling, so the
  #     isolated few-metre cones that pock low-texture pasture survive it;
  #     this removes them by patch AREA, not height.
  chm_clean <- area_open_chm_spikes(chm_clean, min_height = spike_min_height,
                                    max_area_m2 = max_spike_area_m2,
                                    dilate_cells = spike_dilate_cells)

  # 3. Rebuild a consistent surface: DSM = ground + canopy >= ground.
  dsm_clean <- dtm_clean + chm_clean
  names(dtm_clean) <- "DTM"; names(chm_clean) <- "CHM"; names(dsm_clean) <- "DSM"

  result <- list(dsm = dsm_clean, dtm = dtm_clean, chm = chm_clean)

  if (isTRUE(write)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    gopt <- c("COMPRESS=DEFLATE", "PREDICTOR=2", "BIGTIFF=IF_SAFER")
    paths_out <- c(
      dsm = file.path(out_dir, "dsm_consistent.tif"),
      dtm = file.path(out_dir, "dtm_consistent.tif"),
      chm = file.path(out_dir, "chm_consistent.tif")
    )
    terra::writeRaster(dsm_clean, paths_out[["dsm"]], overwrite = TRUE,
                       datatype = "FLT4S", gdal = gopt)
    terra::writeRaster(dtm_clean, paths_out[["dtm"]], overwrite = TRUE,
                       datatype = "FLT4S", gdal = gopt)
    terra::writeRaster(chm_clean, paths_out[["chm"]], overwrite = TRUE,
                       datatype = "FLT4S", gdal = gopt)
    result$paths <- paths_out
    message(sprintf("[harmonize] Wrote consistent DSM/DTM/CHM to %s", out_dir))
  }
  invisible(result)
}

# Area-opening on a CHM: flatten small isolated "tall" patches (SfM cone /
# needle spikes that rise out of low-texture ground) to 0 while preserving
# large contiguous canopy. Height-based despiking (canopy_ceiling) cannot do
# this because the spikes are SHORTER than the ceiling; the discriminator is
# the AREA of the connected tall patch, not its height. On a real grazed
# pasture this dropped 91 isolated cone spikes while keeping an 2820 m^2 tree
# stand and the 21.8 m tallest canopy untouched.
#' @noRd
area_open_chm_spikes <- function(chm, min_height = 1.5, max_area_m2 = 10,
                                 dilate_cells = 15L) {
  if (is.null(max_area_m2) || !is.finite(max_area_m2) || max_area_m2 <= 0) {
    return(chm)
  }
  cell_m2 <- prod(terra::res(chm))
  tall <- terra::ifel(chm > min_height, 1L, NA)
  pa <- tryCatch(terra::patches(tall, directions = 8, zeroAsNA = TRUE),
                 error = function(e) NULL)
  if (is.null(pa)) return(chm)
  fr <- terra::freq(pa)
  if (is.null(fr) || nrow(fr) == 0L) return(chm)
  small <- fr$value[fr$count * cell_m2 <= max_area_m2]
  if (length(small) == 0L) return(chm)            # only large canopy is tall
  spike <- terra::classify(pa,
                           cbind(fr$value, ifelse(fr$value %in% small, 1L, NA)),
                           others = NA)
  if (!is.null(dilate_cells) && dilate_cells >= 1) {
    w <- as.integer(dilate_cells); if (w %% 2L == 0L) w <- w + 1L
    spike <- terra::ifel(terra::focal(spike, w = w, fun = "max", na.rm = TRUE) > 0,
                         1L, NA)
  }
  terra::ifel(!is.na(spike), 0, chm)              # flatten spikes to ground (CHM 0)
}

# Harmonize a project's DSM/DTM/CHM in place: back up the raw ODM
# rasters to `*_raw.tif`, then overwrite the canonical `dsm.tif`,
# `dtm.tif` and `chm.tif` with the consistent versions so EVERYTHING
# downstream (odm_product_paths, build_chm_raster, the Shiny app,
# index computation) transparently uses the clean products. Idempotent:
# always harmonizes from the `*_raw.tif` backup when present, so a
# force-rerun does not compound the cleaning. Called by
# run_odm_dji_mavic_3m() when harmonize = TRUE.
#' @noRd
harmonize_project_dems_inplace <- function(project, canopy_ceiling = 18,
                                           trend_cell_m = 30) {
  paths   <- odm_product_paths(project)
  dsm     <- unname(paths[["dsm"]])
  dtm     <- unname(paths[["dtm"]])
  chm     <- unname(paths[["chm"]])
  if (!file.exists(dsm) || !file.exists(dtm)) return(invisible(FALSE))

  dem_dir <- dirname(dsm)
  raw_dsm <- file.path(dem_dir, "dsm_raw.tif")
  raw_dtm <- file.path(dem_dir, "dtm_raw.tif")
  # Source for harmonization is the raw ODM output. Back it up once, and
  # always read from the backup so repeated runs stay idempotent.
  if (!file.exists(raw_dsm)) file.copy(dsm, raw_dsm, overwrite = FALSE)
  if (!file.exists(raw_dtm)) file.copy(dtm, raw_dtm, overwrite = FALSE)
  src_dsm <- if (file.exists(raw_dsm)) raw_dsm else dsm
  src_dtm <- if (file.exists(raw_dtm)) raw_dtm else dtm

  res <- harmonize_dem_products(dsm = src_dsm, dtm = src_dtm,
                                canopy_ceiling = canopy_ceiling,
                                trend_cell_m = trend_cell_m, write = FALSE)
  gopt <- c("COMPRESS=DEFLATE", "PREDICTOR=2", "BIGTIFF=IF_SAFER")
  terra::writeRaster(res$dsm, dsm, overwrite = TRUE, datatype = "FLT4S", gdal = gopt)
  terra::writeRaster(res$dtm, dtm, overwrite = TRUE, datatype = "FLT4S", gdal = gopt)
  terra::writeRaster(res$chm, chm, overwrite = TRUE, datatype = "FLT4S", gdal = gopt)
  message(sprintf(
    "[harmonize] DSM/DTM/CHM made physically consistent in place (raw kept as dsm_raw.tif / dtm_raw.tif). canopy_ceiling = %g m.",
    canopy_ceiling))
  invisible(TRUE)
}

# Destination name inside products/ for every product finalize collects,
# keyed by odm_product_paths() key (plus the two computed rasters
# run_dronebio_workflow() writes). Rasters flatten to <key>.tif; the 3D
# deliverables keep their real extension -- note point_cloud.copc.laz,
# whose two-part extension tools::file_ext() cannot round-trip. The last
# four are multi-file assets and become folders; see collect_product().
#' @noRd
finalize_product_dests <- function() {
  c(
    orthomosaic      = "orthomosaic.tif",
    dsm              = "dsm.tif",
    dtm              = "dtm.tif",
    chm              = "chm.tif",
    dtm_csf          = "dtm_csf.tif",
    chm_csf          = "chm_csf.tif",
    spectral_indices = "spectral_indices.tif",
    biomass_proxy    = "biomass_proxy.tif",
    point_cloud_copc = "point_cloud.copc.laz",
    point_cloud_laz  = "point_cloud.laz",
    point_cloud_las  = "point_cloud.las",
    point_cloud_ply  = "point_cloud.ply",
    mesh_ply         = "mesh.ply",
    textured_glb     = "textured_model.glb",
    textured_glb_25d = "textured_model_25d.glb",
    report           = "report.pdf",
    textured_obj     = "textured_model",
    textured_obj_25d = "textured_model_25d",
    tiles_3d         = "3d_tiles",
    map_tiles_dir    = "orthomosaic_tiles"
  )
}

# TRUE when `child` is `parent` itself or lives underneath it.
#' @noRd
path_within <- function(child, parent) {
  sep <- .Platform$file.sep
  startsWith(paste0(normalizePath(child,  mustWork = FALSE), sep),
             paste0(normalizePath(parent, mustWork = FALSE), sep))
}

# Copy one file and confirm the bytes actually landed. file.copy()'s
# return value is not proof enough when the source is about to be
# deleted: on a full disk or a stalled cloud-sync folder it can leave a
# short file behind, and finalize would then unlink() the only good copy.
#' @noRd
copy_verified <- function(src, dest) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  ok <- tryCatch(file.copy(src, dest, overwrite = TRUE),
                 error = function(e) FALSE)
  if (!isTRUE(ok)) return(FALSE)
  s <- file.info(src)$size
  d <- file.info(dest)$size
  isTRUE(is.finite(s) && is.finite(d) && s == d)
}

# Recursively copy a whole product directory. The 3D tile set and the XYZ
# map tiles are indexes plus payload -- tileset.json on its own points at
# .b3dm files that would no longer exist -- so they travel as directories.
#' @noRd
copy_tree_verified <- function(src_dir, dest_dir) {
  rel <- list.files(src_dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  if (!length(rel)) return(FALSE)
  ok <- TRUE
  for (f in rel) {
    if (!copy_verified(file.path(src_dir, f), file.path(dest_dir, f))) ok <- FALSE
  }
  ok
}

# Files that make up a textured OBJ. The .obj names its material library
# with a bare `mtllib odm_textured_model_geo.mtl` line and the .mtl names
# each texture PNG the same way, so the group has to travel together under
# its original filenames or the mesh loads untextured. The Shiny 3D tab
# relies on exactly that layout too: it serves the OBJ's folder and probes
# for <stem>.mtl beside it.
#' @noRd
obj_asset_files <- function(obj_path) {
  d    <- dirname(obj_path)
  stem <- tools::file_path_sans_ext(basename(obj_path))
  files <- obj_path
  mtl <- file.path(d, paste0(stem, ".mtl"))
  if (file.exists(mtl)) {
    files <- c(files, mtl)
    lines <- tryCatch(readLines(mtl, warn = FALSE), error = function(e) character())
    # `map_Kd [options] texture.png` -- the filename is the last field.
    maps <- vapply(strsplit(trimws(grep("^\\s*map_", lines, value = TRUE)),
                            "[[:space:]]+"),
                   function(x) x[[length(x)]], character(1))
    files <- c(files, file.path(d, basename(maps)))
  }
  # Fallback for runs whose .mtl is missing or does not name its textures:
  # ODM writes them as <stem>_material0000_map_Kd.png next to the mesh.
  siblings <- list.files(d, pattern = "\\.(png|jpg|jpeg)$",
                         ignore.case = TRUE, full.names = TRUE)
  files <- c(files, siblings[startsWith(basename(siblings), stem)])
  unique(files[file.exists(files)])
}

# Copy one product to its destination in products/, returning the path it
# landed at or NA_character_ when any part of it failed to copy.
#' @noRd
collect_product <- function(key, src, dest) {
  if (key %in% c("textured_obj", "textured_obj_25d")) {
    group <- obj_asset_files(src)
    ok <- all(vapply(group,
                     function(f) copy_verified(f, file.path(dest, basename(f))),
                     logical(1)))
    return(if (ok) file.path(dest, basename(src)) else NA_character_)
  }
  if (key %in% c("tiles_3d", "map_tiles_dir")) {
    # tiles_3d resolves to the tileset.json index, map_tiles_dir to the
    # folder itself; either way the whole folder is the deliverable.
    src_dir <- if (dir.exists(src)) src else dirname(src)
    if (!copy_tree_verified(src_dir, dest)) return(NA_character_)
    return(if (dir.exists(src)) dest else file.path(dest, basename(src)))
  }
  if (copy_verified(src, dest)) dest else NA_character_
}

#' Collect the final products into one flat folder with metadata
#'
#' A DroneBioR run leaves a deep, ODM-shaped tree
#' (`odm_dataset/<name>/odm_dem/`, `.../odm_orthophoto/`,
#' `dronebior_analysis/`) plus raw backups, the redundant RGB-only
#' orthomosaic, the reflectance stack and run logs. For delivery you
#' usually want just the handful of products you will actually reuse,
#' in one place, with a machine-readable description.
#'
#' This copies every product [odm_product_paths()] resolves into
#' `out_dir` under simple names — the rasters as `orthomosaic.tif`,
#' `dsm.tif`, `dtm.tif`, `chm.tif`, `dtm_csf.tif`, `chm_csf.tif`,
#' `spectral_indices.tif` and `biomass_proxy.tif`; the 3D deliverables as
#' `point_cloud.copc.laz` / `.laz` / `.las` / `.ply`, `mesh.ply`,
#' `textured_model.glb` (plus the `_25d` variants) and `report.pdf` —
#' writes a single `metadata.json` (run parameters plus, per raster, the
#' CRS, resolution, extent, band names and per-band min/mean/max), and —
#' unless `remove_scaffolding = FALSE` — deletes the ODM scaffolding,
#' the raw DEM backups, the RGB-only ortho, the reflectance stack and
#' the logs, leaving only `out_dir`.
#'
#' Multi-file products are copied as folders rather than flattened,
#' because their internal references are by bare filename and would break
#' otherwise: the textured OBJ lands in `textured_model/` alongside its
#' `.mtl` and texture images under their original names, and the tile sets
#' land whole in `3d_tiles/` and `orthomosaic_tiles/`.
#'
#' @section Disk space and the no-loss guarantee:
#' The point clouds and textured meshes are by far the largest files a run
#' produces — several GB is routine — and they are copied, not moved, so
#' `out_dir` needs as much free space as the products themselves before
#' the scaffolding goes away. Every copy is size-checked afterwards, and
#' if any of them fails, or if `products` excludes something that is on
#' disk, the scaffolding is **kept** and a warning is raised. Nothing is
#' deleted on the strength of a copy that did not land.
#'
#' @param project A `dronebio_project`.
#' @param orthomosaic Path to the orthomosaic to keep (default: the
#'   7-band DJI stack when present, else the RGB orthomosaic).
#' @param indices,biomass_proxy Optional paths to the spectral index
#'   stack and biomass proxy (default: the files
#'   `run_dronebio_workflow()` writes under the project output dir).
#' @param out_dir Destination folder. Default
#'   `<project_dir>/products`.
#' @param extra_metadata Named list merged into the metadata JSON
#'   (e.g. `list(flight = "ifasbahia10", speed = "balanced")`).
#' @param remove_scaffolding Logical, default `TRUE`. Delete the
#'   intermediate tree after the products are copied out.
#' @param expect Optional character vector of product names that the
#'   caller knows it asked for (e.g. `c("spectral_indices",
#'   "biomass_proxy")` when indices were requested). Any of these whose
#'   source file is missing trigger a warning, so an incomplete
#'   `products/` folder (e.g. indices that crashed before being written)
#'   is never shipped silently. Default `character()` warns about nothing.
#'   Products that exist on disk but are not collected warn regardless of
#'   `expect` — see the disk-space section.
#' @param products Character vector of product keys to collect, from
#'   [odm_product_paths()] plus `"spectral_indices"` and
#'   `"biomass_proxy"`. Defaults to all of them. Narrow it when disk
#'   space is tight and you do not need, say, the redundant point-cloud
#'   formats; anything you drop that exists on disk is reported and stops
#'   the scaffolding from being removed.
#' @return Invisibly, a named character vector of the final product
#'   paths in `out_dir`.
#' @examples
#' \dontrun{
#'   res <- run_odm_dji_mavic_3m(project)
#'   wf  <- run_dronebio_workflow(project, res$stacked_orthomosaic)
#'   finalize_dronebio_products(project, extra_metadata = list(flight = "f1"))
#'   # Rasters and the COPC cloud only, skipping the redundant LAS/LAZ/PLY:
#'   finalize_dronebio_products(project,
#'                              products = c("orthomosaic", "dsm", "dtm", "chm",
#'                                           "point_cloud_copc", "textured_glb"))
#' }
#' @export
finalize_dronebio_products <- function(project,
                                       orthomosaic = NULL,
                                       indices = NULL,
                                       biomass_proxy = NULL,
                                       out_dir = NULL,
                                       extra_metadata = list(),
                                       remove_scaffolding = TRUE,
                                       expect = character(),
                                       products = names(finalize_product_dests())) {
  paths <- odm_product_paths(project)
  if (is.null(orthomosaic)) {
    dji_stack <- file.path(dirname(unname(paths[["orthomosaic"]])),
                           "odm_orthophoto_dji.tif")
    orthomosaic <- if (file.exists(dji_stack)) dji_stack
                   else unname(paths[["orthomosaic"]])
  }
  out_base <- project$output_dir
  if (is.null(indices))       indices       <- file.path(out_base, "spectral_indices.tif")
  if (is.null(biomass_proxy)) biomass_proxy <- file.path(out_base, "biomass_index_proxy.tif")
  if (is.null(out_dir))       out_dir       <- file.path(project$project_dir, "products")

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  dests    <- finalize_product_dests()
  products <- intersect(products, names(dests))

  # Source for every collectable product. Three are chosen or computed
  # outside ODM (the orthomosaic variant, and the two workflow rasters);
  # the rest come straight from odm_product_paths().
  src <- vapply(names(dests), function(k) {
    switch(k,
      orthomosaic      = orthomosaic,
      spectral_indices = indices,
      biomass_proxy    = biomass_proxy,
      unname(paths[[k]]))
  }, character(1))

  out_paths <- character()
  failed    <- character()
  for (nm in products) {
    s <- src[[nm]]
    if (is.null(s) || !nzchar(s) || !file.exists(s)) next
    landed <- collect_product(nm, s, file.path(out_dir, dests[[nm]]))
    if (is.na(landed)) failed <- c(failed, nm) else out_paths[nm] <- landed
  }

  # Surface expected-but-missing products loudly. The computed products
  # (spectral_indices, biomass_proxy) are silently absent when the indices
  # step did not run or crashed; without this, finalize would quietly ship an
  # incomplete products/ folder and then delete the intermediates (the
  # evidence) when remove_scaffolding = TRUE.
  missing_expected <- setdiff(expect, names(out_paths))
  if (length(missing_expected) > 0) {
    # Name each one by the file it would have been, falling back to the
    # key itself so a typo in `expect` reads as the typo, not as NA.
    labels <- ifelse(missing_expected %in% names(dests),
                     dests[missing_expected], missing_expected)
    warning(sprintf(
      paste0("[finalize] expected product(s) not found, so products/ is ",
             "incomplete: %s. The source file(s) were missing - re-run the ",
             "step that makes them (run_dronebio_workflow() for the indices) ",
             "and finalize again."),
      paste(labels, collapse = ", ")
    ), call. = FALSE)
  }

  # Two ways a product that IS on disk can end up outside products/: the
  # copy failed, or `products` excluded it. Either way, removing the
  # scaffolding would destroy the only copy -- exactly how the point
  # clouds and textured meshes used to disappear -- so say so and keep it.
  if (length(failed) > 0) {
    warning(sprintf(
      paste0("[finalize] %d product(s) could not be copied into %s: %s. ",
             "The intermediates were KEPT so nothing is lost - free disk ",
             "space (the point clouds and meshes are several GB), check ",
             "permissions, and finalize again."),
      length(failed), out_dir, paste(dests[failed], collapse = ", ")
    ), call. = FALSE)
  }
  dropped <- setdiff(names(dests), products)
  dropped <- dropped[vapply(dropped, function(k) file.exists(src[[k]]), logical(1))]
  if (isTRUE(remove_scaffolding) && length(dropped) > 0) {
    warning(sprintf(
      paste0("[finalize] %d product(s) exist but were excluded by ",
             "`products`, and removing the scaffolding would delete the ",
             "only copy: %s. The intermediates were KEPT. Widen `products`, ",
             "or pass remove_scaffolding = FALSE and prune by hand."),
      length(dropped), paste(dropped, collapse = ", ")
    ), call. = FALSE)
  }

  # Carry over the small CSV summaries if present.
  for (csv in c("spectral_index_summary.csv", "reflectance_summary.csv")) {
    s <- file.path(out_base, csv)
    if (file.exists(s)) file.copy(s, file.path(out_dir, csv), overwrite = TRUE)
  }

  # Build and write metadata describing each product.
  meta <- build_products_metadata(out_paths, extra_metadata, out_dir = out_dir)
  meta_path <- file.path(out_dir, "metadata.json")
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    writeLines(jsonlite::toJSON(meta, auto_unbox = TRUE, pretty = TRUE,
                                null = "null", digits = 6), meta_path)
  } else {
    # Minimal fallback: dput() the list so it is still recoverable.
    meta_path <- file.path(out_dir, "metadata.txt")
    utils::capture.output(dput(meta), file = meta_path)
  }

  keep_all <- length(failed) == 0 && length(dropped) == 0
  if (isTRUE(remove_scaffolding) && keep_all) {
    # Remove the deep ODM tree and the intermediate analysis folder --
    # never one that contains out_dir, or the products would go with it.
    for (d in c(project$odm_dataset_dir, out_base)) {
      if (dir.exists(d) && !path_within(out_dir, d)) {
        unlink(d, recursive = TRUE, force = TRUE)
      }
    }
    message(sprintf("[finalize] %d products + metadata.json in %s; intermediates removed.",
                    length(out_paths), out_dir))
  } else {
    message(sprintf("[finalize] %d products + metadata.json in %s%s.",
                    length(out_paths), out_dir,
                    if (isTRUE(remove_scaffolding)) "; intermediates kept" else ""))
  }
  invisible(out_paths)
}

# Assemble a metadata list describing each output. Rasters get CRS,
# resolution, extent, band names and per-band min/mean/max; the point
# clouds, meshes, tile sets and report get a path + byte count, so
# metadata.json is a complete inventory of what products/ holds rather
# than only of the parts terra can open.
#' @noRd
build_products_metadata <- function(out_paths, extra_metadata = list(),
                                    out_dir = NULL) {
  # Products inside their own folder (the OBJ group, the tile sets) are
  # named relative to out_dir so the folder is visible in the manifest.
  rel_to_out <- function(p) {
    if (!is.null(out_dir) && path_within(p, out_dir)) {
      sub("^[/\\\\]", "",
          substring(normalizePath(p, mustWork = FALSE),
                    nchar(normalizePath(out_dir, mustWork = FALSE)) + 1L))
    } else basename(p)
  }
  product_meta <- list()
  for (nm in names(out_paths)) {
    p <- out_paths[[nm]]
    r <- if (grepl("\\.tif$", p, ignore.case = TRUE)) {
      tryCatch(terra::rast(p), error = function(e) NULL)
    } else NULL
    if (is.null(r)) {
      # A multi-file asset is reported by its folder, not its index file:
      # the deliverable is textured_model/ or 3d_tiles/ as a whole. Those
      # are the entries that sit one level below out_dir.
      in_own_folder <- !is.null(out_dir) &&
        !identical(normalizePath(dirname(p), mustWork = FALSE),
                   normalizePath(out_dir,    mustWork = FALSE))
      root <- if (dir.exists(p)) p else if (in_own_folder) dirname(p) else p
      files <- if (dir.exists(root)) {
        list.files(root, recursive = TRUE, all.files = TRUE, no.. = TRUE,
                   full.names = TRUE)
      } else root
      product_meta[[nm]] <- list(
        file  = rel_to_out(root),
        files = length(files),
        bytes = tryCatch(sum(as.numeric(file.info(files)$size), na.rm = TRUE),
                         error = function(e) NA_real_)
      )
      next
    }
    stats <- tryCatch(
      terra::global(r, c("min", "mean", "max"), na.rm = TRUE),
      error = function(e) NULL)
    e <- as.vector(terra::ext(r))
    product_meta[[nm]] <- list(
      file        = rel_to_out(p),
      bands       = as.integer(terra::nlyr(r)),
      band_names  = names(r),
      crs         = tryCatch(terra::crs(r, describe = TRUE)$name,
                             error = function(e) NA_character_),
      resolution_m = unname(round(terra::res(r), 4)),
      extent      = list(xmin = e[[1]], xmax = e[[2]],
                         ymin = e[[3]], ymax = e[[4]]),
      stats = if (!is.null(stats)) {
        stats::setNames(lapply(seq_len(nrow(stats)), function(i)
          as.list(round(unlist(stats[i, ]), 4))), rownames(stats))
      } else NULL
    )
  }
  c(
    list(
      generator   = sprintf("DroneBioR %s",
                            as.character(utils::packageVersion("DroneBioR"))),
      products    = product_meta
    ),
    extra_metadata
  )
}

#' Detect existing ODM project subdirectories in a project root
#'
#' Walks `<project_dir>/outputs/` looking for any folder layout that
#' looks like an ODM project — that is, any
#' `<subdir>/<project_name>/odm_orthophoto/odm_orthophoto.tif`. Lets the
#' Shiny app populate a selector instead of assuming the canonical
#' `outputs/odm_micasense_dataset/micasense/` defaults.
#'
#' @param project_dir Path to a DroneBioR project root.
#' @return Data frame with `dataset_subdir`, `project_name`,
#'   `orthomosaic`, sorted with most-recently-modified first. Empty
#'   when nothing is found.
#' @examples
#' detect_odm_projects(tempdir())
#' @export
detect_odm_projects <- function(project_dir) {
  empty <- data.frame(
    dataset_subdir = character(),
    project_name   = character(),
    orthomosaic    = character(),
    stringsAsFactors = FALSE
  )
  if (!is.character(project_dir) || !length(project_dir) ||
      !nzchar(project_dir) || !dir.exists(project_dir)) {
    return(empty)
  }
  outputs <- file.path(project_dir, "outputs")
  if (!dir.exists(outputs)) return(empty)

  # Limited-depth scan instead of `list.files(recursive = TRUE)` — the
  # recursive variant walks every leaf of the outputs tree and on
  # OneDrive-synced folders that can take 1+ seconds for large runs.
  # We only ever need 2 levels: <subdir>/<project_name>/odm_orthophoto/.
  subdirs <- list.dirs(outputs, recursive = FALSE, full.names = TRUE)
  results <- list()
  for (sub in subdirs) {
    project_dirs <- list.dirs(sub, recursive = FALSE, full.names = TRUE)
    for (pd in project_dirs) {
      ortho <- file.path(pd, "odm_orthophoto", "odm_orthophoto.tif")
      if (file.exists(ortho)) {
        results[[length(results) + 1L]] <- list(
          dataset_subdir = file.path("outputs", basename(sub)),
          project_name   = basename(pd),
          orthomosaic    = ortho
        )
      }
    }
  }
  if (!length(results)) return(empty)
  out <- do.call(rbind, lapply(results, as.data.frame, stringsAsFactors = FALSE))
  out$mtime <- file.info(out$orthomosaic)$mtime
  out <- out[order(out$mtime, decreasing = TRUE), , drop = FALSE]
  out$mtime <- NULL
  rownames(out) <- NULL
  out
}

#' Summarize available ODM products
#'
#' @param project A `dronebio_project` object.
#' @return A data frame with product, path, availability and file size.
#' @examples
#' project <- dronebio_project(project_dir = tempdir())
#' summarize_odm_products(project)
#' @export
summarize_odm_products <- function(project) {
  paths <- odm_product_paths(project)
  exists <- file.exists(paths)
  size_mb <- rep(NA_real_, length(paths))
  size_mb[exists] <- round(file.info(paths[exists])$size / 1024^2, 2)

  data.frame(
    product = names(paths),
    available = exists,
    size_mb = size_mb,
    path = unname(paths),
    stringsAsFactors = FALSE
  )
}
