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
  c(
    orthomosaic        = project$odm_orthomosaic,
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

#' Local cache directory for project outputs (outside any cloud sync)
#'
#' @param project A `dronebio_project` object.
#' @param cache_root Root directory for the cache. Defaults to
#'   `~/.dronebior/cache`.
#' @return Absolute path to `<cache_root>/<sanitized-project-name>/`.
#' @noRd
local_cache_dir <- function(project, cache_root = file.path(Sys.getenv("HOME"), ".dronebior", "cache")) {
  slug <- gsub("[^A-Za-z0-9._-]+", "_", basename(project$project_dir))
  file.path(cache_root, slug)
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

#' Resolve a project file path to its local-cache copy when available.
#'
#' Reads against the cloud-synced project folder (OneDrive / iCloud /
#' Dropbox) can stall the UI for seconds at a time, especially in the
#' 3D Modeling tab where the DSM, point cloud and orthomosaic are all
#' large. After `sync_outputs_to_local_cache()` has run, those files
#' live under `~/.dronebior/cache/<slug>/`. This helper returns the
#' cached copy when it exists (same basename inside `local_cache_dir`),
#' and the original path otherwise. Cheap and safe to call on every
#' reactive invocation.
#'
#' @param path Canonical filesystem path (typically from
#'   `odm_product_paths(project)`).
#' @param project A `dronebio_project` object whose cache directory
#'   will be inspected. When `NULL`, returns `path` unchanged.
#' @return Cached path if a copy exists, otherwise the input `path`.
#' @noRd
cache_aware_path <- function(path, project) {
  if (is.null(project) || !is.character(path) || !length(path) ||
      !nzchar(path)) {
    return(path)
  }
  cache_dir <- tryCatch(local_cache_dir(project), error = function(e) NULL)
  if (is.null(cache_dir) || !nzchar(cache_dir)) return(path)
  cached <- file.path(cache_dir, basename(path))
  if (file.exists(cached)) cached else path
}

#' Copy ODM outputs to a fast local cache once, return the new paths
#'
#' Use when the project root lives inside OneDrive / Google Drive /
#' Dropbox and the heavy raster + point-cloud files keep triggering
#' background re-syncs. After this returns, the Shiny app can be
#' repointed at the local cache and never touch the cloud-synced
#' folder again. Files are skipped if a same-size copy already exists.
#'
#' @param project A `dronebio_project` object pointing at the
#'   (typically cloud-synced) outputs you want to migrate.
#' @param cache_root Root for the cache. Defaults to
#'   `~/.dronebior/cache`.
#' @param products Subset of product keys (see [odm_product_paths()])
#'   to copy. Default covers the analysis essentials.
#' @return List with `cache_dir` and a named `paths` character vector
#'   keyed by product (only entries actually present on disk).
#' @examples
#' \dontrun{
#'   project <- dronebio_project("~/cloud_project")
#'   cache <- sync_outputs_to_local_cache(project)
#'   cache$paths[["orthomosaic"]]
#' }
#' @export
sync_outputs_to_local_cache <- function(project,
                                        cache_root = file.path(Sys.getenv("HOME"),
                                                               ".dronebior", "cache"),
                                        products = c("orthomosaic", "dsm", "dtm",
                                                     "point_cloud_copc",
                                                     "point_cloud_laz",
                                                     "textured_obj",
                                                     "textured_glb")) {
  cache_dir <- local_cache_dir(project, cache_root = cache_root)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  paths <- odm_product_paths(project)
  out <- character()
  for (key in products) {
    src <- unname(paths[[key]])
    if (!length(src) || !nzchar(src) || !file.exists(src)) next
    dest <- file.path(cache_dir, basename(src))
    src_size  <- file.info(src)$size
    dest_size <- if (file.exists(dest)) file.info(dest)$size else NA_real_
    if (!is.na(dest_size) && dest_size == src_size) {
      out[key] <- dest
      next
    }
    ok <- tryCatch(file.copy(src, dest, overwrite = TRUE),
                   error = function(e) FALSE)
    if (isTRUE(ok)) {
      out[key] <- dest
    }
  }
  list(cache_dir = cache_dir, paths = out)
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
  cache_dir <- local_cache_dir(project)
  size_ok <- function(p) {
    # Prefer the cache copy when the project path is missing -- this
    # catches CHM (which we always write to the cache when DSM + DTM
    # are cached) and migrated outputs that no longer round-trip
    # through the OneDrive folder.
    if (!file.exists(p)) {
      cached <- file.path(cache_dir, basename(p))
      if (!file.exists(cached)) return(FALSE)
      p <- cached
    }
    info <- tryCatch(file.info(p), error = function(e) NULL)
    if (is.null(info) || !is.finite(info$size)) return(FALSE)
    info$size / 1e6 >= min_size_mb
  }
  ortho <- size_ok(paths[["orthomosaic"]])
  dsm   <- size_ok(paths[["dsm"]])
  dtm   <- size_ok(paths[["dtm"]])
  chm   <- size_ok(paths[["chm"]])
  pc    <- any(c(size_ok(paths[["point_cloud_copc"]]),
                 size_ok(paths[["point_cloud_laz"]]),
                 size_ok(paths[["point_cloud_las"]])))
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
  cache_dir <- local_cache_dir(project)
  # Prefer cache paths over project paths for the validation pass --
  # we want to validate the file the app will actually read, which
  # is the cache copy once migration / Build CHM has run.
  for (k in names(paths)) {
    cached <- file.path(cache_dir, basename(paths[[k]]))
    if (!file.exists(paths[[k]]) && file.exists(cached)) {
      paths[[k]] <- cached
    }
  }

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
#' GeoTIFF. By default reads/writes from the local cache directory
#' (`~/.dronebior/cache/<slug>/`) when DSM + DTM are already cached
#' there, so we never touch the cloud-synced project folder. Falls
#' back to writing into the project's `odm_dem/` directory otherwise.
#'
#' @param project A `dronebio_project` object.
#' @param force Logical. Recompute even when `chm.tif` already exists.
#' @param cache_aware Logical. Prefer the local cache when DSM + DTM
#'   already live there.
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
build_chm_raster <- function(project, force = FALSE, cache_aware = TRUE,
                             outlier_percentile = 99.5) {
  paths <- odm_product_paths(project)
  dsm_path <- unname(paths[["dsm"]])
  dtm_path <- unname(paths[["dtm"]])

  if (isTRUE(cache_aware)) {
    cache_dir <- local_cache_dir(project)
    cached_dsm <- file.path(cache_dir, basename(dsm_path))
    cached_dtm <- file.path(cache_dir, basename(dtm_path))
    if (file.exists(cached_dsm)) dsm_path <- cached_dsm
    if (file.exists(cached_dtm)) dtm_path <- cached_dtm
  }
  if (!file.exists(dsm_path) || !file.exists(dtm_path)) {
    stop("CHM needs DSM + DTM on disk. Missing: ",
         paste(c(if (!file.exists(dsm_path)) "DSM", if (!file.exists(dtm_path)) "DTM"),
               collapse = ", "),
         call. = FALSE)
  }

  # Default output: alongside the DSM. Either the cached copy (if cache
  # is in use) or the project's odm_dem/ folder.
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
                        trend_cell_m = 15,
                        fill = c("median", "NA"), out_path = NULL) {
  fill <- match.arg(fill)
  r <- if (is.character(dem)) terra::rast(dem)[[1L]] else dem[[1L]]
  if (window %% 2L == 0L) window <- window + 1L

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

  if (!is.na(n_spikes)) {
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

  if (!is.null(out_path)) {
    dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
    terra::writeRaster(cleaned, out_path, overwrite = TRUE, datatype = "FLT4S",
                       gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2",
                                "BIGTIFF=IF_SAFER"))
    return(invisible(cleaned))
  }
  cleaned
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
