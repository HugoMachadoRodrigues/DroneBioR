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
  size_ok <- function(p) {
    if (!file.exists(p)) return(FALSE)
    info <- tryCatch(file.info(p), error = function(e) NULL)
    if (is.null(info) || !is.finite(info$size)) return(FALSE)
    info$size / 1e6 >= min_size_mb
  }
  ortho <- size_ok(paths[["orthomosaic"]])
  dsm   <- size_ok(paths[["dsm"]])
  dtm   <- size_ok(paths[["dtm"]])
  pc    <- any(c(size_ok(paths[["point_cloud_copc"]]),
                 size_ok(paths[["point_cloud_laz"]]),
                 size_ok(paths[["point_cloud_las"]])))
  c(orthomosaic = ortho, dsm = dsm, dtm = dtm,
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

  rows <- list(
    validate_raster("Orthomosaic", "orthomosaic"),
    validate_raster("DSM", "dsm"),
    validate_raster("DTM", "dtm"),
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
