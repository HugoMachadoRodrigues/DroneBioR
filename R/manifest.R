#' List MicaSense image files
#'
#' @param images_dir Folder containing raw image files.
#' @return A data frame with file path, file name, capture id, band id and size.
#' @examples
#' tmp <- tempfile("micasense-"); dir.create(tmp)
#' for (cap in sprintf("IMG_%04d", 1:3))
#'   for (band in 1:5)
#'     file.create(file.path(tmp, paste0(cap, "_", band, ".tif")))
#' head(list_micasense_images(tmp))
#' @export
list_micasense_images <- function(images_dir) {
  if (!dir.exists(images_dir)) {
    stop("MicaSense image directory not found: ", images_dir, call. = FALSE)
  }

  files <- list.files(
    images_dir,
    pattern = "\\.(tif|tiff|jpg|jpeg)$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (length(files) == 0) {
    stop("No image files found in: ", images_dir, call. = FALSE)
  }

  names_only <- basename(files)
  parsed <- regexec("^(.+)_([0-9]+)\\.[A-Za-z0-9]+$", names_only)
  matches <- regmatches(names_only, parsed)
  ok <- lengths(matches) == 3

  if (!all(ok)) {
    bad <- names_only[!ok]
    stop(
      "Some image names do not match the expected MicaSense pattern ",
      "'capture_band.tif': ",
      paste(utils::head(bad, 10), collapse = ", "),
      call. = FALSE
    )
  }

  capture_id <- vapply(matches, `[`, character(1), 2)
  band_id <- as.integer(vapply(matches, `[`, character(1), 3))

  data.frame(
    file = files,
    filename = names_only,
    capture_id = capture_id,
    band_id = band_id,
    file_size_mb = round(file.info(files)$size / 1024^2, 3),
    stringsAsFactors = FALSE
  )
}

#' List generic aerial images for an ODM project
#'
#' Permissive image lister for non-MicaSense flights (Sony RX1R, DJI Phantom
#' / Mavic, Phase One, generic RGB). Returns a `list_micasense_images()`-
#' shaped manifest so [copy_images_for_odm()] and [run_odm_project()] can
#' consume it transparently, but without enforcing the
#' `^(.+)_([0-9]+)\.[A-Za-z0-9]+$` capture/band filename pattern. Accepts
#' `.jpg`, `.jpeg`, `.png`, `.tif` and `.tiff` (case-insensitive).
#'
#' **DJI Mavic 3M datasets** drop their multispectral `_MS_{G,R,RE,NIR}.TIF`
#' siblings from the returned manifest when at least one matching `_D.JPG`
#' is also present. This keeps the existing `run_odm_project()` flow
#' viable (ODM only sees the RGB JPGs for SfM, which is what it can
#' actually handle), and the returned data frame gets an attribute
#' `dji_visible_multispectral = TRUE` so callers know the MS TIFs were
#' filtered out. For full DJI Mavic 3M processing — including the four
#' MS bands — use [list_dji_mavic_3m_images()] and
#' [run_odm_dji_mavic_3m()].
#'
#' @param images_dir Folder containing raw image files.
#' @return A data frame with the same columns as
#'   [list_micasense_images()]: `file`, `filename`, `capture_id`, `band_id`,
#'   `file_size_mb`. For aerial RGB sets, `capture_id` is the base filename
#'   without extension and `band_id` is always `1L`.
#' @examples
#' tmp <- tempfile("aerial-"); dir.create(tmp)
#' for (i in 1:3) file.create(file.path(tmp, paste0("DJI_", sprintf("%04d", i), ".JPG")))
#' head(list_aerial_images(tmp))
#' @export
list_aerial_images <- function(images_dir) {
  if (!dir.exists(images_dir)) {
    stop("Image directory not found: ", images_dir, call. = FALSE)
  }
  files <- list.files(
    images_dir,
    pattern = "\\.(jpe?g|png|tiff?)$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (length(files) == 0) {
    stop("No image files found in: ", images_dir, call. = FALSE)
  }
  names_only <- basename(files)

  # DJI Mavic 3M datasets ship 1 RGB JPG + 4 single-band MS TIFFs per
  # capture. ODM cannot reconstruct from the 5-image bursts directly, so
  # when we detect the DJI Mavic 3M filename pattern we keep only the
  # RGB JPGs and tag the manifest with `dji_visible_multispectral = TRUE`
  # so downstream callers know the MS TIFs were filtered out. The
  # MS-band orchestrator [run_odm_dji_mavic_3m()] runs ODM per band on
  # the dropped TIFs separately and stitches the four MS orthos onto
  # the RGB ortho grid.
  dji_visible <- grepl("^DJI_[0-9]+_[0-9]+_D\\.(jpe?g)$", names_only,
                       ignore.case = TRUE)
  dji_ms      <- grepl("^DJI_[0-9]+_[0-9]+_MS_(G|R|RE|NIR)\\.tiff?$",
                       names_only, ignore.case = TRUE)
  if (any(dji_visible) && any(dji_ms)) {
    files <- files[dji_visible]
    names_only <- names_only[dji_visible]
  }

  out <- data.frame(
    file         = files,
    filename     = names_only,
    capture_id   = tools::file_path_sans_ext(names_only),
    band_id      = 1L,
    file_size_mb = round(file.info(files)$size / 1024^2, 3),
    stringsAsFactors = FALSE
  )
  attr(out, "dji_visible_multispectral") <- any(dji_visible) && any(dji_ms)
  out
}

#' List DJI Mavic 3M images grouped by camera band
#'
#' Splits a folder of DJI Mavic 3M raw images into the five camera
#' streams the platform produces per capture: the RGB visible (`D`,
#' `.JPG`) plus the four single-band multispectral TIFFs (`MS_G`,
#' `MS_R`, `MS_RE`, `MS_NIR`). Each stream is returned as a
#' [list_aerial_images()]-style manifest so [copy_images_for_odm()] /
#' [run_odm_project()] can consume it transparently in a per-band ODM
#' workflow (see [run_odm_dji_mavic_3m()]).
#'
#' Files that do not match the DJI Mavic 3M naming convention are
#' ignored. A folder that contains zero `_D.JPG` *and* zero `_MS_*.TIF`
#' is treated as an error.
#'
#' @param images_dir Folder containing raw DJI Mavic 3M images.
#' @return A named list with up to five elements - `D`, `MS_G`, `MS_R`,
#'   `MS_RE`, `MS_NIR` - each a data frame in the
#'   [list_aerial_images()] shape. Bands that have no matching images
#'   are omitted from the list.
#' @examples
#' tmp <- tempfile("djim3m-"); dir.create(tmp)
#' for (i in 1:3) {
#'   stem <- sprintf("DJI_20260501132033_%04d", i)
#'   file.create(file.path(tmp, paste0(stem, "_D.JPG")))
#'   for (b in c("G", "R", "RE", "NIR"))
#'     file.create(file.path(tmp, paste0(stem, "_MS_", b, ".TIF")))
#' }
#' names(list_dji_mavic_3m_images(tmp))
#' @export
list_dji_mavic_3m_images <- function(images_dir) {
  if (!dir.exists(images_dir)) {
    stop("Image directory not found: ", images_dir, call. = FALSE)
  }
  files <- list.files(
    images_dir,
    pattern = "\\.(jpe?g|tiff?)$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (length(files) == 0L) {
    stop("No image files found in: ", images_dir, call. = FALSE)
  }
  names_only <- basename(files)

  band_patterns <- list(
    D      = "^DJI_[0-9]+_[0-9]+_D\\.(jpe?g)$",
    MS_G   = "^DJI_[0-9]+_[0-9]+_MS_G\\.tiff?$",
    MS_R   = "^DJI_[0-9]+_[0-9]+_MS_R\\.tiff?$",
    MS_RE  = "^DJI_[0-9]+_[0-9]+_MS_RE\\.tiff?$",
    MS_NIR = "^DJI_[0-9]+_[0-9]+_MS_NIR\\.tiff?$"
  )

  manifest_for <- function(sel) {
    if (!any(sel)) return(NULL)
    data.frame(
      file         = files[sel],
      filename     = names_only[sel],
      capture_id   = tools::file_path_sans_ext(names_only[sel]),
      band_id      = 1L,
      file_size_mb = round(file.info(files[sel])$size / 1024^2, 3),
      stringsAsFactors = FALSE
    )
  }

  out <- lapply(band_patterns, function(p) {
    manifest_for(grepl(p, names_only, ignore.case = TRUE))
  })
  out <- Filter(Negate(is.null), out)

  if (length(out) == 0L) {
    stop(
      "No DJI Mavic 3M images found in: ", images_dir,
      "\nExpected filenames like DJI_<dt>_<idx>_D.JPG and ",
      "DJI_<dt>_<idx>_MS_{G,R,RE,NIR}.TIF.",
      call. = FALSE
    )
  }
  out
}

#' Copy images into an ODM project folder
#'
#' @param manifest Data frame from `list_micasense_images()`.
#' @param odm_images_dir ODM `images` folder.
#' @return Invisibly returns the destination paths.
#' @examples
#' src <- tempfile("src-"); dir.create(src)
#' for (cap in sprintf("IMG_%04d", 1:2))
#'   for (band in 1:5)
#'     file.create(file.path(src, paste0(cap, "_", band, ".tif")))
#' manifest <- list_micasense_images(src)
#' dest <- tempfile("odm-images-")
#' copy_images_for_odm(manifest, dest)
#' length(list.files(dest))
#' @export
copy_images_for_odm <- function(manifest, odm_images_dir) {
  dir.create(odm_images_dir, recursive = TRUE, showWarnings = FALSE)
  destination <- file.path(odm_images_dir, manifest$filename)
  source_size <- file.info(manifest$file)$size
  destination_size <- ifelse(file.exists(destination), file.info(destination)$size, NA_real_)
  needs_copy <- !file.exists(destination) | destination_size != source_size

  if (any(needs_copy)) {
    ok <- file.copy(manifest$file[needs_copy], destination[needs_copy], overwrite = TRUE)
    if (!all(ok)) {
      failed <- manifest$filename[needs_copy][!ok]
      stop("Failed to copy images for ODM: ", paste(failed, collapse = ", "), call. = FALSE)
    }
  }

  invisible(destination)
}

#' Find the folder that actually holds the flight images.
#'
#' The Studio asks for a photos folder and a project folder, and nothing stops
#' the two being set to the same path. When that path is the parent of the
#' photos, every folder-level test - `has_djim3m_images()`,
#' `detect_camera_from_folder()`, the image listers - sees an empty directory
#' and answers as though the flight were something else. Nothing errors: the
#' run proceeds and produces the wrong product hours later.
#'
#' So resolve the folder before testing it. Images sitting directly in `path`
#' win. Otherwise, if exactly one immediate subfolder holds images, that is
#' unambiguous and is used. More than one is genuinely ambiguous and is
#' reported rather than guessed at.
#'
#' @param path Folder the user nominated.
#' @return A list with `dir` (resolved folder, or `NA_character_`), `n` (images
#'   found there), `moved` (whether resolution descended a level) and
#'   `candidates` (subfolders holding images, when the choice is ambiguous).
#' @noRd
resolve_images_dir <- function(path) {
  none <- list(dir = NA_character_, n = 0L, moved = FALSE, candidates = character(0))
  if (!is.character(path) || !length(path) || is.na(path[[1]]) ||
      !nzchar(path[[1]]) || !dir.exists(path[[1]])) {
    return(none)
  }
  path <- path[[1]]
  pat  <- "\\.(jpe?g|tif?f|png)$"
  n_here <- length(list.files(path, pattern = pat, ignore.case = TRUE))
  if (n_here > 0L) {
    return(list(dir = path, n = n_here, moved = FALSE, candidates = character(0)))
  }

  subs <- list.dirs(path, recursive = FALSE, full.names = TRUE)
  # An ODM output tree lives beside the photos in a typical project; it holds
  # thousands of images of its own and must never be mistaken for the source.
  subs <- subs[!basename(subs) %in% c("outputs", "output", "odm", "covariates")]
  subs <- subs[!startsWith(basename(subs), ".")]
  if (!length(subs)) return(none)

  counts <- vapply(subs, function(d)
    length(list.files(d, pattern = pat, ignore.case = TRUE)), integer(1))
  hits <- subs[counts > 0L]
  if (length(hits) == 1L) {
    return(list(dir = hits, n = unname(counts[counts > 0L]), moved = TRUE,
                candidates = character(0)))
  }
  if (length(hits) > 1L) {
    return(list(dir = NA_character_, n = 0L, moved = FALSE,
                candidates = basename(hits)))
  }
  none
}
