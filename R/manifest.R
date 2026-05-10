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
