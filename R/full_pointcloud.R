empty_point_cloud <- function() {
  data.frame(
    point_id = integer(),
    x = numeric(),
    y = numeric(),
    z = numeric(),
    classification = integer(),
    color = character(),
    stringsAsFactors = FALSE
  )
}

read_uint16_le <- function(con) {
  bytes <- readBin(con, what = "raw", n = 2L)
  sum(as.integer(bytes) * 256^(0:1))
}

read_uint32_le <- function(con) {
  bytes <- readBin(con, what = "raw", n = 4L)
  sum(as.integer(bytes) * 256^(0:3))
}

read_uint64_le <- function(con) {
  bytes <- readBin(con, what = "raw", n = 8L)
  sum(as.numeric(as.integer(bytes)) * 256^(0:7))
}

read_las_header <- function(con) {
  seek(con, where = 0L, origin = "start")
  signature <- readChar(con, nchars = 4L, useBytes = TRUE)
  if (!identical(signature, "LASF")) {
    stop("Not a LAS file. Expected LASF file signature.", call. = FALSE)
  }

  read_uint16_le(con)
  read_uint16_le(con)
  readBin(con, what = "raw", n = 16L)
  version_major <- readBin(con, what = "integer", n = 1L, size = 1L, signed = FALSE)
  version_minor <- readBin(con, what = "integer", n = 1L, size = 1L, signed = FALSE)
  readBin(con, what = "raw", n = 32L)
  readBin(con, what = "raw", n = 32L)
  read_uint16_le(con)
  read_uint16_le(con)

  header_size <- read_uint16_le(con)
  offset_to_point_data <- read_uint32_le(con)
  read_uint32_le(con)
  point_data_format_byte <- readBin(con, what = "integer", n = 1L, size = 1L, signed = FALSE)
  point_data_format <- bitwAnd(point_data_format_byte, 63L)
  point_data_record_length <- read_uint16_le(con)
  number_of_points <- read_uint32_le(con)
  vapply(seq_len(5L), function(i) read_uint32_le(con), numeric(1))

  scale <- readBin(con, what = "numeric", n = 3L, size = 8L, endian = "little")
  offset <- readBin(con, what = "numeric", n = 3L, size = 8L, endian = "little")
  bounds <- readBin(con, what = "numeric", n = 6L, size = 8L, endian = "little")

  if (isTRUE(number_of_points == 0) && header_size >= 375L) {
    seek(con, where = 247L, origin = "start")
    number_of_points <- read_uint64_le(con)
  }

  list(
    version = paste0(version_major, ".", version_minor),
    header_size = header_size,
    offset_to_point_data = offset_to_point_data,
    point_data_format = point_data_format,
    point_data_record_length = point_data_record_length,
    number_of_points = as.numeric(number_of_points),
    scale = scale,
    offset = offset,
    bounds = stats::setNames(bounds, c("max_x", "min_x", "max_y", "min_y", "max_z", "min_z"))
  )
}

read_int32_from_raw <- function(x) {
  readBin(x, what = "integer", n = 1L, size = 4L, signed = TRUE, endian = "little")
}

parse_las_records <- function(raw_records, point_ids, header) {
  n <- length(point_ids)
  if (n == 0) {
    return(empty_point_cloud())
  }

  record_length <- header$point_data_record_length
  x <- y <- z <- numeric(n)
  classification <- integer(n)
  red <- green <- blue <- rep(NA_integer_, n)

  class_pos <- if (header$point_data_format >= 6L) 17L else 16L
  rgb_pos <- switch(
    as.character(header$point_data_format),
    "2" = 21L,
    "3" = 29L,
    "5" = 29L,
    "7" = 31L,
    "8" = 31L,
    "10" = 31L,
    NA_integer_
  )

  for (i in seq_len(n)) {
    base <- (i - 1L) * record_length
    x[[i]] <- read_int32_from_raw(raw_records[(base + 1L):(base + 4L)]) * header$scale[[1]] + header$offset[[1]]
    y[[i]] <- read_int32_from_raw(raw_records[(base + 5L):(base + 8L)]) * header$scale[[2]] + header$offset[[2]]
    z[[i]] <- read_int32_from_raw(raw_records[(base + 9L):(base + 12L)]) * header$scale[[3]] + header$offset[[3]]

    if ((base + class_pos) <= length(raw_records)) {
      classification[[i]] <- as.integer(raw_records[[base + class_pos]])
    }

    if (!is.na(rgb_pos) && (base + rgb_pos + 5L) <= length(raw_records)) {
      red[[i]] <- readBin(raw_records[(base + rgb_pos):(base + rgb_pos + 1L)], "integer", n = 1L, size = 2L, signed = FALSE, endian = "little")
      green[[i]] <- readBin(raw_records[(base + rgb_pos + 2L):(base + rgb_pos + 3L)], "integer", n = 1L, size = 2L, signed = FALSE, endian = "little")
      blue[[i]] <- readBin(raw_records[(base + rgb_pos + 4L):(base + rgb_pos + 5L)], "integer", n = 1L, size = 2L, signed = FALSE, endian = "little")
    }
  }

  has_rgb <- any(is.finite(red) & is.finite(green) & is.finite(blue))
  if (has_rgb) {
    rgb_scale <- max(c(red, green, blue), na.rm = TRUE)
    rgb_scale <- if (is.finite(rgb_scale) && rgb_scale > 255) 65535 else 255
    color <- grDevices::rgb(
      pmax(0, pmin(red, rgb_scale)),
      pmax(0, pmin(green, rgb_scale)),
      pmax(0, pmin(blue, rgb_scale)),
      maxColorValue = rgb_scale
    )
  } else {
    color <- rep("#94a3b8", n)
  }

  data.frame(
    point_id = point_ids,
    x = x,
    y = y,
    z = z,
    classification = classification,
    color = color,
    stringsAsFactors = FALSE
  )
}

roi_bbox <- function(roi_polygon) {
  c(
    xmin = min(roi_polygon$x, na.rm = TRUE),
    xmax = max(roi_polygon$x, na.rm = TRUE),
    ymin = min(roi_polygon$y, na.rm = TRUE),
    ymax = max(roi_polygon$y, na.rm = TRUE)
  )
}

#' Build a 2D ROI polygon from selected points
#'
#' @param points Data frame with `x` and `y` coordinates.
#' @param method `hull` for a convex hull, or `bbox` for an axis-aligned box.
#' @return Data frame with polygon vertex coordinates.
#' @examples
#' set.seed(1)
#' pts <- data.frame(x = runif(50, 0, 10), y = runif(50, 0, 10))
#' build_roi_polygon(pts, method = "bbox")
#' @export
build_roi_polygon <- function(points, method = c("hull", "bbox")) {
  method <- match.arg(method)
  if (nrow(points) < 3) {
    return(data.frame(x = numeric(), y = numeric()))
  }
  if (!all(c("x", "y") %in% names(points))) {
    stop("points must contain x and y columns.", call. = FALSE)
  }

  if (identical(method, "bbox")) {
    xr <- range(points$x, na.rm = TRUE)
    yr <- range(points$y, na.rm = TRUE)
    return(data.frame(
      x = c(xr[[1]], xr[[2]], xr[[2]], xr[[1]]),
      y = c(yr[[1]], yr[[1]], yr[[2]], yr[[2]])
    ))
  }

  hull <- grDevices::chull(points$x, points$y)
  data.frame(x = points$x[hull], y = points$y[hull])
}

#' Test whether coordinates are inside a polygon ROI
#'
#' @param x Numeric x coordinates.
#' @param y Numeric y coordinates.
#' @param roi_polygon Data frame with `x` and `y` polygon vertices.
#' @return Logical vector.
#' @examples
#' roi <- data.frame(x = c(0, 5, 5, 0), y = c(0, 0, 5, 5))
#' points_in_roi(x = c(1, 6), y = c(1, 6), roi_polygon = roi)
#' @export
points_in_roi <- function(x, y, roi_polygon) {
  if (nrow(roi_polygon) < 3) {
    return(rep(FALSE, length(x)))
  }
  px <- roi_polygon$x
  py <- roi_polygon$y
  inside <- rep(FALSE, length(x))
  on_boundary <- rep(FALSE, length(x))
  j <- length(px)
  for (i in seq_along(px)) {
    cross <- (x - px[[j]]) * (py[[i]] - py[[j]]) - (y - py[[j]]) * (px[[i]] - px[[j]])
    within_x <- x >= min(px[[i]], px[[j]]) - 1e-9 & x <= max(px[[i]], px[[j]]) + 1e-9
    within_y <- y >= min(py[[i]], py[[j]]) - 1e-9 & y <= max(py[[i]], py[[j]]) + 1e-9
    on_boundary <- on_boundary | (abs(cross) <= 1e-8 & within_x & within_y)
    intersects <- ((py[[i]] > y) != (py[[j]] > y)) &
      (x < (px[[j]] - px[[i]]) * (y - py[[i]]) / ((py[[j]] - py[[i]]) + 1e-12) + px[[i]])
    inside <- xor(inside, intersects)
    j <- i
  }
  inside | on_boundary
}

#' Filter point cloud data by a polygon ROI
#'
#' @param points Data frame with `x` and `y` coordinates.
#' @param roi_polygon Data frame with `x` and `y` polygon vertices.
#' @return Filtered data frame.
#' @examples
#' set.seed(1)
#' pts <- data.frame(x = runif(50, 0, 10), y = runif(50, 0, 10), z = runif(50, 0, 5))
#' roi <- data.frame(x = c(2, 8, 8, 2), y = c(2, 2, 8, 8))
#' nrow(filter_points_by_roi(pts, roi))
#' @export
filter_points_by_roi <- function(points, roi_polygon) {
  if (nrow(points) == 0 || nrow(roi_polygon) < 3) {
    return(points[0, , drop = FALSE])
  }
  bbox <- roi_bbox(roi_polygon)
  in_bbox <- points$x >= bbox[["xmin"]] & points$x <= bbox[["xmax"]] &
    points$y >= bbox[["ymin"]] & points$y <= bbox[["ymax"]]
  candidates <- points[in_bbox, , drop = FALSE]
  candidates[points_in_roi(candidates$x, candidates$y, roi_polygon), , drop = FALSE]
}

read_las_records_by_index <- function(con, header, point_index) {
  n <- length(point_index)
  if (n == 0) {
    return(empty_point_cloud())
  }

  out <- vector("list", n)
  for (i in seq_along(point_index)) {
    offset <- header$offset_to_point_data + (point_index[[i]] - 1L) * header$point_data_record_length
    seek(con, where = offset, origin = "start")
    raw_record <- readBin(con, what = "raw", n = header$point_data_record_length)
    out[[i]] <- parse_las_records(raw_record, point_index[[i]], header)
  }
  do.call(rbind, out)
}

#' Read an uncompressed LAS point cloud
#'
#' This is a lightweight LAS reader used by the Shiny app when `lidR`, `rlas` or
#' PDAL are not available. It reads X/Y/Z, point classification and RGB colors
#' when they are present in common LAS point formats. Compressed LAZ/COPC files
#' are handled by `read_full_point_cloud()` when an uncompressed LAS sidecar is
#' available or an external reader is installed.
#'
#' @param path Path to an uncompressed `.las` file.
#' @param roi_polygon Optional polygon ROI with `x` and `y` columns.
#' @param max_points Maximum number of points to return when no ROI is supplied.
#' @param chunk_size Number of point records per scan chunk.
#' @return Data frame with point coordinates and attributes.
#' @examples
#' \dontrun{
#' pc <- read_las_point_cloud("flight.las", max_points = 50000)
#' }
#' @export
read_las_point_cloud <- function(path,
                                 roi_polygon = NULL,
                                 max_points = Inf,
                                 chunk_size = 100000) {
  if (!file.exists(path)) {
    stop("LAS file not found: ", path, call. = FALSE)
  }
  if (!grepl("\\.las$", path, ignore.case = TRUE)) {
    stop("read_las_point_cloud() only reads uncompressed .las files.", call. = FALSE)
  }

  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  header <- read_las_header(con)
  n_points <- header$number_of_points
  if (!is.finite(n_points) || n_points <= 0) {
    return(empty_point_cloud())
  }

  if (is.null(roi_polygon) && is.finite(max_points) && n_points > max_points) {
    set.seed(42)
    point_index <- sort(sample.int(n_points, max_points))
    out <- read_las_records_by_index(con, header, point_index)
    attr(out, "point_cloud_source") <- normalizePath(path, mustWork = FALSE)
    attr(out, "coordinate_source") <- "full_georeferenced"
    return(out)
  }

  bbox <- if (!is.null(roi_polygon) && nrow(roi_polygon) >= 3) roi_bbox(roi_polygon) else NULL
  chunks <- vector("list", ceiling(n_points / chunk_size))
  n_chunks <- 0L
  seek(con, where = header$offset_to_point_data, origin = "start")

  chunk_starts <- seq(1, n_points, by = chunk_size)
  for (chunk_start in chunk_starts) {
    chunk_n <- min(chunk_size, n_points - chunk_start + 1)
    raw_chunk <- readBin(con, what = "raw", n = chunk_n * header$point_data_record_length)
    if (length(raw_chunk) == 0) {
      break
    }
    point_ids <- seq(chunk_start, length.out = chunk_n)
    pts <- parse_las_records(raw_chunk, point_ids, header)

    if (!is.null(bbox)) {
      in_bbox <- pts$x >= bbox[["xmin"]] & pts$x <= bbox[["xmax"]] &
        pts$y >= bbox[["ymin"]] & pts$y <= bbox[["ymax"]]
      pts <- pts[in_bbox, , drop = FALSE]
    }
    if (!is.null(roi_polygon) && nrow(roi_polygon) >= 3 && nrow(pts) > 0) {
      pts <- pts[points_in_roi(pts$x, pts$y, roi_polygon), , drop = FALSE]
    }

    if (nrow(pts) > 0) {
      n_chunks <- n_chunks + 1L
      chunks[[n_chunks]] <- pts
    }
  }

  if (n_chunks == 0) {
    out <- empty_point_cloud()
  } else {
    out <- do.call(rbind, chunks[seq_len(n_chunks)])
  }
  attr(out, "point_cloud_source") <- normalizePath(path, mustWork = FALSE)
  attr(out, "coordinate_source") <- "full_georeferenced"
  out
}

compressed_sidecar_candidates <- function(path) {
  unique(c(
    sub("\\.copc\\.laz$", ".las", path, ignore.case = TRUE),
    sub("\\.laz$", ".las", path, ignore.case = TRUE),
    file.path(dirname(path), "odm_georeferenced_model.las")
  ))
}

read_with_optional_lidar_package <- function(path, roi_polygon = NULL,
                                              max_points = Inf) {
  pkg <- "lidR"
  if (!requireNamespace(pkg, quietly = TRUE)) {
    return(NULL)
  }
  reader <- get("readLAS", envir = asNamespace(pkg))

  # Build a PDAL-style filter string that lidR pushes down into the
  # LASlib/LASzip decoder so we do NOT decompress points outside the
  # ROI and so we drop attributes we never read.
  #   * `-keep_xy xmin ymin xmax ymax` is the LASlib spatial filter.
  #     For COPC files this also skips entire chunks that fall
  #     outside the bbox, which can turn a 1.5 GB LAZ read into a
  #     ~50 MB read for a 1 ha ROI.
  #   * `-keep_random_fraction <f>` decimates at the decoder before
  #     each point is decompressed. Used in the no-ROI preview path
  #     so a Load 3D scene click that just wants 35 k points does
  #     NOT decompress the full 50 M-point cloud first. Measured at
  #     ~22.7 s before, sub-second after.
  # `select = "xyzcr"` keeps XYZ + classification + RGB and drops
  #   intensity, return number, scan angle, GPS time, etc - those
  #   are not consumed downstream and they roughly double per-point
  #   memory when retained.
  filter_parts <- character()
  if (!is.null(roi_polygon) && is.data.frame(roi_polygon) &&
      nrow(roi_polygon) >= 3L) {
    xmin <- min(roi_polygon$x, na.rm = TRUE)
    xmax <- max(roi_polygon$x, na.rm = TRUE)
    ymin <- min(roi_polygon$y, na.rm = TRUE)
    ymax <- max(roi_polygon$y, na.rm = TRUE)
    if (is.finite(xmin) && is.finite(xmax) &&
        is.finite(ymin) && is.finite(ymax) && xmax > xmin && ymax > ymin) {
      filter_parts <- c(filter_parts,
                        sprintf("-keep_xy %.6f %.6f %.6f %.6f",
                                xmin, ymin, xmax, ymax))
    }
  } else if (is.finite(max_points) && max_points > 0) {
    # No ROI but a finite point cap. Read the header to estimate the
    # fraction we need to keep so the post-read random sample lands
    # near max_points. The estimate is intentionally conservative
    # (~3x the user's target) so the final sampling step still has
    # something to choose from after the decoder side strips most
    # of the file.
    n_total <- tryCatch({
      hdr_reader <- get("readLASheader", envir = asNamespace(pkg))
      hdr <- hdr_reader(path)
      slot_reader <- get("slot", envir = asNamespace("methods"))
      tryCatch(as.numeric(slot_reader(hdr, "PHB")[["Number of point records"]]),
               error = function(e) NA_real_)
    }, error = function(e) NA_real_)
    if (is.finite(n_total) && n_total > max_points * 3) {
      frac <- min(1, max(0.0001, (max_points * 3) / n_total))
      filter_parts <- c(filter_parts,
                        sprintf("-keep_random_fraction %.6f", frac))
    }
  }
  filter_str <- paste(filter_parts, collapse = " ")

  las <- tryCatch(
    if (nzchar(filter_str)) reader(path, filter = filter_str, select = "xyzcr")
    else                    reader(path,                       select = "xyzcr"),
    error = function(e) {
      # Some lidR builds reject the select argument when reading
      # malformed point records; retry without it before giving up.
      tryCatch(
        if (nzchar(filter_str)) reader(path, filter = filter_str)
        else                    reader(path),
        error = function(e2) NULL
      )
    }
  )
  if (is.null(las)) {
    return(empty_point_cloud())
  }
  slot_reader <- get("slot", envir = asNamespace("methods"))
  data <- as.data.frame(slot_reader(las, "data"))
  if (!all(c("X", "Y", "Z") %in% names(data))) {
    stop("The external LAS reader did not return X/Y/Z columns.", call. = FALSE)
  }
  color <- if (all(c("R", "G", "B") %in% names(data))) {
    scale <- max(c(data$R, data$G, data$B), na.rm = TRUE)
    scale <- if (is.finite(scale) && scale > 255) 65535 else 255
    grDevices::rgb(
      pmax(0, pmin(data$R, scale)),
      pmax(0, pmin(data$G, scale)),
      pmax(0, pmin(data$B, scale)),
      maxColorValue = scale
    )
  } else {
    rep("#94a3b8", nrow(data))
  }
  classification <- if ("Classification" %in% names(data)) data$Classification else NA_integer_
  out <- data.frame(
    point_id = seq_len(nrow(data)),
    x = data$X,
    y = data$Y,
    z = data$Z,
    classification = classification,
    color = color,
    stringsAsFactors = FALSE
  )
  attr(out, "point_cloud_source") <- normalizePath(path, mustWork = FALSE)
  attr(out, "coordinate_source") <- "full_georeferenced"
  out
}

# Serialise a decimated point cloud as a binary_little_endian PLY whose
# byte layout matches what read_ply_point_cloud expects (XYZ float32
# followed by 4 uchars red/blue/green/views per vertex). Used to cache
# a fast-to-reload preview after the first lidR / LAZ read, so a Load
# 3D scene click on a project without an ODM-provided point_cloud.ply
# does NOT pay the 10-20 s LASzip decompression more than once.
write_preview_ply <- function(points, path) {
  if (!is.data.frame(points) || nrow(points) == 0L) return(invisible(NULL))
  required <- c("x", "y", "z")
  if (!all(required %in% names(points))) {
    stop("write_preview_ply needs x, y, z columns.", call. = FALSE)
  }
  n <- nrow(points)
  # Decode the hex color string into 3 x N R, G, B uchars. Default
  # mid-grey when the colour column is missing or invalid so the
  # preview still has a sane palette.
  rgb_mat <- tryCatch(
    grDevices::col2rgb(points$color %||% "#94a3b8"),
    error = function(e) matrix(148L, nrow = 3L, ncol = n)
  )
  if (ncol(rgb_mat) == 1L && n > 1L) {
    rgb_mat <- matrix(rep(rgb_mat[, 1L], n), nrow = 3L)
  }
  header <- paste0(
    "ply\n",
    "format binary_little_endian 1.0\n",
    "comment DroneBioR preview cache\n",
    "element vertex ", n, "\n",
    "property float x\n",
    "property float y\n",
    "property float z\n",
    "property uchar red\n",
    "property uchar blue\n",
    "property uchar green\n",
    "property uchar views\n",
    "end_header\n"
  )
  # Vectorised byte interleave: build the 12-byte XYZ block and the
  # 4-byte RGBV block separately, stack them as a 16 x N raw matrix,
  # then flatten column-major (R's default) so the output is exactly
  # vertex 1 (XYZ + RGBV) followed by vertex 2, etc.
  xyz_raw <- writeBin(as.numeric(rbind(points$x, points$y, points$z)),
                      raw(), size = 4L, endian = "little")
  rgbv_int <- as.integer(rbind(rgb_mat[1L, ], rgb_mat[3L, ],
                               rgb_mat[2L, ], rep(0L, n)))
  rgbv_raw <- writeBin(rgbv_int, raw(), size = 1L)
  xyz_mat  <- matrix(xyz_raw,  nrow = 12L)
  rgbv_mat <- matrix(rgbv_raw, nrow =  4L)
  buf      <- as.raw(rbind(xyz_mat, rgbv_mat))
  con <- file(path, "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw(header), con, useBytes = TRUE)
  writeBin(buf, con, useBytes = TRUE)
  invisible(path)
}

#' Read a full-resolution LAS/LAZ/COPC point cloud
#'
#' The function prefers an uncompressed LAS file because it can be read with the
#' package's built-in reader. If a LAZ or COPC LAZ path is supplied, the function
#' first looks for the corresponding ODM `.las` sidecar. If no LAS sidecar is
#' available, it attempts to use an installed external reader such as `lidR`.
#'
#' @param path Path to `.las`, `.laz` or `.copc.laz`.
#' @param roi_polygon Optional polygon ROI with `x` and `y` columns.
#' @param max_points Maximum number of points to return when no ROI is supplied.
#' @param chunk_size Number of point records per scan chunk for LAS files.
#' @param preview_cache_dir Optional writable directory used to cache a
#'   decimated PLY preview of the source. When supplied and no ROI is
#'   requested, the first successful read of a `.laz` / `.copc.laz`
#'   writes a PLY there keyed on the source's (size, mtime); subsequent
#'   reads of the same source consume the PLY directly, skipping the
#'   LASzip decompression entirely.
#' @return Data frame with full-resolution point coordinates inside the ROI.
#' @examples
#' \dontrun{
#' pc <- read_full_point_cloud("dense.laz")
#' }
#' @export
read_full_point_cloud <- function(path,
                                  roi_polygon = NULL,
                                  max_points = Inf,
                                  chunk_size = 100000,
                                  preview_cache_dir = NULL) {
  if (!file.exists(path)) {
    stop("Point cloud file not found: ", path, call. = FALSE)
  }

  is_compressed <- grepl("\\.(laz|copc)$", path, ignore.case = TRUE) ||
                   grepl("\\.copc\\.laz$", path, ignore.case = TRUE)

  # Preview cache: avoids re-running LASzip / lidR on every Load 3D
  # scene click when the project ships only a LAZ / COPC and no PLY.
  # Cache key = source basename + size + mtime + max_points bucket, so
  # a routine OneDrive resync that touches mtime correctly invalidates,
  # and different max_points buckets get separate caches.
  cache_path <- NULL
  if (is_compressed && is.null(roi_polygon) &&
      !is.null(preview_cache_dir) && nzchar(preview_cache_dir)) {
    dir.create(preview_cache_dir, recursive = TRUE, showWarnings = FALSE)
    info <- file.info(path)
    mp_bucket <- if (is.finite(max_points)) as.integer(max_points) else 0L
    cache_path <- file.path(
      preview_cache_dir,
      sprintf("%s_size%.0f_mt%.0f_mp%d.preview.ply",
              tools::file_path_sans_ext(basename(path)),
              as.numeric(info$size),
              as.numeric(info$mtime),
              mp_bucket)
    )
    if (file.exists(cache_path)) {
      cached <- tryCatch(
        read_ply_point_cloud(cache_path, max_points = max_points),
        error = function(e) NULL
      )
      if (!is.null(cached) && nrow(cached) > 0L) {
        attr(cached, "point_cloud_source") <- normalizePath(path, mustWork = FALSE)
        attr(cached, "coordinate_source") <- "full_georeferenced"
        return(cached)
      }
    }
  }

  if (grepl("\\.las$", path, ignore.case = TRUE)) {
    return(read_las_point_cloud(path, roi_polygon = roi_polygon, max_points = max_points, chunk_size = chunk_size))
  }

  if (grepl("\\.laz$", path, ignore.case = TRUE)) {
    sidecar <- compressed_sidecar_candidates(path)
    sidecar <- sidecar[file.exists(sidecar) & grepl("\\.las$", sidecar, ignore.case = TRUE)]
    if (length(sidecar) > 0) {
      return(read_las_point_cloud(sidecar[[1]], roi_polygon = roi_polygon, max_points = max_points, chunk_size = chunk_size))
    }

    # Push the ROI bbox (or, when no ROI is provided, a random-
    # fraction decoder filter sized by max_points) down into the
    # LAZ decoder so most chunks are never decompressed. After the
    # (much smaller) decoded data is back, run the exact polygon
    # filter and the post-decode max_points cap; the decoder is
    # bbox-based, not polygon-based, and the random fraction is
    # intentionally generous so the post-decode random sample
    # still has options.
    pts <- read_with_optional_lidar_package(path,
                                            roi_polygon = roi_polygon,
                                            max_points = max_points)
    if (!is.null(pts)) {
      if (!is.null(roi_polygon)) {
        pts <- filter_points_by_roi(pts, roi_polygon)
      }
      if (is.finite(max_points) && nrow(pts) > max_points) {
        set.seed(42)
        pts <- pts[sort(sample.int(nrow(pts), max_points)), , drop = FALSE]
      }
      # Persist the decoded preview so the next Load 3D scene click
      # skips the LASzip decompression entirely. Only fires when no
      # ROI was applied (a ROI subset would be wrong for a generic
      # preview).
      if (!is.null(cache_path) && nrow(pts) > 0L) {
        tryCatch(write_preview_ply(pts, cache_path),
                 error = function(e) NULL)
      }
      return(pts)
    }

    stop(
      "Compressed LAZ/COPC reading requires a matching ODM .las sidecar or an installed external reader such as lidR.",
      call. = FALSE
    )
  }

  stop("Unsupported point cloud format. Use .las, .laz or .copc.laz.", call. = FALSE)
}

#' Build a canopy height model from DSM and DTM rasters
#'
#' @param dsm_path DSM GeoTIFF path.
#' @param dtm_path DTM GeoTIFF path.
#' @return A `terra::SpatRaster` with non-negative canopy height in meters.
#' @examples
#' dsm <- system.file("extdata", "dsm_subset.tif", package = "DroneBioR")
#' dtm <- system.file("extdata", "dtm_subset.tif", package = "DroneBioR")
#' chm <- build_chm_from_dsm_dtm(dsm, dtm)
#' terra::minmax(chm)
#' @export
build_chm_from_dsm_dtm <- function(dsm_path, dtm_path) {
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("The terra package is required to build a CHM.", call. = FALSE)
  }
  if (!file.exists(dsm_path) || !file.exists(dtm_path)) {
    stop("DSM and DTM files are required to build a CHM.", call. = FALSE)
  }
  dsm <- terra::rast(dsm_path)
  dtm <- terra::rast(dtm_path)
  if (!terra::compareGeom(dsm, dtm, stopOnError = FALSE)) {
    dtm <- terra::resample(dtm, dsm, method = "bilinear")
  }
  chm <- dsm - dtm
  names(chm) <- "CHM_m"
  terra::clamp(chm, lower = 0, values = TRUE)
}

#' Add CHM-derived heights to selected points
#'
#' @param points Data frame with `x`, `y` and `z`.
#' @param chm A `terra::SpatRaster` canopy height model.
#' @param fallback_quantile Local Z quantile used when CHM is missing for a point.
#' @return Input points with `height_m` derived from the CHM where possible.
#' @examples
#' dsm <- system.file("extdata", "dsm_subset.tif", package = "DroneBioR")
#' dtm <- system.file("extdata", "dtm_subset.tif", package = "DroneBioR")
#' chm <- build_chm_from_dsm_dtm(dsm, dtm)
#' pts <- data.frame(
#'   x = seq(392001, 392015, length.out = 5),
#'   y = seq(3033001, 3033015, length.out = 5),
#'   z = c(50, 51, 52, 53, 54)
#' )
#' add_chm_heights(pts, chm)
#' @export
add_chm_heights <- function(points, chm, fallback_quantile = 0.05) {
  if (nrow(points) == 0) {
    points$height_m <- numeric()
    return(points)
  }
  if (!requireNamespace("terra", quietly = TRUE)) {
    return(add_point_heights(points, ground_quantile = fallback_quantile))
  }
  values <- terra::extract(chm, points[, c("x", "y"), drop = FALSE], ID = FALSE)
  height <- as.numeric(values[[1]])
  fallback <- add_point_heights(points, ground_quantile = fallback_quantile)$height_m
  height[!is.finite(height)] <- fallback[!is.finite(height)]
  points$height_m <- pmax(height, 0)
  points
}

#' Compute CHM metrics for a polygon ROI
#'
#' @param chm A `terra::SpatRaster` canopy height model.
#' @param roi_polygon Data frame with `x` and `y` polygon vertices.
#' @return One-row data frame with CHM area, height and volume metrics.
#' @examples
#' dsm <- system.file("extdata", "dsm_subset.tif", package = "DroneBioR")
#' dtm <- system.file("extdata", "dtm_subset.tif", package = "DroneBioR")
#' chm <- build_chm_from_dsm_dtm(dsm, dtm)
#' roi <- data.frame(
#'   x = c(392004, 392012, 392012, 392004),
#'   y = c(3033004, 3033004, 3033012, 3033012)
#' )
#' compute_chm_roi_metrics(chm, roi)
#' @export
compute_chm_roi_metrics <- function(chm, roi_polygon) {
  if (!requireNamespace("terra", quietly = TRUE) || nrow(roi_polygon) < 3) {
    return(data.frame(
      chm_cell_count = 0L,
      chm_area_m2 = NA_real_,
      chm_height_mean_m = NA_real_,
      chm_height_max_m = NA_real_,
      chm_surface_volume_m3 = NA_real_
    ))
  }

  coords <- as.matrix(roi_polygon[, c("x", "y"), drop = FALSE])
  coords <- rbind(coords, coords[1, , drop = FALSE])
  roi <- terra::vect(list(coords), type = "polygons", crs = terra::crs(chm))
  clipped <- terra::mask(terra::crop(chm, roi), roi)
  values <- terra::values(clipped, mat = FALSE)
  values <- values[is.finite(values) & values > 0]
  cell_area <- prod(abs(terra::res(chm)))
  if (length(values) == 0) {
    return(data.frame(
      chm_cell_count = 0L,
      chm_area_m2 = 0,
      chm_height_mean_m = NA_real_,
      chm_height_max_m = NA_real_,
      chm_surface_volume_m3 = 0
    ))
  }

  data.frame(
    chm_cell_count = length(values),
    chm_area_m2 = length(values) * cell_area,
    chm_height_mean_m = mean(values, na.rm = TRUE),
    chm_height_max_m = max(values, na.rm = TRUE),
    chm_surface_volume_m3 = sum(values, na.rm = TRUE) * cell_area
  )
}
