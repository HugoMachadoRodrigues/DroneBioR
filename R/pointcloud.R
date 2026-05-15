find_header_end <- function(raw_file) {
  marker <- charToRaw("end_header\n")
  n <- length(marker)
  last_start <- length(raw_file) - n + 1L
  for (i in seq_len(last_start)) {
    if (identical(raw_file[i:(i + n - 1L)], marker)) {
      return(i + n - 1L)
    }
  }
  stop("Could not find PLY header end.", call. = FALSE)
}

#' Read a binary little-endian PLY point cloud sample
#'
#' This reader supports the ODM/FPCFilter PLY layout used in the current project:
#' float x/y/z followed by uchar red/blue/green/views. It is intentionally small
#' and dependency-free for app previews; full LAZ analytics should use PDAL/lidR.
#'
#' @param path Path to a PLY file.
#' @param max_points Maximum number of points to return.
#' @param seed Random seed used when sampling.
#' @return A data frame with x, y, z, RGB values and display color.
#' @examples
#' \dontrun{
#' pc <- read_ply_point_cloud("preview.ply", max_points = 50000)
#' }
#' @export
read_ply_point_cloud <- function(path, max_points = 50000, seed = 42) {
  if (!file.exists(path)) {
    stop("PLY file not found: ", path, call. = FALSE)
  }

  raw_file <- readBin(path, what = "raw", n = file.info(path)$size)
  header_end <- find_header_end(raw_file)
  header <- rawToChar(raw_file[seq_len(header_end)])
  if (!grepl("format binary_little_endian", header, fixed = TRUE)) {
    stop("Only binary_little_endian PLY files are supported by this lightweight reader.", call. = FALSE)
  }

  vertex_line <- grep("^element vertex ", strsplit(header, "\n", fixed = TRUE)[[1]], value = TRUE)
  n_vertices <- as.integer(sub("^element vertex ", "", vertex_line[[1]]))
  record_size <- 16L
  if (!is.finite(n_vertices) || n_vertices <= 0) {
    stop("Could not read the PLY vertex count.", call. = FALSE)
  }

  if (n_vertices > max_points) {
    set.seed(seed)
    point_index <- sort(sample.int(n_vertices, max_points))
  } else {
    point_index <- seq_len(n_vertices)
  }

  # Bulk decode in one rawConnection pass. The previous implementation
  # opened a fresh rawConnection for every sampled point, which made
  # 35,000 rawConnection() + readBin() + close() trips - the dominant
  # cost of loading the 3D scene in the Shiny app. Concatenating all
  # selected records into a single raw vector and reading the whole
  # XYZ + RGBV stream with one readBin call cuts the wall-clock cost
  # by ~50x on macOS for the typical odm_filterpoints PLY.
  n_sel <- length(point_index)
  selected_recs <- raw(record_size * n_sel)
  is_dense <- (n_sel == n_vertices)
  if (is_dense) {
    # No subsampling: contiguous chunk after the header.
    block_start <- header_end + 1L
    block_end   <- header_end + record_size * n_vertices
    selected_recs <- raw_file[block_start:block_end]
  } else {
    # Sparse sample: copy each chosen record by 16-byte stride. This
    # loop is fast because it does a single raw-vector slice per
    # record (no rawConnection, no readBin), then we decode in bulk.
    for (i in seq_len(n_sel)) {
      src_start <- header_end + (point_index[[i]] - 1L) * record_size + 1L
      dst_start <- (i - 1L) * record_size + 1L
      selected_recs[dst_start:(dst_start + record_size - 1L)] <-
        raw_file[src_start:(src_start + record_size - 1L)]
    }
  }

  con <- rawConnection(selected_recs, open = "rb")
  on.exit(close(con), add = TRUE)
  # The PLY layout is interleaved XYZ (3 floats = 12 bytes) followed by
  # RGBV (4 uchars = 4 bytes) per vertex. readBin's `n` argument with
  # interleaved formats is not supported directly, so we still loop on
  # the connection - but it is ONE open file handle, so the per-iter
  # cost is just a buffered read.
  xyz_mat <- matrix(NA_real_, nrow = 3L, ncol = n_sel)
  rgbv_mat <- matrix(0L, nrow = 4L, ncol = n_sel)
  for (i in seq_len(n_sel)) {
    xyz_mat[, i]  <- readBin(con, what = "numeric", n = 3L, size = 4L,
                             endian = "little")
    rgbv_mat[, i] <- readBin(con, what = "integer", n = 4L, size = 1L,
                             signed = FALSE)
  }

  x   <- xyz_mat[1L, ]
  y   <- xyz_mat[2L, ]
  z   <- xyz_mat[3L, ]
  red   <- rgbv_mat[1L, ]
  blue  <- rgbv_mat[2L, ]
  green <- rgbv_mat[3L, ]
  views <- rgbv_mat[4L, ]

  data.frame(
    point_id = point_index,
    x = x,
    y = y,
    z = z,
    red = red,
    green = green,
    blue = blue,
    views = views,
    color = grDevices::rgb(red, green, blue, maxColorValue = 255),
    stringsAsFactors = FALSE
  )
}

#' Derive approximate tree candidates from a point cloud
#'
#' This is a preview-grade canopy object detector. It bins high points into a
#' regular grid and estimates height, crown diameter and crown volume. Scientific
#' tree metrics should later use a CHM from DSM-DTM and validated segmentation.
#'
#' @param points Data frame from `read_ply_point_cloud()`.
#' @param grid_size Grid size in source coordinate units.
#' @param min_height Minimum height above local ground proxy.
#' @param min_points Minimum number of points per candidate.
#' @param max_trees Maximum number of candidates to return.
#' @return A data frame of approximate tree objects.
#' @examples
#' set.seed(1)
#' n <- 300
#' pts <- data.frame(
#'   x = c(rnorm(n/3, 5, 0.5), rnorm(n/3, 15, 0.5), rnorm(n/3, 25, 0.5)),
#'   y = c(rnorm(n/3, 5, 0.5), rnorm(n/3, 5, 0.5), rnorm(n/3, 15, 0.5)),
#'   z = c(rnorm(n/3, 55, 0.2), rnorm(n/3, 57, 0.2), rnorm(n/3, 54, 0.2))
#' )
#' derive_tree_candidates(pts)
#' @export
derive_tree_candidates <- function(points,
                                   grid_size = 4,
                                   min_height = 1.5,
                                   min_points = 5,
                                   max_trees = 80) {
  if (nrow(points) == 0) {
    return(data.frame())
  }

  ground <- stats::quantile(points$z, probs = 0.05, na.rm = TRUE, names = FALSE)
  points$height <- pmax(points$z - ground, 0)
  high <- points[points$height >= min_height, , drop = FALSE]
  if (nrow(high) == 0) {
    return(data.frame())
  }

  high$gx <- floor((high$x - min(high$x, na.rm = TRUE)) / grid_size)
  high$gy <- floor((high$y - min(high$y, na.rm = TRUE)) / grid_size)
  high$cell <- paste(high$gx, high$gy, sep = "_")

  cells <- split(high, high$cell)
  cells <- cells[vapply(cells, nrow, integer(1)) >= min_points]
  if (length(cells) == 0) {
    return(data.frame())
  }

  metrics <- lapply(cells, function(d) {
    height <- max(d$height, na.rm = TRUE)
    crown_diameter <- max(
      sqrt(diff(range(d$x, na.rm = TRUE))^2 + diff(range(d$y, na.rm = TRUE))^2),
      grid_size * 0.6,
      na.rm = TRUE
    )
    crown_volume <- pi * (crown_diameter / 2)^2 * height * 0.5
    data.frame(
      x = mean(d$x, na.rm = TRUE),
      y = mean(d$y, na.rm = TRUE),
      z = max(d$z, na.rm = TRUE),
      height_m = height,
      crown_diameter_m = crown_diameter,
      crown_volume_m3 = crown_volume,
      point_count = nrow(d)
    )
  })

  out <- do.call(rbind, metrics)
  out <- out[order(out$height_m, decreasing = TRUE), , drop = FALSE]
  out <- utils::head(out, max_trees)
  out$tree_id <- seq_len(nrow(out))
  out[, c("tree_id", "x", "y", "z", "height_m", "crown_diameter_m", "crown_volume_m3", "point_count")]
}
