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

  x <- y <- z <- numeric(length(point_index))
  red <- blue <- green <- views <- integer(length(point_index))

  for (i in seq_along(point_index)) {
    start <- header_end + (point_index[[i]] - 1L) * record_size + 1L
    rec <- raw_file[start:(start + record_size - 1L)]
    con <- rawConnection(rec, open = "rb")
    xyz <- readBin(con, what = "numeric", n = 3L, size = 4L, endian = "little")
    rgbv <- readBin(con, what = "integer", n = 4L, size = 1L, signed = FALSE)
    close(con)
    x[[i]] <- xyz[[1]]
    y[[i]] <- xyz[[2]]
    z[[i]] <- xyz[[3]]
    red[[i]] <- rgbv[[1]]
    blue[[i]] <- rgbv[[2]]
    green[[i]] <- rgbv[[3]]
    views[[i]] <- rgbv[[4]]
  }

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
