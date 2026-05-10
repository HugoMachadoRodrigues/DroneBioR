#' Add local height above a ground proxy to point cloud data
#'
#' Photogrammetric point clouds from ODM previews can be in a local vertical
#' reference. This helper estimates a local ground proxy from a low Z quantile
#' and stores height above that proxy.
#'
#' @param points Data frame with `z`.
#' @param ground_quantile Quantile used as a local ground proxy.
#' @return The input data frame with `height_m`.
#' @examples
#' pts <- data.frame(z = c(50, 50.1, 51, 53, 55, 56, 55, 53, 51, 50))
#' add_point_heights(pts)
#' @export
add_point_heights <- function(points, ground_quantile = 0.05) {
  if (!"z" %in% names(points)) {
    stop("points must contain a z column.", call. = FALSE)
  }
  ground <- stats::quantile(points$z, probs = ground_quantile, na.rm = TRUE, names = FALSE)
  points$height_m <- pmax(points$z - ground, 0)
  points
}

polygon_area <- function(x, y) {
  n <- length(x)
  if (n < 3) {
    return(NA_real_)
  }
  idx_next <- c(seq_len(n)[-1], 1L)
  abs(sum(x * y[idx_next] - y * x[idx_next]) / 2)
}

max_pairwise_distance <- function(x, y, max_points = 1000) {
  if (length(x) < 2) {
    return(NA_real_)
  }
  if (length(x) > max_points) {
    keep <- round(seq(1, length(x), length.out = max_points))
    x <- x[keep]
    y <- y[keep]
  }
  coords <- cbind(x, y)
  max(stats::dist(coords), na.rm = TRUE)
}

#' Compute selection metrics for a point cloud ROI
#'
#' @param points Selected points with `x`, `y`, `z` and optionally `height_m`.
#' @param voxel_size Voxel size in meters for occupied-volume approximation.
#' @return One-row data frame with distance, area, height and volume metrics.
#' @examples
#' set.seed(1)
#' pts <- data.frame(
#'   x = runif(100, 0, 10),
#'   y = runif(100, 0, 10),
#'   z = runif(100, 50, 55)
#' )
#' compute_selection_metrics(pts, voxel_size = 0.5)
#' @export
compute_selection_metrics <- function(points, voxel_size = 0.5) {
  if (nrow(points) == 0) {
    return(data.frame(
      n_points = 0L,
      footprint_area_m2 = NA_real_,
      max_crown_diameter_m = NA_real_,
      z_min_m = NA_real_,
      z_max_m = NA_real_,
      height_min_m = NA_real_,
      height_mean_m = NA_real_,
      height_max_m = NA_real_,
      occupied_volume_m3 = NA_real_
    ))
  }

  if (!"height_m" %in% names(points)) {
    points <- add_point_heights(points)
  }

  hull_area <- NA_real_
  crown_diameter <- NA_real_
  if (nrow(points) >= 3) {
    hull_idx <- grDevices::chull(points$x, points$y)
    hull_area <- polygon_area(points$x[hull_idx], points$y[hull_idx])
    crown_diameter <- max_pairwise_distance(points$x[hull_idx], points$y[hull_idx])
  }

  occupied_volume <- NA_real_
  if (is.finite(voxel_size) && voxel_size > 0) {
    voxels <- unique(data.frame(
      ix = floor(points$x / voxel_size),
      iy = floor(points$y / voxel_size),
      iz = floor(points$z / voxel_size)
    ))
    occupied_volume <- nrow(voxels) * voxel_size^3
  }

  data.frame(
    n_points = nrow(points),
    footprint_area_m2 = hull_area,
    max_crown_diameter_m = crown_diameter,
    z_min_m = min(points$z, na.rm = TRUE),
    z_max_m = max(points$z, na.rm = TRUE),
    height_min_m = min(points$height_m, na.rm = TRUE),
    height_mean_m = mean(points$height_m, na.rm = TRUE),
    height_max_m = max(points$height_m, na.rm = TRUE),
    occupied_volume_m3 = occupied_volume
  )
}

#' Compute a vertical point-density profile
#'
#' @param points Selected points with `height_m` or `z`.
#' @param bin_size Height bin size in meters.
#' @return A data frame with height bins and point counts.
#' @examples
#' set.seed(1)
#' pts <- data.frame(
#'   x = runif(100, 0, 10),
#'   y = runif(100, 0, 10),
#'   z = runif(100, 50, 55)
#' )
#' compute_vertical_profile(pts, bin_size = 1)
#' @export
compute_vertical_profile <- function(points, bin_size = 1) {
  if (nrow(points) == 0) {
    return(data.frame(bin_bottom_m = numeric(), bin_top_m = numeric(), point_count = integer()))
  }
  if (!"height_m" %in% names(points)) {
    points <- add_point_heights(points)
  }
  if (!is.finite(bin_size) || bin_size <= 0) {
    stop("bin_size must be positive.", call. = FALSE)
  }

  max_height <- max(points$height_m, na.rm = TRUE)
  breaks <- seq(0, ceiling(max_height / bin_size) * bin_size + bin_size, by = bin_size)
  bins <- cut(points$height_m, breaks = breaks, include.lowest = TRUE, right = FALSE)
  counts <- as.integer(table(bins))

  data.frame(
    bin_bottom_m = breaks[-length(breaks)],
    bin_top_m = breaks[-1],
    point_count = counts
  )
}

#' Export a selected point cloud ROI
#'
#' @param points Selected points.
#' @param metrics One-row data frame from `compute_selection_metrics()`.
#' @param profile Data frame from `compute_vertical_profile()`.
#' @param output_dir Output directory.
#' @param label ROI label.
#' @param roi_polygon Optional polygon ROI exported with the point metrics.
#' @return Named character vector with written file paths.
#' @examples
#' set.seed(1)
#' pts <- data.frame(
#'   x = runif(50, 0, 10),
#'   y = runif(50, 0, 10),
#'   z = runif(50, 50, 55)
#' )
#' pts <- add_point_heights(pts)
#' m <- compute_selection_metrics(pts)
#' p <- compute_vertical_profile(pts)
#' export_point_selection(pts, m, p, output_dir = tempfile("sel-"))
#' @export
export_point_selection <- function(points,
                                   metrics,
                                   profile,
                                   output_dir,
                                   label = "selection",
                                   roi_polygon = NULL) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  safe_label <- gsub("[^A-Za-z0-9_]+", "_", label)
  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  prefix <- file.path(output_dir, paste0(safe_label, "_", stamp))

  paths <- c(
    points = paste0(prefix, "_points.csv"),
    metrics = paste0(prefix, "_metrics.csv"),
    vertical_profile = paste0(prefix, "_vertical_profile.csv")
  )
  utils::write.csv(points, paths[["points"]], row.names = FALSE)
  utils::write.csv(metrics, paths[["metrics"]], row.names = FALSE)
  utils::write.csv(profile, paths[["vertical_profile"]], row.names = FALSE)
  if (!is.null(roi_polygon) && nrow(roi_polygon) > 0) {
    paths <- c(paths, roi_polygon = paste0(prefix, "_roi_polygon.csv"))
    utils::write.csv(roi_polygon, paths[["roi_polygon"]], row.names = FALSE)
  }
  paths
}
