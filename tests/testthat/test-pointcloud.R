test_that("points_in_roi flags points inside and outside a square", {
  roi <- data.frame(x = c(0, 5, 5, 0), y = c(0, 0, 5, 5))
  expect_equal(
    points_in_roi(c(1, 3, 6, -1), c(1, 4, 6, 0), roi),
    c(TRUE, TRUE, FALSE, FALSE)
  )
})

test_that("points_in_roi returns all FALSE for a degenerate polygon", {
  expect_equal(
    points_in_roi(c(1, 2), c(1, 2), data.frame(x = c(0, 1), y = c(0, 1))),
    c(FALSE, FALSE)
  )
})

test_that("build_roi_polygon returns hull and bbox variants", {
  set.seed(1)
  pts <- data.frame(x = runif(50, 0, 10), y = runif(50, 0, 10))
  bbox <- build_roi_polygon(pts, method = "bbox")
  hull <- build_roi_polygon(pts, method = "hull")
  expect_equal(nrow(bbox), 4)
  expect_gt(nrow(hull), 2)
})

test_that("build_chm_from_dsm_dtm produces non-negative CHM values", {
  dsm <- system.file("extdata", "dsm_subset.tif", package = "DroneBioR")
  dtm <- system.file("extdata", "dtm_subset.tif", package = "DroneBioR")
  chm <- build_chm_from_dsm_dtm(dsm, dtm)
  expect_equal(names(chm), "CHM_m")
  mm <- terra::minmax(chm)
  expect_true(all(mm[1, ] >= 0))
})

test_that("derive_tree_candidates clusters high points into ranked candidates", {
  set.seed(1)
  n <- 300
  pts <- data.frame(
    x = c(rnorm(n / 3, 5, 0.5), rnorm(n / 3, 15, 0.5), rnorm(n / 3, 25, 0.5)),
    y = c(rnorm(n / 3, 5, 0.5), rnorm(n / 3, 5, 0.5),  rnorm(n / 3, 15, 0.5)),
    z = c(rnorm(n / 3, 55, 0.2), rnorm(n / 3, 57, 0.2), rnorm(n / 3, 54, 0.2))
  )
  trees <- derive_tree_candidates(pts)
  expect_gte(nrow(trees), 1)
  expect_true(all(trees$height_m >= 1.5))
  expect_true(all(diff(trees$height_m) <= 0))
})

test_that("derive_tree_candidates returns an empty frame when no points are high", {
  pts <- data.frame(x = runif(50), y = runif(50), z = rep(0, 50))
  expect_equal(nrow(derive_tree_candidates(pts, min_height = 5)), 0)
})

test_that("export_point_selection writes CSV outputs to the chosen folder", {
  set.seed(1)
  pts <- data.frame(x = runif(50, 0, 10), y = runif(50, 0, 10), z = runif(50, 50, 55))
  pts <- add_point_heights(pts)
  m <- compute_selection_metrics(pts)
  p <- compute_vertical_profile(pts)
  out <- tempfile("sel-")
  paths <- export_point_selection(pts, m, p, output_dir = out, label = "plot 1")
  expect_true(all(file.exists(paths)))
  expect_true(all(c("points", "metrics", "vertical_profile") %in% names(paths)))
  # Label is sanitized
  expect_true(grepl("plot_1", paths[["points"]]))
})
