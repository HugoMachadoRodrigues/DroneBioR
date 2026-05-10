test_that("selection metrics and vertical profile are computed", {
  pts <- data.frame(
    x = c(0, 1, 1, 0, 0.5),
    y = c(0, 0, 1, 1, 0.5),
    z = c(10, 11, 12, 11, 13)
  )
  pts <- add_point_heights(pts)

  metrics <- compute_selection_metrics(pts, voxel_size = 1)
  expect_equal(metrics$n_points, 5)
  expect_true(metrics$footprint_area_m2 > 0)
  expect_true(metrics$occupied_volume_m3 > 0)

  profile <- compute_vertical_profile(pts, bin_size = 1)
  expect_true(nrow(profile) > 0)
  expect_equal(sum(profile$point_count), nrow(pts))
})

test_that("ROI polygons filter full-resolution points", {
  pts <- data.frame(
    x = c(0, 1, 1, 0, 5),
    y = c(0, 0, 1, 1, 5),
    z = c(2, 3, 4, 3, 9)
  )
  roi <- build_roi_polygon(pts[1:4, ], method = "bbox")
  inside <- filter_points_by_roi(pts, roi)

  expect_equal(nrow(inside), 4)
  expect_false(any(inside$x == 5))
})

test_that("CHM heights and CHM ROI metrics are computed", {
  skip_if_not_installed("terra")

  chm <- terra::rast(nrows = 5, ncols = 5, xmin = 0, xmax = 5, ymin = 0, ymax = 5)
  terra::values(chm) <- 2
  pts <- data.frame(x = c(1, 2), y = c(1, 2), z = c(10, 12))
  pts <- add_chm_heights(pts, chm)
  expect_equal(pts$height_m, c(2, 2))

  roi <- data.frame(x = c(0, 3, 3, 0), y = c(0, 0, 3, 3))
  chm_metrics <- compute_chm_roi_metrics(chm, roi)
  expect_true(chm_metrics$chm_area_m2 > 0)
  expect_true(chm_metrics$chm_surface_volume_m3 > 0)
})
