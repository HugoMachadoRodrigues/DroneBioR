make_flat_raster <- function(value, ncol = 10, nrow = 10, res = 1,
                             xmin = 0, ymin = 0) {
  r <- terra::rast(nrows = nrow, ncols = ncol,
                   xmin = xmin, xmax = xmin + ncol * res,
                   ymin = ymin, ymax = ymin + nrow * res,
                   crs  = "EPSG:32617")
  terra::values(r) <- value
  r
}

full_roi <- function(ncol = 10, nrow = 10, res = 1, xmin = 0, ymin = 0) {
  data.frame(
    x = c(xmin,            xmin + ncol * res, xmin + ncol * res, xmin),
    y = c(ymin,            ymin,              ymin + nrow * res, ymin + nrow * res)
  )
}

test_that("user_plane volume on a flat top matches analytic answer", {
  top <- make_flat_raster(value = 5)
  roi <- full_roi()
  res <- compute_survey_volumes(top = top, roi = roi,
                                method = "user_plane", base_z = 2)
  # (5 - 2) m * 10 m * 10 m = 300 m^3, all cut
  expect_equal(res$cut_volume_m3,  300, tolerance = 1e-6)
  expect_equal(res$fill_volume_m3,   0, tolerance = 1e-6)
  expect_equal(res$net_volume_m3,  300, tolerance = 1e-6)
  expect_equal(res$surface_area_planar_m2, 100, tolerance = 1e-6)
})

test_that("user_plane with base above top produces only fill", {
  top <- make_flat_raster(value = 5)
  roi <- full_roi()
  res <- compute_survey_volumes(top = top, roi = roi,
                                method = "user_plane", base_z = 8)
  expect_equal(res$cut_volume_m3,    0, tolerance = 1e-6)
  expect_equal(res$fill_volume_m3, 300, tolerance = 1e-6)
  expect_equal(res$net_volume_m3, -300, tolerance = 1e-6)
})

test_that("min_z and mean_z on a uniform top give zero volumes", {
  top <- make_flat_raster(value = 5)
  roi <- full_roi()
  for (m in c("min_z", "mean_z")) {
    res <- compute_survey_volumes(top = top, roi = roi, method = m)
    expect_equal(res$cut_volume_m3,  0, tolerance = 1e-6, info = m)
    expect_equal(res$fill_volume_m3, 0, tolerance = 1e-6, info = m)
  }
})

test_that("min_z on a step raster captures only the bump", {
  top <- make_flat_raster(value = 0)
  # Bump a 3x3 patch in the centre to height 5
  m <- matrix(0, nrow = 10, ncol = 10)
  m[4:6, 4:6] <- 5
  terra::values(top) <- as.vector(t(m))
  roi <- full_roi()
  res <- compute_survey_volumes(top = top, roi = roi, method = "min_z")
  # 9 cells of height 5, each 1 m^2 -> 45 m^3
  expect_equal(res$cut_volume_m3, 45, tolerance = 1e-6)
  expect_equal(res$fill_volume_m3, 0, tolerance = 1e-6)
})

test_that("mean_z on a linear gradient gives zero net volume", {
  m <- matrix(seq(0, 9), nrow = 10, ncol = 10, byrow = TRUE)
  top <- terra::rast(nrows = 10, ncols = 10,
                     xmin = 0, xmax = 10, ymin = 0, ymax = 10,
                     crs = "EPSG:32617")
  terra::values(top) <- as.vector(t(m))
  roi <- full_roi()
  res <- compute_survey_volumes(top = top, roi = roi, method = "mean_z")
  expect_equal(res$net_volume_m3, 0, tolerance = 1e-6)
  expect_equal(res$cut_volume_m3, res$fill_volume_m3, tolerance = 1e-6)
})

test_that("ground_quantile returns expected base", {
  top <- make_flat_raster(value = 0)
  m <- matrix(0, nrow = 10, ncol = 10)
  # Inject a high pixel so the 5th percentile is still 0
  m[1, 1] <- 100
  terra::values(top) <- as.vector(t(m))
  res <- compute_survey_volumes(top = top, roi = full_roi(),
                                method = "ground_quantile",
                                ground_quantile = 0.05)
  # Base = 0; cut = 100 * 1 m^2 = 100
  expect_equal(res$cut_volume_m3, 100, tolerance = 1e-6)
  expect_equal(res$base_z_summary[["mean"]], 0)
})

test_that("dtm method matches CHM integral on the bundled fixtures", {
  dsm <- terra::rast(system.file("extdata", "dsm_subset.tif", package = "DroneBioR"))
  dtm <- terra::rast(system.file("extdata", "dtm_subset.tif", package = "DroneBioR"))
  # Use the full DSM footprint as the ROI.
  ext <- as.vector(terra::ext(dsm))
  roi <- data.frame(
    x = c(ext[1], ext[2], ext[2], ext[1]),
    y = c(ext[3], ext[3], ext[4], ext[4])
  )
  res <- compute_survey_volumes(top = dsm, roi = roi, method = "dtm", dtm = dtm)
  # Independently compute via CHM
  chm <- DroneBioR::build_chm_from_dsm_dtm(
    system.file("extdata", "dsm_subset.tif", package = "DroneBioR"),
    system.file("extdata", "dtm_subset.tif", package = "DroneBioR")
  )
  vals <- terra::values(chm, mat = FALSE)
  vals <- vals[is.finite(vals) & vals > 0]
  expected <- sum(vals) * prod(abs(terra::res(chm)))
  expect_equal(res$cut_volume_m3, expected, tolerance = 1e-3)
})

test_that("compute_survey_volumes rejects malformed inputs", {
  expect_error(compute_survey_volumes(top = 42, roi = full_roi(), method = "min_z"),
               "SpatRaster")
  expect_error(
    compute_survey_volumes(top = make_flat_raster(1), roi = data.frame(x = 1, y = 2),
                           method = "min_z"),
    "at least 3 vertices"
  )
  expect_error(
    compute_survey_volumes(top = make_flat_raster(1), roi = full_roi(),
                           method = "dtm"),
    "`method = 'dtm'` requires"
  )
  expect_error(
    compute_survey_volumes(top = make_flat_raster(1), roi = full_roi(),
                           method = "user_plane"),
    "`method = 'user_plane'` requires"
  )
  expect_error(
    compute_survey_volumes(top = make_flat_raster(1), roi = full_roi(),
                           method = "ground_quantile",
                           ground_quantile = 2),
    "in \\[0, 1\\]"
  )
})

test_that("returns empty object when ROI misses the raster", {
  top <- make_flat_raster(value = 1, xmin = 0, ymin = 0)
  far_roi <- data.frame(
    x = c(1e6, 1e6 + 5, 1e6 + 5, 1e6),
    y = c(1e6, 1e6,     1e6 + 5, 1e6 + 5)
  )
  res <- compute_survey_volumes(top = top, roi = far_roi, method = "min_z")
  expect_s3_class(res, "dronebio_survey_volume")
  expect_equal(res$cell_count, 0L)
  expect_true(is.na(res$cut_volume_m3))
})

test_that("perimeter_tin returns finite volume on a tilted plane", {
  skip_if_not_installed("interp")
  # Top surface: linear gradient 0..9 across columns
  m <- matrix(seq(0, 9), nrow = 10, ncol = 10, byrow = TRUE)
  top <- terra::rast(nrows = 10, ncols = 10, xmin = 0, xmax = 10,
                     ymin = 0, ymax = 10, crs = "EPSG:32617")
  terra::values(top) <- as.vector(t(m))
  res <- compute_survey_volumes(top = top, roi = full_roi(),
                                method = "perimeter_tin")
  # A perimeter TIN on a true plane equals the plane itself, so the net
  # volume should be ~0 and both cut and fill should be small.
  expect_lt(abs(res$net_volume_m3), 1)
})

test_that("print.dronebio_survey_volume runs without error", {
  res <- compute_survey_volumes(top = make_flat_raster(5), roi = full_roi(),
                                method = "user_plane", base_z = 2)
  expect_output(print(res), "DroneBioR survey volume")
})
