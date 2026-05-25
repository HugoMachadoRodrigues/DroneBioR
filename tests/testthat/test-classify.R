make_ndvi <- function(values, dim = c(4, 4)) {
  r <- terra::rast(nrows = dim[1], ncols = dim[2],
                   xmin = 0, xmax = dim[2], ymin = 0, ymax = dim[1])
  terra::values(r) <- values
  names(r) <- "NDVI"
  r
}

test_that("classify_ground_vegetation produces classes 1..4 from NDVI alone", {
  ndvi <- make_ndvi(c(
    0.05, 0.10, 0.15, 0.18,   # bare
    0.25, 0.30, 0.35, 0.38,   # stress
    0.50, 0.55, 0.60, 0.62,   # moderate
    0.70, 0.80, 0.90, 0.95    # vigorous
  ))
  classes <- classify_ground_vegetation(ndvi)
  v <- terra::values(classes, mat = FALSE)
  expect_equal(sort(unique(v)), 1:4)
  expect_equal(sum(v == 1), 4)
  expect_equal(sum(v == 4), 4)
})

test_that("classify_ground_vegetation promotes tall canopy to class 5 via CHM", {
  ndvi <- make_ndvi(rep(0.8, 16))           # all vigorous
  chm  <- make_ndvi(c(rep(0, 8), rep(5, 8)))
  names(chm) <- "CHM_m"
  classes <- classify_ground_vegetation(ndvi, chm = chm, chm_tall_min = 2)
  v <- terra::values(classes, mat = FALSE)
  expect_equal(sum(v == 4), 8)
  expect_equal(sum(v == 5), 8)
})

test_that("classify_ground_vegetation errors on inconsistent thresholds", {
  ndvi <- make_ndvi(rep(0.5, 16))
  expect_error(
    classify_ground_vegetation(ndvi, ndvi_bare_max = 0.5, ndvi_stress_max = 0.3),
    "Thresholds must satisfy"
  )
})

test_that("classify_ground_vegetation errors on non-SpatRaster inputs", {
  expect_error(classify_ground_vegetation(1:4), "SpatRaster")
})

test_that("classify_ground_csf errors when lidR is missing", {
  skip_if(requireNamespace("lidR", quietly = TRUE))
  expect_error(
    classify_ground_csf("nonexistent.las"),
    "lidR"
  )
})

test_that("classify_ground_csf errors on missing LAS file", {
  skip_if_not_installed("lidR")
  expect_error(
    classify_ground_csf(tempfile(fileext = ".las")),
    "not found"
  )
})

test_that("improve_dtm_csf defaults to non-destructive output filenames", {
  # Regression guard: in 0.4.0 the function defaulted to dtm.tif /
  # chm.tif and overwrote the SMRF originals when rebuild_chm = TRUE.
  # Defaults are now the _csf-suffixed variants so the originals are
  # preserved. Anyone wanting legacy behaviour passes "dtm.tif" /
  # "chm.tif" explicitly.
  args <- formals(improve_dtm_csf)
  expect_equal(eval(args$dtm_filename), "dtm_csf.tif")
  expect_equal(eval(args$chm_filename), "chm_csf.tif")
})
