test_that("spectral indices are computed from named reflectance bands", {
  r <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 2)
  terra::values(r) <- 1
  bands <- c(r + 0.10, r + 0.20, r + 0.30, r + 0.40, r + 0.70)
  names(bands) <- c("Blue", "Green", "Red", "RedEdge", "NIR")

  indices <- compute_spectral_indices(bands)
  expect_equal(names(indices), c("NDVI", "NDRE", "EVI", "SAVI", "NDWI", "GNDVI", "CIrededge", "MSAVI2", "VARI"))
  expect_equal(terra::nlyr(indices), 9)
})
