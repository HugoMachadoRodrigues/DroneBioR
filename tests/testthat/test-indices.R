make_reflectance <- function() {
  r <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 2)
  terra::values(r) <- 1
  bands <- c(r * 0.10, r * 0.20, r * 0.30, r * 0.40, r * 0.70)
  names(bands) <- c("Blue", "Green", "Red", "RedEdge", "NIR")
  bands
}

test_that("spectral indices are computed from named reflectance bands", {
  bands <- make_reflectance()
  indices <- compute_spectral_indices(bands)
  expect_equal(
    names(indices),
    c("NDVI", "NDRE", "EVI", "SAVI", "NDWI", "GNDVI", "CIrededge", "MSAVI2", "VARI")
  )
  expect_equal(terra::nlyr(indices), 9)
})

test_that("compute_spectral_indices errors on missing band names", {
  r <- terra::rast(nrows = 2, ncols = 2)
  terra::values(r) <- 0.5
  names(r) <- "Blue"
  expect_error(compute_spectral_indices(r), "missing")
})

test_that("compute_biomass_proxy returns a single named layer in [-1, 1]", {
  indices <- compute_spectral_indices(make_reflectance())
  proxy <- compute_biomass_proxy(indices)
  expect_equal(names(proxy), "Biomass_Index_Proxy")
  expect_equal(terra::nlyr(proxy), 1)
  mm <- terra::minmax(proxy)
  expect_true(all(mm[1, ] >= -1))
  expect_true(all(mm[2, ] <= 1))
})

test_that("compute_biomass_proxy errors when required indices are absent", {
  r <- terra::rast(nrows = 2, ncols = 2)
  terra::values(r) <- 0.5
  names(r) <- "NDVI"
  expect_error(compute_biomass_proxy(r), "missing")
})
