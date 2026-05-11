ortho_fixture <- function() system.file("extdata", "micasense_subset.tif", package = "DroneBioR")

test_that("read_multispectral_orthomosaic returns named bands and alpha", {
  ortho <- read_multispectral_orthomosaic(ortho_fixture())
  expect_named(ortho, c("bands", "alpha", "source", "n_layers"))
  expect_equal(names(ortho$bands), c("Blue", "Green", "Red", "RedEdge", "NIR"))
  expect_equal(ortho$n_layers, 6)
  expect_false(is.null(ortho$alpha))
})

test_that("read_multispectral_orthomosaic errors on missing file", {
  expect_error(read_multispectral_orthomosaic(tempfile()), "Orthomosaic not found")
})

test_that("read_multispectral_orthomosaic honours an explicit band_map subset", {
  # User-supplied band_map drives which bands come back. Passing only
  # Red and Green should yield a 2-layer SpatRaster, not an error.
  ortho <- read_multispectral_orthomosaic(ortho_fixture(),
                                          band_map  = c(Red = 1, Green = 2),
                                          use_alpha = FALSE)
  expect_equal(names(ortho$bands), c("Green", "Red"))
})

test_that("read_multispectral_orthomosaic auto-detects RGB orthos (3 layers)", {
  # Build a synthetic 3-band RGB raster on disk, no alpha, no NIR.
  r <- terra::rast(nrows = 4, ncols = 4)
  terra::values(r) <- runif(16, 0, 1)
  rgb <- c(r, r * 0.5, r * 0.25)
  names(rgb) <- c("b1", "b2", "b3")
  tmp <- tempfile(fileext = ".tif")
  terra::writeRaster(rgb, tmp, datatype = "FLT4S")
  out <- read_multispectral_orthomosaic(tmp)
  expect_equal(out$n_layers, 3L)
  expect_equal(names(out$bands), c("Blue", "Green", "Red"))
  expect_null(out$alpha)
})

test_that("scale_to_reflectance clamps integer-scale values into 0-1", {
  ortho <- read_multispectral_orthomosaic(ortho_fixture())
  refl <- scale_to_reflectance(ortho$bands)
  mm <- terra::minmax(refl)
  expect_true(all(is.finite(mm)))
  expect_true(all(mm[1, ] >= 0))
  expect_true(all(mm[2, ] <= 1))
})

test_that("scale_to_reflectance is a no-op when already in 0-1", {
  r <- terra::rast(nrows = 4, ncols = 4)
  terra::values(r) <- runif(16, 0, 1)
  scaled <- scale_to_reflectance(r)
  expect_equal(terra::values(scaled), terra::values(r))
})

test_that("scale_to_reflectance accepts an explicit scale_factor", {
  r <- terra::rast(nrows = 4, ncols = 4)
  terra::values(r) <- seq_len(16) * 1000
  scaled <- scale_to_reflectance(r, scale_factor = 32768)
  mm <- terra::minmax(scaled)
  expect_true(all(mm[2, ] <= 1))
})

test_that("summarize_spatraster returns one row per layer", {
  ortho <- read_multispectral_orthomosaic(ortho_fixture())
  smry <- summarize_spatraster(ortho$bands)
  expect_equal(nrow(smry), terra::nlyr(ortho$bands))
  expect_true(all(c("min", "mean", "max", "sd") %in% names(smry)))
})

test_that("write_dronebio_rasters writes the expected files", {
  ortho <- read_multispectral_orthomosaic(ortho_fixture())
  refl <- scale_to_reflectance(ortho$bands)
  ix <- compute_spectral_indices(refl)
  proxy <- compute_biomass_proxy(ix)
  out <- tempfile("dronebior-test-")
  paths <- write_dronebio_rasters(out, refl, ix, proxy)

  expect_true(all(file.exists(paths)))
  expect_true(all(c("reflectance", "indices", "biomass_proxy") %in% names(paths)))
})

test_that("write_dronebio_rasters also writes valid_mask when supplied", {
  ortho <- read_multispectral_orthomosaic(ortho_fixture())
  refl <- scale_to_reflectance(ortho$bands)
  ix <- compute_spectral_indices(refl)
  proxy <- compute_biomass_proxy(ix)
  out <- tempfile("dronebior-mask-")
  paths <- write_dronebio_rasters(out, refl, ix, proxy, valid_mask = ortho$alpha)
  expect_true("valid_data_mask" %in% names(paths))
  expect_true(file.exists(paths[["valid_data_mask"]]))
})
