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
  # Full multispectral input (Blue + Green + Red + RedEdge + NIR) yields
  # all the canonical indices the package supports. The list is checked
  # against the documented canonical_order in compute_spectral_indices().
  expect_equal(
    names(indices),
    c("NDVI", "NDRE", "EVI", "SAVI", "OSAVI", "MSAVI2", "NDWI",
      "GNDVI", "CIrededge", "GCI", "RVI", "DVI", "WDRVI", "TVI",
      "MCARI", "PSRI",
      "VARI", "ExG", "GLI", "TGI", "MGRVI", "RGBVI")
  )
  expect_equal(terra::nlyr(indices), 22)
})

test_that("RGB-only reflectance still yields the visible-band indices", {
  r <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 2)
  terra::values(r) <- 1
  bands <- c(r * 0.10, r * 0.30, r * 0.20)
  names(bands) <- c("Blue", "Green", "Red")
  indices <- compute_spectral_indices(bands)
  # Without NIR / RedEdge the function must still return the six RGB-only
  # indices: VARI, ExG, GLI, TGI, MGRVI, RGBVI.
  expect_equal(
    names(indices),
    c("VARI", "ExG", "GLI", "TGI", "MGRVI", "RGBVI")
  )
})

test_that("compute_spectral_indices errors on missing band names", {
  r <- terra::rast(nrows = 2, ncols = 2)
  terra::values(r) <- 0.5
  names(r) <- "Blue"
  expect_error(compute_spectral_indices(r), "missing")
})

test_that("DJI Mavic 3M (no Blue) yields all non-Blue-dependent indices", {
  # DJI Mavic 3M has Green + Red + RedEdge + NIR but no calibrated Blue.
  # The function should skip EVI, VARI, ExG, GLI, TGI, RGBVI (which need
  # Blue) and still return the rest including MGRVI (Green + Red only).
  r <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 2)
  terra::values(r) <- 1
  bands <- c(r * 0.20, r * 0.30, r * 0.40, r * 0.70)
  names(bands) <- c("Green", "Red", "RedEdge", "NIR")
  indices <- compute_spectral_indices(bands)
  # NIR + RedEdge + Green + Red unlock NDVI, NDRE, SAVI, OSAVI, MSAVI2,
  # NDWI, GNDVI, CIrededge, GCI, RVI, DVI, WDRVI, TVI, MCARI, PSRI,
  # MGRVI - but not the six Blue-dependent ones.
  expect_setequal(
    names(indices),
    c("NDVI", "NDRE", "SAVI", "OSAVI", "MSAVI2", "NDWI", "GNDVI",
      "CIrededge", "GCI", "RVI", "DVI", "WDRVI", "TVI", "MCARI",
      "PSRI", "MGRVI")
  )
  expect_false("EVI"   %in% names(indices))
  expect_false("VARI"  %in% names(indices))
  expect_false("ExG"   %in% names(indices))
  expect_false("RGBVI" %in% names(indices))
})

test_that("compute_spectral_indices errors when even Green is missing", {
  r <- terra::rast(nrows = 2, ncols = 2)
  terra::values(r) <- 0.5
  names(r) <- "Red"
  expect_error(compute_spectral_indices(r), "Green")
})

test_that("strict = TRUE still requires Blue for backward compatibility", {
  r <- terra::rast(nrows = 2, ncols = 2)
  terra::values(r) <- 1
  bands <- c(r * 0.20, r * 0.30, r * 0.40, r * 0.70)
  names(bands) <- c("Green", "Red", "RedEdge", "NIR")
  expect_error(compute_spectral_indices(bands, strict = TRUE), "Blue")
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

test_that("export_all_covariates writes bands, indices, proxies and terrain", {
  bands <- make_reflectance()
  chm <- bands[[1]]; terra::values(chm) <- 3; names(chm) <- "CHM"
  dsm <- bands[[1]]; terra::values(dsm) <- 110
  out <- tempfile("cov_")

  written <- export_all_covariates(bands, out, chm = chm, dsm = dsm)
  files <- basename(list.files(out, pattern = "[.]tif$"))

  # Every reflectance band ...
  expect_true(all(paste0(c("Blue", "Green", "Red", "RedEdge", "NIR"), ".tif")
                  %in% files))
  # ... every spectral index ...
  expect_true(all(paste0(names(compute_spectral_indices(bands)), ".tif")
                  %in% files))
  # ... a canopy-height biomass proxy (needs the CHM we passed) ...
  expect_true("Biomass_NDVI_x_CHM.tif" %in% files)
  # ... and the terrain layers given.
  expect_true(all(c("CHM.tif", "DSM.tif") %in% files))
  expect_false("DTM.tif" %in% files)          # not supplied -> not written
  expect_length(written, length(files))
  # The result is a real, re-readable raster.
  expect_s4_class(terra::rast(file.path(out, "NDVI.tif")), "SpatRaster")

  unlink(out, recursive = TRUE)
})

test_that("export_all_covariates skips the CHM proxies gracefully with no CHM", {
  bands <- make_reflectance()
  out <- tempfile("cov_")
  written <- export_all_covariates(bands, out)   # no CHM/DSM/DTM
  files <- basename(list.files(out, pattern = "[.]tif$"))
  expect_true("NDVI.tif" %in% files)
  expect_false(any(grepl("_x_CHM", files)))      # no CHM -> no CHM proxies
  expect_false("CHM.tif" %in% files)
  unlink(out, recursive = TRUE)
})
