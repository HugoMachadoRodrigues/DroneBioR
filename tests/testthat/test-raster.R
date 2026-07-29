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

test_that("default_dji_mavic_3m_band_map returns the calibrated-only mapping", {
  bm <- default_dji_mavic_3m_band_map()
  # Blue is deliberately absent: the Mavic 3M does not capture a
  # calibrated blue MS band, and the RGB JPG blue channel is not
  # comparable to the radiometrically-corrected MS bands.
  expect_setequal(names(bm), c("Green", "Red", "RedEdge", "NIR"))
  expect_false("Blue" %in% names(bm))
  expect_equal(unname(bm[c("Green", "Red", "RedEdge", "NIR")]),
               c(4L, 5L, 6L, 7L))
})

test_that("read_multispectral_orthomosaic auto-detects DJI Mavic 3M orthos (7 layers)", {
  # Synthetic 7-band stacked DJI Mavic 3M ortho (R, G, B, MS_G, MS_R,
  # MS_RE, MS_NIR), no alpha. Auto-detect must route to the DJI band
  # map and expose only the four calibrated MS bands.
  r <- terra::rast(nrows = 4, ncols = 4)
  terra::values(r) <- 1  # constant base so per-layer values are exact
  stacked <- c(r * 0.30, r * 0.20, r * 0.10,             # R, G, B
               r * 0.22, r * 0.32, r * 0.42, r * 0.72)   # MS_G, MS_R, MS_RE, MS_NIR
  names(stacked) <- paste0("b", 1:7)
  tmp <- tempfile(fileext = ".tif")
  terra::writeRaster(stacked, tmp, datatype = "FLT4S")
  out <- read_multispectral_orthomosaic(tmp)
  expect_equal(out$n_layers, 7L)
  expect_equal(names(out$bands), c("Green", "Red", "RedEdge", "NIR"))
  expect_false("Blue" %in% names(out$bands))
  # Green should be the calibrated MS_G (layer 4 = 0.22), not
  # the RGB JPG green (layer 2 = 0.20).
  green_value <- terra::values(out$bands[["Green"]])[1L]
  expect_equal(green_value, 0.22, tolerance = 1e-4)
})

test_that("DJI Mavic 3M pipeline produces only the 16 non-Blue indices", {
  # End-to-end: synthetic 7-band stacked DJI ortho -> read with the
  # DJI band map -> compute_spectral_indices. The six Blue-dependent
  # indices (EVI, VARI, ExG, GLI, TGI, RGBVI) must be absent and the
  # remaining 16 must all be present.
  r <- terra::rast(nrows = 4, ncols = 4)
  terra::values(r) <- 1
  stacked <- c(r * 0.30, r * 0.20, r * 0.10,             # R, G, B
               r * 0.22, r * 0.32, r * 0.42, r * 0.72)   # MS_G, MS_R, MS_RE, MS_NIR
  tmp <- tempfile(fileext = ".tif")
  terra::writeRaster(stacked, tmp, datatype = "FLT4S")
  ortho <- read_multispectral_orthomosaic(tmp)
  refl  <- scale_to_reflectance(ortho$bands)
  ix    <- compute_spectral_indices(refl)
  expect_setequal(
    names(ix),
    c("NDVI", "NDRE", "SAVI", "OSAVI", "MSAVI2", "NDWI", "GNDVI",
      "CIrededge", "GCI", "RVI", "DVI", "WDRVI", "TVI", "MCARI",
      "PSRI", "MGRVI")
  )
  for (skipped in c("EVI", "VARI", "ExG", "GLI", "TGI", "RGBVI")) {
    expect_false(skipped %in% names(ix), info = skipped)
  }
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

test_that("orthomosaic_band_presence trusts band names over layer count", {
  # The real MicaSense / DJI layout: NIR and RedEdge are named, so a 6-band
  # file with an alpha channel must not be read as RGB.
  b <- orthomosaic_band_presence(
    c("Red", "Green", "Blue", "NIR", "Rededge", "odm_orthophoto_6"))
  expect_true(b$has_nir)
  expect_true(b$has_rededge)
  expect_equal(b$by, "name")

  # Case and spelling of "RedEdge" vary between writers.
  expect_true(orthomosaic_band_presence(c("red", "RedEdge", "nir"))$has_rededge)
  expect_true(orthomosaic_band_presence(c("Red_Edge", "NIR"))$has_rededge)
})

test_that("orthomosaic_band_presence says no for a genuinely RGB file", {
  b <- orthomosaic_band_presence(c("red", "green", "blue", "orthomosaic_4"),
                                 nlyr = 4)
  expect_false(b$has_nir)
  expect_false(b$has_rededge)
  # red/green/blue are recognised by name, so the answer is positive knowledge
  # that NIR is absent rather than a guess from the layer count.
  expect_equal(b$by, "name")
  expect_true(b$has_blue)
})

test_that("orthomosaic_band_presence falls back to the count when unnamed", {
  # No usable names: >4 layers is the only signal left.
  expect_true(orthomosaic_band_presence(c("b1", "b2", "b3", "b4", "b5"),
                                        nlyr = 5)$has_nir)
  expect_false(orthomosaic_band_presence(c("b1", "b2", "b3"), nlyr = 3)$has_nir)
  # A 4-band multispectral subset is indistinguishable by count -- documented
  # limitation of the fallback, and the reason names come first.
  expect_false(orthomosaic_band_presence(c("b1", "b2", "b3", "b4"),
                                         nlyr = 4)$has_nir)
})

test_that("orthomosaic_band_presence accepts a SpatRaster directly", {
  r <- terra::rast(nrows = 2, ncols = 2, nlyrs = 5)
  names(r) <- c("Red", "Green", "Blue", "NIR", "Rededge")
  b <- orthomosaic_band_presence(r)
  expect_true(b$has_nir && b$has_rededge)
})

test_that("canonical_band_names handles the spellings different drones use", {
  expect_equal(canonical_band_names(c("Red", "Green", "Blue", "NIR", "Rededge")),
               c("Red", "Green", "Blue", "NIR", "RedEdge"))
  # DJI Mavic 3M stack: RGB camera then the MS set, so Red/Green appear twice.
  expect_equal(canonical_band_names(c("MS_G", "MS_R", "MS_RE", "MS_NIR")),
               c("Green", "Red", "RedEdge", "NIR"))
  expect_equal(canonical_band_names(c("green", "red", "red_edge", "nir")),
               c("Green", "Red", "RedEdge", "NIR"))
  expect_equal(canonical_band_names(c("R", "G", "B", "RE", "NIR")),
               c("Red", "Green", "Blue", "RedEdge", "NIR"))
  expect_equal(canonical_band_names(c("band_red", "band_nir")), c("Red", "NIR"))
  # Unrecognised means absent, never a guess.
  expect_true(all(is.na(canonical_band_names(c("b1", "b2", "alpha")))))
})

test_that("red edge is never mistaken for red", {
  # "RE" matching /red/ first would file a red-edge layer as red and quietly
  # corrupt every index built on either.
  expect_equal(canonical_band_names("RE"), "RedEdge")
  expect_equal(canonical_band_names("MS_RE"), "RedEdge")
  expect_equal(canonical_band_names("rededge"), "RedEdge")
  expect_equal(canonical_band_names("red"), "Red")
  expect_equal(canonical_band_names("NIR"), "NIR")
})

test_that("orthomosaic_band_presence reports the whole band set by name", {
  b <- orthomosaic_band_presence(c("Red", "Green", "Blue", "MS_G", "MS_R",
                                   "MS_RE", "MS_NIR"))
  expect_equal(b$by, "name")
  expect_true(b$has_nir && b$has_rededge)
  # Blue exists as a layer but only in the uncalibrated RGB triplet, so it is
  # NOT available to the indices -- matching default_dji_mavic_3m_band_map().
  expect_false(b$has_blue)
  expect_setequal(b$bands, c("Red", "Green", "RedEdge", "NIR"))

  rgb <- orthomosaic_band_presence(c("red", "green", "blue", "alpha"), nlyr = 4)
  expect_true(rgb$has_blue)
  expect_false(rgb$has_nir)
  expect_false(rgb$has_rededge)
})

test_that("the DJI band map takes Green and Red from the calibrated MS set", {
  m <- default_dji_mavic_3m_band_map()
  # Never layers 1-2: those are the uncalibrated RGB camera.
  expect_equal(unname(m[["Green"]]), 4)
  expect_equal(unname(m[["Red"]]), 5)
  expect_equal(unname(m[["RedEdge"]]), 6)
  expect_equal(unname(m[["NIR"]]), 7)
})

test_that("a DJI stack yields the calibrated index set, without the Blue ones", {
  skip_if_not_installed("terra")
  set.seed(1)
  r <- terra::rast(nrows = 12, ncols = 12, nlyrs = 7, crs = "EPSG:32634")
  terra::values(r) <- runif(terra::ncell(r) * 7, 1000, 30000)
  names(r) <- c("Red", "Green", "Blue", "MS_G", "MS_R", "MS_RE", "MS_NIR")
  f <- tempfile(fileext = ".tif")
  terra::writeRaster(r, f)
  o <- read_multispectral_orthomosaic(f, band_map = default_dji_mavic_3m_band_map())
  ix <- compute_spectral_indices(scale_to_reflectance(o$bands))
  # The calibrated set supports these; the six Blue-dependent indices stay
  # out, and the UI must agree rather than offering them.
  expect_true(all(c("NDVI", "NDRE", "NDRE", "GNDVI", "CIrededge") %in% names(ix)))
  expect_false(any(c("EVI", "VARI", "RGBVI") %in% names(ix)))
})
