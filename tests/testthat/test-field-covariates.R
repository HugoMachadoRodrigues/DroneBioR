# Tests for the covariate vocabulary and the map-side plumbing:
# field_covariate_catalogue / normalize_covariate_names /
# build_prediction_stack / biomass_map_breaks / classify_biomass_map.

ortho_fixture <- function() {
  system.file("extdata", "micasense_subset.tif", package = "DroneBioR")
}

test_reflectance <- function() {
  scale_to_reflectance(read_multispectral_orthomosaic(ortho_fixture())$bands)
}

test_that("the catalogue only offers proxies whose prerequisites hold", {
  none <- field_covariate_catalogue(
    band_names = c("Red", "Green"),
    index_names = c("MGRVI")
  )
  expect_false("Biomass_Spectral" %in% none$id)
  expect_false(any(grepl("_x_CHM$", none$id)))

  spectral <- field_covariate_catalogue(
    index_names = c("NDVI", "SAVI", "NDRE"), has_chm = FALSE
  )
  expect_true("Biomass_Spectral" %in% spectral$id)
  expect_false(any(grepl("_x_CHM$", spectral$id)))

  vari <- field_covariate_catalogue(index_names = "VARI")
  expect_true("Biomass_Spectral" %in% vari$id)

  with_chm <- field_covariate_catalogue(
    index_names = c("NDVI", "SAVI", "NDRE"), has_chm = TRUE
  )
  expect_true("Biomass_NDVI_x_CHM" %in% with_chm$id)
  expect_true("CHM_m" %in% with_chm$id)
  # SAVI x CHM only when SAVI is there; VARI x CHM must not appear.
  expect_true("Biomass_SAVI_x_CHM" %in% with_chm$id)
  expect_false("Biomass_VARI_x_CHM" %in% with_chm$id)
})

test_that("the catalogue never emits valid_data_mask and marks the recommended set", {
  cat <- field_covariate_catalogue(
    band_names = c("Red", "NIR", "valid_data_mask", "alpha"),
    index_names = c("NDVI", "NDRE", "SAVI", "GNDVI", "MGRVI"),
    has_chm = TRUE
  )
  expect_false("valid_data_mask" %in% cat$id)
  expect_false("alpha" %in% cat$id)
  expect_setequal(cat$id[cat$recommended],
                  c("NDVI", "NDRE", "SAVI", "GNDVI", "CHM_m"))
  expect_setequal(unique(cat$group),
                  c("Reflectance bands", "Spectral indices",
                    "Biomass proxies", "Terrain"))
  expect_named(cat, c("id", "label", "group", "source", "recommended", "note"))
})

test_that("the catalogue includes a named custom index", {
  cat <- field_covariate_catalogue(index_names = "NDVI",
                                   custom_index_name = "my_ratio")
  expect_true("my_ratio" %in% cat$id)
  expect_equal(cat$group[cat$id == "my_ratio"], "Spectral indices")
})

test_that("normalize_covariate_names collapses the CHM spellings", {
  m <- normalize_covariate_names(c("NDVI", "chm", "dsm", "dtm"), warn = FALSE)
  expect_equal(unname(m[["chm"]]), "CHM_m")
  expect_equal(unname(m[["dsm"]]), "DSM")
  expect_equal(unname(m[["dtm"]]), "DTM")
  expect_equal(unname(m[["NDVI"]]), "NDVI")
  expect_equal(names(m), c("NDVI", "chm", "dsm", "dtm"))

  expect_equal(unname(normalize_covariate_names("CHM", warn = FALSE)), "CHM_m")
  expect_equal(unname(normalize_covariate_names("CHM_m", warn = FALSE)), "CHM_m")
})

test_that("normalize_covariate_names errors on true duplicates", {
  expect_error(normalize_covariate_names(c("CHM", "chm"), warn = FALSE),
               "Duplicate")
  expect_error(normalize_covariate_names(c("NDVI", "NDVI"), warn = FALSE),
               "NDVI")
})

test_that("build_prediction_stack returns exactly the requested layers in order", {
  refl <- test_reflectance()
  want <- c("NDRE", "NDVI", "Red")
  stack <- build_prediction_stack(refl, want, max_cells = 2000, window = 3)
  expect_identical(names(stack), want)
  # 32 x 32 / 9 is well inside the budget, so fact stays at the window size
  # and a map cell matches the model's training support.
  expect_equal(attr(stack, "fact"), 3L)
  expect_equal(attr(stack, "cell_size_m"),
               as.numeric(terra::res(refl)[1]) * 3)
})

test_that("build_prediction_stack falls back to the cell budget on a big grid", {
  refl <- test_reflectance()
  stack <- build_prediction_stack(refl, "NDVI", max_cells = 16, window = 3)
  expect_gt(attr(stack, "fact"), 3L)
  expect_lte(terra::ncell(stack), 40)
})

test_that("build_prediction_stack names the covariate it cannot supply", {
  refl <- test_reflectance()
  expect_error(
    build_prediction_stack(refl, c("NDVI", "Biomass_NDVI_x_CHM"),
                           max_cells = 2000, chm = NULL),
    "Biomass_NDVI_x_CHM"
  )
  expect_error(
    build_prediction_stack(refl, "CHM_m", max_cells = 2000, chm = NULL),
    "CHM_m"
  )
})

test_that("build_prediction_stack resamples a supplied CHM onto the grid", {
  refl <- test_reflectance()
  chm <- refl[[1]]
  terra::values(chm) <- seq_len(terra::ncell(chm)) / terra::ncell(chm)
  names(chm) <- "CHM_m"
  stack <- build_prediction_stack(refl, c("NDVI", "CHM_m", "Biomass_NDVI_x_CHM"),
                                  max_cells = 2000, window = 3, chm = chm)
  expect_identical(names(stack), c("NDVI", "CHM_m", "Biomass_NDVI_x_CHM"))
  expect_true(any(is.finite(terra::values(stack[["Biomass_NDVI_x_CHM"]]))))
})

test_that("biomass_map_breaks returns finite monotonic breaks", {
  r <- terra::rast(nrows = 150, ncols = 150)
  terra::values(r) <- seq_len(terra::ncell(r))
  brk <- biomass_map_breaks(r, n = 4L)
  expect_length(brk$breaks, 5L)
  expect_true(all(is.finite(brk$breaks)))
  expect_equal(brk$breaks, sort(brk$breaks))
  expect_length(brk$labels, 4L)
  expect_lt(brk$p01, brk$p99)
  expect_length(brk$quartiles, 3L)
})

test_that("the spatSample path reproduces the full-raster breaks", {
  r <- terra::rast(nrows = 200, ncols = 200)
  terra::values(r) <- seq_len(terra::ncell(r))
  full <- biomass_map_breaks(r, n = 4L, sample_size = terra::ncell(r) + 1)
  sampled <- biomass_map_breaks(r, n = 4L, sample_size = 5000)
  expect_equal(sampled$breaks, full$breaks,
               tolerance = 0.02 * diff(range(full$breaks)))
})

test_that("classify_biomass_map gives equal-count classes", {
  r <- terra::rast(nrows = 150, ncols = 150)
  terra::values(r) <- seq_len(terra::ncell(r))
  brk <- biomass_map_breaks(r, n = 4L)
  cls <- classify_biomass_map(r, brk$breaks)
  expect_equal(names(cls), "biomass_class")
  f <- terra::freq(cls)
  expect_equal(nrow(f), 4L)
  expect_true(all(abs(f$count - terra::ncell(r) / 4) <= 1))
  expect_equal(as.character(f$value), brk$labels)
})

test_that("classify_biomass_map validates label count", {
  r <- terra::rast(nrows = 10, ncols = 10)
  terra::values(r) <- seq_len(terra::ncell(r))
  expect_error(classify_biomass_map(r, c(0, 50, 100), labels = "only one"),
               "2 entries")
})
