ortho_fixture <- function() system.file("extdata", "micasense_subset.tif", package = "DroneBioR")
field_fixture <- function() system.file("extdata", "field_samples.csv", package = "DroneBioR")

test_that("read_field_data reads the bundled fixture", {
  field <- read_field_data(field_fixture())
  expect_true(all(c("sample_id", "biomass_kgha", "x", "y") %in% names(field)))
  expect_gt(nrow(field), 0)
})

test_that("read_field_data errors when biomass_kgha is missing", {
  tmp <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(sample_id = "S01", x = 1, y = 2), tmp, row.names = FALSE)
  expect_error(read_field_data(tmp), "biomass_kgha")
})

test_that("read_field_data errors when no coordinates are provided", {
  tmp <- tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(sample_id = "S01", biomass_kgha = 1000),
    tmp, row.names = FALSE
  )
  expect_error(read_field_data(tmp), "x/y or longitude/latitude")
})

test_that("extract_field_spectral_data joins indices to field samples", {
  ortho <- read_multispectral_orthomosaic(ortho_fixture())
  refl <- scale_to_reflectance(ortho$bands)
  ix <- compute_spectral_indices(refl)
  field <- read_field_data(field_fixture())

  joined <- extract_field_spectral_data(field, ix)
  expect_equal(nrow(joined), nrow(field))
  expect_true(all(c("biomass_kgha", "NDVI", "NDRE") %in% names(joined)))
})

test_that("extract_field_spectral_data with default args is the single-cell extract", {
  ortho <- read_multispectral_orthomosaic(ortho_fixture())
  refl <- scale_to_reflectance(ortho$bands)
  ix <- compute_spectral_indices(refl)
  field <- read_field_data(field_fixture())

  # The three-argument call must stay byte-identical: R/report.R and the
  # Shiny studio both depend on it.
  joined <- extract_field_spectral_data(field, ix)
  pts <- sf::st_as_sf(field, coords = c("x", "y"), crs = terra::crs(ix),
                      remove = FALSE)
  want <- data.frame(
    sf::st_drop_geometry(pts),
    terra::extract(ix, terra::vect(pts), ID = FALSE),
    check.names = FALSE
  )
  expect_equal(joined, want)
})

test_that("extract_field_spectral_data keeps its shape with a 3x3 window", {
  ortho <- read_multispectral_orthomosaic(ortho_fixture())
  refl <- scale_to_reflectance(ortho$bands)
  ix <- compute_spectral_indices(refl)
  field <- read_field_data(field_fixture())

  single <- extract_field_spectral_data(field, ix)
  windowed <- extract_field_spectral_data(field, ix, window = 3, fun = "mean")
  expect_equal(dim(windowed), dim(single))
  expect_identical(names(windowed), names(single))
  expect_identical(windowed$sample_id, single$sample_id)
  # Averaging over 9 pixels must actually change something.
  expect_false(isTRUE(all.equal(windowed$NDVI, single$NDVI)))
})

test_that("fit_biomass_lm picks default predictors automatically", {
  set.seed(1)
  ndvi <- runif(30, 0.3, 0.9)
  ndre <- ndvi * 0.6 + rnorm(30, sd = 0.05)
  df <- data.frame(
    sample_id = sprintf("S%02d", 1:30),
    biomass_kgha = 1000 + 2500 * ndvi + 500 * ndre + rnorm(30, sd = 100),
    NDVI = ndvi,
    NDRE = ndre
  )
  model <- fit_biomass_lm(df)
  expect_s3_class(model, "lm")
  expect_true(all(c("NDVI", "NDRE") %in% names(coef(model))))
})

test_that("fit_biomass_lm respects an explicit predictors argument", {
  set.seed(1)
  df <- data.frame(
    biomass_kgha = runif(20, 100, 5000),
    NDVI = runif(20, 0.2, 0.9),
    NDRE = runif(20, 0.1, 0.6)
  )
  model <- fit_biomass_lm(df, predictors = "NDVI")
  expect_equal(names(coef(model)), c("(Intercept)", "NDVI"))
})

test_that("fit_biomass_lm errors when there are no predictor columns", {
  df <- data.frame(biomass_kgha = 1:5)
  expect_error(fit_biomass_lm(df), "No predictor columns")
})

test_that("fit_biomass_lm errors when sample size is insufficient", {
  df <- data.frame(biomass_kgha = c(1, 2), NDVI = c(0.3, 0.4))
  expect_error(fit_biomass_lm(df, predictors = "NDVI"), "Not enough")
})
