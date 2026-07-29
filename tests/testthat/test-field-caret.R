# Tests for the caret layer. Every model trained here is "lm", which caret
# implements on base R, so the suite never needs an uninstalled backend.

ortho_fixture <- function() {
  system.file("extdata", "micasense_subset.tif", package = "DroneBioR")
}

# A well-conditioned synthetic calibration set: two informative covariates
# and one pure-noise covariate.
synthetic_field <- function(n = 60, seed = 11) {
  set.seed(seed)
  d <- data.frame(
    sample_id = sprintf("S%02d", seq_len(n)),
    NDVI = stats::runif(n, 0.2, 0.9),
    NDRE = stats::runif(n, 0.1, 0.6),
    noise = stats::rnorm(n),
    stringsAsFactors = FALSE
  )
  d$biomass_kgha <- 800 + 3000 * d$NDVI + 700 * d$NDRE +
    stats::rnorm(n, sd = 150)
  attr(d, "window_px") <- 3L
  attr(d, "window_fun") <- "mean"
  attr(d, "crs") <- "EPSG:32617"
  d
}

fit_reference_model <- function(d = synthetic_field(), holdout = 0.25,
                                folds = 10L, predictors = c("NDVI", "NDRE"),
                                metric = "RMSE") {
  split <- field_train_split(d, holdout = holdout, folds = folds, seed = 42L)
  fit_field_caret_model(d, predictors = predictors, method = "lm",
                        metric = metric, split = split)
}

# ---- catalogue and availability --------------------------------------------

test_that("caret_model_catalogue lists the regression models with the documented shape", {
  skip_if_not_installed("caret")
  cat <- caret_model_catalogue()
  expect_named(cat, c("method", "label", "packages", "missing", "ready",
                      "tags", "n_params", "needs_scaling"))
  expect_gt(nrow(cat), 100L)
  expect_equal(nrow(cat), sum(vapply(caret::getModelInfo(),
                                     function(m) "Regression" %in% m$type,
                                     logical(1))))
  expect_type(cat$ready, "logical")
  expect_false(any(is.na(cat$ready)))

  lm_row <- cat[cat$method == "lm", , drop = FALSE]
  expect_equal(nrow(lm_row), 1L)
  expect_equal(lm_row$packages, "")
  expect_true(lm_row$ready)

  # `ready` is a strict subset of everything on offer.
  expect_lte(sum(cat$ready), nrow(cat))
  expect_true(all(cat$ready[cat$packages == ""]))

  if ("nnet" %in% cat$method) {
    expect_true(cat$needs_scaling[cat$method == "nnet"])
  }
})

test_that("caret_model_catalogue warns and returns zero rows without an installed backend list", {
  skip_if_not_installed("caret")
  cat <- caret_model_catalogue(installed = character(0))
  expect_false(any(cat$ready[cat$packages != ""]))
})

test_that("caret_model_available gates training and names the missing package", {
  skip_if_not_installed("caret")
  expect_true(caret_model_available("lm")$ok)
  expect_true(is.na(caret_model_available("lm")$install_call))

  # Pretend nothing is installed: the message must name the package rather
  # than opening caret's interactive menu() prompt.
  gated <- caret_model_available("ranger", installed = character(0))
  expect_false(gated$ok)
  expect_true("ranger" %in% gated$missing)
  expect_match(gated$install_call, "install\\.packages")
  expect_match(gated$install_call, "ranger")

  expect_error(caret_model_available("definitely_not_a_caret_method"),
               "Unknown caret method")
})

# ---- metric definitions -----------------------------------------------------

test_that(".biomass_metrics reports a hand-computed rmse, mae and rpiq", {
  obs <- c(1, 2, 3, 4, 5, 6, 7, 8)
  pred <- obs + c(1, -1, 1, -1, 1, -1, 1, -1)
  m <- .biomass_metrics(obs, pred)
  expect_equal(m$rmse, 1)
  expect_equal(m$mae, 1)
  expect_equal(m$rpiq,
               as.numeric(diff(stats::quantile(obs, c(0.25, 0.75)))) / 1)
})

test_that(".biomass_metrics returns NA rpiq on a perfect fit", {
  obs <- c(1, 2, 3, 4)
  m <- .biomass_metrics(obs, obs)
  expect_equal(m$r2, 1)
  expect_equal(m$rmse, 0)
  expect_true(is.na(m$rpiq))
})

test_that("r2 is 1 - SSres/SStot and demonstrably not caret's squared Pearson", {
  # A biased, mis-scaled prediction: perfectly correlated, badly calibrated.
  obs <- c(100, 200, 300, 400, 500)
  pred <- 0.5 * obs + 400
  m <- .biomass_metrics(obs, pred)
  expect_equal(stats::cor(obs, pred)^2, 1)
  expect_lt(m$r2, 0.5)
  expect_false(isTRUE(all.equal(m$r2, stats::cor(obs, pred)^2)))
})

# ---- split ------------------------------------------------------------------

test_that("field_train_split is reproducible and covers every training row out-of-fold", {
  skip_if_not_installed("caret")
  d <- synthetic_field()
  a <- field_train_split(d, holdout = 0.25, folds = 10L, seed = 42L)
  b <- field_train_split(d, holdout = 0.25, folds = 10L, seed = 42L)
  expect_identical(a$train_idx, b$train_idx)
  expect_identical(a$folds, b$folds)

  expect_equal(sort(c(a$train_idx, a$test_idx)), seq_len(nrow(d)))
  expect_equal(a$folds_k, 10L)
  expect_equal(sort(unlist(a$folds_out, use.names = FALSE)),
               seq_along(a$train_idx))
})

test_that("holdout = 0 gives an empty test set", {
  skip_if_not_installed("caret")
  split <- field_train_split(synthetic_field(), holdout = 0, folds = 5L)
  expect_identical(split$test_idx, integer(0))
  expect_equal(length(split$train_idx), 60L)
})

test_that("field_train_split refuses a response with missing values", {
  skip_if_not_installed("caret")
  d <- synthetic_field()
  d$biomass_kgha[3] <- NA
  expect_error(field_train_split(d), "missing value")
})

# ---- training contract ------------------------------------------------------

test_that("fit_field_caret_model returns the documented object", {
  skip_if_not_installed("caret")
  m <- fit_reference_model()
  expect_s3_class(m, "dronebio_field_model")
  expect_named(m, c("method", "label", "response", "predictors", "metric",
                    "window_px", "window_fun", "n", "train_idx", "test_idx",
                    "fit", "metrics", "predictions", "tuning", "best_tune",
                    "crs", "reference_geom", "settings", "caret_version",
                    "r_version", "trained_at", "elapsed_s"))
  expect_equal(m$predictors, c("NDVI", "NDRE"))
  expect_equal(m$window_px, 3L)
  expect_equal(m$window_fun, "mean")
  expect_equal(m$settings$r2_definition, "1 - SSres/SStot")
  expect_named(m$predictions,
               c("sample_id", "split", "observed", "predicted"))
})

test_that("trainControl pins exactly the four intended deviations", {
  skip_if_not_installed("caret")
  m <- fit_reference_model()
  expect_equal(m$fit$control$method, "cv")
  expect_equal(m$fit$control$number, 10L)
  expect_equal(m$fit$control$savePredictions, "final")
  expect_false(m$fit$control$allowParallel)
  # The custom summaryFunction adds RPIQ to the tuning table.
  expect_true("RPIQ" %in% names(m$tuning))
})

test_that("the out-of-fold series covers every training row exactly once", {
  skip_if_not_installed("caret")
  m <- fit_reference_model()
  cv <- m$predictions[m$predictions$split == "cv", , drop = FALSE]
  expect_equal(nrow(cv), length(m$train_idx))
  expect_setequal(cv$sample_id, sprintf("S%02d", m$train_idx))
})

test_that("the metric choice sets caret's maximize flag", {
  skip_if_not_installed("caret")
  expect_false(fit_reference_model(metric = "RMSE")$fit$maximize)
  expect_true(fit_reference_model(metric = "Rsquared")$fit$maximize)
})

test_that("re-training with the same split reproduces the predictions", {
  skip_if_not_installed("caret")
  d <- synthetic_field()
  split <- field_train_split(d, holdout = 0.25, folds = 10L, seed = 42L)
  a <- fit_field_caret_model(d, predictors = c("NDVI", "NDRE"),
                             method = "lm", split = split)
  b <- fit_field_caret_model(d, predictors = c("NDVI", "NDRE"),
                             method = "lm", split = split)
  expect_equal(a$predictions$predicted, b$predictions$predicted)
})

test_that("two models from one split share the fold assignment", {
  skip_if_not_installed("caret")
  d <- synthetic_field()
  split <- field_train_split(d, holdout = 0.25, folds = 10L, seed = 42L)
  a <- fit_field_caret_model(d, predictors = c("NDVI", "NDRE"),
                             method = "lm", split = split)
  b <- fit_field_caret_model(d, predictors = c("NDVI", "NDRE", "noise"),
                             method = "lm", split = split)
  expect_identical(a$fit$control$index, b$fit$control$index)
  expect_identical(a$train_idx, b$train_idx)
  expect_identical(a$test_idx, b$test_idx)
  # Same rows in the same order for both -> the leaderboard is comparable.
  cv_a <- a$predictions[a$predictions$split == "cv", "sample_id"]
  cv_b <- b$predictions[b$predictions$split == "cv", "sample_id"]
  expect_identical(cv_a, cv_b)
})

test_that("field_model_metrics has three rows with a holdout and two without", {
  skip_if_not_installed("caret")
  with_holdout <- field_model_metrics(fit_reference_model(holdout = 0.25))
  expect_equal(nrow(with_holdout), 3L)
  expect_equal(with_holdout$split, c("CV (10-fold)", "Train", "Test"))
  expect_named(with_holdout, c("split", "n", "r2", "rmse", "mae", "rpiq"))
  expect_type(with_holdout$r2, "double")

  no_holdout <- field_model_metrics(fit_reference_model(holdout = 0))
  expect_equal(nrow(no_holdout), 2L)
  expect_equal(no_holdout$split, c("CV (10-fold)", "Train"))
})

test_that("displayed r2 comes from .biomass_metrics, not caret's Rsquared", {
  skip_if_not_installed("caret")
  m <- fit_reference_model()
  cv <- m$predictions[m$predictions$split == "cv", , drop = FALSE]
  expect_equal(m$metrics$r2[1],
               .biomass_metrics(cv$observed, cv$predicted)$r2)
  expect_equal(m$metrics$rpiq[1],
               .biomass_metrics(cv$observed, cv$predicted)$rpiq)
})

test_that("format_field_metrics renders exactly two decimals", {
  skip_if_not_installed("caret")
  ft <- format_field_metrics(fit_reference_model())
  expect_named(ft, c("Split", "n", "R2", "RMSE", "MAE", "RPIQ"))
  expect_type(ft$R2, "character")
  expect_true(all(grepl("^-?[0-9]+\\.[0-9]{2}$", ft$R2)))
  expect_true(all(grepl("^-?[0-9]+\\.[0-9]{2}$", ft$RMSE)))
})

# ---- guard rails ------------------------------------------------------------

test_that("training refuses an absent covariate", {
  skip_if_not_installed("caret")
  d <- synthetic_field()
  split <- field_train_split(d, folds = 5L)
  expect_error(
    fit_field_caret_model(d, predictors = c("NDVI", "not_extracted"),
                          method = "lm", split = split),
    "not_extracted"
  )
})

test_that("training refuses too few samples for the covariate count", {
  skip_if_not_installed("caret")
  d <- synthetic_field(n = 12)
  d$a <- stats::runif(12); d$b <- stats::runif(12); d$c <- stats::runif(12)
  d$d <- stats::runif(12); d$e <- stats::runif(12); d$f <- stats::runif(12)
  split <- field_train_split(d, holdout = 0.5, folds = 3L)
  expect_error(
    fit_field_caret_model(d, predictors = c("NDVI", "NDRE", "a", "b", "c",
                                            "d", "e", "f"),
                          method = "lm", split = split),
    "at least"
  )
})

test_that("training refuses a zero-variance response", {
  skip_if_not_installed("caret")
  d <- synthetic_field()
  split <- field_train_split(d, folds = 5L)
  d$biomass_kgha <- 2000
  expect_error(
    fit_field_caret_model(d, predictors = "NDVI", method = "lm", split = split),
    "zero variance"
  )
})

test_that("training refuses a covariate that is entirely NA", {
  skip_if_not_installed("caret")
  d <- synthetic_field()
  d$off_raster <- NA_real_
  split <- field_train_split(d, folds = 5L)
  expect_error(
    fit_field_caret_model(d, predictors = c("NDVI", "off_raster"),
                          method = "lm", split = split),
    "off_raster"
  )
})

test_that("incomplete rows are dropped from the shared split, not re-partitioned", {
  skip_if_not_installed("caret")
  d <- synthetic_field()
  d$NDRE[c(2, 5)] <- NA
  split <- field_train_split(d, holdout = 0.25, folds = 10L, seed = 42L)
  m <- fit_field_caret_model(d, predictors = c("NDVI", "NDRE"),
                             method = "lm", split = split)
  expect_false(any(c(2L, 5L) %in% m$train_idx))
  expect_false(any(c(2L, 5L) %in% m$test_idx))
  expect_true(all(m$train_idx %in% split$train_idx))
  expect_true(all(m$test_idx %in% split$test_idx))
})

# ---- prediction -------------------------------------------------------------

test_that("predict.dronebio_field_model names a missing covariate", {
  skip_if_not_installed("caret")
  m <- fit_reference_model()
  expect_error(predict(m, data.frame(NDVI = 0.5)), "NDRE")
})

test_that("predict returns one value per row even with NA rows present", {
  skip_if_not_installed("caret")
  m <- fit_reference_model()
  nd <- data.frame(NDVI = c(0.4, NA, 0.7), NDRE = c(0.2, 0.3, NA))
  p <- predict(m, nd)
  expect_length(p, 3L)
  expect_true(is.finite(p[1]))
  expect_true(all(is.na(p[2:3])))
})

test_that("predict_field_model_map preserves NA cells and clamps at min_biomass", {
  skip_if_not_installed("caret")
  m <- fit_reference_model()
  stack <- terra::rast(nrows = 20, ncols = 20, nlyrs = 2, crs = "EPSG:32617")
  terra::values(stack) <- cbind(
    stats::runif(terra::ncell(stack), 0.2, 0.9),
    stats::runif(terra::ncell(stack), 0.1, 0.6)
  )
  names(stack) <- c("NDVI", "NDRE")
  stack[[1]][1:17] <- NA

  map <- predict_field_model_map(m, stack, min_biomass = 0)
  expect_equal(names(map), "biomass_kgha")
  vals <- terra::values(map)[, 1]
  expect_equal(sum(is.na(vals)), 17L)
  expect_true(all(vals[is.finite(vals)] >= 0))
})

test_that("predict_field_model_map rejects renamed and duplicated layers", {
  skip_if_not_installed("caret")
  m <- fit_reference_model()
  stack <- terra::rast(nrows = 10, ncols = 10, nlyrs = 2, crs = "EPSG:32617")
  terra::values(stack) <- cbind(stats::runif(100, 0.2, 0.9),
                                stats::runif(100, 0.1, 0.6))
  names(stack) <- c("NDVI", "ndre")
  expect_error(predict_field_model_map(m, stack), "NDRE")

  names(stack) <- c("NDVI", "NDRE")
  dup <- c(stack, stack[[2]])
  names(dup) <- c("NDVI", "NDRE", "NDRE")
  expect_error(predict_field_model_map(m, dup), "[Dd]uplicate")
})

test_that("predict_field_model_map writes a GeoTIFF with the right layer name", {
  skip_if_not_installed("caret")
  m <- fit_reference_model()
  stack <- terra::rast(nrows = 10, ncols = 10, nlyrs = 2, crs = "EPSG:32617")
  terra::values(stack) <- cbind(stats::runif(100, 0.2, 0.9),
                                stats::runif(100, 0.1, 0.6))
  names(stack) <- c("NDVI", "NDRE")
  out <- tempfile(fileext = ".tif")
  predict_field_model_map(m, stack, out_path = out)
  expect_true(file.exists(out))
  expect_equal(names(terra::rast(out)), "biomass_kgha")
})

test_that("export_field_biomass_map matches a manual per-pixel prediction", {
  skip_if_not_installed("caret")
  refl <- scale_to_reflectance(read_multispectral_orthomosaic(ortho_fixture())$bands)

  # Train on covariates extracted from the real reflectance so the model and
  # the raster share a vocabulary.
  set.seed(4)
  band_all <- terra::extract(refl, seq_len(terra::ncell(refl)))
  cov_all <- covariate_frame_from_pixels(band_all, c("NDVI", "NDRE"),
                                         crs = terra::crs(refl))
  ok <- which(stats::complete.cases(band_all) & stats::complete.cases(cov_all))
  cells <- sample(ok, 60)
  xy <- terra::xyFromCell(refl[[1]], cells)
  pts <- sf::st_as_sf(
    data.frame(sample_id = sprintf("P%02d", seq_along(cells)),
               biomass_kgha = 500 + 4000 * cov_all$NDVI[cells] +
                 stats::rnorm(60, sd = 120),
               x = xy[, 1], y = xy[, 2]),
    coords = c("x", "y"), crs = terra::crs(refl), remove = FALSE
  )
  tab <- extract_field_covariates(pts, refl, c("NDVI", "NDRE"), window = 3)
  split <- field_train_split(tab, holdout = 0.25, folds = 10L, seed = 5L)
  m <- fit_field_caret_model(tab, predictors = c("NDVI", "NDRE"),
                             method = "lm", split = split)

  out <- tempfile(fileext = ".tif")
  exported <- export_field_biomass_map(m, refl, out)
  expect_equal(names(exported), "biomass_kgha")
  expect_equal(terra::ncell(exported), terra::ncell(refl))

  probe <- utils::head(ok, 25)
  manual <- pmax(as.numeric(stats::predict(
    m$fit,
    newdata = covariate_frame_from_pixels(terra::extract(refl, probe),
                                          m$predictors, crs = m$crs),
    na.action = stats::na.pass
  )), 0)
  got <- terra::extract(exported, probe)[[1]]
  # The arithmetic is identical; the only difference is FLT4S storage in the
  # GeoTIFF, which is single precision.
  expect_equal(got, manual, tolerance = 1e-6)

  expect_error(export_field_biomass_map(m, refl, out_path = ""), "out_path")
})

# ---- persistence ------------------------------------------------------------

test_that("save_field_model_bundle writes a runnable bundle", {
  skip_if_not_installed("caret")
  skip_if(!nzchar(Sys.which(Sys.getenv("R_ZIPCMD", "zip"))), "no zip command")
  d <- synthetic_field()
  m <- fit_reference_model(d)
  zip_path <- tempfile(fileext = ".zip")
  save_field_model_bundle(m, zip_path, samples = d)

  contents <- utils::unzip(zip_path, list = TRUE)$Name
  expect_true(all(c("model.rds", "README.txt", "predict_biomass.R",
                    "metrics.csv", "samples.csv") %in% contents))

  dir <- tempfile(); dir.create(dir)
  utils::unzip(zip_path, exdir = dir)
  reloaded <- readRDS(file.path(dir, "model.rds"))
  expect_s3_class(reloaded, "dronebio_field_model")
  samples <- utils::read.csv(file.path(dir, "samples.csv"),
                             stringsAsFactors = FALSE)
  expect_length(predict(reloaded, samples), nrow(samples))

  readme <- readLines(file.path(dir, "README.txt"))
  expect_true(any(grepl("install.packages", readme, fixed = TRUE)))
  expect_true(any(grepl("NDVI", readme, fixed = TRUE)))
})

test_that("save_field_model_bundle honours a relative path", {
  skip_if_not_installed("caret")
  skip_if(!nzchar(Sys.which(Sys.getenv("R_ZIPCMD", "zip"))), "no zip command")
  m <- fit_reference_model()
  # The zip runs from a staging directory, so a relative destination has to
  # be absolutised first or the bundle lands in the staging dir and is
  # deleted with it.
  dir <- tempfile(); dir.create(dir)
  old <- setwd(dir)
  on.exit(setwd(old), add = TRUE)
  save_field_model_bundle(m, "bundle.zip")
  expect_true(file.exists(file.path(dir, "bundle.zip")))
})

test_that("the saved model carries no reference to the DroneBioR namespace", {
  skip_if_not_installed("caret")
  m <- fit_reference_model()
  # The bundle promises readRDS + predict with no library() calls, so the
  # summary closure must serialise by value.
  expect_identical(
    environmentName(environment(m$fit$control$summaryFunction)), "base"
  )
})

test_that("load_field_model_bundle round-trips the model", {
  skip_if_not_installed("caret")
  skip_if(!nzchar(Sys.which(Sys.getenv("R_ZIPCMD", "zip"))), "no zip command")
  m <- fit_reference_model()
  zip_path <- tempfile(fileext = ".zip")
  save_field_model_bundle(m, zip_path)
  reloaded <- load_field_model_bundle(zip_path)
  expect_s3_class(reloaded, "dronebio_field_model")
  expect_equal(reloaded$predictors, m$predictors)
  expect_equal(predict(reloaded, data.frame(NDVI = 0.5, NDRE = 0.3)),
               predict(m, data.frame(NDVI = 0.5, NDRE = 0.3)))
})

test_that("write_field_model_summary creates the outputs directory it needs", {
  skip_if_not_installed("caret")
  m <- fit_reference_model()
  # The workflow stepper has always checked this exact path and nothing has
  # ever created it, so the intermediate directory matters.
  path <- file.path(tempfile("project_"), "outputs",
                    "biomass_model_summary.txt")
  write_field_model_summary(m, path)
  expect_true(file.exists(path))

  txt <- readLines(path)
  expect_true(any(grepl("RPIQ", txt, fixed = TRUE)))
  expect_true(any(grepl("lm", txt, fixed = TRUE)))
  expect_true(any(grepl("1 - SSres/SStot", txt, fixed = TRUE)))

  ft <- format_field_metrics(m)
  expect_true(any(grepl(ft$RMSE[1], txt, fixed = TRUE)))
  expect_true(any(grepl(ft$R2[1], txt, fixed = TRUE)))
})

test_that("print.dronebio_field_model reports the metric definitions", {
  skip_if_not_installed("caret")
  m <- fit_reference_model()
  txt <- utils::capture.output(print(m))
  expect_true(any(grepl("SSres/SStot", txt, fixed = TRUE)))
  expect_true(any(grepl("RPIQ", txt, fixed = TRUE)))
  expect_true(any(grepl("CV (10-fold)", txt, fixed = TRUE)))
})
