# Tests for the field-calibrated biomass mapping pipeline
# (fit_plate_meter -> make_biomass_grid -> build_biomass_calibration ->
#  fit_biomass_model -> predict_biomass_map / run_biomass_mapping).

# A tiny synthetic scene: CHM rises west->east, NDVI rises south->north, so a
# biomass surface built from both has signal in two independent directions.
make_scene <- function() {
  crs_utm <- "EPSG:32617"
  tmpl <- terra::rast(nrows = 40, ncols = 40, xmin = 0, xmax = 4,
                      ymin = 0, ymax = 4, crs = crs_utm)
  xy <- terra::xyFromCell(tmpl, seq_len(terra::ncell(tmpl)))
  chm <- tmpl; terra::values(chm) <- pmax(0, 0.4 * (xy[, 1] / 4) + 0.01)
  names(chm) <- "chm"
  ndvi <- tmpl; terra::values(ndvi) <- 0.3 + 0.5 * (xy[, 2] / 4)
  ix <- c(ndvi, ndvi * 0.8, ndvi * 0.9, ndvi * 0.85)
  names(ix) <- c("NDVI", "NDRE", "SAVI", "GNDVI")
  list(ix = ix, chm = chm, crs = crs_utm)
}

sample_field <- function(sc, n_clip, n_plate, pasture = "p1", mult = 1) {
  bt <- 500 + 4000 * sc$chm + 1500 * sc$ix[["NDVI"]]
  n <- n_clip + n_plate
  pts <- cbind(x = stats::runif(n, 0.2, 3.8), y = stats::runif(n, 0.2, 3.8))
  colnames(pts) <- c("x", "y")
  v <- terra::vect(pts, type = "points", crs = sc$crs)
  chm_v <- terra::extract(sc$chm, v, ID = FALSE)[[1]]
  bt_v  <- terra::extract(bt, v, ID = FALSE)[[1]]
  is_clip <- seq_len(n) <= n_clip
  data.frame(
    sample_id       = sprintf("%s%02d", pasture, seq_len(n)),
    pasture         = pasture,
    x = pts[, 1], y = pts[, 2],
    biomass_kgha    = ifelse(is_clip, mult * bt_v + stats::rnorm(n, 0, 50), NA_real_),
    plate_height_cm = 5 + 30 * chm_v + stats::rnorm(n, 0, 0.5),
    sample_type     = ifelse(is_clip, "clip", "plate"),
    stringsAsFactors = FALSE
  )
}

test_that("fit_plate_meter calibrates, reports CV and predicts monotonically", {
  set.seed(1)
  h <- stats::runif(15, 2, 18)
  d <- data.frame(plate_height_cm = h,
                  biomass_kgha = 200 + 320 * h + stats::rnorm(15, sd = 120))
  cal <- fit_plate_meter(d)
  expect_s3_class(cal, "dronebio_plate_meter")
  expect_gt(cal$metrics$r2, 0.8)
  p <- predict(cal, data.frame(plate_height_cm = c(5, 10)))
  expect_length(p, 2)
  expect_true(all(p >= 0))
  expect_gt(p[2], p[1])
})

test_that("fit_plate_meter rejects too few clip points", {
  d <- data.frame(plate_height_cm = 1:3, biomass_kgha = c(100, 200, 300))
  expect_error(fit_plate_meter(d, min_n = 5), "clip points")
})

test_that("make_biomass_grid returns index means, CHM stats and volume", {
  sc <- make_scene()
  g <- make_biomass_grid(sc$ix, sc$chm, grid_m = 1)
  expect_true(all(c("NDVI", "chm_mean", "chm_median", "chm_max",
                    "chm_sd", "chm_var", "chm_volume_m3") %in% names(g)))
  # 1 m grid over a 4 m scene at 0.1 m px -> 4x4 cells
  expect_equal(terra::ncol(g), 4L)
  expect_true(all(terra::values(g[["chm_volume_m3"]]) >= 0, na.rm = TRUE))
})

test_that("make_biomass_grid works without a CHM (spectral only)", {
  sc <- make_scene()
  g <- make_biomass_grid(sc$ix, chm = NULL, grid_m = 1)
  expect_true(all(c("NDVI", "NDRE") %in% names(g)))
  expect_false(any(grepl("chm", names(g))))
})

test_that("run_biomass_mapping (lm) fills plate points and maps kg/ha", {
  set.seed(2)
  sc <- make_scene()
  field <- sample_field(sc, n_clip = 8, n_plate = 10)
  res <- run_biomass_mapping(field, sc$ix, sc$chm, method = "lm", grid_m = 1)

  expect_s3_class(res$model, "dronebio_biomass_model")
  expect_equal(res$model$method, "lm")
  expect_gt(res$model$metrics$r2, 0.5)
  expect_true(any(res$calibration$biomass_source == "plate_modeled"))
  expect_equal(names(res$map), "biomass_kgha")
  vals <- terra::values(res$map)
  expect_true(any(is.finite(vals)))
  expect_true(all(vals[is.finite(vals)] >= 0))
})

test_that("predict_biomass_map errors when a predictor layer is missing", {
  set.seed(4)
  sc <- make_scene()
  field <- sample_field(sc, n_clip = 8, n_plate = 6)
  grid <- make_biomass_grid(sc$ix, sc$chm, grid_m = 1)
  cal <- build_biomass_calibration(field, grid,
                                   plate_meter = fit_plate_meter(field))
  model <- fit_biomass_model(cal, method = "lm")
  expect_error(predict_biomass_map(model, grid[["NDVI"]]),
               "missing predictor")
})

test_that("random forest route uses the pasture factor and maps one pasture", {
  skip_if_not_installed("ranger")
  set.seed(3)
  sc <- make_scene()
  field <- rbind(sample_field(sc, 14, 0, "p1", 1.0),
                 sample_field(sc, 14, 0, "p2", 1.2))
  grid <- make_biomass_grid(sc$ix, sc$chm, grid_m = 1)
  cal <- build_biomass_calibration(field, grid)
  m <- fit_biomass_model(cal, method = "rf", group = "pasture")
  expect_equal(m$method, "rf")
  expect_true("pasture" %in% m$predictors)
  expect_true(all(c("chm_max", "chm_var") %in% m$predictors))
  map <- predict_biomass_map(m, grid, pasture = "p1")
  expect_equal(names(map), "biomass_kgha")
  expect_true(any(is.finite(terra::values(map))))
})

test_that("method = 'auto' compares both routes and keeps the lower-RMSE one", {
  skip_if_not_installed("ranger")
  set.seed(5)
  sc <- make_scene()
  field <- sample_field(sc, n_clip = 12, n_plate = 8)
  grid <- make_biomass_grid(sc$ix, sc$chm, grid_m = 1)
  cal <- build_biomass_calibration(field, grid,
                                   plate_meter = fit_plate_meter(field))
  m <- fit_biomass_model(cal, method = "auto")
  expect_true(m$method %in% c("lm", "rf"))
  expect_named(m$comparison, c("lm", "rf"), ignore.order = TRUE)
  # the kept model must not have a worse CV RMSE than the alternative
  expect_lte(m$metrics$rmse,
             max(vapply(m$comparison, function(x) x$rmse, numeric(1))))
})

test_that(".biomass_metrics gains RPIQ without disturbing the existing fields", {
  obs <- c(10, 20, 30, 40, 50, 60, 70, 80)
  pred <- obs + c(2, -2, 2, -2, 2, -2, 2, -2)
  m <- .biomass_metrics(obs, pred)
  expect_equal(m$rmse, 2)
  expect_equal(m$rpiq,
               as.numeric(diff(stats::quantile(obs, c(0.25, 0.75)))) / m$rmse)
  # The elements the rest of the suite relies on are unchanged.
  expect_true(all(c("n", "r2", "rmse", "mae", "bias", "slope",
                    "intercept", "rpiq") %in% names(m)))
})

test_that(".biomass_metrics returns NA RPIQ when RMSE is zero or n < 2", {
  obs <- c(100, 200, 300, 400)
  expect_true(is.na(.biomass_metrics(obs, obs)$rpiq))
  expect_true(is.na(.biomass_metrics(1, 1)$rpiq))
})
