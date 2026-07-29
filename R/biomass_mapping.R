# =============================================================================
# Field-calibrated biomass mapping
#
# Turns georeferenced field samples + drone products (CHM + spectral indices)
# into a calibrated above-ground biomass map (kg/ha), following the two
# methodologies the workflow targets:
#
#   * Page et al. 2025 (Rangeland Ecology & Management) - low-altitude RTK
#     drone over rangeland; vegetation volume (CHM height x area) regressed
#     against a small set of clipped/double-sampled forage-mass points.
#   * Vahidi et al. 2023 (Remote Sensing) - per grid-cell predictor stack of
#     CHM statistics (mean/median/max/sd/var) + spectral values + a
#     categorical pasture label, fed to a (here optional) random forest.
#
# The bridge that makes a handful of clipped points usable is the rising
# plate / disc meter: many cheap compressed-height readings are converted to
# biomass through a small clip-anchored calibration, multiplying the
# effective calibration sample size before the drone model is fitted.
# =============================================================================

# ---- shared helpers ---------------------------------------------------------

# Coerce a field data frame (x/y or longitude/latitude) to an sf point layer
# in the CRS of `ref` (a SpatRaster or crs string). Mirrors the coordinate
# handling in extract_field_spectral_data() so both code paths agree.
.field_points_sf <- function(field_data, ref) {
  ref_crs <- if (inherits(ref, "SpatRaster")) terra::crs(ref) else ref
  if (all(c("x", "y") %in% names(field_data))) {
    pts <- sf::st_as_sf(field_data, coords = c("x", "y"),
                        crs = ref_crs, remove = FALSE)
  } else if (all(c("longitude", "latitude") %in% names(field_data))) {
    pts <- sf::st_as_sf(field_data, coords = c("longitude", "latitude"),
                        crs = 4326, remove = FALSE)
    if (nzchar(ref_crs)) pts <- sf::st_transform(pts, ref_crs)
  } else {
    stop("Field data needs x/y or longitude/latitude columns.", call. = FALSE)
  }
  pts
}

# Regression skill metrics used by both papers: variance explained (R2),
# RMSE / MAE / bias, plus the slope and intercept of the observed-vs-predicted
# 1:1 line that Vahidi et al. report.
#
# `r2` is 1 - SSres/SStot throughout DroneBioR. That is deliberately NOT
# caret's `Rsquared`, which is the squared Pearson correlation and ignores
# any bias or slope error in the predictions. Every number the Field Models
# tab displays comes from here so a single definition is on screen.
#
# `rpiq` (ratio of performance to interquartile distance, Bellon-Maurel et
# al. 2010) is the unitless companion to RMSE that agronomy reviewers ask
# for; it is undefined when the residuals or the observations are constant.
.biomass_metrics <- function(observed, predicted) {
  ok <- is.finite(observed) & is.finite(predicted)
  observed  <- observed[ok]
  predicted <- predicted[ok]
  n <- length(observed)
  if (n < 2L) {
    return(list(n = n, r2 = NA_real_, rmse = NA_real_, mae = NA_real_,
                bias = NA_real_, slope = NA_real_, intercept = NA_real_,
                rpiq = NA_real_))
  }
  resid  <- observed - predicted
  ss_res <- sum(resid^2)
  ss_tot <- sum((observed - mean(observed))^2)
  r2 <- if (ss_tot > 0) 1 - ss_res / ss_tot else NA_real_
  fit11 <- tryCatch(stats::lm(observed ~ predicted), error = function(e) NULL)
  rmse <- sqrt(mean(resid^2))
  iqr <- as.numeric(diff(stats::quantile(observed, c(0.25, 0.75), na.rm = TRUE)))
  list(
    n         = n,
    r2        = r2,
    rmse      = rmse,
    mae       = mean(abs(resid)),
    bias      = mean(predicted - observed),
    slope     = if (is.null(fit11)) NA_real_ else unname(stats::coef(fit11)[2]),
    intercept = if (is.null(fit11)) NA_real_ else unname(stats::coef(fit11)[1]),
    rpiq      = if (!is.finite(rmse) || rmse == 0 || !is.finite(iqr)) NA_real_ else iqr / rmse
  )
}

# Leave-one-out cross-validated predictions for a refit closure. `fit_fun`
# takes a training data frame and returns a model; `pred_fun` takes that model
# and a one-row data frame and returns a scalar prediction.
.loo_predictions <- function(data, fit_fun, pred_fun) {
  n <- nrow(data)
  out <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    model_i <- tryCatch(fit_fun(data[-i, , drop = FALSE]), error = function(e) NULL)
    if (is.null(model_i)) next
    out[i] <- tryCatch(
      as.numeric(pred_fun(model_i, data[i, , drop = FALSE])),
      error = function(e) NA_real_
    )
  }
  out
}

# ---- 1. rising plate / disc meter calibration -------------------------------

#' Calibrate a rising plate / disc meter against clipped biomass
#'
#' Fits the classic forage calibration `biomass = a + b * height` (optionally
#' with a pasture/species factor) from the small set of clipped quadrats that
#' carry both a compressed plate height and a measured dry-matter biomass.
#' The fitted object then predicts biomass for the many plate-only points,
#' multiplying the effective calibration sample before the drone model is
#' built - the double-sampling idea behind Page et al. (2025).
#'
#' @param data Data frame with a height column and a biomass column. Rows
#'   missing either are dropped from the fit.
#' @param height Name of the compressed-height column (e.g. `plate_height_cm`).
#' @param biomass Name of the dry-matter biomass column (`biomass_kgha`).
#' @param group Optional factor column (e.g. `pasture`) added as a fixed
#'   effect. `NULL` pools all points into one equation.
#' @param min_n Minimum number of complete clip points required.
#' @return An object of class `dronebio_plate_meter`: the `lm`, the coefficient
#'   table, leave-one-out CV metrics and the column names used.
#' @examples
#' set.seed(1)
#' h <- runif(12, 2, 18)
#' d <- data.frame(plate_height_cm = h,
#'                 biomass_kgha = 200 + 320 * h + rnorm(12, sd = 150))
#' cal <- fit_plate_meter(d)
#' cal$metrics$r2
#' @export
fit_plate_meter <- function(data,
                            height  = "plate_height_cm",
                            biomass = "biomass_kgha",
                            group   = NULL,
                            min_n   = 5L) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  need <- c(height, biomass, group)
  miss <- setdiff(need, names(data))
  if (length(miss) > 0) {
    stop("Plate-meter data is missing column(s): ",
         paste(miss, collapse = ", "), call. = FALSE)
  }

  df <- data[, need, drop = FALSE]
  df <- df[is.finite(df[[height]]) & is.finite(df[[biomass]]), , drop = FALSE]
  if (nrow(df) < min_n) {
    stop(sprintf("Only %d clip points carry both height and biomass; need >= %d.",
                 nrow(df), min_n), call. = FALSE)
  }

  use_group <- !is.null(group) && length(unique(df[[group]])) > 1L
  if (use_group) df[[group]] <- factor(df[[group]])
  rhs <- if (use_group) paste(height, "+", group) else height
  form <- stats::as.formula(paste(biomass, "~", rhs))
  model <- stats::lm(form, data = df)

  loo <- .loo_predictions(
    df,
    fit_fun  = function(d) stats::lm(form, data = d),
    pred_fun = function(m, d) stats::predict(m, newdata = d)
  )
  metrics <- .biomass_metrics(df[[biomass]], loo)

  structure(
    list(
      model    = model,
      formula  = form,
      height   = height,
      biomass  = biomass,
      group    = if (use_group) group else NULL,
      n        = nrow(df),
      coef     = stats::coef(model),
      metrics  = metrics
    ),
    class = "dronebio_plate_meter"
  )
}

#' Predict biomass from plate-meter heights
#'
#' @param object A `dronebio_plate_meter` from [fit_plate_meter()].
#' @param newdata Data frame containing the height (and group) column(s).
#' @param ... Unused.
#' @return Numeric vector of predicted biomass (kg/ha), clamped at 0.
#' @export
predict.dronebio_plate_meter <- function(object, newdata, ...) {
  if (!is.null(object$group) && object$group %in% names(newdata)) {
    newdata[[object$group]] <- factor(newdata[[object$group]],
                                      levels = object$model$xlevels[[object$group]])
  }
  pred <- as.numeric(stats::predict(object$model, newdata = newdata))
  pmax(pred, 0)
}

#' @export
print.dronebio_plate_meter <- function(x, ...) {
  cat("DroneBioR plate-meter calibration\n")
  b <- x$coef
  if (length(b) >= 2L) {
    cat(sprintf("  biomass_kgha = %.1f + %.1f * %s%s\n",
                b[[1]], b[[2]], x$height,
                if (!is.null(x$group)) " (+ pasture effects)" else ""))
  }
  cat(sprintf("  n clips = %d | LOO R2 = %.2f | RMSE = %.0f kg/ha\n",
              x$n, x$metrics$r2, x$metrics$rmse))
  invisible(x)
}

# ---- 2. management grid of predictors ---------------------------------------

#' Build a grid of biomass predictors from indices and a CHM
#'
#' Aggregates a fine-resolution spectral index stack and Canopy Height Model
#' onto a coarser management grid (default 1 m, matching the quadrat / grid
#' size used by Page et al. 2025 and Vahidi et al. 2023). Per grid cell it
#' returns the spectral index means plus the structural statistics both papers
#' rely on - CHM mean, median, max, standard deviation and variance - and the
#' Page vegetation volume (mean height x cell area).
#'
#' Calibration extraction and wall-to-wall prediction both read from this same
#' stack, so the features a model is trained on are defined identically to the
#' features it is mapped over.
#'
#' @param indices Spectral index `SpatRaster` from [compute_spectral_indices()].
#' @param chm Optional CHM `SpatRaster` (m above ground). Resampled onto the
#'   index grid when geometries differ. When `NULL` only spectral means are
#'   returned.
#' @param grid_m Target grid-cell size in metres. Use `NA` or a size at/below
#'   the native resolution to skip aggregation.
#' @param indices_keep Optional character vector restricting which index layers
#'   are aggregated (default: all).
#' @return A `SpatRaster` of per-cell predictors.
#' @examples
#' \dontrun{
#'   refl <- scale_to_reflectance(read_multispectral_orthomosaic(path)$bands)
#'   ix   <- compute_spectral_indices(refl)
#'   chm  <- terra::rast("chm.tif")
#'   grid <- make_biomass_grid(ix, chm, grid_m = 1)
#'   names(grid)
#' }
#' @export
make_biomass_grid <- function(indices, chm = NULL, grid_m = 1,
                              indices_keep = NULL) {
  if (!inherits(indices, "SpatRaster")) {
    stop("`indices` must be a terra SpatRaster.", call. = FALSE)
  }
  if (!is.null(indices_keep)) {
    keep <- intersect(indices_keep, names(indices))
    if (length(keep) == 0) {
      stop("None of `indices_keep` are layers of `indices`.", call. = FALSE)
    }
    indices <- indices[[keep]]
  }

  # Align CHM to the index grid before aggregating so the two stacks share
  # geometry (the ortho is often cropped a few pixels tighter than the DSM).
  if (!is.null(chm)) {
    chm <- chm[[1]]
    if (!terra::compareGeom(indices[[1]], chm, stopOnError = FALSE,
                            lyrs = FALSE, messages = FALSE)) {
      chm <- terra::resample(chm, indices[[1]], method = "bilinear")
    }
    chm <- terra::clamp(chm, lower = 0, upper = Inf, values = TRUE)
    names(chm) <- "chm"
  }

  # Translate the target cell size into an integer aggregation factor.
  res_xy <- terra::res(indices)
  fact <- if (is.null(grid_m) || is.na(grid_m)) 1L else
    as.integer(round(grid_m / mean(res_xy)))
  fact <- max(fact, 1L)

  agg <- function(r, fun) {
    if (fact <= 1L) return(r)
    terra::aggregate(r, fact = fact, fun = fun, na.rm = TRUE)
  }

  index_means <- agg(indices, "mean")

  if (is.null(chm)) {
    return(index_means)
  }

  chm_mean <- agg(chm, "mean");   names(chm_mean) <- "chm_mean"
  chm_max  <- agg(chm, "max");    names(chm_max)  <- "chm_max"
  chm_sd   <- agg(chm, "sd");     names(chm_sd)   <- "chm_sd"
  chm_sd   <- terra::ifel(is.na(chm_sd), 0, chm_sd)  # single-cell aggregates -> 0
  chm_var  <- chm_sd^2;           names(chm_var)  <- "chm_var"
  chm_med  <- tryCatch({
    m <- agg(chm, function(v, na.rm = TRUE) stats::median(v, na.rm = na.rm))
    names(m) <- "chm_median"; m
  }, error = function(e) {
    m <- chm_mean; names(m) <- "chm_median"; m
  })

  # Page vegetation volume per grid cell = mean canopy height x cell area.
  cell_area <- prod(terra::res(chm_mean))
  chm_vol <- chm_mean * cell_area
  names(chm_vol) <- "chm_volume_m3"

  c(index_means, chm_mean, chm_med, chm_max, chm_sd, chm_var, chm_vol)
}

# ---- 3. calibration table ---------------------------------------------------

#' Assemble the biomass calibration table
#'
#' Reads georeferenced field points, optionally fills the biomass of
#' plate-only points from a [fit_plate_meter()] calibration, and joins each
#' point to the predictor grid cell it falls in (or the mean of a buffer
#' around it). The result is one tidy table ready for [fit_biomass_model()].
#'
#' @param field Field data frame or path to a CSV understood by
#'   [read_field_data()].
#' @param grid Predictor `SpatRaster` from [make_biomass_grid()].
#' @param plate_meter Optional `dronebio_plate_meter`; when supplied, rows with
#'   a finite height but missing biomass are filled with its prediction.
#' @param biomass,height,group,id Column names in `field`.
#' @param buffer_m Optional radius (m). When set, predictors are the mean over
#'   a circular buffer instead of the single containing cell - useful when GPS
#'   error spans more than one grid cell.
#' @return A data frame with the field columns, a `biomass_source` flag
#'   (`measured` / `plate_modeled`) and one column per predictor.
#' @export
build_biomass_calibration <- function(field, grid,
                                      plate_meter = NULL,
                                      biomass  = "biomass_kgha",
                                      height   = "plate_height_cm",
                                      group    = "pasture",
                                      id       = "sample_id",
                                      buffer_m = NULL) {
  if (is.character(field) && length(field) == 1L) field <- read_field_data(field)
  if (!is.data.frame(field)) stop("`field` must be a data frame or CSV path.", call. = FALSE)
  if (!inherits(grid, "SpatRaster")) stop("`grid` must be a terra SpatRaster.", call. = FALSE)
  if (!biomass %in% names(field)) {
    stop("Field data has no `", biomass, "` column.", call. = FALSE)
  }

  field <- as.data.frame(field, stringsAsFactors = FALSE)

  # 1. Fill plate-only biomass from the plate-meter calibration.
  field$biomass_source <- ifelse(is.finite(field[[biomass]]), "measured", NA_character_)
  if (!is.null(plate_meter)) {
    fill <- !is.finite(field[[biomass]]) &
      height %in% names(field) & is.finite(field[[height]])
    if (any(fill)) {
      field[[biomass]][fill] <- stats::predict(plate_meter, field[fill, , drop = FALSE])
      field$biomass_source[fill] <- "plate_modeled"
    }
  }

  # 2. Extract predictors at each point (single cell or buffer mean).
  pts <- .field_points_sf(field, grid)
  vect_pts <- terra::vect(pts)
  if (is.null(buffer_m)) {
    vals <- terra::extract(grid, vect_pts, ID = FALSE)
  } else {
    buf <- terra::buffer(vect_pts, width = buffer_m)
    vals <- terra::extract(grid, buf, fun = mean, na.rm = TRUE, ID = FALSE)
  }

  out <- data.frame(sf::st_drop_geometry(pts), vals, check.names = FALSE)
  # Drop rows with no usable biomass for fitting (keep all for inspection by
  # the caller via attributes).
  attr(out, "predictor_names") <- names(grid)
  out
}

# ---- 4. staged biomass model ------------------------------------------------

# Predictor defaults per methodology. Page (2025) favours a parsimonious
# structural + greenness model; Vahidi (2023) feeds a random forest the full
# CHM statistic set plus several spectral indices. When neither set is present
# (e.g. no CHM) both fall back to whatever indices exist.
.biomass_predictors <- function(data, route = c("page", "vahidi")) {
  route <- match.arg(route)
  wanted <- switch(
    route,
    page   = c("chm_volume_m3", "chm_mean", "NDVI", "NDRE"),
    vahidi = c("chm_mean", "chm_median", "chm_max", "chm_sd", "chm_var",
               "NDVI", "NDRE", "SAVI", "GNDVI")
  )
  cols <- intersect(wanted, names(data))
  if (length(cols) == 0) {
    cols <- intersect(c("NDVI", "NDRE", "EVI", "SAVI", "NDWI", "NIR", "RedEdge"),
                      names(data))
  }
  cols
}

#' Fit a field-calibrated biomass model (staged LM / random forest)
#'
#' Stage 1 fits the defensible pooled linear model (Page et al. 2025:
#' vegetation volume / canopy height + greenness). Stage 2 optionally fits a
#' random forest (Vahidi et al. 2023: CHM statistics + spectral values + a
#' categorical pasture label) when the `ranger` package is available. With
#' `method = "auto"` both are fitted and the one with the lower leave-one-out
#' RMSE is returned as the primary model.
#'
#' @param data Calibration data frame from [build_biomass_calibration()].
#' @param response Biomass column (kg/ha).
#' @param predictors Optional predictor override applied to both routes. When
#'   `NULL`, each route uses its paper's default set present in `data` (the
#'   parsimonious Page set for the LM, the richer Vahidi CHM-statistics + index
#'   set for the RF).
#' @param method `"lm"`, `"rf"`, or `"auto"` (compare both).
#' @param group Optional categorical column (e.g. `pasture`) added to the RF.
#' @param num_trees Random-forest tree count when `ranger` is used.
#' @return An object of class `dronebio_biomass_model` with the chosen `fit`,
#'   its `method`, the predictor names, LOO-CV `metrics`, and (for `auto`) the
#'   per-method comparison.
#' @examples
#' set.seed(1)
#' v <- runif(30, 0, 0.5); g <- runif(30, 0.3, 0.9)
#' d <- data.frame(biomass_kgha = 500 + 4000 * v + 1500 * g + rnorm(30, sd = 120),
#'                 chm_volume_m3 = v, NDVI = g)
#' m <- fit_biomass_model(d, predictors = c("chm_volume_m3", "NDVI"))
#' m$metrics$r2
#' @export
fit_biomass_model <- function(data,
                              response   = "biomass_kgha",
                              predictors = NULL,
                              method     = c("lm", "rf", "auto"),
                              group      = "pasture",
                              num_trees  = 500L) {
  method <- match.arg(method)
  if (!response %in% names(data)) {
    stop("`data` has no `", response, "` column.", call. = FALSE)
  }
  ranger_ok <- requireNamespace("ranger", quietly = TRUE)
  has_group <- !is.null(group) && group %in% names(data) &&
    length(unique(stats::na.omit(data[[group]]))) > 1L

  results <- list()

  # ---- Stage 1: linear model (Page: volume / height + greenness) ----
  if (method %in% c("lm", "auto")) {
    lp <- if (!is.null(predictors)) predictors else .biomass_predictors(data, "page")
    lp <- intersect(lp, names(data))
    if (length(lp) == 0 && method == "lm") {
      stop("No usable predictor columns for the linear model.", call. = FALSE)
    }
    if (length(lp) > 0) {
      md <- data[stats::complete.cases(data[, c(response, lp), drop = FALSE]),
                 c(response, lp), drop = FALSE]
      if (nrow(md) >= length(lp) + 2L) {
        form <- stats::as.formula(paste(response, "~", paste(lp, collapse = " + ")))
        fit_lm <- function(d) stats::lm(form, data = d)
        lm_fit <- fit_lm(md)
        lm_loo <- .loo_predictions(md, fit_lm,
                                   function(m, d) stats::predict(m, newdata = d))
        results$lm <- list(fit = lm_fit, method = "lm", predictors = lp,
                           group = NULL,
                           metrics = .biomass_metrics(md[[response]], lm_loo),
                           training = md)
      } else if (method == "lm") {
        stop(sprintf("Only %d complete rows for %d linear-model predictors.",
                     nrow(md), length(lp)), call. = FALSE)
      }
    }
  }

  # ---- Stage 2: random forest (Vahidi: CHM stats + indices + pasture) ----
  if (method %in% c("rf", "auto")) {
    if (!ranger_ok) {
      if (method == "rf") {
        stop("method = 'rf' needs the 'ranger' package. Install it or use 'lm'.",
             call. = FALSE)
      }
    } else {
      rp <- if (!is.null(predictors)) predictors else .biomass_predictors(data, "vahidi")
      rp <- intersect(rp, names(data))
      if (length(rp) == 0 && method == "rf") {
        stop("No usable predictor columns for the random forest.", call. = FALSE)
      }
      if (length(rp) > 0) {
        cols <- c(response, rp, if (has_group) group)
        md <- data[stats::complete.cases(data[, c(response, rp), drop = FALSE]),
                   cols, drop = FALSE]
        if (has_group) md[[group]] <- factor(md[[group]])
        rf_terms <- if (has_group) c(rp, group) else rp
        if (nrow(md) >= length(rp) + 2L) {
          form <- stats::as.formula(paste(response, "~", paste(rf_terms, collapse = " + ")))
          fit_rf <- function(d) ranger::ranger(form, data = d,
                                               num.trees = num_trees, seed = 42L)
          rf_fit <- fit_rf(md)
          rf_loo <- .loo_predictions(
            md, fit_rf,
            function(m, d) stats::predict(m, data = d)$predictions
          )
          results$rf <- list(fit = rf_fit, method = "rf", predictors = rf_terms,
                             group = if (has_group) group else NULL,
                             metrics = .biomass_metrics(md[[response]], rf_loo),
                             training = md)
        } else if (method == "rf") {
          stop(sprintf("Only %d complete rows for the random forest.", nrow(md)),
               call. = FALSE)
        }
      }
    }
  }

  if (length(results) == 0) {
    stop("No biomass model could be fitted from the calibration data.", call. = FALSE)
  }

  chosen <- if (method == "auto") {
    rmses <- vapply(results, function(r) r$metrics$rmse %||% Inf, numeric(1))
    names(which.min(rmses))
  } else if (!is.null(results[[method]])) {
    method
  } else {
    names(results)[1]
  }
  primary <- results[[chosen]]

  structure(
    list(
      fit        = primary$fit,
      method     = primary$method,
      response   = response,
      predictors = primary$predictors,
      group      = primary$group,
      n          = nrow(primary$training),
      metrics    = primary$metrics,
      comparison = lapply(results, function(r) r$metrics),
      training   = primary$training
    ),
    class = "dronebio_biomass_model"
  )
}

#' @export
print.dronebio_biomass_model <- function(x, ...) {
  cat("DroneBioR biomass model\n")
  cat(sprintf("  method     : %s\n", x$method))
  cat(sprintf("  predictors : %s\n", paste(x$predictors, collapse = ", ")))
  cat(sprintf("  n          : %d calibration points\n", x$n))
  m <- x$metrics
  cat(sprintf("  LOO-CV     : R2 = %.2f | RMSE = %.0f | MAE = %.0f kg/ha\n",
              m$r2, m$rmse, m$mae))
  cat(sprintf("  1:1 line   : slope = %.2f | intercept = %.0f\n",
              m$slope, m$intercept))
  if (length(x$comparison) > 1L) {
    cat("  comparison :\n")
    for (nm in names(x$comparison)) {
      cm <- x$comparison[[nm]]
      cat(sprintf("    %-3s R2 = %.2f | RMSE = %.0f kg/ha\n",
                  nm, cm$r2, cm$rmse))
    }
  }
  invisible(x)
}

# ---- 5. predict a wall-to-wall biomass map ----------------------------------

#' Predict a biomass map from a fitted model
#'
#' Applies a [fit_biomass_model()] result across the predictor grid to produce
#' a per-cell above-ground biomass raster (kg/ha), clamped at `min_biomass`.
#'
#' @param model A `dronebio_biomass_model`.
#' @param grid Predictor `SpatRaster` from [make_biomass_grid()] - must contain
#'   the model's predictor layers.
#' @param pasture Optional single pasture label to assign to every cell when
#'   the model used a categorical pasture term (a map covers one pasture). Must
#'   be one of the training levels.
#' @param out_path Optional GeoTIFF path to write.
#' @param min_biomass Lower clamp (kg/ha).
#' @return The biomass `SpatRaster` (named `biomass_kgha`).
#' @export
predict_biomass_map <- function(model, grid, pasture = NULL,
                                out_path = NULL, min_biomass = 0) {
  if (!inherits(model, "dronebio_biomass_model")) {
    stop("`model` must be a dronebio_biomass_model.", call. = FALSE)
  }
  if (!inherits(grid, "SpatRaster")) {
    stop("`grid` must be a terra SpatRaster.", call. = FALSE)
  }
  cont_preds <- setdiff(model$predictors, model$group)
  miss <- setdiff(cont_preds, names(grid))
  if (length(miss) > 0) {
    stop("Grid is missing predictor layer(s): ",
         paste(miss, collapse = ", "), call. = FALSE)
  }
  stack <- grid[[cont_preds]]

  if (model$method == "lm") {
    # terra::predict matches lm terms to layer names; a constant factor layer
    # is not needed because the pooled LM has no categorical term.
    map <- terra::predict(stack, model$fit, na.rm = TRUE)
  } else {
    # ranger: predict cell-wise, injecting the constant pasture level.
    grp <- model$group
    if (!is.null(grp) && is.null(pasture)) {
      lv <- levels(model$training[[grp]])
      if (length(lv) == 1L) pasture <- lv
    }
    fun <- function(mod, data, ...) {
      if (!is.null(grp)) data[[grp]] <- factor(pasture,
                                               levels = levels(model$training[[grp]]))
      p <- stats::predict(mod, data = data)$predictions
      p
    }
    map <- terra::predict(stack, model$fit, fun = fun, na.rm = TRUE)
  }

  map <- terra::clamp(map, lower = min_biomass, upper = Inf, values = TRUE)
  names(map) <- "biomass_kgha"
  if (!is.null(out_path)) {
    terra::writeRaster(map, out_path, overwrite = TRUE)
  }
  map
}

#' Run the field-calibrated biomass mapping workflow
#'
#' End-to-end driver: read field samples, calibrate the plate meter, build the
#' predictor grid, assemble the calibration table, fit the staged model and
#' write a biomass map. Returns every intermediate so a script or the Shiny
#' studio can report on them.
#'
#' @param field Field data frame or CSV path (see [read_field_data()]). Needs
#'   `biomass_kgha` on the clip rows; `plate_height_cm` on all rows enables the
#'   plate-meter step; a `pasture` column enables the categorical RF term.
#' @param indices Spectral index `SpatRaster` from [compute_spectral_indices()].
#' @param chm Optional CHM `SpatRaster`.
#' @param pasture Optional pasture label for the mapped raster (defaults to the
#'   single pasture present in `field`).
#' @param out_path Optional GeoTIFF path for the biomass map.
#' @param grid_m Management grid-cell size (m).
#' @param method Model route: `"lm"`, `"rf"`, or `"auto"`.
#' @param predictors Optional predictor override.
#' @param buffer_m Optional buffer radius for calibration extraction.
#' @param min_biomass Lower clamp (kg/ha) for the map.
#' @return A list with `plate_meter`, `grid`, `calibration`, `model`, `map`,
#'   `metrics` and `map_path`.
#' @examples
#' \dontrun{
#'   res <- run_biomass_mapping(
#'     field   = "field_biomass_plate.csv",
#'     indices = ix, chm = chm,
#'     out_path = "biomass_kgha.tif", method = "auto"
#'   )
#'   res$model
#' }
#' @export
run_biomass_mapping <- function(field, indices, chm = NULL,
                                pasture    = NULL,
                                out_path   = NULL,
                                grid_m     = 1,
                                method     = c("auto", "lm", "rf"),
                                predictors = NULL,
                                buffer_m   = NULL,
                                min_biomass = 0) {
  method <- match.arg(method)
  if (is.character(field) && length(field) == 1L) field <- read_field_data(field)
  field <- as.data.frame(field, stringsAsFactors = FALSE)

  # Plate-meter calibration (only when heights exist and some biomass is
  # missing, i.e. there genuinely are plate-only points to fill).
  plate <- NULL
  if ("plate_height_cm" %in% names(field) &&
      any(is.finite(field$plate_height_cm) & !is.finite(field$biomass_kgha))) {
    plate <- tryCatch(
      fit_plate_meter(field,
                      group = if ("pasture" %in% names(field)) "pasture" else NULL),
      error = function(e) {
        message("Plate-meter calibration skipped: ", conditionMessage(e))
        NULL
      }
    )
  }

  grid <- make_biomass_grid(indices, chm, grid_m = grid_m)
  cal  <- build_biomass_calibration(field, grid, plate_meter = plate,
                                    buffer_m = buffer_m)
  model <- fit_biomass_model(cal, predictors = predictors, method = method)

  if (is.null(pasture) && "pasture" %in% names(field)) {
    pp <- unique(stats::na.omit(field$pasture))
    if (length(pp) == 1L) pasture <- pp
  }
  map <- predict_biomass_map(model, grid, pasture = pasture,
                             out_path = out_path, min_biomass = min_biomass)

  list(
    plate_meter = plate,
    grid        = grid,
    calibration = cal,
    model       = model,
    map         = map,
    metrics     = model$metrics,
    map_path    = out_path
  )
}
