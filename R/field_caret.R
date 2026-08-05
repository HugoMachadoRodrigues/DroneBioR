# =============================================================================
# caret-backed field biomass models
#
# Every caret touchpoint sits behind requireNamespace("caret") so the Field
# Models tab degrades to ingest + extraction + fit_biomass_lm() when caret is
# not installed. Model backend packages (ranger, xgboost, ...) are referenced
# only as strings taken from caret's own model metadata, never imported.
#
# Two rules that this file exists to enforce:
#
#   * caret_model_available() gates every caret::train() call. caret's
#     internal checkInstall() calls menu() when interactive() is TRUE, which
#     blocks the R process on stdin and hangs a Shiny session with no error.
#   * every displayed metric comes from .biomass_metrics() (r2 =
#     1 - SSres/SStot). caret's `Rsquared` is squared Pearson and ranks
#     hyperparameters only; it is labelled as such wherever it is shown.
#
# The trainControl deviates from caret's defaults in exactly four ways -
# method = "cv" with `number` folds instead of 25x bootstrap, saved
# out-of-fold predictions, an added RPIQ metric, and no parallel backend -
# and the UI states all four.
# =============================================================================

.caret_missing_msg <- paste(
  "The 'caret' package is required for field model training.",
  "Install it with install.packages(\"caret\")."
)

.require_caret <- function() {
  if (!requireNamespace("caret", quietly = TRUE)) {
    stop(.caret_missing_msg, call. = FALSE)
  }
  invisible(TRUE)
}

# Neural nets on a raw kg/ha response return RMSE in the thousands and
# Rsquared = NaN, which looks like a broken model rather than a scaling
# problem. Flag them so the caller can force centre + scale.
.needs_scaling_tags <- "Neural Network"
.needs_scaling_methods <- c("nnet", "pcaNNet", "avNNet", "neuralnet", "mlp",
                            "mlpWeightDecay", "brnn", "monmlp")

.caret_catalogue_columns <- c("method", "label", "packages", "missing",
                              "ready", "tags", "n_params", "needs_scaling")

.empty_caret_catalogue <- function() {
  data.frame(
    method = character(), label = character(), packages = character(),
    missing = character(), ready = logical(), tags = character(),
    n_params = integer(), needs_scaling = logical(),
    stringsAsFactors = FALSE
  )
}

#' Catalogue caret models of a given type
#'
#' All 137 regression-capable caret models are offered; the remaining 102 are
#' classification-only and cannot fit a continuous biomass response. `ready`
#' says whether every backend package the model needs is already installed,
#' which is what the picker groups on.
#'
#' @param type caret model type, normally `"Regression"`.
#' @param installed Character vector of installed package names.
#' @return A data frame with `method`, `label`, `packages`, `missing`,
#'   `ready`, `tags`, `n_params` and `needs_scaling`. Zero rows (with a
#'   warning) when caret is not installed.
#' @examples
#' if (requireNamespace("caret", quietly = TRUE)) {
#'   cat <- caret_model_catalogue()
#'   nrow(cat)
#'   head(cat[cat$ready, c("method", "label")])
#' }
#' @export
caret_model_catalogue <- function(type = "Regression",
                                  installed = rownames(utils::installed.packages())) {
  if (!requireNamespace("caret", quietly = TRUE)) {
    warning(.caret_missing_msg, call. = FALSE)
    return(.empty_caret_catalogue())
  }
  info <- caret::getModelInfo()
  keep <- vapply(info, function(m) type %in% m$type, logical(1))
  info <- info[keep]
  if (length(info) == 0L) return(.empty_caret_catalogue())

  rows <- lapply(names(info), function(method) {
    m <- info[[method]]
    libs <- if (is.null(m$library)) character(0) else as.character(m$library)
    miss <- setdiff(libs, installed)
    tags <- if (is.null(m$tags)) character(0) else as.character(m$tags)
    data.frame(
      method        = method,
      label         = if (is.null(m$label)) method else as.character(m$label)[[1L]],
      packages      = paste(libs, collapse = ", "),
      missing       = paste(miss, collapse = ", "),
      ready         = length(miss) == 0L,
      tags          = paste(tags, collapse = ", "),
      n_params      = if (is.null(m$parameters)) 0L else nrow(m$parameters),
      needs_scaling = method %in% .needs_scaling_methods ||
        any(grepl(.needs_scaling_tags, tags, fixed = TRUE)),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out <- out[order(out$method), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Check whether a caret model can be trained here
#'
#' Must be called before every [caret::train()] call. caret's own install
#' check opens an interactive `menu()` prompt when a backend is missing,
#' which hangs a Shiny session started from an interactive R process.
#'
#' @param method caret method id.
#' @param installed Character vector of installed package names.
#' @return A list with `ok`, `missing` (packages) and `install_call` (a
#'   ready-to-paste `install.packages()` line, `NA` when nothing is missing).
#' @examples
#' if (requireNamespace("caret", quietly = TRUE)) {
#'   caret_model_available("lm")$ok
#' }
#' @export
caret_model_available <- function(method,
                                  installed = rownames(utils::installed.packages())) {
  if (!requireNamespace("caret", quietly = TRUE)) {
    return(list(ok = FALSE, missing = "caret",
                install_call = 'install.packages(c("caret"))'))
  }
  method <- as.character(method)[[1L]]
  # regex = FALSE: getModelInfo() matches by regular expression by default,
  # so getModelInfo("lm") would return 20-odd models.
  info <- tryCatch(caret::getModelInfo(method, regex = FALSE),
                   error = function(e) list())
  # An unknown id comes back as a length-1 list holding NULL, not as list().
  if (length(info) == 0L || is.null(info[[1L]])) {
    stop("Unknown caret method: ", method, call. = FALSE)
  }
  libs <- info[[1L]]$library
  libs <- if (is.null(libs)) character(0) else as.character(libs)
  miss <- setdiff(libs, installed)
  list(
    ok      = length(miss) == 0L,
    missing = miss,
    install_call = if (length(miss) == 0L) {
      NA_character_
    } else {
      sprintf("install.packages(c(%s))",
              paste(sprintf('"%s"', miss), collapse = ", "))
    }
  )
}

#' Build the train / test partition and CV folds for a model sweep
#'
#' Computed **once per sweep**, outside the model loop, and passed into every
#' [fit_field_caret_model()] call. Sharing one fold assignment is what makes
#' the leaderboard a fair comparison and the whole sweep reproducible from
#' the seed shown in the sidebar.
#'
#' Samples drawn from a single flight are spatially autocorrelated, so a random
#' partition can place neighbouring quadrats on both sides of the split and
#' return an optimistic score. Set `blocking = "spatial"` to lay a regular grid
#' over the sample coordinates and keep whole blocks together: the hold-out
#' withholds entire blocks, and the cross-validation folds are grouped by block
#' as well, so no block is ever split between fitting and scoring.
#'
#' @param data Extraction table with the response column.
#' @param response Response column name.
#' @param holdout Fraction held out as an independent test set (0 to 0.5).
#'   `0` disables the holdout.
#' @param folds Number of cross-validation folds (requirement: k = 10).
#' @param seed Random seed.
#' @param blocking How the partition and the folds are formed. `"random"`
#'   (default) reproduces the previous behaviour. `"spatial"` groups samples
#'   into square blocks and assigns whole blocks, which is the appropriate
#'   choice when the samples come from one flight.
#' @param coords Length-2 character vector naming the coordinate columns used
#'   for spatial blocking. Ignored when `blocking = "random"`.
#' @param block_size Side length of a block, in the units of `coords`. When
#'   `NULL` (default) a size is chosen so that roughly `5 * folds` blocks are
#'   occupied, which keeps the folds balanced without making blocks so small
#'   that neighbouring samples still straddle them.
#' @return A list with `train_idx`, `test_idx`, `folds` (in-fold row
#'   positions **within the training block**), `folds_out`, `holdout`,
#'   `folds_k` and `seed`. When `blocking = "spatial"` it also carries
#'   `blocking`, `block_id` (one block label per row of `data`), `block_size`
#'   and `n_blocks`.
#' @examples
#' if (requireNamespace("caret", quietly = TRUE)) {
#'   d <- data.frame(biomass_kgha = runif(60, 500, 4000))
#'   split <- field_train_split(d, holdout = 0.25, folds = 10)
#'   length(split$train_idx)
#'
#'   # spatially blocked: neighbouring samples stay on the same side
#'   d$x <- runif(60, 0, 100)
#'   d$y <- runif(60, 0, 100)
#'   sp <- field_train_split(d, holdout = 0.25, folds = 5,
#'                           blocking = "spatial")
#'   sp$n_blocks
#' }
#' @export
field_train_split <- function(data, response = "biomass_kgha", holdout = 0.25,
                              folds = 10L, seed = 42L,
                              blocking = c("random", "spatial"),
                              coords = c("x", "y"), block_size = NULL) {
  .require_caret()
  if (!response %in% names(data)) {
    stop("Response column '", response, "' is not in the extraction table.",
         call. = FALSE)
  }
  y <- as.numeric(data[[response]])
  n <- length(y)
  if (any(!is.finite(y))) {
    stop("The response '", response, "' has ", sum(!is.finite(y)),
         " missing value(s); drop those samples before splitting so every ",
         "model in the sweep sees the same rows.", call. = FALSE)
  }
  holdout <- as.numeric(holdout)
  if (!is.finite(holdout) || holdout < 0 || holdout > 0.5) {
    stop("`holdout` must be between 0 and 0.5.", call. = FALSE)
  }
  folds_k <- as.integer(folds)
  if (folds_k < 2L) stop("`folds` must be at least 2.", call. = FALSE)
  blocking <- match.arg(blocking)

  block_id <- NULL
  if (identical(blocking, "spatial")) {
    block <- .spatial_blocks(data, coords, folds_k, block_size)
    block_id   <- block$id
    block_size <- block$size
  }

  set.seed(seed)
  if (is.null(block_id)) {
    train_idx <- if (holdout > 0) {
      as.integer(caret::createDataPartition(y, p = 1 - holdout, list = FALSE)[, 1L])
    } else {
      seq_len(n)
    }
  } else {
    train_idx <- .block_holdout(block_id, holdout)
  }
  test_idx <- setdiff(seq_len(n), train_idx)

  folds_k <- min(folds_k, length(train_idx))
  if (is.null(block_id)) {
    in_fold <- caret::createFolds(y[train_idx], k = folds_k, returnTrain = TRUE)
  } else {
    g <- block_id[train_idx]
    folds_k <- min(folds_k, length(unique(g)))
    if (folds_k < 2L) {
      stop("Spatial blocking left only ", length(unique(g)), " block(s) in the ",
           "training set, so cross-validation folds cannot be formed. Use a ",
           "smaller `block_size`, a smaller `holdout`, or blocking = \"random\".",
           call. = FALSE)
    }
    in_fold <- caret::groupKFold(g, k = folds_k)
  }
  out_fold <- lapply(in_fold, function(i) setdiff(seq_along(train_idx), i))

  out <- list(
    train_idx = train_idx,
    test_idx  = as.integer(test_idx),
    folds     = in_fold,
    folds_out = out_fold,
    holdout   = holdout,
    folds_k   = folds_k,
    seed      = as.integer(seed)
  )
  if (!is.null(block_id)) {
    out$blocking   <- "spatial"
    out$block_id   <- block_id
    out$block_size <- block_size
    out$n_blocks   <- length(unique(block_id))
  } else {
    out$blocking <- "random"
  }
  out
}

# Lay a regular square grid over the sample coordinates and label each row with
# the block it falls in. `block_size` defaults to whatever makes roughly
# 5 * folds blocks occupied: enough blocks to fill the folds, large enough that
# neighbouring samples are not routinely split across a boundary.
#' @noRd
.spatial_blocks <- function(data, coords, folds_k, block_size = NULL) {
  if (length(coords) != 2L) {
    stop("`coords` must name exactly two columns.", call. = FALSE)
  }
  miss <- setdiff(coords, names(data))
  if (length(miss)) {
    stop("Spatial blocking needs the coordinate column(s) ",
         paste(sprintf("'%s'", miss), collapse = " and "),
         ", which are not in the extraction table. Extraction tables built by ",
         "extract_field_covariates() carry 'x' and 'y'; pass `coords` if yours ",
         "are named differently.", call. = FALSE)
  }
  xy <- data.frame(x = as.numeric(data[[coords[[1L]]]]),
                   y = as.numeric(data[[coords[[2L]]]]))
  if (any(!is.finite(xy$x)) || any(!is.finite(xy$y))) {
    stop("The coordinate columns hold ",
         sum(!is.finite(xy$x) | !is.finite(xy$y)),
         " non-finite value(s); spatial blocking needs a position for every ",
         "sample.", call. = FALSE)
  }
  span <- max(diff(range(xy$x)), diff(range(xy$y)))
  if (!is.finite(span) || span <= 0) {
    stop("All samples share one position, so they cannot be blocked spatially.",
         call. = FALSE)
  }
  if (is.null(block_size)) {
    # a k x k grid gives k^2 blocks; solve k^2 ~= 5 * folds_k
    k <- max(2L, ceiling(sqrt(5 * folds_k)))
    block_size <- span / k
  }
  block_size <- as.numeric(block_size)
  if (!is.finite(block_size) || block_size <= 0) {
    stop("`block_size` must be a positive number.", call. = FALSE)
  }
  ix <- floor((xy$x - min(xy$x)) / block_size)
  iy <- floor((xy$y - min(xy$y)) / block_size)
  list(id = paste(ix, iy, sep = "_"), size = block_size)
}

# Withhold whole blocks until the held-out share is as close to `holdout` as a
# whole number of blocks allows. Blocks are visited in random order, and the
# loop stops before it would empty the training set.
#' @noRd
.block_holdout <- function(block_id, holdout) {
  n <- length(block_id)
  if (holdout <= 0) return(seq_len(n))
  blocks <- unique(block_id)
  if (length(blocks) < 2L) {
    stop("Spatial blocking produced a single block, so nothing can be held ",
         "out. Use a smaller `block_size`.", call. = FALSE)
  }
  target <- holdout * n
  ord    <- sample(blocks)
  held   <- character(0)
  size   <- 0L
  for (b in ord) {
    nb <- sum(block_id == b)
    if (size > 0 && abs(size + nb - target) > abs(size - target)) break
    if (size + nb >= n) break
    held <- c(held, b)
    size <- size + nb
    if (size >= target) break
  }
  train <- which(!block_id %in% held)
  if (!length(train) || length(train) == n) {
    stop("Spatial blocking could not form a hold-out at holdout = ", holdout,
         ". Use a smaller `block_size` or blocking = \"random\".", call. = FALSE)
  }
  as.integer(train)
}

# caret's defaultSummary plus RPIQ, so the ratio of performance to
# interquartile distance appears in fit$results and the tuning table.
#
# Every call inside is namespace-qualified, which lets
# .portable_caret_summary() cut the closure loose from the DroneBioR
# namespace before it is stored in the fitted object.
#' @noRd
.dronebio_caret_summary <- function(data, lev = NULL, model = NULL) {
  out <- caret::defaultSummary(data, lev, model)
  obs <- as.numeric(data[["obs"]])
  rmse <- unname(out[["RMSE"]])
  iqr <- as.numeric(diff(stats::quantile(obs, c(0.25, 0.75), na.rm = TRUE)))
  rpiq <- if (!is.finite(rmse) || rmse == 0 || !is.finite(iqr) || iqr == 0) {
    NA_real_
  } else {
    iqr / rmse
  }
  c(out, RPIQ = rpiq)
}

# caret stores trControl verbatim inside the fitted object. Rebinding the
# summary closure to baseenv() before it goes in keeps model.rds free of any
# reference to the DroneBioR namespace, which is what lets the bundle be
# readRDS'd and predicted from anywhere caret is installed. Every call in
# .dronebio_caret_summary() is namespace-qualified, so nothing is lost.
.portable_caret_summary <- function() {
  f <- .dronebio_caret_summary
  environment(f) <- baseenv()
  f
}

# Out-of-fold predictions for the winning hyperparameter combination.
# merge() would reorder rows; filtering keeps fit$pred's own order.
.caret_cv_predictions <- function(fit) {
  p <- fit$pred
  if (is.null(p) || nrow(p) == 0L) return(NULL)
  bt <- fit$bestTune
  keys <- intersect(names(bt), names(p))
  if (length(keys) > 0L) {
    keep <- rep(TRUE, nrow(p))
    for (k in keys) {
      keep <- keep & (as.character(p[[k]]) == as.character(bt[[k]][[1L]]))
    }
    p <- p[keep, , drop = FALSE]
  }
  p
}

#' Fit one caret model on a shared train / test split
#'
#' Runs 10-fold cross-validation inside the training partition to select
#' hyperparameters, then reports three honest metric rows: out-of-fold CV,
#' resubstitution on the training rows, and the independent holdout.
#'
#' @param data Extraction table from [extract_field_covariates()].
#' @param response Response column.
#' @param predictors Covariate ids, in order.
#' @param method caret method id.
#' @param metric `"RMSE"` (minimised) or `"Rsquared"` (maximised). This is
#'   caret's internal ranking metric only.
#' @param split A [field_train_split()] result, shared across the sweep.
#' @param preprocess Optional `caret::preProcess` steps, e.g.
#'   `c("center", "scale")`.
#' @param allow_parallel Passed to `trainControl()`. Keep `FALSE` inside
#'   Shiny, which already runs a `future` plan.
#' @return An object of class `dronebio_field_model`.
#' @examples
#' if (requireNamespace("caret", quietly = TRUE)) {
#'   set.seed(1)
#'   d <- data.frame(NDVI = runif(60, 0.2, 0.9))
#'   d$biomass_kgha <- 800 + 3000 * d$NDVI + rnorm(60, sd = 150)
#'   split <- field_train_split(d, holdout = 0.25, folds = 5)
#'   m <- fit_field_caret_model(d, predictors = "NDVI", method = "lm",
#'                              split = split)
#'   m$metrics
#' }
#' @export
fit_field_caret_model <- function(data, response = "biomass_kgha", predictors,
                                  method = "lm",
                                  metric = c("RMSE", "Rsquared"),
                                  split, preprocess = NULL,
                                  allow_parallel = FALSE) {
  .require_caret()
  metric <- match.arg(metric)
  predictors <- as.character(predictors)
  started <- Sys.time()

  if (!response %in% names(data)) {
    stop("Response column '", response, "' is not in the extraction table.",
         call. = FALSE)
  }
  absent <- setdiff(predictors, names(data))
  if (length(absent) > 0L) {
    stop("Covariate(s) missing from the extraction table: ",
         paste(absent, collapse = ", "), call. = FALSE)
  }
  if (missing(split) || is.null(split$train_idx)) {
    stop("`split` must come from field_train_split(); computing it per model ",
         "would make the leaderboard an unfair comparison.", call. = FALSE)
  }

  all_na <- predictors[vapply(predictors,
                              function(p) all(is.na(data[[p]])), logical(1))]
  if (length(all_na) > 0L) {
    stop("Covariate(s) with no data at any sample: ",
         paste(all_na, collapse = ", "),
         ". Every point may lie outside that layer's extent.", call. = FALSE)
  }

  keep <- stats::complete.cases(data[, c(response, predictors), drop = FALSE])
  # Rows are dropped from the shared split rather than re-partitioned, so
  # every model in a sweep still sees the same fold assignment.
  pos_keep <- which(keep[split$train_idx])
  train_rows <- split$train_idx[pos_keep]
  test_rows <- split$test_idx[keep[split$test_idx]]

  n_min <- max(10L, length(predictors) + 3L)
  if (length(train_rows) < n_min) {
    stop("Only ", length(train_rows), " complete training sample(s) for ",
         length(predictors), " covariate(s); at least ", n_min,
         " are needed. Tick fewer covariates or add field points.",
         call. = FALSE)
  }
  y_train <- as.numeric(data[[response]][train_rows])
  if (!is.finite(stats::var(y_train)) || stats::var(y_train) == 0) {
    stop("The response has zero variance in the training rows; there is ",
         "nothing to model.", call. = FALSE)
  }

  avail <- caret_model_available(method)
  if (!isTRUE(avail$ok)) {
    stop("caret method '", method, "' needs package(s): ",
         paste(avail$missing, collapse = ", "), ". Install with: ",
         avail$install_call, call. = FALSE)
  }

  index <- lapply(split$folds, function(i) match(i[i %in% pos_keep], pos_keep))
  index_out <- lapply(index, function(i) setdiff(seq_along(train_rows), i))

  x_train <- data[train_rows, predictors, drop = FALSE]
  control <- caret::trainControl(
    method          = "cv",
    number          = split$folds_k,
    index           = index,
    indexOut        = index_out,
    savePredictions = "final",
    summaryFunction = .portable_caret_summary(),
    allowParallel   = isTRUE(allow_parallel)
  )
  # x/y interface: no model.frame overhead and a smaller saved object.
  # suppressMessages() swallows caret's "Truncating the grid" stderr note.
  fit <- suppressMessages(caret::train(
    x          = x_train,
    y          = y_train,
    method     = method,
    metric     = metric,
    maximize   = identical(metric, "Rsquared"),
    preProcess = preprocess,
    trControl  = control
  ))

  sid <- if ("sample_id" %in% names(data)) {
    as.character(data$sample_id)
  } else {
    as.character(seq_len(nrow(data)))
  }

  preds <- list()
  cv <- .caret_cv_predictions(fit)
  if (!is.null(cv) && nrow(cv) > 0L) {
    preds[[length(preds) + 1L]] <- data.frame(
      sample_id = sid[train_rows[cv$rowIndex]],
      split     = "cv",
      observed  = as.numeric(cv$obs),
      predicted = as.numeric(cv$pred),
      stringsAsFactors = FALSE
    )
  }
  preds[[length(preds) + 1L]] <- data.frame(
    sample_id = sid[train_rows],
    split     = "train",
    observed  = y_train,
    predicted = as.numeric(stats::predict(fit, newdata = x_train,
                                          na.action = stats::na.pass)),
    stringsAsFactors = FALSE
  )
  if (length(test_rows) > 0L) {
    x_test <- data[test_rows, predictors, drop = FALSE]
    preds[[length(preds) + 1L]] <- data.frame(
      sample_id = sid[test_rows],
      split     = "test",
      observed  = as.numeric(data[[response]][test_rows]),
      predicted = as.numeric(stats::predict(fit, newdata = x_test,
                                            na.action = stats::na.pass)),
      stringsAsFactors = FALSE
    )
  }
  predictions <- do.call(rbind, preds)
  rownames(predictions) <- NULL

  window_px <- attr(data, "window_px")
  if (is.null(window_px) && ".window_px" %in% names(data)) {
    window_px <- as.integer(sqrt(stats::median(data$.window_px, na.rm = TRUE)))
  }
  window_fun <- attr(data, "window_fun")

  model <- structure(
    list(
      method        = method,
      label         = .caret_model_label(method),
      response      = response,
      predictors    = predictors,
      metric        = metric,
      window_px     = if (is.null(window_px)) NA_integer_ else as.integer(window_px),
      window_fun    = if (is.null(window_fun)) NA_character_ else as.character(window_fun),
      n             = as.integer(length(train_rows) + length(test_rows)),
      train_idx     = as.integer(train_rows),
      test_idx      = as.integer(test_rows),
      fit           = fit,
      metrics       = NULL,
      predictions   = predictions,
      tuning        = fit$results,
      best_tune     = fit$bestTune,
      crs           = if (is.null(attr(data, "crs"))) "" else attr(data, "crs"),
      reference_geom = attr(data, "reference_geom"),
      settings      = list(
        folds         = split$folds_k,
        holdout       = split$holdout,
        seed          = split$seed,
        preprocess    = preprocess,
        r2_definition = "1 - SSres/SStot"
      ),
      caret_version = as.character(utils::packageVersion("caret")),
      r_version     = R.version.string,
      trained_at    = started,
      elapsed_s     = as.numeric(difftime(Sys.time(), started, units = "secs"))
    ),
    class = "dronebio_field_model"
  )
  model$metrics <- field_model_metrics(model)
  model
}

.caret_model_label <- function(method) {
  if (!requireNamespace("caret", quietly = TRUE)) return(method)
  info <- tryCatch(caret::getModelInfo(method, regex = FALSE),
                   error = function(e) list())
  if (length(info) == 0L || is.null(info[[1L]]) || is.null(info[[1L]]$label)) {
    return(method)
  }
  as.character(info[[1L]]$label)[[1L]]
}

#' Metrics for a fitted field model
#'
#' Three rows - out-of-fold cross-validation, resubstitution on the training
#' rows, and the independent holdout (omitted when `holdout = 0`) - all
#' computed by `.biomass_metrics()`, so `r2` is `1 - SSres/SStot` and never
#' caret's squared Pearson `Rsquared`.
#'
#' @param model A `dronebio_field_model`.
#' @return A data frame with `split`, `n`, `r2`, `rmse`, `mae` and `rpiq`,
#'   numeric and unrounded.
#' @examples
#' if (requireNamespace("caret", quietly = TRUE)) {
#'   set.seed(1)
#'   d <- data.frame(NDVI = runif(60, 0.2, 0.9))
#'   d$biomass_kgha <- 800 + 3000 * d$NDVI + rnorm(60, sd = 150)
#'   m <- fit_field_caret_model(d, predictors = "NDVI", method = "lm",
#'                              split = field_train_split(d, folds = 5))
#'   field_model_metrics(m)
#' }
#' @export
field_model_metrics <- function(model) {
  if (!inherits(model, "dronebio_field_model")) {
    stop("`model` must be a dronebio_field_model.", call. = FALSE)
  }
  p <- model$predictions
  folds_k <- model$settings$folds
  spec <- list(
    c(key = "cv", label = sprintf("CV (%d-fold)", folds_k)),
    c(key = "train", label = "Train"),
    c(key = "test", label = "Test")
  )
  rows <- list()
  for (s in spec) {
    sub <- p[p$split == s[["key"]], , drop = FALSE]
    if (nrow(sub) == 0L) next
    m <- .biomass_metrics(sub$observed, sub$predicted)
    rows[[length(rows) + 1L]] <- data.frame(
      split = s[["label"]], n = as.integer(m$n), r2 = m$r2, rmse = m$rmse,
      mae = m$mae, rpiq = m$rpiq, stringsAsFactors = FALSE
    )
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Format field model metrics for display
#'
#' The one place the two-decimal rounding happens, so the Shiny table, the
#' console `print()` and the `.txt` summary all show identical numbers.
#'
#' @param model A `dronebio_field_model`.
#' @param digits Decimal places.
#' @return A data frame with `Split`, `n`, `R2`, `RMSE`, `MAE` and `RPIQ`,
#'   the numeric columns already formatted as strings.
#' @examples
#' if (requireNamespace("caret", quietly = TRUE)) {
#'   set.seed(1)
#'   d <- data.frame(NDVI = runif(60, 0.2, 0.9))
#'   d$biomass_kgha <- 800 + 3000 * d$NDVI + rnorm(60, sd = 150)
#'   m <- fit_field_caret_model(d, predictors = "NDVI", method = "lm",
#'                              split = field_train_split(d, folds = 5))
#'   format_field_metrics(m)
#' }
#' @export
format_field_metrics <- function(model, digits = 2) {
  mt <- if (inherits(model, "dronebio_field_model")) {
    field_model_metrics(model)
  } else if (is.data.frame(model)) {
    model
  } else {
    stop("`model` must be a dronebio_field_model or a metrics data frame.",
         call. = FALSE)
  }
  fmt <- function(z) formatC(as.numeric(z), format = "f", digits = digits)
  data.frame(
    Split = mt$split,
    n     = mt$n,
    R2    = fmt(mt$r2),
    RMSE  = fmt(mt$rmse),
    MAE   = fmt(mt$mae),
    RPIQ  = fmt(mt$rpiq),
    stringsAsFactors = FALSE
  )
}

#' Predict from a fitted field model
#'
#' @param object A `dronebio_field_model`.
#' @param newdata Data frame containing every covariate in
#'   `object$predictors`.
#' @param na.action Forced to `stats::na.pass` by default: `predict.train()`
#'   defaults to `na.omit` and silently returns fewer rows than supplied.
#' @param ... Passed to `predict.train()`.
#' @return Numeric vector with one prediction per row of `newdata`.
#' @export
predict.dronebio_field_model <- function(object, newdata,
                                         na.action = stats::na.pass, ...) {
  absent <- setdiff(object$predictors, names(newdata))
  if (length(absent) > 0L) {
    stop("`newdata` is missing covariate(s): ",
         paste(absent, collapse = ", "), call. = FALSE)
  }
  nd <- as.data.frame(newdata, stringsAsFactors = FALSE)[, object$predictors,
                                                         drop = FALSE]
  as.numeric(stats::predict(object$fit, newdata = nd, na.action = na.action, ...))
}

#' @export
print.dronebio_field_model <- function(x, ...) {
  cat("DroneBioR field biomass model\n")
  cat(sprintf("  method     : %s (%s)\n", x$method, x$label))
  cat(sprintf("  response   : %s\n", x$response))
  cat(sprintf("  covariates : %s\n", paste(x$predictors, collapse = ", ")))
  cat(sprintf("  window     : %s px, aggregated with %s\n",
              x$window_px, x$window_fun))
  cat(sprintf("  n          : %d samples (%d train / %d test)\n",
              x$n, length(x$train_idx), length(x$test_idx)))
  cat(sprintf("  CV         : %d-fold, seed %s, hold-out %.0f%%\n",
              x$settings$folds, x$settings$seed, 100 * x$settings$holdout))
  cat(sprintf("  ranked on  : %s (caret internal)\n", x$metric))
  cat("  metrics    : R2 = 1 - SSres/SStot, RPIQ = IQR(observed)/RMSE\n")
  ft <- format_field_metrics(x)
  for (i in seq_len(nrow(ft))) {
    cat(sprintf("    %-14s n = %-4d R2 = %-6s RMSE = %-10s MAE = %-10s RPIQ = %s\n",
                ft$Split[i], ft$n[i], ft$R2[i], ft$RMSE[i], ft$MAE[i], ft$RPIQ[i]))
  }
  invisible(x)
}

#' Predict a biomass map from a covariate stack
#'
#' @param model A `dronebio_field_model`.
#' @param stack Covariate `SpatRaster` from [build_prediction_stack()].
#' @param out_path Optional GeoTIFF path. Writing through `filename=` keeps
#'   the result out of memory.
#' @param min_biomass Lower clamp for predictions (kg/ha).
#' @param wopt Extra `terra` write options, merged over the defaults.
#' @return A single-layer `SpatRaster` named `biomass_kgha`.
#' @export
predict_field_model_map <- function(model, stack, out_path = NULL,
                                    min_biomass = 0, wopt = list()) {
  if (!inherits(model, "dronebio_field_model")) {
    stop("`model` must be a dronebio_field_model.", call. = FALSE)
  }
  if (!inherits(stack, "SpatRaster")) {
    stop("`stack` must be a terra SpatRaster.", call. = FALSE)
  }
  absent <- setdiff(model$predictors, names(stack))
  if (length(absent) > 0L) {
    stop("Prediction stack is missing covariate layer(s): ",
         paste(absent, collapse = ", "), call. = FALSE)
  }
  dup <- intersect(model$predictors,
                   unique(names(stack)[duplicated(names(stack))]))
  if (length(dup) > 0L) {
    stop("Prediction stack has duplicate layer name(s): ",
         paste(dup, collapse = ", "), call. = FALSE)
  }

  # na.action = na.pass inside the fun: predict.train() otherwise drops rows
  # and terra fails on the length mismatch. Clamping here (rather than after)
  # keeps the streamed write a single pass.
  fun <- function(mod, data, ...) {
    p <- stats::predict(mod$fit, newdata = data, na.action = stats::na.pass)
    pmax(as.numeric(p), min_biomass)
  }
  wopt <- utils::modifyList(
    list(names = "biomass_kgha", datatype = "FLT4S", gdal = "COMPRESS=DEFLATE"),
    wopt
  )
  # na.rm = TRUE is mandatory: without it terra errors on the first NA cell,
  # and every real orthomosaic has a transparent border.
  map <- terra::predict(
    stack[[model$predictors]], model, fun = fun, na.rm = TRUE,
    filename = if (is.null(out_path)) "" else out_path,
    overwrite = TRUE, wopt = wopt
  )
  names(map) <- "biomass_kgha"
  map
}

#' Export the exact full-resolution biomass map
#'
#' Builds a native-resolution stack of the reflectance bands plus whatever
#' terrain layers the model uses, then predicts block by block through
#' [covariate_frame_from_pixels()] - the same primitive extraction used - so
#' the exported surface is computed by identical arithmetic to training,
#' with no block-mean approximation. Streams straight to `out_path`.
#'
#' @param model A `dronebio_field_model`.
#' @param reflectance Native-resolution reflectance `SpatRaster`.
#' @param out_path GeoTIFF path to write.
#' @param custom_index Optional custom index `SpatRaster`.
#' @param chm,dsm,dtm Optional terrain `SpatRaster`s.
#' @param min_biomass Lower clamp for predictions (kg/ha).
#' @return A single-layer `SpatRaster` named `biomass_kgha`, backed by
#'   `out_path`.
#' @export
export_field_biomass_map <- function(model, reflectance, out_path,
                                     custom_index = NULL, chm = NULL,
                                     dsm = NULL, dtm = NULL, min_biomass = 0) {
  if (!inherits(model, "dronebio_field_model")) {
    stop("`model` must be a dronebio_field_model.", call. = FALSE)
  }
  if (!inherits(reflectance, "SpatRaster")) {
    stop("`reflectance` must be a terra SpatRaster.", call. = FALSE)
  }
  if (missing(out_path) || is.null(out_path) || !nzchar(out_path)) {
    stop("`out_path` is required; the full-resolution export streams to disk.",
         call. = FALSE)
  }

  band_names <- names(reflectance)
  stack <- reflectance
  custom_name <- NULL
  attach_layer <- function(stack, r, nm) {
    if (is.null(r)) return(stack)
    rr <- r[[1L]]
    if (!terra::compareGeom(reflectance, rr, stopOnError = FALSE,
                            lyrs = FALSE, messages = FALSE)) {
      rr <- terra::resample(rr, reflectance[[1L]], method = "bilinear")
    }
    names(rr) <- nm
    c(stack, rr)
  }
  stack <- attach_layer(stack, chm, "CHM_m")
  stack <- attach_layer(stack, dsm, "DSM")
  stack <- attach_layer(stack, dtm, "DTM")
  if (!is.null(custom_index)) {
    custom_name <- names(custom_index)[[1L]]
    stack <- attach_layer(stack, custom_index, custom_name)
  }

  crs_str <- if (nzchar(model$crs)) model$crs else terra::crs(reflectance)
  fun <- function(mod, data, ...) {
    data <- as.data.frame(data, stringsAsFactors = FALSE)
    cf <- covariate_frame_from_pixels(
      data[, band_names, drop = FALSE],
      mod$predictors,
      chm_values    = if ("CHM_m" %in% names(data)) data[["CHM_m"]] else NULL,
      dsm_values    = if ("DSM" %in% names(data)) data[["DSM"]] else NULL,
      dtm_values    = if ("DTM" %in% names(data)) data[["DTM"]] else NULL,
      custom_values = if (!is.null(custom_name) && custom_name %in% names(data)) {
        stats::setNames(data[, custom_name, drop = FALSE], custom_name)
      } else {
        NULL
      },
      crs = crs_str
    )
    p <- stats::predict(mod$fit, newdata = cf, na.action = stats::na.pass)
    pmax(as.numeric(p), min_biomass)
  }

  map <- terra::predict(
    stack, model, fun = fun, na.rm = TRUE, filename = out_path,
    overwrite = TRUE,
    wopt = list(names = "biomass_kgha", datatype = "FLT4S",
                gdal = "COMPRESS=DEFLATE")
  )
  names(map) <- "biomass_kgha"
  map
}

#' Write a plain-text summary of a fitted field model
#'
#' Creates intermediate directories. The Shiny studio writes this to
#' `<project_dir>/outputs/biomass_model_summary.txt`, which is the path the
#' workflow stepper checks for its Field step.
#'
#' @param model A `dronebio_field_model`.
#' @param path Output text file.
#' @return `path`, invisibly.
#' @export
write_field_model_summary <- function(model, path) {
  if (!inherits(model, "dronebio_field_model")) {
    stop("`model` must be a dronebio_field_model.", call. = FALSE)
  }
  dir <- dirname(path)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  ft <- format_field_metrics(model)
  geom <- model$reference_geom
  lines <- c(
    "DroneBioR field biomass model",
    "=============================",
    sprintf("Trained at   : %s", format(model$trained_at, "%Y-%m-%d %H:%M:%S")),
    sprintf("caret method : %s (%s)", model$method, model$label),
    sprintf("Response     : %s", model$response),
    sprintf("Samples      : %d (%d train / %d test)",
            model$n, length(model$train_idx), length(model$test_idx)),
    sprintf("Covariates   : %s", paste(model$predictors, collapse = ", ")),
    sprintf("Window       : %s px, aggregated with %s",
            model$window_px, model$window_fun),
    sprintf("Cross-val    : %d-fold, hold-out %.0f%%, seed %s",
            model$settings$folds, 100 * model$settings$holdout,
            model$settings$seed),
    sprintf("Pre-process  : %s",
            if (is.null(model$settings$preprocess)) "none"
            else paste(model$settings$preprocess, collapse = ", ")),
    sprintf("Ranked on    : %s (caret internal; Rsquared there is corr^2)",
            model$metric),
    "",
    "Grid",
    "----",
    sprintf("CRS          : %s", if (nzchar(model$crs)) model$crs else "unknown"),
    if (is.null(geom)) "Geometry     : not recorded" else
      sprintf("Geometry     : %d x %d cells at %s m",
              geom$nrow, geom$ncol,
              paste(signif(geom$res, 4), collapse = " x ")),
    "",
    "Best hyperparameters",
    "--------------------",
    paste(utils::capture.output(print(model$best_tune)), collapse = "\n"),
    "",
    "Performance",
    "-----------",
    "R2 = 1 - SSres/SStot (DroneBioR convention).",
    "RPIQ = IQR(observed) / RMSE.",
    paste(utils::capture.output(print(ft, row.names = FALSE)), collapse = "\n"),
    "",
    "Session",
    "-------",
    sprintf("R            : %s", model$r_version),
    sprintf("caret        : %s", model$caret_version),
    sprintf("DroneBioR    : %s", as.character(utils::packageVersion("DroneBioR")))
  )
  writeLines(lines, path)
  invisible(path)
}

# Runnable scoring script shipped inside the model bundle.
.bundle_predict_script <- function(model) {
  c(
    "#!/usr/bin/env Rscript",
    "# Score a new covariate stack with a DroneBioR field biomass model.",
    "# Usage: Rscript predict_biomass.R <covariate_stack.tif> <out.tif>",
    "",
    "# Run from inside the unpacked bundle; no library() calls are needed",
    "# because caret's namespace loads with the saved object.",
    "args <- commandArgs(trailingOnly = TRUE)",
    "model <- readRDS(\"model.rds\")",
    "",
    sprintf("covariates <- c(%s)",
            paste(sprintf('"%s"', model$predictors), collapse = ", ")),
    "",
    "if (length(args) >= 1L) {",
    "  stack <- terra::rast(args[[1]])",
    "  absent <- setdiff(covariates, names(stack))",
    "  if (length(absent)) stop(\"stack is missing: \", paste(absent, collapse = \", \"))",
    "  fun <- function(mod, data, ...) {",
    "    as.numeric(stats::predict(mod$fit, newdata = data, na.action = stats::na.pass))",
    "  }",
    "  out <- if (length(args) >= 2L) args[[2]] else \"biomass_prediction.tif\"",
    "  map <- terra::predict(stack[[covariates]], model, fun = fun, na.rm = TRUE,",
    "                        filename = out, overwrite = TRUE)",
    "  message(\"wrote \", out)",
    "} else {",
    "  samples <- utils::read.csv(\"samples.csv\", stringsAsFactors = FALSE)",
    "  samples$predicted_kgha <- stats::predict(model$fit, newdata = samples[, covariates, drop = FALSE],",
    "                                           na.action = stats::na.pass)",
    "  print(utils::head(samples[, c(covariates, \"predicted_kgha\")]))",
    "}"
  )
}

.bundle_readme <- function(model) {
  needed <- unique(c("terra", "caret",
                     caret_model_available(model$method)$missing,
                     tryCatch({
                       cat_row <- caret_model_catalogue()
                       row <- cat_row[cat_row$method == model$method, , drop = FALSE]
                       if (nrow(row) && nzchar(row$packages[[1L]])) {
                         trimws(strsplit(row$packages[[1L]], ",")[[1L]])
                       } else character(0)
                     }, error = function(e) character(0))))
  needed <- needed[nzchar(needed)]
  ft <- format_field_metrics(model)
  c(
    "DroneBioR field biomass model bundle",
    "====================================",
    "",
    "Files",
    "-----",
    "model.rds          the fitted dronebio_field_model (readRDS + predict)",
    "metrics.csv        CV / train / test metrics, unrounded",
    "samples.csv        the extraction table the model was fitted on",
    "predict_biomass.R  runnable scoring script",
    "",
    "Covariates, in the order the model expects them",
    "-----------------------------------------------",
    paste(seq_along(model$predictors), model$predictors, sep = ". "),
    "",
    "Extraction settings",
    "-------------------",
    sprintf("Window       : %s px, aggregated with %s",
            model$window_px, model$window_fun),
    sprintf("CRS          : %s", if (nzchar(model$crs)) model$crs else "unknown"),
    "",
    "Performance",
    "-----------",
    "R2 = 1 - SSres/SStot. RPIQ = IQR(observed) / RMSE.",
    paste(utils::capture.output(print(ft, row.names = FALSE)), collapse = "\n"),
    "",
    "Reproducing this environment",
    "----------------------------",
    sprintf("R            : %s", model$r_version),
    sprintf("caret        : %s", model$caret_version),
    sprintf("DroneBioR    : %s", as.character(utils::packageVersion("DroneBioR"))),
    sprintf("install.packages(c(%s))",
            paste(sprintf('"%s"', needed), collapse = ", ")),
    "",
    "Scoring a new stack",
    "-------------------",
    "Rscript predict_biomass.R covariate_stack.tif biomass_prediction.tif"
  )
}

#' Save a trained field model as a self-contained zip bundle
#'
#' A bare `saveRDS()` is not enough to run a model later outside the app, so
#' the bundle also carries the metrics, the samples, a README naming the
#' exact package versions and covariate order, and a runnable scoring script.
#'
#' @param model A `dronebio_field_model`.
#' @param path Destination `.zip` path.
#' @param samples Optional extraction table to include as `samples.csv`.
#' @param map_path Optional predicted-map GeoTIFF to include.
#' @return `path`, invisibly.
#' @export
save_field_model_bundle <- function(model, path, samples = NULL,
                                    map_path = NULL) {
  if (!inherits(model, "dronebio_field_model")) {
    stop("`model` must be a dronebio_field_model.", call. = FALSE)
  }
  zip_cmd <- Sys.getenv("R_ZIPCMD", "zip")
  if (!nzchar(Sys.which(zip_cmd))) {
    stop("No 'zip' command is available to build the model bundle.",
         call. = FALSE)
  }
  stage <- tempfile("dronebio_bundle_")
  dir.create(stage, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)

  saveRDS(model, file.path(stage, "model.rds"))
  utils::write.csv(field_model_metrics(model),
                   file.path(stage, "metrics.csv"), row.names = FALSE)
  if (!is.null(samples)) {
    flat <- if (inherits(samples, "sf")) sf::st_drop_geometry(samples) else samples
    utils::write.csv(as.data.frame(flat), file.path(stage, "samples.csv"),
                     row.names = FALSE)
  }
  writeLines(.bundle_readme(model), file.path(stage, "README.txt"))
  writeLines(.bundle_predict_script(model), file.path(stage, "predict_biomass.R"))
  if (!is.null(map_path) && file.exists(map_path)) {
    file.copy(map_path, file.path(stage, basename(map_path)), overwrite = TRUE)
  }

  out_dir <- dirname(path)
  if (nzchar(out_dir) && !dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }
  # Absolutise via the directory: normalizePath() leaves a relative path to a
  # not-yet-existing file untouched, and the zip runs from the staging dir.
  path <- file.path(
    suppressWarnings(normalizePath(out_dir, winslash = "/", mustWork = FALSE)),
    basename(path)
  )
  if (file.exists(path)) unlink(path)
  old <- setwd(stage)
  on.exit(setwd(old), add = TRUE, after = FALSE)
  status <- utils::zip(zipfile = path, files = list.files("."),
                       flags = c("-r9X", "-q"))
  if (!identical(as.integer(status), 0L) || !file.exists(path)) {
    stop("Could not write the model bundle to: ", path, call. = FALSE)
  }
  invisible(path)
}

#' Load a field model bundle written by save_field_model_bundle()
#'
#' @param path Bundle `.zip` path.
#' @param check_packages Warn when the caret backend the model needs is not
#'   installed here.
#' @return The `dronebio_field_model`, with a `bundle_dir` attribute pointing
#'   at the unpacked files.
#' @export
load_field_model_bundle <- function(path, check_packages = TRUE) {
  if (!file.exists(path)) {
    stop("Model bundle not found: ", path, call. = FALSE)
  }
  dir <- tempfile("dronebio_bundle_")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  utils::unzip(path, exdir = dir)
  rds <- list.files(dir, pattern = "^model\\.rds$", recursive = TRUE,
                    full.names = TRUE)
  if (length(rds) == 0L) {
    stop("The bundle does not contain model.rds: ", path, call. = FALSE)
  }
  model <- readRDS(rds[[1L]])
  if (!inherits(model, "dronebio_field_model")) {
    stop("model.rds is not a dronebio_field_model.", call. = FALSE)
  }
  if (isTRUE(check_packages)) {
    avail <- tryCatch(caret_model_available(model$method),
                      error = function(e) list(ok = TRUE, missing = character(0)))
    if (!isTRUE(avail$ok)) {
      warning("This model needs package(s) that are not installed: ",
              paste(avail$missing, collapse = ", "), ". ", avail$install_call,
              call. = FALSE)
    }
  }
  attr(model, "bundle_dir") <- dir
  model
}
