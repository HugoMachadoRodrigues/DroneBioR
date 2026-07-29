# Fit one caret model on a shared train / test split

Runs 10-fold cross-validation inside the training partition to select
hyperparameters, then reports three honest metric rows: out-of-fold CV,
resubstitution on the training rows, and the independent holdout.

## Usage

``` r
fit_field_caret_model(
  data,
  response = "biomass_kgha",
  predictors,
  method = "lm",
  metric = c("RMSE", "Rsquared"),
  split,
  preprocess = NULL,
  allow_parallel = FALSE
)
```

## Arguments

- data:

  Extraction table from
  [`extract_field_covariates()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/extract_field_covariates.md).

- response:

  Response column.

- predictors:

  Covariate ids, in order.

- method:

  caret method id.

- metric:

  `"RMSE"` (minimised) or `"Rsquared"` (maximised). This is caret's
  internal ranking metric only.

- split:

  A
  [`field_train_split()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/field_train_split.md)
  result, shared across the sweep.

- preprocess:

  Optional
  [`caret::preProcess`](https://rdrr.io/pkg/caret/man/preProcess.html)
  steps, e.g. `c("center", "scale")`.

- allow_parallel:

  Passed to `trainControl()`. Keep `FALSE` inside Shiny, which already
  runs a `future` plan.

## Value

An object of class `dronebio_field_model`.

## Examples

``` r
if (requireNamespace("caret", quietly = TRUE)) {
  set.seed(1)
  d <- data.frame(NDVI = runif(60, 0.2, 0.9))
  d$biomass_kgha <- 800 + 3000 * d$NDVI + rnorm(60, sd = 150)
  split <- field_train_split(d, holdout = 0.25, folds = 5)
  m <- fit_field_caret_model(d, predictors = "NDVI", method = "lm",
                             split = split)
  m$metrics
}
#>         split  n        r2     rmse       mae     rpiq
#> 1 CV (5-fold) 48 0.9491800 132.6016 103.37746 6.027551
#> 2       Train 48 0.9528857 127.6754  99.43514 6.260113
#> 3        Test 12 0.9226631 139.2169 114.31456 3.972417
```
