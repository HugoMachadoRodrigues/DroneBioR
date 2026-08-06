# Metrics for a fitted field model

Three rows - out-of-fold cross-validation, resubstitution on the
training rows, and the independent holdout (omitted when
`holdout = 0`) - all computed by `.biomass_metrics()`, so `r2` is
`1 - SSres/SStot` and never caret's squared Pearson `Rsquared`.

## Usage

``` r
field_model_metrics(model)
```

## Arguments

- model:

  A `dronebio_field_model`.

## Value

A data frame with `split`, `n`, `r2`, `rmse`, `mae` and `rpiq`, numeric
and unrounded.

## Examples

``` r
if (requireNamespace("caret", quietly = TRUE)) {
  set.seed(1)
  d <- data.frame(NDVI = runif(60, 0.2, 0.9))
  d$biomass_kgha <- 800 + 3000 * d$NDVI + rnorm(60, sd = 150)
  m <- fit_field_caret_model(d, predictors = "NDVI", method = "lm",
                             split = field_train_split(d, folds = 5))
  field_model_metrics(m)
}
#>         split  n        r2     rmse       mae     rpiq
#> 1 CV (5-fold) 48 0.9491800 132.6016 103.37746 6.027551
#> 2       Train 48 0.9528857 127.6754  99.43514 6.260113
#> 3        Test 12 0.9226631 139.2169 114.31456 3.972417
```
