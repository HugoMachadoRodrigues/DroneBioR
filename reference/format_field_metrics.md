# Format field model metrics for display

The one place the two-decimal rounding happens, so the Shiny table, the
console [`print()`](https://rdrr.io/r/base/print.html) and the `.txt`
summary all show identical numbers.

## Usage

``` r
format_field_metrics(model, digits = 2)
```

## Arguments

- model:

  A `dronebio_field_model`.

- digits:

  Decimal places.

## Value

A data frame with `Split`, `n`, `R2`, `RMSE`, `MAE` and `RPIQ`, the
numeric columns already formatted as strings.

## Examples

``` r
if (requireNamespace("caret", quietly = TRUE)) {
  set.seed(1)
  d <- data.frame(NDVI = runif(60, 0.2, 0.9))
  d$biomass_kgha <- 800 + 3000 * d$NDVI + rnorm(60, sd = 150)
  m <- fit_field_caret_model(d, predictors = "NDVI", method = "lm",
                             split = field_train_split(d, folds = 5))
  format_field_metrics(m)
}
#>         Split  n   R2   RMSE    MAE RPIQ
#> 1 CV (5-fold) 48 0.95 132.60 103.38 6.03
#> 2       Train 48 0.95 127.68  99.44 6.26
#> 3        Test 12 0.92 139.22 114.31 3.97
```
