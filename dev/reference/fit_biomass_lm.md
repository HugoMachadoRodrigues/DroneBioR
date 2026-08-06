# Fit a baseline biomass linear model

Fit a baseline biomass linear model

## Usage

``` r
fit_biomass_lm(data, response = "biomass_kgha", predictors = NULL)
```

## Arguments

- data:

  Data frame containing biomass and predictor columns.

- response:

  Response column.

- predictors:

  Optional predictor columns.

## Value

An `lm` object.

## Examples

``` r
set.seed(1)
ndvi <- runif(20, 0.3, 0.9)
field <- data.frame(
  sample_id = sprintf("S%02d", 1:20),
  biomass_kgha = 1000 + 3000 * ndvi + rnorm(20, sd = 200),
  NDVI = ndvi
)
model <- fit_biomass_lm(field, predictors = "NDVI")
coef(model)
#> (Intercept)        NDVI 
#>    1347.414    2469.441 
```
