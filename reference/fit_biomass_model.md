# Fit a field-calibrated biomass model (staged LM / random forest)

Stage 1 fits the defensible pooled linear model (Page et al. 2025:
vegetation volume / canopy height + greenness). Stage 2 optionally fits
a random forest (Vahidi et al. 2023: CHM statistics + spectral values +
a categorical pasture label) when the `ranger` package is available.
With `method = "auto"` both are fitted and the one with the lower
leave-one-out RMSE is returned as the primary model.

## Usage

``` r
fit_biomass_model(
  data,
  response = "biomass_kgha",
  predictors = NULL,
  method = c("lm", "rf", "auto"),
  group = "pasture",
  num_trees = 500L
)
```

## Arguments

- data:

  Calibration data frame from
  [`build_biomass_calibration()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/build_biomass_calibration.md).

- response:

  Biomass column (kg/ha).

- predictors:

  Optional predictor override applied to both routes. When `NULL`, each
  route uses its paper's default set present in `data` (the parsimonious
  Page set for the LM, the richer Vahidi CHM-statistics + index set for
  the RF).

- method:

  `"lm"`, `"rf"`, or `"auto"` (compare both).

- group:

  Optional categorical column (e.g. `pasture`) added to the RF.

- num_trees:

  Random-forest tree count when `ranger` is used.

## Value

An object of class `dronebio_biomass_model` with the chosen `fit`, its
`method`, the predictor names, LOO-CV `metrics`, and (for `auto`) the
per-method comparison.

## Examples

``` r
set.seed(1)
v <- runif(30, 0, 0.5); g <- runif(30, 0.3, 0.9)
d <- data.frame(biomass_kgha = 500 + 4000 * v + 1500 * g + rnorm(30, sd = 120),
                chm_volume_m3 = v, NDVI = g)
m <- fit_biomass_model(d, predictors = c("chm_volume_m3", "NDVI"))
m$metrics$r2
#> [1] 0.9765155
```
