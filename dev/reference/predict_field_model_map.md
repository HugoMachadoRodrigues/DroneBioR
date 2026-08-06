# Predict a biomass map from a covariate stack

Predict a biomass map from a covariate stack

## Usage

``` r
predict_field_model_map(
  model,
  stack,
  out_path = NULL,
  min_biomass = 0,
  wopt = list()
)
```

## Arguments

- model:

  A `dronebio_field_model`.

- stack:

  Covariate `SpatRaster` from
  [`build_prediction_stack()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/build_prediction_stack.md).

- out_path:

  Optional GeoTIFF path. Writing through `filename=` keeps the result
  out of memory.

- min_biomass:

  Lower clamp for predictions (kg/ha).

- wopt:

  Extra `terra` write options, merged over the defaults.

## Value

A single-layer `SpatRaster` named `biomass_kgha`.
