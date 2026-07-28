# Predict a biomass map from a fitted model

Applies a
[`fit_biomass_model()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/fit_biomass_model.md)
result across the predictor grid to produce a per-cell above-ground
biomass raster (kg/ha), clamped at `min_biomass`.

## Usage

``` r
predict_biomass_map(
  model,
  grid,
  pasture = NULL,
  out_path = NULL,
  min_biomass = 0
)
```

## Arguments

- model:

  A `dronebio_biomass_model`.

- grid:

  Predictor `SpatRaster` from
  [`make_biomass_grid()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/make_biomass_grid.md) -
  must contain the model's predictor layers.

- pasture:

  Optional single pasture label to assign to every cell when the model
  used a categorical pasture term (a map covers one pasture). Must be
  one of the training levels.

- out_path:

  Optional GeoTIFF path to write.

- min_biomass:

  Lower clamp (kg/ha).

## Value

The biomass `SpatRaster` (named `biomass_kgha`).
