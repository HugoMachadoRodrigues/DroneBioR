# Export the exact full-resolution biomass map

Builds a native-resolution stack of the reflectance bands plus whatever
terrain layers the model uses, then predicts block by block through
[`covariate_frame_from_pixels()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/covariate_frame_from_pixels.md) -
the same primitive extraction used - so the exported surface is computed
by identical arithmetic to training, with no block-mean approximation.
Streams straight to `out_path`.

## Usage

``` r
export_field_biomass_map(
  model,
  reflectance,
  out_path,
  custom_index = NULL,
  chm = NULL,
  dsm = NULL,
  dtm = NULL,
  min_biomass = 0
)
```

## Arguments

- model:

  A `dronebio_field_model`.

- reflectance:

  Native-resolution reflectance `SpatRaster`.

- out_path:

  GeoTIFF path to write.

- custom_index:

  Optional custom index `SpatRaster`.

- chm, dsm, dtm:

  Optional terrain `SpatRaster`s.

- min_biomass:

  Lower clamp for predictions (kg/ha).

## Value

A single-layer `SpatRaster` named `biomass_kgha`, backed by `out_path`.
