# Run the field-calibrated biomass mapping workflow

End-to-end driver: read field samples, calibrate the plate meter, build
the predictor grid, assemble the calibration table, fit the staged model
and write a biomass map. Returns every intermediate so a script or the
Shiny studio can report on them.

## Usage

``` r
run_biomass_mapping(
  field,
  indices,
  chm = NULL,
  pasture = NULL,
  out_path = NULL,
  grid_m = 1,
  method = c("auto", "lm", "rf"),
  predictors = NULL,
  buffer_m = NULL,
  min_biomass = 0
)
```

## Arguments

- field:

  Field data frame or CSV path (see
  [`read_field_data()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/read_field_data.md)).
  Needs `biomass_kgha` on the clip rows; `plate_height_cm` on all rows
  enables the plate-meter step; a `pasture` column enables the
  categorical RF term.

- indices:

  Spectral index `SpatRaster` from
  [`compute_spectral_indices()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/compute_spectral_indices.md).

- chm:

  Optional CHM `SpatRaster`.

- pasture:

  Optional pasture label for the mapped raster (defaults to the single
  pasture present in `field`).

- out_path:

  Optional GeoTIFF path for the biomass map.

- grid_m:

  Management grid-cell size (m).

- method:

  Model route: `"lm"`, `"rf"`, or `"auto"`.

- predictors:

  Optional predictor override.

- buffer_m:

  Optional buffer radius for calibration extraction.

- min_biomass:

  Lower clamp (kg/ha) for the map.

## Value

A list with `plate_meter`, `grid`, `calibration`, `model`, `map`,
`metrics` and `map_path`.

## Examples

``` r
if (FALSE) { # \dontrun{
  res <- run_biomass_mapping(
    field   = "field_biomass_plate.csv",
    indices = ix, chm = chm,
    out_path = "biomass_kgha.tif", method = "auto"
  )
  res$model
} # }
```
