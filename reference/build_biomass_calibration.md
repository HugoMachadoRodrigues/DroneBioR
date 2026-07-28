# Assemble the biomass calibration table

Reads georeferenced field points, optionally fills the biomass of
plate-only points from a
[`fit_plate_meter()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/fit_plate_meter.md)
calibration, and joins each point to the predictor grid cell it falls in
(or the mean of a buffer around it). The result is one tidy table ready
for
[`fit_biomass_model()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/fit_biomass_model.md).

## Usage

``` r
build_biomass_calibration(
  field,
  grid,
  plate_meter = NULL,
  biomass = "biomass_kgha",
  height = "plate_height_cm",
  group = "pasture",
  id = "sample_id",
  buffer_m = NULL
)
```

## Arguments

- field:

  Field data frame or path to a CSV understood by
  [`read_field_data()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/read_field_data.md).

- grid:

  Predictor `SpatRaster` from
  [`make_biomass_grid()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/make_biomass_grid.md).

- plate_meter:

  Optional `dronebio_plate_meter`; when supplied, rows with a finite
  height but missing biomass are filled with its prediction.

- biomass, height, group, id:

  Column names in `field`.

- buffer_m:

  Optional radius (m). When set, predictors are the mean over a circular
  buffer instead of the single containing cell - useful when GPS error
  spans more than one grid cell.

## Value

A data frame with the field columns, a `biomass_source` flag (`measured`
/ `plate_modeled`) and one column per predictor.
