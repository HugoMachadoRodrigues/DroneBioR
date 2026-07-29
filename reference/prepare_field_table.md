# Rename, convert and reproject field points for modelling

Produces the canonical field table every downstream function expects:
`sample_id` (character) and `biomass_kgha` (numeric), in the raster CRS.
All other attributes are preserved untouched.

## Usage

``` r
prepare_field_table(
  points,
  id_col,
  biomass_col,
  units = c("kg/ha", "Mg/ha", "g/m^2", "unknown"),
  target_crs
)
```

## Arguments

- points:

  An `sf` POINT layer from
  [`read_field_points()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/read_field_points.md).

- id_col:

  Column holding the sample identifier.

- biomass_col:

  Column holding the biomass measurement.

- units:

  Units of `biomass_col`: `"kg/ha"`, `"Mg/ha"`, `"g/m^2"` or `"unknown"`
  (passed through unchanged).

- target_crs:

  CRS of the raster stack the samples will be extracted against.

## Value

An `sf` POINT layer of class `dronebio_field_points`, carrying a
`dropped_na` attribute with the number of rows removed for a missing
biomass value.

## Examples

``` r
path <- system.file("extdata", "field_samples.csv", package = "DroneBioR")
pts <- read_field_points(path, crs = 32617)
tab <- prepare_field_table(pts, "sample_id", "biomass_kgha",
                           units = "kg/ha", target_crs = "EPSG:32617")
names(tab)[1:2]
#> [1] "sample_id"    "biomass_kgha"
```
