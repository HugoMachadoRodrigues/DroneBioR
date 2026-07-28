# Calibrate a rising plate / disc meter against clipped biomass

Fits the classic forage calibration `biomass = a + b * height`
(optionally with a pasture/species factor) from the small set of clipped
quadrats that carry both a compressed plate height and a measured
dry-matter biomass. The fitted object then predicts biomass for the many
plate-only points, multiplying the effective calibration sample before
the drone model is built - the double-sampling idea behind Page et al.
(2025).

## Usage

``` r
fit_plate_meter(
  data,
  height = "plate_height_cm",
  biomass = "biomass_kgha",
  group = NULL,
  min_n = 5L
)
```

## Arguments

- data:

  Data frame with a height column and a biomass column. Rows missing
  either are dropped from the fit.

- height:

  Name of the compressed-height column (e.g. `plate_height_cm`).

- biomass:

  Name of the dry-matter biomass column (`biomass_kgha`).

- group:

  Optional factor column (e.g. `pasture`) added as a fixed effect.
  `NULL` pools all points into one equation.

- min_n:

  Minimum number of complete clip points required.

## Value

An object of class `dronebio_plate_meter`: the `lm`, the coefficient
table, leave-one-out CV metrics and the column names used.

## Examples

``` r
set.seed(1)
h <- runif(12, 2, 18)
d <- data.frame(plate_height_cm = h,
                biomass_kgha = 200 + 320 * h + rnorm(12, sd = 150))
cal <- fit_plate_meter(d)
cal$metrics$r2
#> [1] 0.990146
```
