# Extract windowed covariates at field sample points

The extraction pipeline for the Field Models tab. A covariate value at a
field point is the aggregate (`fun`) over the `window` x `window` block
of **per-pixel covariate values computed at native resolution** -
indices are never computed from block means during training.

## Usage

``` r
extract_field_covariates(
  points,
  reflectance,
  selected,
  window = 1L,
  window_m = NULL,
  fun = "mean",
  custom_index = NULL,
  chm = NULL,
  dsm = NULL,
  dtm = NULL
)
```

## Arguments

- points:

  Field samples from
  [`prepare_field_table()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/prepare_field_table.md)
  (or any `sf` POINT layer).

- reflectance:

  Reflectance `SpatRaster` defining the grid.

- selected:

  Covariate ids to extract, in order (see
  [`field_covariate_catalogue()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/field_covariate_catalogue.md)).

- window:

  Odd window size (see
  [`window_cells()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/window_cells.md)).

- window_m:

  Support in metres instead of pixels. A window in pixels spans a
  different area at every ground sampling distance, so a quadrat size is
  better stated in metres and converted per survey: this rounds to the
  nearest odd pixel count for `reflectance` and errors when the request
  falls outside the supported range. See
  [`window_from_metres()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/window_from_metres.md).

- fun:

  Aggregation: `"mean"`, `"median"`, `"max"`, `"min"`, `"sd"`.

- custom_index:

  Optional single-layer `SpatRaster` with a user index.

- chm, dsm, dtm:

  Optional terrain `SpatRaster`s.

## Value

A plain data frame with one row per input point, in input order: every
original attribute, then one numeric column per `selected` id in that
order, then `.n_valid_px`, `.window_px`, `.window_valid_frac` and
`.in_extent`. Out-of-extent points keep their row with all-`NA`
covariates. Carries `window_px`, `window_fun`, `crs` and
`reference_geom` attributes describing the training grid.

## Details

Terrain layers and a custom index are sampled bilinearly at the window
pixel centres, which is mathematically identical to
resample-then-extract and avoids a whole-raster
[`terra::resample()`](https://rspatial.github.io/terra/reference/resample.html).

## Examples

``` r
ortho <- system.file("extdata", "micasense_subset.tif", package = "DroneBioR")
refl <- scale_to_reflectance(read_multispectral_orthomosaic(ortho)$bands)
pts <- read_field_points(
  system.file("extdata", "field_samples.csv", package = "DroneBioR"),
  crs = 32617
)
tab <- extract_field_covariates(pts, refl, c("NDVI", "NDRE"), window = 3)
head(tab[, c("sample_id", "NDVI", "NDRE", ".n_valid_px")])
#>   sample_id         NDVI       NDRE .n_valid_px
#> 1       S01  0.744418056 0.32954092           9
#> 2       S02  0.733129092 0.30641730           9
#> 3       S03  0.743121732 0.32776524           9
#> 4       S04 -0.001227593 0.09225931           9
#> 5       S05  0.712076597 0.25233967           9
#> 6       S06  0.747637945 0.23674902           9
```
