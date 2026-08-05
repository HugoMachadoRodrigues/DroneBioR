# Build an aggregated covariate stack for map prediction

The fast map path: aggregate the reflectance first, then compute
indices, proxies and terrain layers on the small grid. `fact` defaults
to the training window whenever the cell budget allows, so one map cell
matches the support the model was trained on.

## Usage

``` r
build_prediction_stack(
  reflectance,
  covariates,
  max_cells = 1e+06,
  window = 1L,
  window_m = NULL,
  custom_index = NULL,
  chm = NULL,
  dsm = NULL,
  dtm = NULL
)
```

## Arguments

- reflectance:

  Reflectance `SpatRaster`.

- covariates:

  Covariate ids to supply, in order (`model$predictors`).

- max_cells:

  Approximate cell budget for the aggregated grid.

- window:

  Extraction window the model was trained with.

- window_m:

  The same support expressed in metres, converted to the nearest odd
  pixel count for `reflectance`. Prefer this when the calibration table
  was extracted with `window_m`, so the prediction stack matches the
  support the model saw. See
  [`window_from_metres()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/window_from_metres.md).

- custom_index:

  Optional custom index `SpatRaster`.

- chm, dsm, dtm:

  Optional terrain `SpatRaster`s.

## Value

A `SpatRaster` whose [`names()`](https://rdrr.io/r/base/names.html) are
exactly `covariates`, in order, with `fact` and `cell_size_m`
attributes.

## Details

**Approximation:** indices here come from block-mean reflectance,
whereas training used the mean of native-resolution indices. The two
differ slightly because the index formulas are nonlinear. Use
[`export_field_biomass_map()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/export_field_biomass_map.md)
for the exact surface.

## Examples

``` r
ortho <- system.file("extdata", "micasense_subset.tif", package = "DroneBioR")
refl <- scale_to_reflectance(read_multispectral_orthomosaic(ortho)$bands)
stack <- build_prediction_stack(refl, c("NDVI", "NDRE"), max_cells = 500)
names(stack)
#> [1] "NDVI" "NDRE"
```
