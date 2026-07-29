# Extract raster values at field sample points

With the default `window = 1` this is a plain single-pixel extraction
and the result is byte-identical to earlier releases. A larger odd
`window` aggregates the `window` x `window` block of native-resolution
pixels around each point with `fun`, which smooths GPS error and mixed
pixels; see
[`extract_field_covariates()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/extract_field_covariates.md)
for the richer version used by the Field Models tab.

## Usage

``` r
extract_field_spectral_data(
  field_data,
  predictors,
  predictor_crs = terra::crs(predictors),
  window = 1L,
  fun = "mean"
)
```

## Arguments

- field_data:

  Field data frame.

- predictors:

  Raster stack with bands and indices.

- predictor_crs:

  CRS of x/y coordinates when x/y are used.

- window:

  Odd window size in pixels (1, 3, 5, ... 21). `1` keeps the original
  single-cell behaviour.

- fun:

  Aggregation applied over the window: `"mean"`, `"median"`, `"max"`,
  `"min"` or `"sd"`. Ignored when `window = 1`.

## Value

A data frame with field columns and extracted raster values.

## Examples

``` r
ortho_path <- system.file("extdata", "micasense_subset.tif", package = "DroneBioR")
field_path <- system.file("extdata", "field_samples.csv", package = "DroneBioR")
refl <- scale_to_reflectance(read_multispectral_orthomosaic(ortho_path)$bands)
ix <- compute_spectral_indices(refl)
field <- read_field_data(field_path)
head(extract_field_spectral_data(field, ix))
#>   sample_id biomass_kgha        x       y      NDVI        NDRE        EVI
#> 1       S01       1763.3 392004.0 3033007 0.7468699  0.34232348 0.41658388
#> 2       S02       2446.1 392012.1 3033012 0.7189247  0.24192308 0.09692537
#> 3       S03       1551.4 392006.7 3033007 0.7499179  0.35101151 0.35264725
#> 4       S04       1763.5 392013.6 3033003        NA -1.00000000 0.00000000
#> 5       S05       1939.4 392005.4 3033003 0.7504640  0.33880126 0.12806346
#> 6       S06       2299.4 392002.8 3033005 0.6446371  0.09012567 0.04393349
#>         SAVI      OSAVI     MSAVI2       NDWI     GNDVI  CIrededge      GCI
#> 1 0.41239767 0.48206907 0.38829315 -0.6379802 0.6379802  1.0410087 3.524559
#> 2 0.11092554 0.18964234 0.08100883 -0.6516624 0.6516624  0.6382547 3.741557
#> 3 0.35688464 0.44410134 0.32101379 -0.6551339 0.6551339  1.0817187 3.799352
#> 4 0.00000000 0.00000000 0.00000000         NA        NA -1.0000000       NA
#> 5 0.14510919 0.23730648 0.10880107 -0.7037334 0.7037334  1.0248092 4.750678
#> 6 0.05156947 0.09650005 0.03594327 -0.6960894 0.6960894  0.1981058 4.580882
#>        RVI        DVI       WDRVI       TVI      MCARI        PSRI       VARI
#> 1 6.901076 0.21754788  0.15973985 13.827420 0.25055047 -0.15534337  0.2749729
#> 2 6.115530 0.04121462  0.10035781  2.566262 0.06749906 -0.07762557  0.1686880
#> 3 6.997374 0.17424277  0.16648426 10.986801 0.19342798 -0.13625000  0.2537835
#> 4       NA 0.00000000          NA  0.000000         NA  0.00000000         NA
#> 5 7.014876 0.05552758  0.16769845  3.412833 0.06446274 -0.06345420  0.1295034
#> 6 4.628049 0.01815824 -0.03863205  1.055314 0.04361754  0.04419890 -0.1314554
#>            ExG        GLI        TGI      MGRVI     RGBVI
#> 1 0.0529182879 0.30766501 2.69100481  0.3987462 0.5817937
#> 2 0.0081177996 0.24270073 0.42870222  0.2491122 0.4882810
#> 3 0.0366826886 0.27632184 1.86831464  0.3601464 0.5297996
#> 4 0.0000000000         NA 0.00000000         NA        NA
#> 5 0.0084687572 0.23153942 0.45738918  0.1961404 0.4803649
#> 6 0.0006408789 0.04015296 0.05981537 -0.1850546 0.1290441
```
