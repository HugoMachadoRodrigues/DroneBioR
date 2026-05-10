# Extract raster values at field sample points

Extract raster values at field sample points

## Usage

``` r
extract_field_spectral_data(
  field_data,
  predictors,
  predictor_crs = terra::crs(predictors)
)
```

## Arguments

- field_data:

  Field data frame.

- predictors:

  Raster stack with bands and indices.

- predictor_crs:

  CRS of x/y coordinates when x/y are used.

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
#>         SAVI       NDWI     GNDVI  CIrededge     MSAVI2       VARI
#> 1 0.41239767 -0.6379802 0.6379802  1.0410087 0.38829315  0.2749729
#> 2 0.11092554 -0.6516624 0.6516624  0.6382547 0.08100883  0.1686880
#> 3 0.35688464 -0.6551339 0.6551339  1.0817187 0.32101379  0.2537835
#> 4 0.00000000         NA        NA -1.0000000 0.00000000         NA
#> 5 0.14510919 -0.7037334 0.7037334  1.0248092 0.10880107  0.1295034
#> 6 0.05156947 -0.6960894 0.6960894  0.1981058 0.03594327 -0.1314554
```
