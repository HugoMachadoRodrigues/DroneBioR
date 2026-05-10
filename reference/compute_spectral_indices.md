# Compute spectral vegetation indices

Compute spectral vegetation indices

## Usage

``` r
compute_spectral_indices(reflectance, eps = 1e-06)
```

## Arguments

- reflectance:

  A reflectance-scale
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  with Blue, Green, Red, RedEdge and NIR layers.

- eps:

  Small denominator threshold.

## Value

A
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
with NDVI, NDRE, EVI, SAVI, NDWI, GNDVI, CIrededge, MSAVI2 and VARI.

## Examples

``` r
ortho_path <- system.file("extdata", "micasense_subset.tif", package = "DroneBioR")
refl <- scale_to_reflectance(read_multispectral_orthomosaic(ortho_path)$bands)
ix <- compute_spectral_indices(refl)
names(ix)
#> [1] "NDVI"      "NDRE"      "EVI"       "SAVI"      "NDWI"      "GNDVI"    
#> [7] "CIrededge" "MSAVI2"    "VARI"     
```
