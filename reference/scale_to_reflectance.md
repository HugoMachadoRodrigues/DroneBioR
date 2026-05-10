# Scale raster values to reflectance-like 0-1 values

Scale raster values to reflectance-like 0-1 values

## Usage

``` r
scale_to_reflectance(x, scale_factor = NULL)
```

## Arguments

- x:

  A
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html).

- scale_factor:

  Optional numeric scale factor.

## Value

A
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html).

## Examples

``` r
ortho_path <- system.file("extdata", "micasense_subset.tif", package = "DroneBioR")
ortho <- read_multispectral_orthomosaic(ortho_path)
refl <- scale_to_reflectance(ortho$bands)
terra::minmax(refl)
#>           Blue      Green        Red   RedEdge       NIR
#> min 0.00000000 0.00000000 0.00000000 0.0000000 0.0000000
#> max 0.02618448 0.06164645 0.04097047 0.1448539 0.2835279
```
