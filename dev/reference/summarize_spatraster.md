# Summarize a SpatRaster by layer

Summarize a SpatRaster by layer

## Usage

``` r
summarize_spatraster(x, fun = c("min", "mean", "max", "sd"))
```

## Arguments

- x:

  A
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html).

- fun:

  Summary functions supported by
  [`terra::global()`](https://rspatial.github.io/terra/reference/global.html).

## Value

A data frame.

## Examples

``` r
ortho_path <- system.file("extdata", "micasense_subset.tif", package = "DroneBioR")
ortho <- read_multispectral_orthomosaic(ortho_path)
summarize_spatraster(ortho$bands)
#>     layer min      mean   max        sd
#> 1    Blue   0  475.4757  1716  566.2983
#> 2   Green   0 1140.4727  4040 1356.7164
#> 3     Red   0  758.9939  2685  905.1125
#> 4 RedEdge   0 2661.2034  9493 3168.7032
#> 5     NIR   0 5220.9666 18581 6235.9881
```
