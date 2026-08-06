# Add CHM-derived heights to selected points

Add CHM-derived heights to selected points

## Usage

``` r
add_chm_heights(points, chm, fallback_quantile = 0.05)
```

## Arguments

- points:

  Data frame with `x`, `y` and `z`.

- chm:

  A
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  canopy height model.

- fallback_quantile:

  Local Z quantile used when CHM is missing for a point.

## Value

Input points with `height_m` derived from the CHM where possible.

## Examples

``` r
dsm <- system.file("extdata", "dsm_subset.tif", package = "DroneBioR")
dtm <- system.file("extdata", "dtm_subset.tif", package = "DroneBioR")
chm <- build_chm_from_dsm_dtm(dsm, dtm)
pts <- data.frame(
  x = seq(392001, 392015, length.out = 5),
  y = seq(3033001, 3033015, length.out = 5),
  z = c(50, 51, 52, 53, 54)
)
add_chm_heights(pts, chm)
#>          x       y  z  height_m
#> 1 392001.0 3033001 50 0.0000000
#> 2 392004.5 3033004 51 0.3958473
#> 3 392008.0 3033008 52 2.9106445
#> 4 392011.5 3033012 53 0.4596252
#> 5 392015.0 3033015 54 0.0000000
```
