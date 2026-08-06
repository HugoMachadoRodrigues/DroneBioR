# Add local height above a ground proxy to point cloud data

Photogrammetric point clouds from ODM previews can be in a local
vertical reference. This helper estimates a local ground proxy from a
low Z quantile and stores height above that proxy.

## Usage

``` r
add_point_heights(points, ground_quantile = 0.05)
```

## Arguments

- points:

  Data frame with `z`.

- ground_quantile:

  Quantile used as a local ground proxy.

## Value

The input data frame with `height_m`.

## Examples

``` r
pts <- data.frame(z = c(50, 50.1, 51, 53, 55, 56, 55, 53, 51, 50))
add_point_heights(pts)
#>       z height_m
#> 1  50.0      0.0
#> 2  50.1      0.1
#> 3  51.0      1.0
#> 4  53.0      3.0
#> 5  55.0      5.0
#> 6  56.0      6.0
#> 7  55.0      5.0
#> 8  53.0      3.0
#> 9  51.0      1.0
#> 10 50.0      0.0
```
