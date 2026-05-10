# Compute a vertical point-density profile

Compute a vertical point-density profile

## Usage

``` r
compute_vertical_profile(points, bin_size = 1)
```

## Arguments

- points:

  Selected points with `height_m` or `z`.

- bin_size:

  Height bin size in meters.

## Value

A data frame with height bins and point counts.

## Examples

``` r
set.seed(1)
pts <- data.frame(
  x = runif(100, 0, 10),
  y = runif(100, 0, 10),
  z = runif(100, 50, 55)
)
compute_vertical_profile(pts, bin_size = 1)
#>   bin_bottom_m bin_top_m point_count
#> 1            0         1          35
#> 2            1         2          23
#> 3            2         3          18
#> 4            3         4          12
#> 5            4         5          12
#> 6            5         6           0
```
