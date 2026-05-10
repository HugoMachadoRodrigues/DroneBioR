# Filter point cloud data by a polygon ROI

Filter point cloud data by a polygon ROI

## Usage

``` r
filter_points_by_roi(points, roi_polygon)
```

## Arguments

- points:

  Data frame with `x` and `y` coordinates.

- roi_polygon:

  Data frame with `x` and `y` polygon vertices.

## Value

Filtered data frame.

## Examples

``` r
set.seed(1)
pts <- data.frame(x = runif(50, 0, 10), y = runif(50, 0, 10), z = runif(50, 0, 5))
roi <- data.frame(x = c(2, 8, 8, 2), y = c(2, 2, 8, 8))
nrow(filter_points_by_roi(pts, roi))
#> [1] 23
```
