# Build a 2D ROI polygon from selected points

Build a 2D ROI polygon from selected points

## Usage

``` r
build_roi_polygon(points, method = c("hull", "bbox"))
```

## Arguments

- points:

  Data frame with `x` and `y` coordinates.

- method:

  `hull` for a convex hull, or `bbox` for an axis-aligned box.

## Value

Data frame with polygon vertex coordinates.

## Examples

``` r
set.seed(1)
pts <- data.frame(x = runif(50, 0, 10), y = runif(50, 0, 10))
build_roi_polygon(pts, method = "bbox")
#>           x         y
#> 1 0.1339033 0.5893438
#> 2 9.9190609 0.5893438
#> 3 9.9190609 9.6061800
#> 4 0.1339033 9.6061800
```
