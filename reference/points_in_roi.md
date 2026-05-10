# Test whether coordinates are inside a polygon ROI

Test whether coordinates are inside a polygon ROI

## Usage

``` r
points_in_roi(x, y, roi_polygon)
```

## Arguments

- x:

  Numeric x coordinates.

- y:

  Numeric y coordinates.

- roi_polygon:

  Data frame with `x` and `y` polygon vertices.

## Value

Logical vector.

## Examples

``` r
roi <- data.frame(x = c(0, 5, 5, 0), y = c(0, 0, 5, 5))
points_in_roi(x = c(1, 6), y = c(1, 6), roi_polygon = roi)
#> [1]  TRUE FALSE
```
