# Cell numbers of the n x n window around each field point

Cell numbers of the n x n window around each field point

## Usage

``` r
window_cells(reference, points, window = 1L)
```

## Arguments

- reference:

  A
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  defining the grid. Only its first layer's geometry is used.

- points:

  Sample locations: an `sf` layer, a `SpatVector`, a two-column matrix,
  or a data frame with `x` / `y`.

- window:

  Odd window size, one of 1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21.

## Value

A list with `cells` (integer matrix, one row per **in-extent** point and
`window^2` columns), `in_extent` (logical, one per input point) and
`index` (the original point index of each row of `cells`).

## Examples

``` r
ortho <- system.file("extdata", "micasense_subset.tif", package = "DroneBioR")
r <- terra::rast(ortho)
pts <- data.frame(x = c(392004, 392012), y = c(3033007, 3033012))
wc <- window_cells(r, pts, window = 3)
dim(wc$cells)
#> [1] 2 9
```
