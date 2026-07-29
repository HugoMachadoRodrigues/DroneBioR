# Aggregate raster values over pre-computed window cells

One windowed
[`terra::extract()`](https://rspatial.github.io/terra/reference/extract.html)
read per call, reshaped per layer and collapsed with `fun`. Cells that
fall outside the raster come back as `NA`, so an edge point aggregates
over its in-raster pixels only.

## Usage

``` r
extract_window_values(
  x,
  cells,
  fun = c("mean", "median", "max", "min", "sd"),
  na.rm = TRUE
)
```

## Arguments

- x:

  A
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html).

- cells:

  Integer matrix of cell numbers, as returned in the `cells` element of
  [`window_cells()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/window_cells.md).

- fun:

  Aggregation: `"mean"`, `"median"`, `"max"`, `"min"` or `"sd"`.

- na.rm:

  Drop missing window pixels before aggregating.

## Value

A data frame with `nrow(cells)` rows, one numeric column per layer of
`x` (named after the layer), plus `.n_valid_px` - the number of window
pixels with data in the first layer.

## Details

Never use `terra::extract(..., buffer =)` for this: terra accepts and
silently ignores `buffer` for point extraction (unlike
[`raster::extract`](https://rspatial.github.io/terra/reference/extract.html)),
returning single-cell values that look plausible.

## Examples

``` r
ortho <- system.file("extdata", "micasense_subset.tif", package = "DroneBioR")
r <- terra::rast(ortho)[[1:2]]
pts <- data.frame(x = 392004, y = 3033007)
wc <- window_cells(r, pts, window = 3)
extract_window_values(r, wc$cells, fun = "mean")
#>    Red    Green .n_valid_px
#> 1 2229 3301.222           9
```
