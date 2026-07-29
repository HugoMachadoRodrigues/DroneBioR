# Quantile breaks and a robust stretch for a biomass map

Large maps are sampled rather than read in full; regular sampling
reproduces the full-raster quantiles closely enough for a legend and
costs a fraction of the I/O.

## Usage

``` r
biomass_map_breaks(map, n = 4L, sample_size = 1e+05)
```

## Arguments

- map:

  Single-layer biomass `SpatRaster`.

- n:

  Number of classes.

- sample_size:

  Cell budget for
  [`terra::spatSample()`](https://rspatial.github.io/terra/reference/sample.html).

## Value

A list with `breaks` (length `n + 1`), `quartiles`, `p01`, `p99` and
`labels`.

## Examples

``` r
r <- terra::rast(nrows = 20, ncols = 20)
terra::values(r) <- seq_len(terra::ncell(r))
biomass_map_breaks(r, n = 4)$breaks
#> [1]   1.00 100.75 200.50 300.25 400.00
```
