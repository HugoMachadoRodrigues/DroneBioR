# Classify a biomass map into labelled classes

Classify a biomass map into labelled classes

## Usage

``` r
classify_biomass_map(map, breaks, labels = NULL)
```

## Arguments

- map:

  Single-layer biomass `SpatRaster`.

- breaks:

  Cut values, as returned by
  [`biomass_map_breaks()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/biomass_map_breaks.md).

- labels:

  Optional class labels; defaults to `"lo - hi"` ranges.

## Value

A categorical `SpatRaster` named `biomass_class`.

## Examples

``` r
r <- terra::rast(nrows = 20, ncols = 20)
terra::values(r) <- seq_len(terra::ncell(r))
brk <- biomass_map_breaks(r, n = 4)
terra::freq(classify_biomass_map(r, brk$breaks))
#>   layer     value count
#> 1     1   1 - 101   100
#> 2     1 101 - 200   100
#> 3     1 200 - 300   100
#> 4     1 300 - 400   100
```
