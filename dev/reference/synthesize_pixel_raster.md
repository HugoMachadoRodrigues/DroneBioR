# Pack pixel values into a one-row SpatRaster

Lets per-pixel raster functions
([`compute_spectral_indices()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/compute_spectral_indices.md),
[`compute_biomass_proxies()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/compute_biomass_proxies.md))
run on a handful of window pixels instead of hundreds of millions of
cells. terra fills row-major, so cell *i* is row *i* of `values` and the
input order is preserved exactly.

## Usage

``` r
synthesize_pixel_raster(values, crs = "")
```

## Arguments

- values:

  Numeric matrix or data frame; one column per layer, one row per pixel.
  Column names become layer names.

- crs:

  CRS string for the synthetic grid. Any consistent value works - it
  only has to match between stacks that will be compared with
  [`terra::compareGeom()`](https://rspatial.github.io/terra/reference/compareGeom.html).

## Value

A
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
with 1 row, `nrow(values)` columns and `ncol(values)` layers.

## Examples

``` r
r <- synthesize_pixel_raster(data.frame(Red = c(0.1, 0.2), NIR = c(0.5, 0.6)))
dim(r)
#> [1] 1 2 2
terra::values(r)
#>      Red NIR
#> [1,] 0.1 0.5
#> [2,] 0.2 0.6
```
