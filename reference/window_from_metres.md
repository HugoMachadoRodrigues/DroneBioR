# Convert a metric window to the nearest odd pixel window

A window expressed in pixels changes physical meaning with the ground
sampling distance, so a quadrat-matched support has to be restated for
every survey. This converts metres to the nearest odd pixel count for a
given raster, and reports the metric support that count actually spans.

## Usage

``` r
window_from_metres(reference, window_m)
```

## Arguments

- reference:

  A
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  whose resolution defines the conversion.

- window_m:

  Desired support in metres (the side of the square).

## Value

A list with `window` (odd pixel count), `window_m_actual` (what that
count spans) and `resolution`.

## Examples

``` r
r <- terra::rast(nrows = 100, ncols = 100, xmin = 0, xmax = 5.76, ymin = 0, ymax = 5.76)
window_from_metres(r, 0.52)
#> $window
#> [1] 9
#> 
#> $window_m_actual
#> [1] 0.5184
#> 
#> $resolution
#> [1] 0.0576
#> 
```
