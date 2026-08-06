# Which spectral bands an orthomosaic actually carries

Decides whether NIR and RedEdge are present from the layer *names* a
raster declares, falling back to the layer count only when the file was
written without informative names.

## Usage

``` r
orthomosaic_band_presence(x, nlyr = NULL)
```

## Arguments

- x:

  A `SpatRaster`, or a character vector of layer names.

- nlyr:

  Layer count, used only as a fallback when `x` carries no recognisable
  band names. Taken from `x` when it is a `SpatRaster`.

## Value

A list with `has_nir`, `has_rededge` and `by`, the last being `"name"`
or `"count"` depending on which signal was used.

## Details

Counting layers alone is not enough, and getting this wrong is
expensive: it silently hides NDVI, NDRE, EVI and every other
multispectral index from a flight that has the bands. A MicaSense
orthomosaic labels its bands `Red, Green, Blue, NIR, Rededge` (plus
alpha), and a DJI Mavic 3M stack likewise, so the names are the reliable
signal; a 4-band RGB + alpha file and a 4-band multispectral subset are
indistinguishable by count.

## Examples

``` r
orthomosaic_band_presence(c("Red", "Green", "Blue", "NIR", "Rededge"))
#> $bands
#> [1] "Red"     "Green"   "Blue"    "NIR"     "RedEdge"
#> 
#> $has_blue
#> [1] TRUE
#> 
#> $has_green
#> [1] TRUE
#> 
#> $has_red
#> [1] TRUE
#> 
#> $has_nir
#> [1] TRUE
#> 
#> $has_rededge
#> [1] TRUE
#> 
#> $by
#> [1] "name"
#> 
orthomosaic_band_presence(c("red", "green", "blue"), nlyr = 3)
#> $bands
#> [1] "Red"   "Green" "Blue" 
#> 
#> $has_blue
#> [1] TRUE
#> 
#> $has_green
#> [1] TRUE
#> 
#> $has_red
#> [1] TRUE
#> 
#> $has_nir
#> [1] FALSE
#> 
#> $has_rededge
#> [1] FALSE
#> 
#> $by
#> [1] "name"
#> 
```
