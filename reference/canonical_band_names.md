# Canonical band names from whatever a raster calls its layers

Maps layer names to the canonical `Blue` / `Green` / `Red` / `RedEdge` /
`NIR` used throughout the package, so band detection does not depend on
one vendor's spelling. Recognises the common forms seen in the wild:
MicaSense (`Red`, `Rededge`, `NIR`), the DJI Mavic 3M stack (`MS_R`,
`MS_RE`, `MS_NIR`), Parrot Sequoia (`red`, `red_edge`, `nir`), plain
one-letter names, and `band_*` / `b*` prefixes.

## Usage

``` r
canonical_band_names(x)
```

## Arguments

- x:

  Character vector of layer names, or a `SpatRaster`.

## Value

A character vector the same length as `x`, holding canonical names or
`NA`.

## Details

Order matters and the rules are deliberately specific-first: `RE` and
`rededge` must win over `red`, and `NIR` over `N`, or a red-edge layer
gets silently filed as red and every index built on it is quietly wrong.

Anything unrecognised returns `NA`, which callers should treat as "this
band is absent" rather than guessing.

## Examples

``` r
canonical_band_names(c("Red", "Green", "Blue", "MS_RE", "MS_NIR"))
#> [1] "Red"     "Green"   "Blue"    "RedEdge" "NIR"    
canonical_band_names(c("b1", "b2", "b3"))
#> [1] NA NA NA
```
