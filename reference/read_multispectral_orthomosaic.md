# Read a multispectral orthomosaic

Read a multispectral orthomosaic

## Usage

``` r
read_multispectral_orthomosaic(
  orthomosaic,
  band_map = default_micasense_band_map(),
  use_alpha = TRUE
)
```

## Arguments

- orthomosaic:

  Path to a multispectral GeoTIFF.

- band_map:

  Named integer vector with Red, Green, Blue, NIR and RedEdge.

- use_alpha:

  Logical. Use layer 6 as an alpha mask when available.

## Value

A list containing `bands`, `alpha`, `source` and `n_layers`.

## Examples

``` r
ortho_path <- system.file("extdata", "micasense_subset.tif", package = "DroneBioR")
ortho <- read_multispectral_orthomosaic(ortho_path)
names(ortho$bands)
#> [1] "Blue"    "Green"   "Red"     "RedEdge" "NIR"    
ortho$n_layers
#> [1] 6
```
