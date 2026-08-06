# Read a multispectral or RGB orthomosaic

Reads an orthomosaic produced by OpenDroneMap / WebODM / Pix4Dmapper /
Agisoft Metashape. The function adapts to two common layouts:

## Usage

``` r
read_multispectral_orthomosaic(orthomosaic, band_map = NULL, use_alpha = TRUE)
```

## Arguments

- orthomosaic:

  Path to an orthomosaic GeoTIFF.

- band_map:

  Optional named integer vector. Default `NULL` = auto-detect: 3-4 layer
  inputs use
  [`default_rgb_band_map()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/default_rgb_band_map.md);
  5+ layer inputs use the internal MicaSense default
  (`Red=1, Green=2, Blue=3, NIR=4, RedEdge=5`). Override with a custom
  named integer vector.

- use_alpha:

  Logical. Treat the layer immediately after the highest band-map index
  as an alpha mask if available.

## Value

A list containing `bands`, `alpha`, `source` and `n_layers`.

## Details

- **Multispectral** (MicaSense / Sequoia, 5 bands +/- alpha) - returned
  bands are Red, Green, Blue, RedEdge, NIR. Alpha is read from layer 6
  when present.

- **RGB** (3 bands +/- alpha) - returned bands are Red, Green, Blue.
  Alpha is read from layer 4 when present.

Layout is auto-detected from
[`terra::nlyr()`](https://rspatial.github.io/terra/reference/dimensions.html)
when `band_map` is left as `NULL`; explicit band maps are honoured
otherwise.

## Examples

``` r
ortho_path <- system.file("extdata", "micasense_subset.tif", package = "DroneBioR")
ortho <- read_multispectral_orthomosaic(ortho_path)
names(ortho$bands)
#> [1] "Blue"    "Green"   "Red"     "RedEdge" "NIR"    
ortho$n_layers
#> [1] 6
```
