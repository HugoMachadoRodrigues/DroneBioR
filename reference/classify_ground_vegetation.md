# Rule-based ground / vegetation classification from NDVI (and CHM)

Applies a small ladder of thresholds to an NDVI raster (and, optionally,
a CHM raster) to produce a five-class categorical raster. Designed as a
first-pass label layer for visualization and Shiny app legends; for
research-grade classification, train a supervised classifier on the
index stack instead.

## Usage

``` r
classify_ground_vegetation(
  ndvi,
  chm = NULL,
  ndvi_bare_max = 0.2,
  ndvi_stress_max = 0.4,
  ndvi_vigorous_min = 0.65,
  chm_tall_min = 2
)
```

## Arguments

- ndvi:

  A
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  of NDVI (typically -1..1).

- chm:

  Optional
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  canopy height model in meters.

- ndvi_bare_max:

  Upper bound for "bare / soil" (default 0.20).

- ndvi_stress_max:

  Upper bound for "stressed / sparse" (default 0.40).

- ndvi_vigorous_min:

  Lower bound for "vigorous" (default 0.65).

- chm_tall_min:

  Lower bound (m) for "tall vegetation" (default 2.0).

## Value

A single-layer
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
named `class` with integer codes.

## Details

Classes:

- 1:

  Bare / soil (low NDVI).

- 2:

  Stressed or sparse vegetation.

- 3:

  Moderate vigor.

- 4:

  Vigorous vegetation (high NDVI, short / unknown height).

- 5:

  Tall vegetation (CHM above `chm_tall_min`).

When `chm` is `NULL`, the output uses classes 1-4 only.

## Examples

``` r
ortho_path <- system.file("extdata", "micasense_subset.tif", package = "DroneBioR")
refl <- scale_to_reflectance(read_multispectral_orthomosaic(ortho_path)$bands)
ix   <- compute_spectral_indices(refl)
classes <- classify_ground_vegetation(ix[["NDVI"]])
names(classes)
#> [1] "class"
```
