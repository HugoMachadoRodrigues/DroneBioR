# Compute a covariate frame from pixel values

The single definition of "how a covariate is computed from a pixel".
Extraction, map prediction and the full-resolution export all route
through this function, so training and prediction can never diverge.

## Usage

``` r
covariate_frame_from_pixels(
  band_values,
  selected,
  chm_values = NULL,
  custom_values = NULL,
  dsm_values = NULL,
  dtm_values = NULL,
  crs = ""
)
```

## Arguments

- band_values:

  Numeric matrix / data frame of reflectance values, one column per band
  (`Blue`, `Green`, `Red`, `RedEdge`, `NIR`, ...).

- selected:

  Character vector of covariate ids to return, in order.

- chm_values:

  Optional canopy-height values (one per pixel).

- custom_values:

  Optional custom-index values: a named vector, or a data frame whose
  column names are the covariate ids.

- dsm_values, dtm_values:

  Optional terrain values (one per pixel).

- crs:

  CRS for the synthetic grid; must be the same for all stacks compared
  inside one call.

## Value

A data frame with `nrow(band_values)` rows and columns named exactly
`selected`, in that order.

## Details

Indices are computed by packing the pixels into a one-row raster
([`synthesize_pixel_raster()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/synthesize_pixel_raster.md))
and calling
[`compute_spectral_indices()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/compute_spectral_indices.md).
Biomass proxies get the CHM as an extra layer on that **same** one-row
grid, so
[`terra::compareGeom()`](https://rspatial.github.io/terra/reference/compareGeom.html)
inside
[`compute_biomass_proxies()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/compute_biomass_proxies.md)
succeeds and its no-resample branch is taken - resampling a one-row
raster would be meaningless.

## Examples

``` r
bands <- data.frame(Green = c(0.2, 0.3), Red = c(0.1, 0.15),
                    RedEdge = c(0.3, 0.35), NIR = c(0.6, 0.7))
covariate_frame_from_pixels(bands, c("NDVI", "NDRE", "Red"))
#>        NDVI      NDRE  Red
#> 1 0.7142857 0.3333333 0.10
#> 2 0.6470588 0.3333333 0.15
```
