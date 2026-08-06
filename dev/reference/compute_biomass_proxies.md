# Compute multiple biomass-proxy rasters

Returns a stack of biomass-related surfaces derived from a spectral
index stack and (optionally) a Canopy Height Model. The greenness x
height products (`NDVI_CHM`, `NDRE_CHM`, ...) are the most defensible
image-only proxies for above-ground biomass on drone surveys: a
multiplicative greenness x canopy-height surface tracks the volume of
photosynthetically active material per pixel, which scales with fresh
biomass for most herbaceous and shrub canopies (Bendig et al. 2015;
Lussem et al. 2019).

## Usage

``` r
compute_biomass_proxies(indices, chm = NULL)
```

## Arguments

- indices:

  Spectral index stack from
  [`compute_spectral_indices()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/compute_spectral_indices.md).

- chm:

  Optional
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  with the Canopy Height Model (m above ground). When `NULL` only the
  spectral biomass surface is returned. The CHM is resampled onto the
  index grid when geometry differs.

## Value

A
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
with one or more biomass-proxy layers.

## Details

Without a CHM the function still returns the pure-spectral proxies so
RGB-only or DSM-only datasets get something useful. None of these are
biomass in kg/ha without field calibration - they are surfaces that
correlate with biomass and should be regressed against ground truth.

Layers always returned when the input indices allow: Biomass_Spectral
mean(NDVI, SAVI, NDRE) clipped to `[-1, 1]` (the legacy
[`compute_biomass_proxy()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/compute_biomass_proxy.md)
output) Biomass_NDVI_x_CHM NDVI \* CHM (m greenness) Biomass_NDRE_x_CHM
NDRE \* CHM (m greenness, RedEdge) Biomass_SAVI_x_CHM SAVI \* CHM
Biomass_GNDVI_x_CHM GNDVI \* CHM Biomass_VARI_x_CHM VARI \* CHM
(RGB-only) Biomass_EXG_x_CHM ExG \* CHM (RGB-only; common in turf / crop
UAS) Biomass_MGRVI_x_CHM MGRVI \* CHM (RGB-only; Bendig 2015)
Biomass_RGBVI_x_CHM RGBVI \* CHM (RGB-only; Bendig 2015)

## Examples

``` r
if (FALSE) { # \dontrun{
  refl <- scale_to_reflectance(read_multispectral_orthomosaic(path)$bands)
  ix   <- compute_spectral_indices(refl)
  chm  <- terra::rast("chm.tif")
  compute_biomass_proxies(ix, chm)
} # }
```
