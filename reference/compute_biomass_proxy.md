# Compute an image-only biomass proxy

Compute an image-only biomass proxy

## Usage

``` r
compute_biomass_proxy(indices)
```

## Arguments

- indices:

  Spectral index stack from
  [`compute_spectral_indices()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/compute_spectral_indices.md).

## Value

A
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
named `Biomass_Index_Proxy`.

## Examples

``` r
ortho_path <- system.file("extdata", "micasense_subset.tif", package = "DroneBioR")
refl <- scale_to_reflectance(read_multispectral_orthomosaic(ortho_path)$bands)
proxy <- compute_biomass_proxy(compute_spectral_indices(refl))
names(proxy)
#> [1] "Biomass_Index_Proxy"
```
