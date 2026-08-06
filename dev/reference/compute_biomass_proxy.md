# Compute an image-only biomass proxy

Combines NDVI, SAVI and NDRE into a single -1..1 surface. Use as a
qualitative biomass surrogate when no canopy-height information is
available. For a more defensible biomass estimate, use
[`compute_biomass_proxies()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/compute_biomass_proxies.md)
which also produces height-weighted variants (greenness x CHM).

## Usage

``` r
compute_biomass_proxy(indices)
```

## Arguments

- indices:

  Spectral index stack from
  [`compute_spectral_indices()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/compute_spectral_indices.md).

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
