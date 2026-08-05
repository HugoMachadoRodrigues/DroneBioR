# Write DroneBioR raster products

Write DroneBioR raster products

## Usage

``` r
write_dronebio_rasters(
  output_dir,
  reflectance,
  indices,
  biomass_proxy,
  valid_mask = NULL
)
```

## Arguments

- output_dir:

  Output folder.

- reflectance:

  Reflectance band stack.

- indices:

  Spectral index stack.

- biomass_proxy:

  Biomass proxy raster.

- valid_mask:

  Optional alpha/valid-data mask.

## Value

Named character vector of output paths.

## Examples

``` r
ortho_path <- system.file("extdata", "micasense_subset.tif", package = "DroneBioR")
ortho <- read_multispectral_orthomosaic(ortho_path)
refl <- scale_to_reflectance(ortho$bands)
ix <- compute_spectral_indices(refl)
proxy <- compute_biomass_proxy(ix)
out <- tempfile("dronebior-rasters-")
write_dronebio_rasters(out, refl, ix, proxy)
#>                                                             reflectance 
#>   "/tmp/Rtmp2iFtP8/dronebior-rasters-27778ec4fdb/reflectance_bands.tif" 
#>                                                                 indices 
#>    "/tmp/Rtmp2iFtP8/dronebior-rasters-27778ec4fdb/spectral_indices.tif" 
#>                                                           biomass_proxy 
#> "/tmp/Rtmp2iFtP8/dronebior-rasters-27778ec4fdb/biomass_index_proxy.tif" 
```
