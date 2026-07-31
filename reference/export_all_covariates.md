# Compute and export every covariate to a folder

Builds the full covariate set the Field Models tab can use – the
reflectance bands, every spectral index
[`compute_spectral_indices()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/compute_spectral_indices.md)
can derive, the biomass proxies
[`compute_biomass_proxies()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/compute_biomass_proxies.md)
(the canopy-height ones when a CHM is supplied), and the terrain layers
– and writes each as its own GeoTIFF into `out_dir` (a folder named
`covariates/` by convention), at the orthomosaic's native resolution,
ready to load into any GIS afterwards.

## Usage

``` r
export_all_covariates(
  reflectance,
  out_dir,
  chm = NULL,
  dsm = NULL,
  dtm = NULL,
  custom_index = NULL,
  overwrite = TRUE
)
```

## Arguments

- reflectance:

  A
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  of the scaled reflectance bands (named, e.g. `Red`, `Green`, `NIR`,
  `RedEdge`).

- out_dir:

  Destination folder. Created if missing.

- chm, dsm, dtm:

  Optional terrain `SpatRaster`s; each exported when given, and the CHM
  also feeds the `*_x_CHM` biomass proxies.

- custom_index:

  Optional single-layer `SpatRaster` of a user index.

- overwrite:

  Overwrite existing files (default `TRUE`); when `FALSE`, existing
  outputs are kept and still reported as written.

## Value

Invisibly, a character vector of the GeoTIFFs written.

## Details

Layers that cannot be produced (e.g. the biomass proxies when the cloud
has none of the bands they need) are skipped, not fatal, so the export
always returns whatever could be built.

## Examples

``` r
if (FALSE) { # \dontrun{
refl <- read_multispectral_orthomosaic("odm_orthophoto.tif")
chm  <- build_chm_from_dsm_dtm("odm_dem/dsm.tif", "odm_dem/dtm.tif")
export_all_covariates(refl, file.path(project$project_dir, "covariates"),
                      chm = chm)
} # }
```
