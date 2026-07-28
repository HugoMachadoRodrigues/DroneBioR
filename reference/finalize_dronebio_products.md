# Collect the final products into one flat folder with metadata

A DroneBioR run leaves a deep, ODM-shaped tree
(`odm_dataset/<name>/odm_dem/`, `.../odm_orthophoto/`,
`dronebior_analysis/`) plus raw backups, the redundant RGB-only
orthomosaic, the reflectance stack and run logs. For delivery you
usually want just the handful of products you will actually reuse, in
one place, with a machine-readable description.

## Usage

``` r
finalize_dronebio_products(
  project,
  orthomosaic = NULL,
  indices = NULL,
  biomass_proxy = NULL,
  out_dir = NULL,
  extra_metadata = list(),
  remove_scaffolding = TRUE,
  expect = character()
)
```

## Arguments

- project:

  A `dronebio_project`.

- orthomosaic:

  Path to the orthomosaic to keep (default: the 7-band DJI stack when
  present, else the RGB orthomosaic).

- indices, biomass_proxy:

  Optional paths to the spectral index stack and biomass proxy (default:
  the files
  [`run_dronebio_workflow()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/run_dronebio_workflow.md)
  writes under the project output dir).

- out_dir:

  Destination folder. Default `<project_dir>/products`.

- extra_metadata:

  Named list merged into the metadata JSON (e.g.
  `list(flight = "ifasbahia10", speed = "balanced")`).

- remove_scaffolding:

  Logical, default `TRUE`. Delete the intermediate tree after the
  products are copied out.

- expect:

  Optional character vector of product names that the caller knows it
  asked for (e.g. `c("spectral_indices", "biomass_proxy")` when indices
  were requested). Any of these whose source file is missing trigger a
  warning, so an incomplete `products/` folder (e.g. indices that
  crashed before being written) is never shipped silently. Default
  [`character()`](https://rdrr.io/r/base/character.html) warns about
  nothing.

## Value

Invisibly, a named character vector of the final product paths in
`out_dir`.

## Details

This copies the final products into `out_dir` under simple names —
`orthomosaic.tif`, `dsm.tif`, `dtm.tif`, `chm.tif`,
`spectral_indices.tif`, `biomass_proxy.tif` — writes a single
`metadata.json` (run parameters plus, per raster, the CRS, resolution,
extent, band names and per-band min/mean/max), and — unless
`remove_scaffolding = FALSE` — deletes the ODM scaffolding, the raw DEM
backups, the RGB-only ortho, the reflectance stack and the logs, leaving
only `out_dir`.

## Examples

``` r
if (FALSE) { # \dontrun{
  res <- run_odm_dji_mavic_3m(project)
  wf  <- run_dronebio_workflow(project, res$stacked_orthomosaic)
  finalize_dronebio_products(project, extra_metadata = list(flight = "f1"))
} # }
```
