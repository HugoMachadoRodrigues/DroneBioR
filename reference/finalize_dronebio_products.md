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
  expect = character(),
  products = names(finalize_product_dests())
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
  nothing. Products that exist on disk but are not collected warn
  regardless of `expect` — see the disk-space section.

- products:

  Character vector of product keys to collect, from
  [`odm_product_paths()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/odm_product_paths.md)
  plus `"spectral_indices"` and `"biomass_proxy"`. Defaults to all of
  them. Narrow it when disk space is tight and you do not need, say, the
  redundant point-cloud formats; anything you drop that exists on disk
  is reported and stops the scaffolding from being removed.

## Value

Invisibly, a named character vector of the final product paths in
`out_dir`.

## Details

This copies every product
[`odm_product_paths()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/odm_product_paths.md)
resolves into `out_dir` under simple names — the rasters as
`orthomosaic.tif`, `dsm.tif`, `dtm.tif`, `chm.tif`, `dtm_csf.tif`,
`chm_csf.tif`, `spectral_indices.tif` and `biomass_proxy.tif`; the 3D
deliverables as `point_cloud.copc.laz` / `.laz` / `.las` / `.ply`,
`mesh.ply`, `textured_model.glb` (plus the `_25d` variants) and
`report.pdf` — writes a single `metadata.json` (run parameters plus, per
raster, the CRS, resolution, extent, band names and per-band
min/mean/max), and — unless `remove_scaffolding = FALSE` — deletes the
ODM scaffolding, the raw DEM backups, the RGB-only ortho, the
reflectance stack and the logs, leaving only `out_dir`.

Multi-file products are copied as folders rather than flattened, because
their internal references are by bare filename and would break
otherwise: the textured OBJ lands in `textured_model/` alongside its
`.mtl` and texture images under their original names, and the tile sets
land whole in `3d_tiles/` and `orthomosaic_tiles/`.

## Disk space and the no-loss guarantee

The point clouds and textured meshes are by far the largest files a run
produces — several GB is routine — and they are copied, not moved, so
`out_dir` needs as much free space as the products themselves before the
scaffolding goes away. Every copy is size-checked afterwards, and if any
of them fails, or if `products` excludes something that is on disk, the
scaffolding is **kept** and a warning is raised. Nothing is deleted on
the strength of a copy that did not land.

## Examples

``` r
if (FALSE) { # \dontrun{
  res <- run_odm_dji_mavic_3m(project)
  wf  <- run_dronebio_workflow(project, res$stacked_orthomosaic)
  finalize_dronebio_products(project, extra_metadata = list(flight = "f1"))
  # Rasters and the COPC cloud only, skipping the redundant LAS/LAZ/PLY:
  finalize_dronebio_products(project,
                             products = c("orthomosaic", "dsm", "dtm", "chm",
                                          "point_cloud_copc", "textured_glb"))
} # }
```
