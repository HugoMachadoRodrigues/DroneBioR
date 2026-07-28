# Copy ODM outputs to a fast local cache once, return the new paths

Use when the project root lives inside OneDrive / Google Drive / Dropbox
and the heavy raster + point-cloud files keep triggering background
re-syncs. After this returns, the Shiny app can be repointed at the
local cache and never touch the cloud-synced folder again. Files are
skipped if a same-size copy already exists.

## Usage

``` r
sync_outputs_to_local_cache(
  project,
  cache_root = file.path(Sys.getenv("HOME"), ".dronebior", "cache"),
  products = c("orthomosaic", "dsm", "dtm", "point_cloud_copc", "point_cloud_laz",
    "textured_obj", "textured_glb")
)
```

## Arguments

- project:

  A `dronebio_project` object pointing at the (typically cloud-synced)
  outputs you want to migrate.

- cache_root:

  Root for the cache. Defaults to `~/.dronebior/cache`.

- products:

  Subset of product keys (see
  [`odm_product_paths()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/odm_product_paths.md))
  to copy. Default covers the analysis essentials.

## Value

List with `cache_dir` and a named `paths` character vector keyed by
product (only entries actually present on disk).

## Examples

``` r
if (FALSE) { # \dontrun{
  project <- dronebio_project("~/cloud_project")
  cache <- sync_outputs_to_local_cache(project)
  cache$paths[["orthomosaic"]]
} # }
```
