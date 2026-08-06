# Run the DroneBioR orthomosaic analysis workflow

Run the DroneBioR orthomosaic analysis workflow

## Usage

``` r
run_dronebio_workflow(
  project = dronebio_project(),
  orthomosaic = NULL,
  output_dir = NULL,
  band_map = default_micasense_band_map(),
  use_alpha = TRUE,
  max_memory_gb = getOption("dronebior.workflow_memmax_gb", 4)
)
```

## Arguments

- project:

  A `dronebio_project` object or project directory path.

- orthomosaic:

  Optional orthomosaic path. Defaults to the ODM output path.

- output_dir:

  Optional output folder.

- band_map:

  Named band map.

- use_alpha:

  Logical. Use layer 6 as alpha mask when available.

- max_memory_gb:

  Numeric cap (GB) on terra's working memory while the reflectance
  scaling, spectral indices and their summaries/writes run, so large
  orthomosaics stream to disk in blocks instead of OOM-killing the R
  session. Restored on exit. `NULL` (or
  `options(dronebior.skip_terra_memcap = TRUE)`) leaves terra's settings
  untouched. Default `getOption("dronebior.workflow_memmax_gb", 4)`.

## Value

A list with rasters, summaries and output paths.

## Examples

``` r
# \donttest{
project <- dronebio_project(project_dir = tempdir())
ortho <- system.file("extdata", "micasense_subset.tif", package = "DroneBioR")
result <- run_dronebio_workflow(
  project = project,
  orthomosaic = ortho,
  output_dir = tempfile("dronebior-out-")
)
names(result)
#>  [1] "project"             "orthomosaic"         "bands"              
#>  [4] "reflectance"         "indices"             "biomass_proxy"      
#>  [7] "alpha"               "reflectance_summary" "index_summary"      
#> [10] "output_paths"       
# }
```
