# Lightweight existence + size check on ODM outputs

Cheap counterpart to
[`validate_odm_outputs()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/validate_odm_outputs.md)
that never opens a raster file (so it doesn't trigger OneDrive
Files-On-Demand downloads on cloud-synced project folders). Just checks
that the orthomosaic and DSM exist and are bigger than `min_size_mb` MB.

## Usage

``` r
quick_outputs_check(project, min_size_mb = 1)
```

## Arguments

- project:

  A `dronebio_project` object.

- min_size_mb:

  Numeric size threshold (MB) below which a file is treated as a
  placeholder / aborted artifact. Default 1 MB.

## Value

Named logical vector with `orthomosaic`, `dsm`, `dtm`, `point_cloud`,
plus a top-level `outputs_complete` summary that is TRUE iff the
orthomosaic and DSM both pass.

## Examples

``` r
p <- dronebio_project(project_dir = tempdir())
quick_outputs_check(p)
#>      orthomosaic              dsm              dtm              chm 
#>            FALSE            FALSE            FALSE            FALSE 
#>      point_cloud outputs_complete 
#>            FALSE            FALSE 
```
