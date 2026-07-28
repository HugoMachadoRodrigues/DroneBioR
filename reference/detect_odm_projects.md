# Detect existing ODM project subdirectories in a project root

Walks `<project_dir>/outputs/` looking for any folder layout that looks
like an ODM project — that is, any
`<subdir>/<project_name>/odm_orthophoto/odm_orthophoto.tif`. Lets the
Shiny app populate a selector instead of assuming the canonical
`outputs/odm_micasense_dataset/micasense/` defaults.

## Usage

``` r
detect_odm_projects(project_dir)
```

## Arguments

- project_dir:

  Path to a DroneBioR project root.

## Value

Data frame with `dataset_subdir`, `project_name`, `orthomosaic`, sorted
with most-recently-modified first. Empty when nothing is found.

## Examples

``` r
detect_odm_projects(tempdir())
#> [1] dataset_subdir project_name   orthomosaic   
#> <0 rows> (or 0-length row.names)
```
