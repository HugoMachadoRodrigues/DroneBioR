# Validate ODM output products against sanity thresholds

Inspects each rasterizable product (orthomosaic, DSM, DTM) and the
point-cloud / mesh files for the obvious failure modes that bit us on
the GeoScan dataset: degenerate extents (\<50 m wide), pixel grids under
100x100, missing or tiny binary files. The `valid` flag is the simple
bottom-line; `notes` tells the user *why*.

## Usage

``` r
validate_odm_outputs(project)
```

## Arguments

- project:

  A `dronebio_project` object.

## Value

A data frame with one row per product and columns `product`, `path`,
`exists`, `size_mb`, `dimensions`, `extent_m`, `crs`, `valid`, `notes`.

## Examples

``` r
project <- dronebio_project(project_dir = tempdir())
validate_odm_outputs(project)
#>                 product
#> 1           Orthomosaic
#> 2                   DSM
#> 3                   DTM
#> 4                   CHM
#> 5    Point cloud (COPC)
#> 6     Point cloud (LAZ)
#> 7     Point cloud (LAS)
#> 8     Point cloud (PLY)
#> 9   Textured mesh (OBJ)
#> 10 Textured mesh (glTF)
#> 11     3D tiles tileset
#> 12       ODM report PDF
#>                                                                                                           path
#> 1                    /tmp/RtmpqLEWU5/outputs/odm_micasense_dataset/micasense/odm_orthophoto/odm_orthophoto.tif
#> 2                                      /tmp/RtmpqLEWU5/outputs/odm_micasense_dataset/micasense/odm_dem/dsm.tif
#> 3                                      /tmp/RtmpqLEWU5/outputs/odm_micasense_dataset/micasense/odm_dem/dtm.tif
#> 4                                      /tmp/RtmpqLEWU5/outputs/odm_micasense_dataset/micasense/odm_dem/chm.tif
#> 5  /tmp/RtmpqLEWU5/outputs/odm_micasense_dataset/micasense/odm_georeferencing/odm_georeferenced_model.copc.laz
#> 6       /tmp/RtmpqLEWU5/outputs/odm_micasense_dataset/micasense/odm_georeferencing/odm_georeferenced_model.laz
#> 7       /tmp/RtmpqLEWU5/outputs/odm_micasense_dataset/micasense/odm_georeferencing/odm_georeferenced_model.las
#> 8                     /tmp/RtmpqLEWU5/outputs/odm_micasense_dataset/micasense/odm_filterpoints/point_cloud.ply
#> 9             /tmp/RtmpqLEWU5/outputs/odm_micasense_dataset/micasense/odm_texturing/odm_textured_model_geo.obj
#> 10            /tmp/RtmpqLEWU5/outputs/odm_micasense_dataset/micasense/odm_texturing/odm_textured_model_geo.glb
#> 11                               /tmp/RtmpqLEWU5/outputs/odm_micasense_dataset/micasense/3d_tiles/tileset.json
#> 12                               /tmp/RtmpqLEWU5/outputs/odm_micasense_dataset/micasense/odm_report/report.pdf
#>    exists size_mb dimensions extent_m  crs valid   notes
#> 1   FALSE      NA       <NA>     <NA> <NA> FALSE missing
#> 2   FALSE      NA       <NA>     <NA> <NA> FALSE missing
#> 3   FALSE      NA       <NA>     <NA> <NA> FALSE missing
#> 4   FALSE      NA       <NA>     <NA> <NA> FALSE missing
#> 5   FALSE      NA       <NA>     <NA> <NA> FALSE missing
#> 6   FALSE      NA       <NA>     <NA> <NA> FALSE missing
#> 7   FALSE      NA       <NA>     <NA> <NA> FALSE missing
#> 8   FALSE      NA       <NA>     <NA> <NA> FALSE missing
#> 9   FALSE      NA       <NA>     <NA> <NA> FALSE missing
#> 10  FALSE      NA       <NA>     <NA> <NA> FALSE missing
#> 11  FALSE      NA       <NA>     <NA> <NA> FALSE missing
#> 12  FALSE      NA       <NA>     <NA> <NA> FALSE missing
```
