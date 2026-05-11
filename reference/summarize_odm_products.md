# Summarize available ODM products

Summarize available ODM products

## Usage

``` r
summarize_odm_products(project)
```

## Arguments

- project:

  A `dronebio_project` object.

## Value

A data frame with product, path, availability and file size.

## Examples

``` r
project <- dronebio_project(project_dir = tempdir())
summarize_odm_products(project)
#>            product available size_mb
#> 1      orthomosaic     FALSE      NA
#> 2              dsm     FALSE      NA
#> 3              dtm     FALSE      NA
#> 4  point_cloud_las     FALSE      NA
#> 5  point_cloud_laz     FALSE      NA
#> 6 point_cloud_copc     FALSE      NA
#> 7  point_cloud_ply     FALSE      NA
#> 8         mesh_ply     FALSE      NA
#> 9     textured_obj     FALSE      NA
#>                                                                                                          path
#> 1                   /tmp/RtmpbQQgD4/outputs/odm_micasense_dataset/micasense/odm_orthophoto/odm_orthophoto.tif
#> 2                                     /tmp/RtmpbQQgD4/outputs/odm_micasense_dataset/micasense/odm_dem/dsm.tif
#> 3                                     /tmp/RtmpbQQgD4/outputs/odm_micasense_dataset/micasense/odm_dem/dtm.tif
#> 4      /tmp/RtmpbQQgD4/outputs/odm_micasense_dataset/micasense/odm_georeferencing/odm_georeferenced_model.las
#> 5      /tmp/RtmpbQQgD4/outputs/odm_micasense_dataset/micasense/odm_georeferencing/odm_georeferenced_model.laz
#> 6 /tmp/RtmpbQQgD4/outputs/odm_micasense_dataset/micasense/odm_georeferencing/odm_georeferenced_model.copc.laz
#> 7                    /tmp/RtmpbQQgD4/outputs/odm_micasense_dataset/micasense/odm_filterpoints/point_cloud.ply
#> 8                         /tmp/RtmpbQQgD4/outputs/odm_micasense_dataset/micasense/odm_meshing/odm_25dmesh.ply
#> 9        /tmp/RtmpbQQgD4/outputs/odm_micasense_dataset/micasense/odm_texturing_25d/odm_textured_model_geo.obj
```
