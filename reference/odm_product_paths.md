# Return expected ODM product paths

Return expected ODM product paths

## Usage

``` r
odm_product_paths(project)
```

## Arguments

- project:

  A `dronebio_project` object.

## Value

Named character vector of expected product paths.

## Examples

``` r
project <- dronebio_project(project_dir = tempdir())
odm_product_paths(project)
#>                                                                                                   orthomosaic 
#>                   "/tmp/Rtmpc2Wbkz/outputs/odm_micasense_dataset/micasense/odm_orthophoto/odm_orthophoto.tif" 
#>                                                                                                           dsm 
#>                                     "/tmp/Rtmpc2Wbkz/outputs/odm_micasense_dataset/micasense/odm_dem/dsm.tif" 
#>                                                                                                           dtm 
#>                                     "/tmp/Rtmpc2Wbkz/outputs/odm_micasense_dataset/micasense/odm_dem/dtm.tif" 
#>                                                                                               point_cloud_las 
#>      "/tmp/Rtmpc2Wbkz/outputs/odm_micasense_dataset/micasense/odm_georeferencing/odm_georeferenced_model.las" 
#>                                                                                               point_cloud_laz 
#>      "/tmp/Rtmpc2Wbkz/outputs/odm_micasense_dataset/micasense/odm_georeferencing/odm_georeferenced_model.laz" 
#>                                                                                              point_cloud_copc 
#> "/tmp/Rtmpc2Wbkz/outputs/odm_micasense_dataset/micasense/odm_georeferencing/odm_georeferenced_model.copc.laz" 
#>                                                                                               point_cloud_ply 
#>                    "/tmp/Rtmpc2Wbkz/outputs/odm_micasense_dataset/micasense/odm_filterpoints/point_cloud.ply" 
#>                                                                                                      mesh_ply 
#>                         "/tmp/Rtmpc2Wbkz/outputs/odm_micasense_dataset/micasense/odm_meshing/odm_25dmesh.ply" 
#>                                                                                                  textured_obj 
#>        "/tmp/Rtmpc2Wbkz/outputs/odm_micasense_dataset/micasense/odm_texturing_25d/odm_textured_model_geo.obj" 
```
