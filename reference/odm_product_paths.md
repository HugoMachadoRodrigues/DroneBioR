# Return expected ODM product paths

Covers both the 3D texturing folder (`odm_texturing/`, produced when
`--fast-orthophoto` is off) and the 2.5D fallback
(`odm_texturing_25d/`). Use
[`pick_best_textured_obj()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/pick_best_textured_obj.md)
/
[`pick_best_textured_glb()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/pick_best_textured_glb.md)
to choose whichever variant actually exists on disk.

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
#>                   "/tmp/RtmpTFgppz/outputs/odm_micasense_dataset/micasense/odm_orthophoto/odm_orthophoto.tif" 
#>                                                                                                           dsm 
#>                                     "/tmp/RtmpTFgppz/outputs/odm_micasense_dataset/micasense/odm_dem/dsm.tif" 
#>                                                                                                           dtm 
#>                                     "/tmp/RtmpTFgppz/outputs/odm_micasense_dataset/micasense/odm_dem/dtm.tif" 
#>                                                                                                           chm 
#>                                     "/tmp/RtmpTFgppz/outputs/odm_micasense_dataset/micasense/odm_dem/chm.tif" 
#>                                                                                                       dtm_csf 
#>                                 "/tmp/RtmpTFgppz/outputs/odm_micasense_dataset/micasense/odm_dem/dtm_csf.tif" 
#>                                                                                                       chm_csf 
#>                                 "/tmp/RtmpTFgppz/outputs/odm_micasense_dataset/micasense/odm_dem/chm_csf.tif" 
#>                                                                                               point_cloud_las 
#>      "/tmp/RtmpTFgppz/outputs/odm_micasense_dataset/micasense/odm_georeferencing/odm_georeferenced_model.las" 
#>                                                                                               point_cloud_laz 
#>      "/tmp/RtmpTFgppz/outputs/odm_micasense_dataset/micasense/odm_georeferencing/odm_georeferenced_model.laz" 
#>                                                                                              point_cloud_copc 
#> "/tmp/RtmpTFgppz/outputs/odm_micasense_dataset/micasense/odm_georeferencing/odm_georeferenced_model.copc.laz" 
#>                                                                                               point_cloud_ply 
#>                    "/tmp/RtmpTFgppz/outputs/odm_micasense_dataset/micasense/odm_filterpoints/point_cloud.ply" 
#>                                                                                                      mesh_ply 
#>                         "/tmp/RtmpTFgppz/outputs/odm_micasense_dataset/micasense/odm_meshing/odm_25dmesh.ply" 
#>                                                                                                  textured_obj 
#>            "/tmp/RtmpTFgppz/outputs/odm_micasense_dataset/micasense/odm_texturing/odm_textured_model_geo.obj" 
#>                                                                                              textured_obj_25d 
#>        "/tmp/RtmpTFgppz/outputs/odm_micasense_dataset/micasense/odm_texturing_25d/odm_textured_model_geo.obj" 
#>                                                                                                  textured_glb 
#>            "/tmp/RtmpTFgppz/outputs/odm_micasense_dataset/micasense/odm_texturing/odm_textured_model_geo.glb" 
#>                                                                                              textured_glb_25d 
#>        "/tmp/RtmpTFgppz/outputs/odm_micasense_dataset/micasense/odm_texturing_25d/odm_textured_model_geo.glb" 
#>                                                                                                      tiles_3d 
#>                               "/tmp/RtmpTFgppz/outputs/odm_micasense_dataset/micasense/3d_tiles/tileset.json" 
#>                                                                                                 map_tiles_dir 
#>                 "/tmp/RtmpTFgppz/outputs/odm_micasense_dataset/micasense/odm_orthophoto/odm_orthophoto_tiles" 
#>                                                                                                        report 
#>                               "/tmp/RtmpTFgppz/outputs/odm_micasense_dataset/micasense/odm_report/report.pdf" 
```
