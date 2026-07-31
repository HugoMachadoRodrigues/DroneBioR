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
#>             product available size_mb
#> 1       orthomosaic     FALSE      NA
#> 2               dsm     FALSE      NA
#> 3               dtm     FALSE      NA
#> 4               chm     FALSE      NA
#> 5           dtm_csf     FALSE      NA
#> 6           chm_csf     FALSE      NA
#> 7   point_cloud_las     FALSE      NA
#> 8   point_cloud_laz     FALSE      NA
#> 9  point_cloud_copc     FALSE      NA
#> 10  point_cloud_ply     FALSE      NA
#> 11         mesh_ply     FALSE      NA
#> 12     textured_obj     FALSE      NA
#> 13 textured_obj_25d     FALSE      NA
#> 14     textured_glb     FALSE      NA
#> 15 textured_glb_25d     FALSE      NA
#> 16         tiles_3d     FALSE      NA
#> 17    map_tiles_dir     FALSE      NA
#> 18           report     FALSE      NA
#>                                                                                                           path
#> 1                    /tmp/RtmpsVBnJn/outputs/odm_micasense_dataset/micasense/odm_orthophoto/odm_orthophoto.tif
#> 2                                      /tmp/RtmpsVBnJn/outputs/odm_micasense_dataset/micasense/odm_dem/dsm.tif
#> 3                                      /tmp/RtmpsVBnJn/outputs/odm_micasense_dataset/micasense/odm_dem/dtm.tif
#> 4                                      /tmp/RtmpsVBnJn/outputs/odm_micasense_dataset/micasense/odm_dem/chm.tif
#> 5                                  /tmp/RtmpsVBnJn/outputs/odm_micasense_dataset/micasense/odm_dem/dtm_csf.tif
#> 6                                  /tmp/RtmpsVBnJn/outputs/odm_micasense_dataset/micasense/odm_dem/chm_csf.tif
#> 7       /tmp/RtmpsVBnJn/outputs/odm_micasense_dataset/micasense/odm_georeferencing/odm_georeferenced_model.las
#> 8       /tmp/RtmpsVBnJn/outputs/odm_micasense_dataset/micasense/odm_georeferencing/odm_georeferenced_model.laz
#> 9  /tmp/RtmpsVBnJn/outputs/odm_micasense_dataset/micasense/odm_georeferencing/odm_georeferenced_model.copc.laz
#> 10                    /tmp/RtmpsVBnJn/outputs/odm_micasense_dataset/micasense/odm_filterpoints/point_cloud.ply
#> 11                         /tmp/RtmpsVBnJn/outputs/odm_micasense_dataset/micasense/odm_meshing/odm_25dmesh.ply
#> 12            /tmp/RtmpsVBnJn/outputs/odm_micasense_dataset/micasense/odm_texturing/odm_textured_model_geo.obj
#> 13        /tmp/RtmpsVBnJn/outputs/odm_micasense_dataset/micasense/odm_texturing_25d/odm_textured_model_geo.obj
#> 14            /tmp/RtmpsVBnJn/outputs/odm_micasense_dataset/micasense/odm_texturing/odm_textured_model_geo.glb
#> 15        /tmp/RtmpsVBnJn/outputs/odm_micasense_dataset/micasense/odm_texturing_25d/odm_textured_model_geo.glb
#> 16                               /tmp/RtmpsVBnJn/outputs/odm_micasense_dataset/micasense/3d_tiles/tileset.json
#> 17                 /tmp/RtmpsVBnJn/outputs/odm_micasense_dataset/micasense/odm_orthophoto/odm_orthophoto_tiles
#> 18                               /tmp/RtmpsVBnJn/outputs/odm_micasense_dataset/micasense/odm_report/report.pdf
```
