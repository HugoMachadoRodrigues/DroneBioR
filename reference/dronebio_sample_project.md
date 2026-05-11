# Seed a clickable sample DroneBioR project from bundled fixtures

Builds a working `dronebio_project` in `target_dir` and seeds it with
the synthetic fixtures shipped under `inst/extdata/`. The seeded tree
mirrors the layout an OpenDroneMap run would produce, so downstream
functions (`read_multispectral_orthomosaic`, `build_chm_from_dsm_dtm`,
`summarize_odm_products`, `run_dronebio_workflow`, the Shiny app) work
against it with no extra configuration.

## Usage

``` r
dronebio_sample_project(target_dir = file.path(tempdir(), "DroneBioR-sample"))
```

## Arguments

- target_dir:

  Target project directory. Defaults to a stable folder under
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html) so multiple Shiny
  launches reuse the same seed.

## Value

A `dronebio_project` pointing at `target_dir`.

## Details

Useful for clicking through the package and the Shiny app before you
have real flight data of your own. The fixtures are intentionally tiny
(32x32 pixel multispectral subset, ~17 KB total) and **must not be used
for science**.

Files are copied with `overwrite = FALSE`, so re-running the function is
safe: it tops up missing files but does not clobber edits you have made
to the seeded folder.

## Examples

``` r
project <- dronebio_sample_project(target_dir = tempfile("dronebior-sample-"))
file.exists(project$odm_orthomosaic)
#> [1] TRUE
summarize_odm_products(project)
#>            product available size_mb
#> 1      orthomosaic      TRUE    0.01
#> 2              dsm      TRUE    0.00
#> 3              dtm      TRUE    0.00
#> 4  point_cloud_las     FALSE      NA
#> 5  point_cloud_laz     FALSE      NA
#> 6 point_cloud_copc     FALSE      NA
#> 7  point_cloud_ply     FALSE      NA
#> 8         mesh_ply     FALSE      NA
#> 9     textured_obj     FALSE      NA
#>                                                                                                                                        path
#> 1                   /tmp/RtmpHeCnen/dronebior-sample-22b1569ea894/outputs/odm_micasense_dataset/micasense/odm_orthophoto/odm_orthophoto.tif
#> 2                                     /tmp/RtmpHeCnen/dronebior-sample-22b1569ea894/outputs/odm_micasense_dataset/micasense/odm_dem/dsm.tif
#> 3                                     /tmp/RtmpHeCnen/dronebior-sample-22b1569ea894/outputs/odm_micasense_dataset/micasense/odm_dem/dtm.tif
#> 4      /tmp/RtmpHeCnen/dronebior-sample-22b1569ea894/outputs/odm_micasense_dataset/micasense/odm_georeferencing/odm_georeferenced_model.las
#> 5      /tmp/RtmpHeCnen/dronebior-sample-22b1569ea894/outputs/odm_micasense_dataset/micasense/odm_georeferencing/odm_georeferenced_model.laz
#> 6 /tmp/RtmpHeCnen/dronebior-sample-22b1569ea894/outputs/odm_micasense_dataset/micasense/odm_georeferencing/odm_georeferenced_model.copc.laz
#> 7                    /tmp/RtmpHeCnen/dronebior-sample-22b1569ea894/outputs/odm_micasense_dataset/micasense/odm_filterpoints/point_cloud.ply
#> 8                         /tmp/RtmpHeCnen/dronebior-sample-22b1569ea894/outputs/odm_micasense_dataset/micasense/odm_meshing/odm_25dmesh.ply
#> 9        /tmp/RtmpHeCnen/dronebior-sample-22b1569ea894/outputs/odm_micasense_dataset/micasense/odm_texturing_25d/odm_textured_model_geo.obj
```
