# Pick whichever textured mesh actually exists, preferring full 3D over 2.5D.

Pick whichever textured mesh actually exists, preferring full 3D over
2.5D.

## Usage

``` r
pick_best_textured_obj(project)
```

## Arguments

- project:

  A `dronebio_project` object.

## Value

Absolute path to an existing `.obj`, or the 3D path (which may not exist
yet) as a sensible default.

## Examples

``` r
project <- dronebio_project(project_dir = tempdir())
pick_best_textured_obj(project)
#> [1] "/tmp/RtmpfuJRDB/outputs/odm_micasense_dataset/micasense/odm_texturing/odm_textured_model_geo.obj"
```
