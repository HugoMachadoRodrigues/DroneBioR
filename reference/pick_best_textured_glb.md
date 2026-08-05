# Pick whichever glTF binary actually exists, preferring full 3D over 2.5D.

Pick whichever glTF binary actually exists, preferring full 3D over
2.5D.

## Usage

``` r
pick_best_textured_glb(project)
```

## Arguments

- project:

  A `dronebio_project` object.

## Value

Absolute path to an existing `.glb`, or the 3D path as default.

## Examples

``` r
project <- dronebio_project(project_dir = tempdir())
pick_best_textured_glb(project)
#> [1] "/tmp/Rtmpc1YYQq/outputs/odm_micasense_dataset/micasense/odm_texturing/odm_textured_model_geo.glb"
```
