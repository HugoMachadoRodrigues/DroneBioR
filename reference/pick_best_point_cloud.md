# Pick the best available point cloud, in order: COPC \> LAZ \> LAS \> PLY.

Pick the best available point cloud, in order: COPC \> LAZ \> LAS \>
PLY.

## Usage

``` r
pick_best_point_cloud(project)
```

## Arguments

- project:

  A `dronebio_project` object.

## Value

Absolute path to the best point cloud found, or the COPC path as default
(typical preference) when nothing exists yet.

## Examples

``` r
project <- dronebio_project(project_dir = tempdir())
pick_best_point_cloud(project)
#> [1] "/tmp/RtmpXebL88/outputs/odm_micasense_dataset/micasense/odm_georeferencing/odm_georeferenced_model.copc.laz"
```
