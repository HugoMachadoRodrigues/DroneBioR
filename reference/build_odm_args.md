# Build an ODM Docker command

Build an ODM Docker command

## Usage

``` r
build_odm_args(
  dataset_dir,
  project_name = "micasense",
  image = "opendronemap/odm",
  radiometric_calibration = "camera+sun",
  orthophoto_resolution_cm = 5,
  max_concurrency = 4,
  fast_orthophoto = TRUE,
  build_dsm = FALSE,
  build_dtm = FALSE,
  pc_las = FALSE,
  pc_csv = FALSE,
  pc_copc = FALSE,
  tiles = FALSE,
  three_d_tiles = FALSE,
  gltf = FALSE,
  rerun_from = NULL,
  end_with = NULL,
  extra_args = character()
)
```

## Arguments

- dataset_dir:

  Host folder mounted to `/datasets`.

- project_name:

  ODM project name inside `dataset_dir`.

- image:

  Docker image name.

- radiometric_calibration:

  ODM radiometric calibration option.

- orthophoto_resolution_cm:

  Orthophoto resolution in centimeters.

- max_concurrency:

  Maximum concurrent ODM workers.

- fast_orthophoto:

  Logical. Add `--fast-orthophoto`.

- build_dsm:

  Logical. Add DSM generation.

- build_dtm:

  Logical. Add DTM generation options.

- pc_las:

  Logical. Export LAS point cloud.

- pc_csv:

  Logical. Export CSV point cloud.

- pc_copc:

  Logical. Export COPC point cloud.

- tiles:

  Logical. Export web map tiles.

- three_d_tiles:

  Logical. Export 3D tiles.

- gltf:

  Logical. Export glTF model.

- rerun_from:

  Optional ODM stage.

- end_with:

  Optional ODM end stage.

- extra_args:

  Additional ODM arguments.

## Value

Character vector of arguments for `system2("docker", args)`.

## Examples

``` r
args <- build_odm_args(
  dataset_dir = tempdir(),
  project_name = "demo",
  build_dsm = TRUE,
  build_dtm = TRUE
)
head(args)
#> [1] "run"                       "--rm"                     
#> [3] "-v"                        "/tmp/RtmpICcyiR:/datasets"
#> [5] "opendronemap/odm"          "--project-path"           
```
