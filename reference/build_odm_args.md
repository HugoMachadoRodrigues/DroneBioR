# Build an ODM Docker command

Build an ODM Docker command

## Usage

``` r
build_odm_args(
  dataset_dir,
  project_name = "micasense",
  image = "opendronemap/odm",
  camera_type = c("multispectral", "rgb"),
  radiometric_calibration = NULL,
  orthophoto_resolution_cm = 5,
  max_concurrency = 4,
  fast_orthophoto = TRUE,
  build_dsm = FALSE,
  build_dtm = FALSE,
  pc_filter = 2.5,
  pc_sample = NULL,
  pc_rectify = FALSE,
  pc_las = FALSE,
  pc_csv = FALSE,
  pc_copc = FALSE,
  tiles = FALSE,
  three_d_tiles = FALSE,
  gltf = FALSE,
  skip_3dmodel = FALSE,
  skip_report = FALSE,
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

- camera_type:

  One of `"multispectral"` (MicaSense / Sequoia-style 5-band cameras,
  applies `--radiometric-calibration camera+sun` by default) or `"rgb"`
  (Sony, DJI, Phantom, generic RGB - skips the radiometric flag because
  it does not apply).

- radiometric_calibration:

  ODM radiometric-calibration value. When `NULL`, the default is chosen
  by `camera_type`: `"camera+sun"` for multispectral, omitted for RGB.
  Set to `"none"` to skip explicitly.

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

- pc_filter:

  Statistical outlier removal, in standard deviations from the local
  mean (`--pc-filter`). Applied in ODM's `odm_filterpoints` stage, which
  runs *before* meshing, texturing, the DEMs and the orthophoto, so it
  is the one knob that cleans reconstruction noise out of every
  downstream product at once. ODM's own default is 5, which is loose
  enough to leave floating specks and needles in the point cloud; the
  default here is 2.5. Use `0` to disable filtering.

- pc_sample:

  Optional thinning radius in metres (`--pc-sample`): keeps a single
  point per sphere of this radius. `NULL` (default) keeps every point.

- pc_rectify:

  Logical. Re-classify wrongly classified ground points and fill gaps
  (`--pc-rectify`). Worth enabling when the DTM matters.

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

- skip_3dmodel:

  Logical. Add `--skip-3dmodel` to skip the ODM `odm_meshing` and
  `mvs_texturing` stages. Saves 10-30 min on a typical 300-image flight
  when the consumer only needs DSM / DTM / orthomosaic. The skipped
  artifacts are the textured 3D `.obj` / `.glb` files.

- skip_report:

  Logical. Add `--skip-report` to skip ODM's PDF report generation
  stage. Saves ~1-2 min and avoids the known `gdal_translate` / numpy
  ABI crash inside some `opendronemap/odm` Docker images.

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
#> [3] "-v"                        "/tmp/RtmpHYBE3H:/datasets"
#> [5] "opendronemap/odm"          "--project-path"           

# RGB camera (Sony / DJI / Phantom): no radiometric calibration flag.
rgb_args <- build_odm_args(
  dataset_dir  = tempdir(),
  project_name = "rgb_flight",
  camera_type  = "rgb",
  build_dsm    = TRUE
)
"--radiometric-calibration" %in% rgb_args
#> [1] FALSE
```
