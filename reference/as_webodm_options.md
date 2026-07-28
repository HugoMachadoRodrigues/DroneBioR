# Translate `build_odm_args()` arguments into a WebODM options list

WebODM's `/api/projects/{id}/tasks/` endpoint takes options as
`[{"name": "dsm", "value": true}, ...]`. This helper maps the same
boolean / numeric flags the
[`build_odm_args()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/build_odm_args.md)
ODM-CLI driver uses so a Shiny form can drive either engine from one set
of inputs.

## Usage

``` r
as_webodm_options(
  camera_type = c("multispectral", "rgb"),
  orthophoto_resolution_cm = 5,
  fast_orthophoto = TRUE,
  build_dsm = FALSE,
  build_dtm = FALSE,
  pc_las = FALSE,
  pc_copc = FALSE,
  pc_csv = FALSE,
  tiles = FALSE,
  three_d_tiles = FALSE,
  gltf = FALSE,
  extra = list()
)
```

## Arguments

- camera_type:

  `"multispectral"` or `"rgb"`. Sets
  `radiometric-calibration = "camera+sun"` for multispectral; omits it
  for RGB.

- orthophoto_resolution_cm:

  Orthophoto resolution (cm).

- fast_orthophoto, build_dsm, build_dtm, pc_las, pc_copc, pc_csv, tiles,
  three_d_tiles, gltf:

  Same semantics as
  [`build_odm_args()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/build_odm_args.md).

- extra:

  A named list of additional WebODM options to merge.

## Value

A named list with one entry per WebODM option, ready for
[`webodm_submit_task()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/webodm_submit_task.md).
