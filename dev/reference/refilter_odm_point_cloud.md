# Re-clean an ODM point cloud without redoing the reconstruction

Reruns an existing ODM project from the `odm_filterpoints` stage with
new point-cloud filter settings. Everything before that stage –
`opensfm` and `openmvs`, which together are the bulk of the runtime and
disk of a run – is reused from the project directory, while meshing,
texturing, the DEMs and the orthophoto are rebuilt from the newly
filtered cloud.

## Usage

``` r
refilter_odm_point_cloud(
  project,
  pc_filter = 2.5,
  pc_sample = NULL,
  pc_rectify = FALSE,
  ...
)
```

## Arguments

- project:

  A `dronebio_project` whose ODM project directory already holds a
  finished (or at least past-`openmvs`) run.

- pc_filter, pc_sample, pc_rectify:

  Point-cloud cleanup settings, as in
  [`build_odm_args()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/build_odm_args.md).

- ...:

  Further arguments passed to
  [`run_odm_project()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/run_odm_project.md),
  e.g. `build_dsm`, `build_dtm`, `camera_type`.

## Value

Invisibly, the list returned by
[`run_odm_project()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/run_odm_project.md).

## Details

This is the loop to use when tuning `pc_filter` against the 3D view: the
reconstruction is paid for once, and each retune costs only the
downstream stages.

## Examples

``` r
if (FALSE) { # \dontrun{
project <- dronebio_project("~/flights/2026-05-01")
# First pass at ODM's default-ish setting, then tighten twice; only the
# stages after odm_filterpoints are recomputed each time.
refilter_odm_point_cloud(project, pc_filter = 2.5, build_dsm = TRUE)
refilter_odm_point_cloud(project, pc_filter = 1.5, pc_rectify = TRUE,
                         build_dsm = TRUE, build_dtm = TRUE)
} # }
```
