# Reconstruct up to the point cloud and stop

Runs ODM from the raw images through alignment (`opensfm`) and
densification (`openmvs`) and stops at `odm_filterpoints`, leaving
`odm_filterpoints/point_cloud.ply` ready to inspect and edit before any
product is derived from it. Pair it with
[`rebuild_from_edited_cloud()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/rebuild_from_edited_cloud.md),
which resumes at `odm_meshing` once the cloud is clean.

## Usage

``` r
build_point_cloud_only(
  project,
  pc_quality = "medium",
  pc_filter = 2.5,
  pc_sample = NULL,
  pc_rectify = FALSE,
  ...
)
```

## Arguments

- project:

  A `dronebio_project`.

- pc_quality:

  Densification detail: `"ultra"`, `"high"`, `"medium"` (ODM's default),
  `"low"` or `"lowest"`. Each step up costs roughly four times the time.

- pc_filter, pc_sample, pc_rectify:

  Cleanup applied in `odm_filterpoints`, as in
  [`build_odm_args()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/build_odm_args.md).
  The cloud you inspect is the filtered one, so these settings decide
  how much is left to remove by hand.

- ...:

  Passed to
  [`run_odm_project()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/run_odm_project.md),
  e.g. `camera_type`, `max_concurrency`.

## Value

Invisibly, a list with the `point_cloud` path and the result of
[`run_odm_project()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/run_odm_project.md).

## Details

`fast_orthophoto` is forced off and cannot be enabled: it skips the
dense reconstruction entirely, so there would be no dense cloud to
inspect – only the sparse SfM points, which is what produces the jagged
DSMs this workflow exists to avoid.

## Examples

``` r
if (FALSE) { # \dontrun{
p <- dronebio_project("~/flights/2026-05-01")
stage <- build_point_cloud_only(p, pc_quality = "medium", pc_filter = 2.5)
pc <- read_ply_point_cloud(stage$point_cloud, max_points = 2e5)
# ... inspect, then delete blunders with write_ply_subset(), then:
rebuild_from_edited_cloud(p, build_dsm = TRUE, build_dtm = TRUE)
} # }
```
