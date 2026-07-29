# Build the ODM products from an edited point cloud

Reruns an existing ODM project from `odm_meshing`, so the mesh, texture,
DEMs and orthophoto are rebuilt from whatever
`odm_filterpoints/point_cloud.ply` currently holds – including a cloud
you cleaned by hand with
[`write_ply_subset()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/write_ply_subset.md)
or the app's 3D editor.

## Usage

``` r
rebuild_from_edited_cloud(project, ...)
```

## Arguments

- project:

  A `dronebio_project` with a finished (or at least
  past-`odm_filterpoints`) run.

- ...:

  Passed to
  [`run_odm_project()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/run_odm_project.md),
  e.g. `build_dsm`, `build_dtm`, `camera_type`.

## Value

Invisibly, the list returned by
[`run_odm_project()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/run_odm_project.md).

## Details

This works because ODM hands that single file from `odm_filterpoints` to
`odm_meshing`, and `odm_filterpoints` skips itself when the file already
exists and it is not the stage being rerun. Starting at `odm_meshing`
therefore leaves the edited cloud untouched, while `opensfm` and
`openmvs` – the bulk of the runtime – are reused.

## Examples

``` r
if (FALSE) { # \dontrun{
p <- dronebio_project("~/flights/2026-05-01")
ply <- file.path(p$odm_project_dir, "odm_filterpoints", "point_cloud.ply")
pc <- read_ply_point_cloud(ply, max_points = 5e5)
keep <- setdiff(seq_len(parse_ply_header(ply)$n_vertices),
                pc$point_id[pc$z > quantile(pc$z, 0.999)])
write_ply_subset(ply, ply, keep = keep)
rebuild_from_edited_cloud(p, build_dsm = TRUE, build_dtm = TRUE)
} # }
```
