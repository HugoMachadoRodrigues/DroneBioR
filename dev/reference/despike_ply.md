# Remove reconstruction spikes from a point-cloud file

Reads a binary PLY in full, flags spikes with
[`despike_point_cloud()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/despike_point_cloud.md),
and writes the cleaned cloud back with
[`write_ply_subset()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/write_ply_subset.md)
– preserving every vertex property (colours, normals) so the result
still meshes. The default rewrites the file in place after snapshotting
the untouched original, so the app's "restore" and
rerun-from-`odm_meshing` flow work unchanged.

## Usage

``` r
despike_ply(path, out_path = path, backup = TRUE, backup_path = NULL, ...)
```

## Arguments

- path:

  Source binary PLY.

- out_path:

  Destination; defaults to `path` (in place).

- backup, backup_path:

  Passed to
  [`write_ply_subset()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/write_ply_subset.md);
  by default the untouched original is kept next to the cloud.

- ...:

  Despike tuning passed to
  [`despike_point_cloud()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/despike_point_cloud.md)
  (`methods`, `sor_k`, `sor_mult`, `cell`, `ground_q`, `smooth_w`,
  `height_cap`, `surface_mult`).

## Value

Invisibly, a list with `n_before`, `n_after`, `n_removed`.

## Examples

``` r
if (FALSE) { # \dontrun{
ply <- file.path(project$odm_project_dir, "odm_filterpoints", "point_cloud.ply")
despike_ply(ply, backup_path = sub("\\.ply$", ".original.ply", ply))
} # }
```
