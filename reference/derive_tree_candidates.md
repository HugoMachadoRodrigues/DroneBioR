# Derive approximate tree candidates from a point cloud

This is a preview-grade canopy object detector. It bins high points into
a regular grid and estimates height, crown diameter and crown volume.
Scientific tree metrics should later use a CHM from DSM-DTM and
validated segmentation.

## Usage

``` r
derive_tree_candidates(
  points,
  grid_size = 4,
  min_height = 1.5,
  min_points = 5,
  max_trees = 80
)
```

## Arguments

- points:

  Data frame from
  [`read_ply_point_cloud()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/read_ply_point_cloud.md).

- grid_size:

  Grid size in source coordinate units.

- min_height:

  Minimum height above local ground proxy.

- min_points:

  Minimum number of points per candidate.

- max_trees:

  Maximum number of candidates to return.

## Value

A data frame of approximate tree objects.

## Examples

``` r
set.seed(1)
n <- 300
pts <- data.frame(
  x = c(rnorm(n/3, 5, 0.5), rnorm(n/3, 15, 0.5), rnorm(n/3, 25, 0.5)),
  y = c(rnorm(n/3, 5, 0.5), rnorm(n/3, 5, 0.5), rnorm(n/3, 15, 0.5)),
  z = c(rnorm(n/3, 55, 0.2), rnorm(n/3, 57, 0.2), rnorm(n/3, 54, 0.2))
)
derive_tree_candidates(pts)
#>     tree_id         x        y        z height_m crown_diameter_m
#> 2_0       1 14.935118 4.988297 57.40427 3.601457         3.898896
#> 0_0       2  5.190691 4.642583 55.53515 1.732335         2.485884
#>     crown_volume_m3 point_count
#> 2_0       21.499157          96
#> 0_0        4.203912           7
```
