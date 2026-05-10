# Compute selection metrics for a point cloud ROI

Compute selection metrics for a point cloud ROI

## Usage

``` r
compute_selection_metrics(points, voxel_size = 0.5)
```

## Arguments

- points:

  Selected points with `x`, `y`, `z` and optionally `height_m`.

- voxel_size:

  Voxel size in meters for occupied-volume approximation.

## Value

One-row data frame with distance, area, height and volume metrics.

## Examples

``` r
set.seed(1)
pts <- data.frame(
  x = runif(100, 0, 10),
  y = runif(100, 0, 10),
  z = runif(100, 50, 55)
)
compute_selection_metrics(pts, voxel_size = 0.5)
#>   n_points footprint_area_m2 max_crown_diameter_m  z_min_m  z_max_m
#> 1      100          82.83169             12.23773 50.13894 54.90782
#>   height_min_m height_mean_m height_max_m occupied_volume_m3
#> 1            0      1.857693      4.58609                 12
```
