# Export a selected point cloud ROI

Export a selected point cloud ROI

## Usage

``` r
export_point_selection(
  points,
  metrics,
  profile,
  output_dir,
  label = "selection",
  roi_polygon = NULL
)
```

## Arguments

- points:

  Selected points.

- metrics:

  One-row data frame from
  [`compute_selection_metrics()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/compute_selection_metrics.md).

- profile:

  Data frame from
  [`compute_vertical_profile()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/compute_vertical_profile.md).

- output_dir:

  Output directory.

- label:

  ROI label.

- roi_polygon:

  Optional polygon ROI exported with the point metrics.

## Value

Named character vector with written file paths.

## Examples

``` r
set.seed(1)
pts <- data.frame(
  x = runif(50, 0, 10),
  y = runif(50, 0, 10),
  z = runif(50, 50, 55)
)
pts <- add_point_heights(pts)
m <- compute_selection_metrics(pts)
p <- compute_vertical_profile(pts)
export_point_selection(pts, m, p, output_dir = tempfile("sel-"))
#>                                                                            points 
#>           "/tmp/RtmpKPK8wl/sel-227b7cf540d3/selection_20260511_201704_points.csv" 
#>                                                                           metrics 
#>          "/tmp/RtmpKPK8wl/sel-227b7cf540d3/selection_20260511_201704_metrics.csv" 
#>                                                                  vertical_profile 
#> "/tmp/RtmpKPK8wl/sel-227b7cf540d3/selection_20260511_201704_vertical_profile.csv" 
```
