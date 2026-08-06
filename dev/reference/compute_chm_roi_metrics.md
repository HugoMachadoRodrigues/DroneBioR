# Compute CHM metrics for a polygon ROI

Compute CHM metrics for a polygon ROI

## Usage

``` r
compute_chm_roi_metrics(chm, roi_polygon)
```

## Arguments

- chm:

  A
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  canopy height model.

- roi_polygon:

  Data frame with `x` and `y` polygon vertices.

## Value

One-row data frame with CHM area, height and volume metrics.

## Examples

``` r
dsm <- system.file("extdata", "dsm_subset.tif", package = "DroneBioR")
dtm <- system.file("extdata", "dtm_subset.tif", package = "DroneBioR")
chm <- build_chm_from_dsm_dtm(dsm, dtm)
roi <- data.frame(
  x = c(392004, 392012, 392012, 392004),
  y = c(3033004, 3033004, 3033012, 3033012)
)
compute_chm_roi_metrics(chm, roi)
#>   chm_cell_count chm_area_m2 chm_height_mean_m chm_height_max_m
#> 1            256          64          1.459604         3.003296
#>   chm_surface_volume_m3
#> 1              93.41468
```
