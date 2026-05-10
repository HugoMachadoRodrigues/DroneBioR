# Build a canopy height model from DSM and DTM rasters

Build a canopy height model from DSM and DTM rasters

## Usage

``` r
build_chm_from_dsm_dtm(dsm_path, dtm_path)
```

## Arguments

- dsm_path:

  DSM GeoTIFF path.

- dtm_path:

  DTM GeoTIFF path.

## Value

A
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
with non-negative canopy height in meters.

## Examples

``` r
dsm <- system.file("extdata", "dsm_subset.tif", package = "DroneBioR")
dtm <- system.file("extdata", "dtm_subset.tif", package = "DroneBioR")
chm <- build_chm_from_dsm_dtm(dsm, dtm)
terra::minmax(chm)
#>        CHM_m
#> min 0.000000
#> max 3.003296
```
