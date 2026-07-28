# Read GeoScan-style camera position file

GeoScan drones ship Sony JPGs without GPS EXIF; instead, an external
`Cameras_WGS84.txt` records per-image WGS84 lat/lon/H + IMU + std-dev.
This reader parses the tab-separated file into a data frame the ODM
geo-txt writer understands.

## Usage

``` r
read_geoscan_cameras(path)
```

## Arguments

- path:

  Absolute path to `Cameras_WGS84.txt`.

## Value

A data frame with columns `file`, `lat`, `lon`, `H`, `roll`, `pitch`,
`yaw`, `time`, `std_n`, `std_e`, `std_u`, `std_hz`.

## Examples

``` r
if (FALSE) { # \dontrun{
  read_geoscan_cameras("Metadata/Cameras_WGS84.txt")
} # }
```
