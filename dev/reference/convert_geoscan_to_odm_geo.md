# Convert GeoScan camera metadata to ODM `geo.txt` and write to disk

Builds an OpenDroneMap-compatible `geo.txt` from GeoScan's per-image
WGS84 records. Output schema (one image per line, space-separated):

## Usage

``` r
convert_geoscan_to_odm_geo(cameras_path, geo_txt_path, gnss_offset = NULL)
```

## Arguments

- cameras_path:

  Path to `Cameras_WGS84.txt`.

- geo_txt_path:

  Output path for the ODM `geo.txt`.

- gnss_offset:

  Either a path to `GNSS_offset.txt` or a named numeric vector
  `c(X, Y, Z)` in metres. Pass `NULL` to skip the correction.

## Value

Invisibly, the path written.

## Details

    EPSG:4326
    image_name lon lat H yaw pitch roll horz_acc vert_acc

GeoScan stores lat/lon (in that order); ODM expects lon/lat. We
translate accordingly. The optional GNSS-to-camera offset is applied as
a small (sub-metre) correction in metres at the local latitude.

## Examples

``` r
if (FALSE) { # \dontrun{
  convert_geoscan_to_odm_geo(
    "Metadata/Cameras_WGS84.txt",
    "aerial_geoscan/geo.txt",
    gnss_offset = "Metadata/GNSS_offset.txt"
  )
} # }
```
