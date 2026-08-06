# Read GeoScan GNSS-to-camera offset file

`GNSS_offset.txt` records the lever-arm offset between the GNSS antenna
and the camera optical centre, in metres (X east, Y north, Z up). Pass
to
[`convert_geoscan_to_odm_geo()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/convert_geoscan_to_odm_geo.md)
to correct camera positions before writing geo.txt.

## Usage

``` r
read_geoscan_gnss_offset(path)
```

## Arguments

- path:

  Absolute path to `GNSS_offset.txt`.

## Value

A named numeric vector with `X`, `Y`, `Z` offsets in metres.

## Examples

``` r
if (FALSE) { # \dontrun{
  read_geoscan_gnss_offset("Metadata/GNSS_offset.txt")
} # }
```
