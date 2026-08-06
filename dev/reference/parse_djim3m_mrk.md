# Parse a DJI Mavic 3M `_Timestamp.MRK` file

The .MRK is tab-separated and uses a comma to glue each numeric value to
a one-letter label (e.g. `27.39880752,Lat`). Schema:

## Usage

``` r
parse_djim3m_mrk(mrk_path)
```

## Arguments

- mrk_path:

  Path to a `_Timestamp.MRK` file.

## Value

A data.frame with columns `photo_num`, `gps_time_sec`, `gps_week`,
`lat`, `lon`, `alt`, `lat_std`, `lon_std`, `alt_std`, `fix_quality`,
plus a `source` column carrying the .MRK basename so rows from multiple
.MRK files remain distinguishable when merged.

## Details

|     |                       |                                      |
|-----|-----------------------|--------------------------------------|
| col | example               | meaning                              |
| 1   | `1`                   | photo number (1-based)               |
| 2   | `494452.918260`       | GPS time of week, seconds            |
| 3   | `[2416]`              | GPS week, in brackets                |
| 4   | `-31,N`               | delta-N in millimetres (RTK offset)  |
| 5   | `0,E`                 | delta-E in millimetres               |
| 6   | `89,V`                | delta-V (vertical) in millimetres    |
| 7   | `27.3988,Lat`         | latitude in decimal degrees (WGS84)  |
| 8   | `-81.943,Lon`         | longitude in decimal degrees (WGS84) |
| 9   | `34.586,Ellh`         | ellipsoidal height in metres         |
| 10  | `0.026, 0.030, 0.083` | std dev (lat, lon, alt) in metres    |
| 11  | `50,Q`                | RTK fix quality (50 = Fixed)         |

Fix quality codes (DJI / NMEA convention): 0 = no fix, 1 = single, 2 =
DGPS, 4 = RTK Float, 5 = RTK Fixed, 50 = DJI's own "Fixed" code.
Anything \< 50 is degraded; anything \>= 50 is centimetre-class.

## Examples

``` r
if (FALSE) { # \dontrun{
  df <- parse_djim3m_mrk("DJI_..._Timestamp.MRK")
  head(df)
} # }
```
