# Default location of the DroneBioR flight registry

The registry is a CSV that stores one row per flight: a date, the
project directory for that flight, and an optional notes string. The
default location is the package's own directory under
`tools::R_user_dir("DroneBioR", "data")`, so the same registry can be
reused across separate R sessions. It is created on first write, not by
asking where it is.

## Usage

``` r
default_flight_registry()
```

## Value

Absolute path to the default registry CSV.

## Examples

``` r
default_flight_registry()
#> [1] "/home/runner/.local/share/R/DroneBioR/flights.csv"
```
