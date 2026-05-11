# Default location of the DroneBioR flight registry

The registry is a CSV that stores one row per flight: a date, the
project directory for that flight, and an optional notes string. The
default location lives under the user's home directory so the same
registry can be reused across separate R sessions.

## Usage

``` r
default_flight_registry()
```

## Value

Absolute path to the default registry CSV.

## Examples

``` r
default_flight_registry()
#> [1] "/home/runner/.dronebior/flights.csv"
```
