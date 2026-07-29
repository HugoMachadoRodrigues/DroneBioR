# Register a flight in the time-series registry

Appends one row to the registry CSV. The `flight_id` is auto-generated
from the date plus a short hash of the project directory, so repeated
calls with the same date and project_dir are idempotent.

## Usage

``` r
register_flight(
  date,
  project_dir,
  notes = "",
  registry_path = default_flight_registry()
)
```

## Arguments

- date:

  Date or character that
  [`as.Date()`](https://rdrr.io/r/base/as.Date.html) can parse.

- project_dir:

  Project directory for the flight. Will be `normalizePath`-ed.

- notes:

  Optional free-text notes.

- registry_path:

  Path to the registry CSV. Defaults to
  [`default_flight_registry()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/default_flight_registry.md).

## Value

Invisibly returns the updated registry data frame.

## Examples

``` r
# Registering a flight only records a path, so a throwaway directory is
# enough to demonstrate it; in practice project_dir is the ODM project
# directory produced by the flight.
reg <- tempfile(fileext = ".csv")
register_flight(date = Sys.Date(), project_dir = tempdir(),
                registry_path = reg)
list_flights(reg)
#>           flight_id       date     project_dir notes
#> 1 20260729-4310fd67 2026-07-29 /tmp/Rtmp3cHaux    NA
```
