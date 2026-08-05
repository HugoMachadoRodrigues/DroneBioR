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
  registry_path = default_flight_registry(),
  odm_dataset_subdir = NA_character_,
  odm_project_name = NA_character_
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

- odm_dataset_subdir, odm_project_name:

  Optional ODM sub-project coordinates for this flight (as passed to
  [`dronebio_project()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/dronebio_project.md)).
  Recorded so the flight can later be rebuilt with the exact ODM run
  that produced its products, rather than the `micasense` default. When
  left `NA`,
  [`flight_time_series()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/flight_time_series.md)
  falls back to the
  [`dronebio_project()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/dronebio_project.md)
  defaults.

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
#>           flight_id       date     project_dir notes odm_dataset_subdir
#> 1 20260805-5142c834 2026-08-05 /tmp/Rtmp8u3Ym3    NA                 NA
#>   odm_project_name
#> 1               NA
```
