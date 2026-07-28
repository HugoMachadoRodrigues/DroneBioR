# Compute a time series of a custom flight summary

For each registered flight, opens the project at `flights$project_dir`
and applies `summary_fn(project)` to obtain a single numeric value.
Returns the values keyed by date so the result can be plotted directly
as a time series.

## Usage

``` r
flight_time_series(summary_fn, registry_path = default_flight_registry())
```

## Arguments

- summary_fn:

  A function that takes a `dronebio_project` and returns a single
  numeric value.

- registry_path:

  Path to the registry CSV.

## Value

A data frame with columns `date`, `value`, `flight_id`, `project_dir`.

## Details

Errors thrown by `summary_fn` for individual flights surface as `NA`
values, so a partial registry (e.g. a few flights with missing
orthomosaics) still produces a usable plot.

## Examples

``` r
if (FALSE) { # \dontrun{
# One row per flight, so register the projects first.
reg <- tempfile(fileext = ".csv")
register_flight("2026-04-01", "~/flights/2026-04-01", registry_path = reg)
register_flight("2026-05-01", "~/flights/2026-05-01", registry_path = reg)
flight_time_series(flight_ndvi_mean, registry_path = reg)
} # }
```
