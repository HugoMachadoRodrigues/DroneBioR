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
reg <- tempfile(fileext = ".csv")
project <- dronebio_sample_project(target_dir = tempfile("ts-flight-"))
register_flight(Sys.Date(), project$project_dir, registry_path = reg)
#> Warning: NAs produced by integer overflow
ts <- flight_time_series(flight_ndvi_mean, registry_path = reg)
ts
#>         date     value   flight_id                            project_dir
#> 1 2026-05-11 0.5848023 20260511-NA /tmp/RtmpK129zh/ts-flight-227016aa811a
```
