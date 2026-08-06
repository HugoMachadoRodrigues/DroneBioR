# Stock summary helpers for [`flight_time_series()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/flight_time_series.md)

Each helper accepts a `dronebio_project` and returns a single numeric
value, suitable for use as the `summary_fn` argument of
[`flight_time_series()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/flight_time_series.md).

## Usage

``` r
flight_ndvi_mean(project)

flight_biomass_proxy_mean(project)

flight_chm_mean(project)
```

## Arguments

- project:

  A `dronebio_project` object.

## Value

A single numeric value, or `NA` when the underlying product is missing.

## Examples

``` r
if (FALSE) { # \dontrun{
# Each helper reads the products of one flight, so point dronebio_project()
# at a directory that already holds ODM output.
project <- dronebio_project("~/flights/2026-05-01")
flight_ndvi_mean(project)
flight_biomass_proxy_mean(project)
flight_chm_mean(project)
} # }
```
