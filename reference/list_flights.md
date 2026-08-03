# List flights registered in the time-series registry

List flights registered in the time-series registry

## Usage

``` r
list_flights(registry_path = default_flight_registry())
```

## Arguments

- registry_path:

  Path to the registry CSV. Defaults to
  [`default_flight_registry()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/default_flight_registry.md).

## Value

A data frame with columns `flight_id`, `date`, `project_dir`, `notes`.

## Examples

``` r
reg <- tempfile(fileext = ".csv")
list_flights(reg)  # empty registry
#> [1] flight_id          date               project_dir        notes             
#> [5] odm_dataset_subdir odm_project_name  
#> <0 rows> (or 0-length row.names)
```
