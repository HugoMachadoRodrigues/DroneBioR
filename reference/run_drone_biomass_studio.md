# Start Drone Biomass Studio

Start Drone Biomass Studio

## Usage

``` r
run_drone_biomass_studio(
  project_dir = getwd(),
  port = NULL,
  launch.browser = TRUE,
  ...
)
```

## Arguments

- project_dir:

  Default project directory. Should point at the root of a DroneBioR
  project (i.e. the folder that contains `outputs/odm_*` from a previous
  engine run, or the parent of a folder where you will place real flight
  images and run the engine from inside the app).

- port:

  Optional local port.

- launch.browser:

  Logical. Open a browser window.

- ...:

  Additional arguments passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

## Value

The result of
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

## Examples

``` r
if (FALSE) { # \dontrun{
run_drone_biomass_studio(project_dir = "/path/to/Drone_Biomass")
} # }
```
