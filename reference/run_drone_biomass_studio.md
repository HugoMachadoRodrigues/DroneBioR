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

  Default project directory.

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
