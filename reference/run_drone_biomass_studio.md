# Start Drone Biomass Studio

Start Drone Biomass Studio

## Usage

``` r
run_drone_biomass_studio(
  project_dir = getwd(),
  port = NULL,
  launch.browser = TRUE,
  sample = FALSE,
  ...
)
```

## Arguments

- project_dir:

  Default project directory. Ignored when `sample = TRUE`.

- port:

  Optional local port.

- launch.browser:

  Logical. Open a browser window.

- sample:

  Logical. When `TRUE`, seed and open the bundled sample project via
  [`dronebio_sample_project()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/dronebio_sample_project.md)
  so the app is immediately clickable without real flight data. Useful
  for demos and first-time users.

- ...:

  Additional arguments passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

## Value

The result of
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

## Examples

``` r
if (FALSE) { # \dontrun{
# First-time / no data of your own:
run_drone_biomass_studio(sample = TRUE)

# Real project:
run_drone_biomass_studio(project_dir = "/path/to/Drone_Biomass")
} # }
```
