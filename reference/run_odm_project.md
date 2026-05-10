# Run ODM through Docker for a DroneBioR project

Run ODM through Docker for a DroneBioR project

## Usage

``` r
run_odm_project(project, run = TRUE, force = FALSE, ...)
```

## Arguments

- project:

  A `dronebio_project` object.

- run:

  Logical. If `FALSE`, only return the Docker command.

- force:

  Logical. Remove the existing orthomosaic before running.

- ...:

  Additional arguments passed to
  [`build_odm_args()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/build_odm_args.md).

## Value

A list with command, status and output orthomosaic path.

## Examples

``` r
if (FALSE) { # \dontrun{
project <- dronebio_project("/path/to/Drone_Biomass")
run_odm_project(project, build_dsm = TRUE, build_dtm = TRUE)
} # }
```
