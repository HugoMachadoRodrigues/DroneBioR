# Configure PROJ paths for terra and sf

Some macOS R installations can load sf or terra without a valid pointer
to `proj.db`. This helper searches common locations and sets `PROJ_DATA`
and `PROJ_LIB` before spatial packages need CRS transformations.

Whatever this changes is recorded and restored when the package is
unloaded.

## Usage

``` r
configure_proj_database(verbose = FALSE, force = TRUE)
```

## Arguments

- verbose:

  Logical. Print the selected PROJ path when found.

- force:

  Logical. When `TRUE` (the default for a direct call), search and set
  the variables whatever they currently hold. When `FALSE`, an existing
  `PROJ_DATA` or `PROJ_LIB` that already points at a directory
  containing `proj.db` is left untouched, and nothing is warned about if
  no database is found. The package's load hook uses `FALSE`, so that
  attaching DroneBioR does not replace a configuration the user or
  another geospatial package chose.

## Value

Invisibly returns `TRUE` if `proj.db` was found, otherwise `FALSE`.

## Examples

``` r
configure_proj_database(verbose = FALSE)
```
