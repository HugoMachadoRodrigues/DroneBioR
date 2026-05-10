# Configure PROJ paths for terra and sf

Some macOS R installations can load sf or terra without a valid pointer
to `proj.db`. This helper searches common locations and sets `PROJ_DATA`
and `PROJ_LIB` before spatial packages need CRS transformations.

## Usage

``` r
configure_proj_database(verbose = FALSE)
```

## Arguments

- verbose:

  Logical. Print the selected PROJ path when found.

## Value

Invisibly returns `TRUE` if `proj.db` was found, otherwise `FALSE`.

## Examples

``` r
configure_proj_database(verbose = FALSE)
#> Warning: Could not find proj.db. Install PROJ or set PROJ_DATA/PROJ_LIB to the directory containing proj.db.
```
