# Probe a field-sample file for columns, CRS and a column-mapping guess

Reads only what is needed to populate the column-mapping wizard. The
returned `columns` are the names as they actually come back from the
driver - DBF truncates to 10 characters, so downstream code must never
match hard-coded names.

## Usage

``` r
field_source_columns(path, layer = NULL)
```

## Arguments

- path:

  Path to a CSV or vector dataset.

- layer:

  Optional layer name for multi-layer sources (GeoPackage).

## Value

A list with `kind` (`"table"` or `"vector"`), `columns`, `classes`,
`n_missing`, `n_rows`, `crs`, `epsg`, `has_geometry`, `geom_type` and
`guess` (`id` / `biomass` / `x` / `y`).

## Examples

``` r
path <- system.file("extdata", "field_samples.csv", package = "DroneBioR")
probe <- field_source_columns(path)
probe$kind
#> [1] "table"
probe$guess$biomass
#> [1] "biomass_kgha"
```
