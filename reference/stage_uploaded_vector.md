# Stage a Shiny multi-file upload back onto disk

Shiny writes uploaded files to its own temporary names (`0.dbf`,
`1.prj`, `2.shp`, `3.shx`), and GDAL cannot open a shapefile whose
sidecars no longer share the base name -
[`sf::st_read()`](https://r-spatial.github.io/sf/reference/st_read.html)
fails with `Unable to open .../2.shx`. This helper copies each upload
back under its original `name` so the parts line up again, and unpacks a
single `.zip` upload the same way.

## Usage

``` r
stage_uploaded_vector(name, datapath, dir = tempfile("dronebio_field_"))
```

## Arguments

- name:

  Character vector of original file names (`input$file$name`).

- datapath:

  Character vector of temporary paths (`input$file$datapath`), same
  length as `name`.

- dir:

  Directory to stage into. Created if missing.

## Value

The path of the dataset to open, with attributes `staged_dir` (the
directory holding every part) and `crs_known` (`FALSE` when a shapefile
arrived without a `.prj`).

## Examples

``` r
tmp <- tempfile(fileext = ".csv")
utils::write.csv(data.frame(sample_id = "S01", biomass_kgha = 1000,
                            x = 1, y = 2), tmp, row.names = FALSE)
staged <- stage_uploaded_vector("field.csv", tmp)
basename(staged)
#> [1] "field.csv"
```
