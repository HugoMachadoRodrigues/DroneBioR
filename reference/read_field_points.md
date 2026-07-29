# Read field sample points from a CSV or vector file

The single ingest entry point for the Field Models tab. Column names are
never renamed here - see
[`prepare_field_table()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/prepare_field_table.md)
for that.

## Usage

``` r
read_field_points(path, crs = NULL, x_col = NULL, y_col = NULL, layer = NULL)
```

## Arguments

- path:

  Path to a CSV or vector dataset (see
  [`stage_uploaded_vector()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/stage_uploaded_vector.md)).

- crs:

  CRS of the coordinates: an EPSG code, a WKT / PROJ string, or `NULL`.
  Required for CSV input, and used for a vector file whose CRS is
  undefined (a shapefile with no `.prj`).

- x_col, y_col:

  Coordinate columns for CSV input. Guessed with
  [`field_source_columns()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/field_source_columns.md)
  when `NULL`.

- layer:

  Optional layer name for multi-layer sources.

## Value

An `sf` POINT layer. When polygons were reduced to centroids the result
carries a `centroid_note` attribute.

## Details

A CSV **must** be given a `crs`: bare `x` / `y` numbers carry no spatial
meaning, and adopting the orthomosaic's CRS for what are actually
longitude / latitude degrees produces silently wrong extractions.

## Examples

``` r
path <- system.file("extdata", "field_samples.csv", package = "DroneBioR")
pts <- read_field_points(path, crs = 32617)
nrow(pts)
#> [1] 8
```
