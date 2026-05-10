# Read an uncompressed LAS point cloud

This is a lightweight LAS reader used by the Shiny app when `lidR`,
`rlas` or PDAL are not available. It reads X/Y/Z, point classification
and RGB colors when they are present in common LAS point formats.
Compressed LAZ/COPC files are handled by
[`read_full_point_cloud()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/read_full_point_cloud.md)
when an uncompressed LAS sidecar is available or an external reader is
installed.

## Usage

``` r
read_las_point_cloud(
  path,
  roi_polygon = NULL,
  max_points = Inf,
  chunk_size = 1e+05
)
```

## Arguments

- path:

  Path to an uncompressed `.las` file.

- roi_polygon:

  Optional polygon ROI with `x` and `y` columns.

- max_points:

  Maximum number of points to return when no ROI is supplied.

- chunk_size:

  Number of point records per scan chunk.

## Value

Data frame with point coordinates and attributes.

## Examples

``` r
if (FALSE) { # \dontrun{
pc <- read_las_point_cloud("flight.las", max_points = 50000)
} # }
```
