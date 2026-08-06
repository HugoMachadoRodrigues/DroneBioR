# Read a binary little-endian PLY point cloud sample

The vertex layout is taken from the header by
[`parse_ply_header()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/parse_ply_header.md),
so any binary little-endian PLY with scalar vertex properties is read
correctly – ODM's filtered cloud (x/y/z + normals + colours, 28 bytes)
and its georeferenced cloud (no normals, 16 bytes) alike. Colours are
matched by property name, never by position. Intentionally small and
dependency-free for app previews; full LAZ analytics should use
PDAL/lidR.

## Usage

``` r
read_ply_point_cloud(path, max_points = 50000, seed = 42)
```

## Arguments

- path:

  Path to a PLY file.

- max_points:

  Maximum number of points to return.

- seed:

  Random seed used when sampling.

## Value

A data frame with x, y, z, RGB values and display color.

## Details

`point_id` is the 1-based index of each returned point in the FULL
cloud, which is what makes a selection made on a decimated preview
usable to edit the file itself – see
[`write_ply_subset()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/write_ply_subset.md).

## Examples

``` r
if (FALSE) { # \dontrun{
pc <- read_ply_point_cloud("preview.ply", max_points = 50000)
} # }
```
