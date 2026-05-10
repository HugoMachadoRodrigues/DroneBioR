# Read a binary little-endian PLY point cloud sample

This reader supports the ODM/FPCFilter PLY layout used in the current
project: float x/y/z followed by uchar red/blue/green/views. It is
intentionally small and dependency-free for app previews; full LAZ
analytics should use PDAL/lidR.

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

## Examples

``` r
if (FALSE) { # \dontrun{
pc <- read_ply_point_cloud("preview.ply", max_points = 50000)
} # }
```
