# Read a full-resolution LAS/LAZ/COPC point cloud

The function prefers an uncompressed LAS file because it can be read
with the package's built-in reader. If a LAZ or COPC LAZ path is
supplied, the function first looks for the corresponding ODM `.las`
sidecar. If no LAS sidecar is available, it attempts to use an installed
external reader such as `lidR`.

## Usage

``` r
read_full_point_cloud(
  path,
  roi_polygon = NULL,
  max_points = Inf,
  chunk_size = 1e+05,
  preview_cache_dir = NULL
)
```

## Arguments

- path:

  Path to `.las`, `.laz` or `.copc.laz`.

- roi_polygon:

  Optional polygon ROI with `x` and `y` columns.

- max_points:

  Maximum number of points to return when no ROI is supplied.

- chunk_size:

  Number of point records per scan chunk for LAS files.

- preview_cache_dir:

  Optional writable directory used to cache a decimated PLY preview of
  the source. When supplied and no ROI is requested, the first
  successful read of a `.laz` / `.copc.laz` writes a PLY there keyed on
  the source's (size, mtime); subsequent reads of the same source
  consume the PLY directly, skipping the LASzip decompression entirely.

## Value

Data frame with full-resolution point coordinates inside the ROI.

## Examples

``` r
if (FALSE) { # \dontrun{
pc <- read_full_point_cloud("dense.laz")
} # }
```
