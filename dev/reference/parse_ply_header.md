# Parse the header of a binary little-endian PLY file

Returns the vertex layout so readers and writers agree on the record
stride. Assuming a fixed stride is how a reader silently produces
garbage: ODM's `odm_filterpoints/point_cloud.ply` carries x/y/z +
nx/ny/nz + red/blue/green/views, 28 bytes per vertex, while its
`odm_georeferencing` cloud omits the normals and uses 16.

## Usage

``` r
parse_ply_header(path)
```

## Arguments

- path:

  Path to a `.ply` file.

## Value

A list with `header_end` (bytes), `header_text`, `n_vertices`, `props`
(a data frame of `name`, `type`, `size`, `offset`) and `stride`.

## Examples

``` r
if (FALSE) { # \dontrun{
h <- parse_ply_header("odm_filterpoints/point_cloud.ply")
h$stride
h$props$name
} # }
```
