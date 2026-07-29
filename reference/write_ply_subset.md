# Write a filtered copy of a binary PLY, preserving its exact vertex layout

Copies `path` to `out_path` keeping only the vertices in `keep`, byte
for byte. Every property the source declares survives, including the
normals (`nx`, `ny`, `nz`) that ODM's screened-Poisson meshing needs –
dropping them would leave a file ODM still reads but meshes badly.

## Usage

``` r
write_ply_subset(
  path,
  out_path,
  keep,
  backup = TRUE,
  backup_path = NULL,
  chunk_size = 500000L
)
```

## Arguments

- path:

  Source `.ply`.

- out_path:

  Destination. Writing back over `path` is allowed and is the normal
  case when handing an edited cloud back to ODM; the temporary file
  makes that safe.

- keep:

  Either a logical vector of length `n_vertices`, or the integer indices
  of the vertices to keep (1-based, as `point_id` reports them).

- backup:

  When `TRUE` (the default) and `out_path` is the same file as `path`,
  the original is snapshotted first, unless that snapshot already exists
  – so the very first edit is always recoverable and later edits never
  overwrite that safety net.

- backup_path:

  Where to write that snapshot. `NULL` (the default) keeps the
  historical `<path>.orig`; pass an explicit path (for example
  `point_cloud.original.ply` beside the cloud) to keep a visible, named
  copy of the untouched reconstruction instead of a hidden dotfile.

- chunk_size:

  Vertices per streamed chunk.

## Value

Invisibly, the number of vertices written.

## Details

Vertices are streamed in chunks, so a 900 MB cloud does not have to fit
in memory, and the output is assembled in a temporary file and moved
into place only once it is complete. A run interrupted halfway therefore
leaves the original intact rather than a truncated cloud.

## Examples

``` r
if (FALSE) { # \dontrun{
pc <- read_ply_point_cloud("odm_filterpoints/point_cloud.ply", max_points = 5e4)
drop <- pc$point_id[pc$z > 400]          # obvious blunders
n <- write_ply_subset("odm_filterpoints/point_cloud.ply",
                      "odm_filterpoints/point_cloud.ply",
                      keep = setdiff(seq_len(2371187), drop))
} # }
```
