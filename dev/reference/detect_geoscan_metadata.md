# Detect GeoScan metadata sibling of a source-images folder

GeoScan datasets ship as `<root>/Images/` (the JPGs) and
`<root>/Metadata/` (the per-image camera positions, the GCPs and the
GNSS lever-arm offset). When
[`run_odm_project()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/run_odm_project.md)
is pointed at the images folder, this helper walks up to a few levels
looking for a `Metadata/Cameras_WGS84.txt` sibling and returns the paths
it found.

## Usage

``` r
detect_geoscan_metadata(images_dir, max_levels_up = 3L)
```

## Arguments

- images_dir:

  Folder containing the drone JPGs / TIFFs.

- max_levels_up:

  How many parent directories to inspect for a `Metadata/` sibling.
  Default 3 covers the common `dataset/Images/*.JPG` layout.

## Value

`NULL` when nothing was found, otherwise a list with `cameras_path`,
`gnss_offset_path`, `gcps_path` and `metadata_dir`. Paths to
non-existent files (e.g. `GNSS_offset.txt` when only cameras are
provided) are still returned so the caller can decide what to do.

## Examples

``` r
if (FALSE) { # \dontrun{
  detect_geoscan_metadata("aerial_images_with_gcps/Images")
} # }
```
