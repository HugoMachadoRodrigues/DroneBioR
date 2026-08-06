# Run ODM through Docker for a DroneBioR project

Run ODM through Docker for a DroneBioR project

## Usage

``` r
run_odm_project(
  project,
  run = TRUE,
  force = FALSE,
  camera_type = c("multispectral", "rgb"),
  auto_geoscan = TRUE,
  ...
)
```

## Arguments

- project:

  A `dronebio_project` object.

- run:

  Logical. If `FALSE`, only return the Docker command.

- force:

  Logical. Remove the existing orthomosaic before running.

- camera_type:

  `"multispectral"` (default; expects the MicaSense filename pattern via
  [`list_micasense_images()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/list_micasense_images.md))
  or `"rgb"` (uses
  [`list_aerial_images()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/list_aerial_images.md)
  which accepts any JPG/PNG/TIF without a band-id suffix).

- auto_geoscan:

  When `TRUE` (default) and `camera_type = "rgb"`, look for a
  `Metadata/Cameras_WGS84.txt` sibling of the source images folder (via
  [`detect_geoscan_metadata()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/detect_geoscan_metadata.md));
  if found, generate `<project_dir>/geo.txt` and append `--geo` +
  `--matcher-neighbors 8` to the ODM args. Sony RX1R / GeoScan datasets
  ship with empty EXIF GPS but per-image WGS84 records in this sidecar —
  without it, ODM reconstructs at scene origin.

- ...:

  Additional arguments passed to
  [`build_odm_args()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/build_odm_args.md).

## Value

A list with command, status and output orthomosaic path.

## Examples

``` r
if (FALSE) { # \dontrun{
project <- dronebio_project("/path/to/Drone_Biomass")
run_odm_project(project, build_dsm = TRUE, build_dtm = TRUE)
# Sony RX1R / DJI RGB flight - no radiometric calibration, permissive
# image lister:
run_odm_project(project, camera_type = "rgb")
} # }
```
