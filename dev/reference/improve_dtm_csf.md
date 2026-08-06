# Re-classify ground via CSF and rebuild the DTM (and optionally the CHM)

ODM's default SMRF ground classification with `--smrf-threshold 0.5` is
conservative on dense canopy and often labels the entire scene as
ground, producing a DTM nearly identical to the DSM and a CHM around
zero. Cloth Simulation Filter (CSF) from lidR handles dense vegetation
more reliably. This helper:

## Usage

``` r
improve_dtm_csf(
  project,
  resolution = 0.5,
  class_threshold = 0.5,
  cloth_resolution = 0.5,
  rigidness = 1L,
  rebuild_chm = TRUE,
  dtm_filename = "dtm_csf.tif",
  chm_filename = "chm_csf.tif"
)
```

## Arguments

- project:

  A `dronebio_project` object.

- resolution:

  DTM grid spacing in metres. Default 0.5 m, plenty of detail for
  vegetation work while keeping the rasterisation fast.

- class_threshold, cloth_resolution, rigidness:

  Passed straight through to
  [`classify_ground_csf()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/classify_ground_csf.md).
  Defaults are sensible for moderate-to-dense canopy; lower
  `class_threshold` (0.1-0.3) and smaller `cloth_resolution` (0.3) for
  sparser vegetation.

- rebuild_chm:

  Logical. Build a new CHM from DSM minus the CSF DTM and write it to
  `chm_filename` (without touching the original `chm.tif`).

- dtm_filename:

  Output filename for the CSF DTM. Default `"dtm_csf.tif"`. Pass
  `"dtm.tif"` to overwrite the SMRF DTM in place (legacy behaviour from
  \<= 0.4.0).

- chm_filename:

  Output filename for the CSF CHM. Default `"chm_csf.tif"`. Pass
  `"chm.tif"` to overwrite the SMRF CHM in place.

## Value

Invisibly returns the absolute path to the new DTM, or a
`list(dtm = ..., chm = ...)` when `rebuild_chm = TRUE`.

## Details

1.  Reads the LAZ / LAS point cloud (preferring the local cache when
    migration has run).

2.  Runs CSF via
    [`classify_ground_csf()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/classify_ground_csf.md).

3.  Rasterises the ground points to a new DTM at the requested
    resolution.

4.  Writes the CSF DTM **alongside** the original (as `dtm_csf.tif` by
    default) — the SMRF DTM produced by ODM is preserved so users can
    compare both methods.

5.  Optionally builds a CHM from DSM + CSF DTM and writes it next to the
    new DTM (`chm_csf.tif` by default), again preserving the original
    `chm.tif`.

Both new files are exposed by
[`odm_product_paths()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/odm_product_paths.md)
under the keys `dtm_csf` and `chm_csf` so downstream code can discover
them.

## Examples

``` r
if (FALSE) { # \dontrun{
  project <- dronebio_project("~/aerial_geoscan_project")
  improve_dtm_csf(project, resolution = 0.5, rebuild_chm = TRUE)
  # Original dtm.tif / chm.tif are preserved; new files at
  # dtm_csf.tif / chm_csf.tif in the same directory.
} # }
```
