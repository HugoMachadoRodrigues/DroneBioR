# Run OpenDroneMap on a DJI Mavic 3M flight, producing a 7-band ortho

DJI Mavic 3M captures 1 RGB (`_D.JPG`) and 4 single-band multispectral
TIFFs (`_MS_G/R/RE/NIR.TIF`) per shot — five independent cameras from
ODM's perspective. ODM cannot bundle-adjust the burst as one capture, so
this function orchestrates **five separate ODM runs**: one on the RGB
JPGs (full pipeline - orthomosaic, DSM, DTM, point cloud) and one per MS
band with `--fast-orthophoto` (orthomosaic only, calibrated to
reflectance via the DLS camera+sun flag). All five resulting orthos are
then resampled onto the RGB ortho's grid and stacked into a single
7-band GeoTIFF (`Red, Green, Blue, MS_G, MS_R, MS_RE, MS_NIR`) at
`<project_dir>/<odm_dataset_subdir>/odm_orthophoto_dji.tif`.

## Usage

``` r
run_odm_dji_mavic_3m(
  project,
  force = FALSE,
  odm_image = "opendronemap/odm",
  orthophoto_resolution_cm = 5,
  max_concurrency = NULL,
  ms_mode = c("multispectral", "per_band"),
  primary_band = "auto",
  build_dsm = TRUE,
  build_dtm = TRUE,
  pc_filter = 2.5,
  pc_sample = NULL,
  pc_rectify = FALSE,
  fast_orthophoto = FALSE,
  auto_boundary = TRUE,
  pc_las = FALSE,
  skip_3dmodel = TRUE,
  skip_report = TRUE,
  cleanup_intermediates = TRUE,
  harmonize = TRUE,
  canopy_ceiling = 18,
  use_ppk_mrk = TRUE,
  ppk_min_fix_quality = 4L,
  ppk_cli = "auto",
  rgb_extra_args = character(),
  ms_extra_args = character()
)
```

## Arguments

- project:

  A `dronebio_project` object whose `images_dir` contains DJI Mavic 3M
  raw images.

- force:

  Logical. Re-run every band even if outputs already exist. Useful after
  changing camera or radiometric parameters.

- odm_image:

  Docker image tag for the ODM container.

- orthophoto_resolution_cm:

  Orthophoto ground sampling distance.

- max_concurrency:

  Concurrent ODM workers per band. `NULL` (default) auto-detects the
  machine's physical cores.

- ms_mode:

  One of `"multispectral"` (default) or `"per_band"`. In
  `"multispectral"` mode the four `_MS_*.TIF` bands are processed in a
  **single** ODM run: ODM reads each image's DJI band metadata
  (`BandName`, `RigCameraIndex`, `CentralWavelength`) to group the bands
  by capture, reconstructs once, and co-registers the bands — so the
  resulting orthomosaic is pixel-aligned across bands and the spectral
  indices come out clean with identical coverage. This is only 2 ODM
  runs total (RGB + MS). In the legacy `"per_band"` mode each MS band
  gets its own independent ODM run (5 runs total); the band orthos then
  end up mis-registered, which yields noisy indices and different
  valid-data coverage per index. Use `"per_band"` only if ODM fails to
  recognise the multispectral grouping on your data.

- primary_band:

  Multispectral mode only. The band ODM uses to drive the
  reconstruction, passed as `--primary-band`. `"auto"` (default) lets
  ODM choose. Override with a band name (e.g. `"NIR"`) if the auto
  choice reconstructs poorly.

- build_dsm, build_dtm:

  Logical, default `TRUE`. Build the DSM / DTM on the RGB run. Set both
  to `FALSE` when you only need the orthomosaic + spectral indices —
  combined with `fast_orthophoto = TRUE` this is the fastest path.

- pc_filter, pc_sample, pc_rectify:

  Point-cloud cleanup, as in
  [`build_odm_args()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/build_odm_args.md).
  They are applied to the RGB run, which is the one whose geometry the
  DEMs and the stacked orthomosaic inherit; the four MS runs contribute
  calibrated radiance only and already use `--fast-orthophoto`.

- fast_orthophoto:

  Logical, default `FALSE`. When `TRUE`, the RGB run adds ODM's
  `--fast-orthophoto`, which skips the dense MVS reconstruction (often
  the single longest stage). The orthomosaic is built from the 2.5D mesh
  instead — much faster, but any DSM / DTM produced alongside are lower
  quality. Leave `FALSE` for scientifically defensible canopy heights.

- auto_boundary:

  Logical, default `TRUE`. Adds ODM's `--auto-boundary`, which crops the
  reconstruction and — crucially — the orthophoto canvas to a polygon
  derived from the camera GPS. This prevents a common failure on bounded
  aerial surveys where a few stray, mis-registered points scatter
  kilometres from the true footprint: without cropping, the orthophoto
  stage tries to render that whole sprawl at full resolution and the
  Docker container is OOM-killed (exit 137) even though the cropped DSM
  / DTM wrote fine. Set `FALSE` only if your survey genuinely has no
  usable camera GPS.

- pc_las:

  Logical. When `TRUE`, export the dense point cloud from the RGB run as
  a `.las` file (~640 MB for a 300-image flight). Default `FALSE` —
  DroneBioR's DSM/DTM/CHM pipeline does not need the LAS. Set to `TRUE`
  if you plan to run
  [`improve_dtm_csf()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/improve_dtm_csf.md)
  afterwards (which reads the LAS).

- skip_3dmodel:

  Logical. Default `TRUE`. Adds `--skip-3dmodel` to every ODM invocation
  so the `odm_meshing` and `mvs_texturing` stages — which together cost
  10-30 min per flight and only produce a textured `.obj` / `.glb` 3D
  model that DroneBioR's spectral pipeline never reads — are skipped.
  Set to `FALSE` if you want the textured 3D model for visualization.

- skip_report:

  Logical. Default `TRUE`. Adds `--skip-report` so ODM does not generate
  its PDF run report. Saves ~1-2 min per band and avoids the well-known
  `gdal_translate` / numpy ABI crash inside some `opendronemap/odm`
  Docker images.

- cleanup_intermediates:

  Logical. Default `TRUE`. After the 7-band stacked orthomosaic is
  written, perform two cleanups so the user is left with only the
  products DroneBioR's downstream pipeline actually consumes:

  - Delete the per-MS-band ODM project directories (`dji_ms_g/`,
    `dji_ms_r/`, `dji_ms_re/`, `dji_ms_nir/`). The per-band orthos
    already live in the 7-band stack.

  - Inside the canonical RGB project folder (`dji/`), strip every
    directory and file except `odm_dem/` (DSM/DTM, plus CHM later),
    `odm_orthophoto/` (RGB ortho + 7-band DJI stack), `log.json` (ODM's
    log) and `dronebior_odm.log` (our docker output). The discarded
    intermediates — `images/`, `opensfm/`, `openmvs/`,
    `odm_filterpoints/`, `odm_georeferencing/`, `odm_postprocess/` and
    the small JSON/TXT bookkeeping files — are never read by the
    downstream R pipeline.

  Set `FALSE` to keep everything for debugging.

- harmonize:

  Logical, default `TRUE`. After the run, make the DSM, DTM and CHM
  physically consistent and spike-free in place via
  [`harmonize_dem_products()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/harmonize_dem_products.md):
  the DTM is despiked, the CHM is derived non-negative and despiked, and
  the DSM is rebuilt as `DTM + CHM` so `CHM >= 0` and `DSM >= DTM` hold
  everywhere. The raw ODM rasters are preserved as `dsm_raw.tif` /
  `dtm_raw.tif`, and the canonical `dsm.tif` / `dtm.tif` / `chm.tif`
  become the clean versions so every downstream consumer (indices, Shiny
  app,
  [`build_chm_raster()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/build_chm_raster.md))
  uses them transparently.

- canopy_ceiling:

  Height (m) above the local canopy beyond which a cell is treated as a
  reconstruction tower during harmonization. Default `18` (pasture with
  scattered trees). Raise for forest, lower for open pasture.

- use_ppk_mrk:

  Logical. Default `TRUE`. When the source folder carries the DJI
  `_Timestamp.MRK` sidecar(s), parse them with
  [`parse_djim3m_mrk_folder()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/parse_djim3m_mrk_folder.md),
  resolve each per-band filename to its photo number, and write an ODM
  `geo.txt` so ODM uses the RTK / PPK positions instead of the EXIF GPS
  (which the Mavic 3M notoriously corrupts on altitude — that is the bug
  that makes OpenSfM diverge and `odm_orthophoto` OOM). ODM then runs
  with `--geo /datasets/<proj>/geo.txt --gps-accuracy 0.10`.

- ppk_min_fix_quality:

  Integer. Default `4` (RTK Float). Photos whose .MRK row reports a
  lower fix quality are dropped from the geo.txt — including them would
  let degraded positions destabilise the bundle adjustment. Set to `50`
  to demand RTK-Fixed-only, or `0` to keep everything.

- ppk_cli:

  Controls the **PPK CLI step that runs before ODM** when the source
  folder ships the `.bin` / `.nav` rover files. Three forms are
  accepted:

  `"auto"` (default)

  :   Probe the system for everything
      [`ppk_cli_rtklib_dji()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/ppk_cli_rtklib_dji.md)
      needs: `rnx2rtkp` on PATH (e.g. `brew install rtklib`), a DJI
      `.bin` -\> RINEX converter on PATH (tried in order:
      `klauppk_dji_to_rinex`, `klauppk`, `dji_to_rinex`, `djiparsekit`,
      `djirinexconverter`, `convbin`), and a base-station RINEX
      observation file located via (a) the `DRONEBIOR_PPK_BASE_OBS`
      environment variable, (b) the `dronebior.ppk_base_obs` R option,
      or (c) `<images_dir>/base/*.obs|*.YYo`. When every piece is in
      place, run full PPK before ODM. When anything is missing, emit a
      clear message naming what is missing and fall back to the
      .MRK-as-shipped path.

  `NULL` / `FALSE`

  :   Skip the CLI step. Use the .MRK as it ships from the drone (still
      better than EXIF GPS because the .MRK already holds RTK-quality
      positions when the drone had an RTK Fix in flight).

  A function

  :   Custom hook with signature
      `function(images_dir, bin_paths, nav_paths, mrk_paths)`. Use
      [`ppk_cli_rtklib_dji()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/ppk_cli_rtklib_dji.md)
      to build one with explicit paths if the auto-detect probes need an
      override.

  In every case DroneBioR then reads the (possibly improved) .MRK and
  writes an ODM `geo.txt` consumed via `--geo`.

- rgb_extra_args:

  Extra arguments appended to the **RGB** ODM run
  (`build_odm_args(..., extra_args = ...)`).

- ms_extra_args:

  Extra arguments appended to **each MS** ODM run.

## Value

A list with paths to the per-band orthos, the RGB DSM / DTM, and the
stacked 7-band orthomosaic.

## Details

Downstream
[`run_dronebio_workflow()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/run_dronebio_workflow.md)
/
[`read_multispectral_orthomosaic()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/read_multispectral_orthomosaic.md)
auto-detects the 7-layer stack and uses
[`default_dji_mavic_3m_band_map()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/default_dji_mavic_3m_band_map.md)
to expose Blue, Green, Red, RedEdge and NIR — Green/Red/RedEdge/NIR are
pulled from the calibrated MS bands, Blue from the RGB JPG channel (the
Mavic 3M does not capture a calibrated blue MS band).

## Examples

``` r
if (FALSE) { # \dontrun{
  project <- dronebio_project("/path/to/flight",
                              images_subdir      = ".",
                              odm_dataset_subdir = "odm_dji_dataset",
                              odm_project_name   = "dji")
  project$images_dir <- "/path/to/raw/images"
  result <- run_odm_dji_mavic_3m(project)
  result$stacked_orthomosaic
} # }
```
