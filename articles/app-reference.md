# Drone Biomass Studio: full app reference

## How to read this reference

The Shiny app `Drone Biomass Studio` (launched with
[`run_drone_biomass_studio()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/run_drone_biomass_studio.md))
is organised as **seven top-level tabs**. Each tab is a `bslib` page
with a sidebar and a main area; the main area is built out of `card()`
containers — what this document calls **boxes**.

For every box the reference lists:

- **Purpose** — what the box does in one line.
- **Inputs** — every control that feeds the box, with allowed values and
  defaults.
- **Outputs** — every artefact the box writes to the UI or to disk.
- **Science** — the formula or algorithm, with the function and source
  line that implements it.
- **Accuracy & precision** — what the user can expect from the number,
  and where it depends on calibration that the user must supply.
- **Source of data** — sensor or external service the data ultimately
  comes from.

When a number depends on field calibration, the entry says so explicitly
instead of quoting an unverified figure. Anything marked **“to be
measured”** is a value DroneBioR does not yet validate internally for
this project; it is *not* missing science, it is missing local
validation data.

------------------------------------------------------------------------

## Tab 1 — Processing Engine

Sidebar: project root, source images folder, ODM project picker, camera
type (multispectral / RGB), preset, engine (local Docker / WebODM REST),
max concurrency.

### Box — Processing workflow

- **Purpose.** Visualises the OpenDroneMap stage list for the chosen
  preset.
- **Inputs.** `engine`, `preset`, `camera_type`.
- **Outputs.** Reactive HTML in `processing_workflow`.
- **Science.** Mirrors the ODM stage sequence
  (`opensfm → mvs_texturing → odm_dem → odm_orthophoto`).
- **Accuracy.** Informational; no measurement.
- **Source.** OpenDroneMap documentation
  (<https://docs.opendronemap.org/>).

### Box — What this preset creates

- **Purpose.** Lists the file products a preset produces (orthophoto,
  DSM, DTM, LAS, COPC, tiles, 3D tiles).
- **Inputs.** `preset` only.
- **Outputs.** `preset_outputs` table.
- **Science.** Built from
  [`build_odm_args()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/build_odm_args.md)
  flag inspection (`R/engine_odm.R:46-128`).
- **Accuracy.** Deterministic — the table describes the flags, not the
  expected quality of each product.

### Box — ODM Docker command

- **Purpose.** Shows the exact `docker run` command DroneBioR will
  execute, including bind mounts.
- **Inputs.** all sidebar fields.
- **Outputs.** `odm_command` (verbatim text).
- **Science.** Container is `opendronemap/odm`; dataset mounted at
  `/datasets`; project name supplied via positional argument
  (`R/engine_odm.R:46-128`, `R/engine_odm.R:184-227`).
- **Accuracy.** Exact command; verified before launch.

### Box — ODM run progress

- **Purpose.** Tails the live ODM log and renders per-stage progress.
- **Inputs.** `odm_log_path` (defaults to
  `/tmp/dronebior_logs/odm.log`).
- **Outputs.** `odm_progress_ui` and tail of log.
- **Science.** Stage detection by regex on the ODM log; ETA from
  `odm_eta_estimate()` (`R/odm_eta.R`) which blends image count, GSD and
  historical run times.
- **Accuracy of ETA.** Heuristic; better than ±30 % once two stages have
  completed, weaker on the first stage.
- **Source.** ODM stdout/stderr.

### Box — Product status

- **Purpose.** After a run, lists which products are now on disk.
- **Outputs.** `processing_products` table
  ([`odm_product_paths()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/odm_product_paths.md),
  [`summarize_odm_products()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/summarize_odm_products.md)).

------------------------------------------------------------------------

## Tab 2 — GIS Workspace

Sidebar accordions: **Project paths**, **Map layers**, **Display**,
**Annotations**, **ROI comparison**.

### Box — Map (Leaflet, fullscreen card)

- **Purpose.** 2D web-Mercator map for inspecting layers, drawing ROIs,
  measuring distances, pinning annotations.
- **Inputs.** Layer toggles (orthomosaic, CHM, biomass proxies, slope,
  hillshade), draw / measure toolbar, basemap selector.
- **Outputs.** `gis_map` (Leaflet), plus reactive metric strip
  (`metric_images`, `metric_ortho`, `metric_cloud`, `metric_dem`).
- **Science.** Raster overlays are warped to EPSG:3857 on the fly via
  [`terra::project()`](https://rspatial.github.io/terra/reference/project.html);
  vector ROIs stored as `sf` and serialized as GeoJSON.
- **Accuracy & precision.**
  - Pixel position: bounded by the orthomosaic CRS metadata. With ODM
    - good GCPs / RTK, horizontal RMSE is **2–5 × GSD** (≈ 0.10–0.25 m
      at a 5 cm/px ortho); without GCPs, GPS-EXIF-only photogrammetry
      typically delivers **1–5 m** absolute (relative accuracy still 2–5
      × GSD). See OpenDroneMap accuracy notes,
      <https://docs.opendronemap.org/>.
  - Measure tool: planar distance on the projected CRS; great-circle
    distance via Leaflet on geographic CRS. Precision is shown to 1 mm
    but is meaningful only to the orthomosaic accuracy.
- **Source.** Basemaps:
  - **Satellite** — Esri ArcGIS World Imagery
    (`https://server.arcgisonline.com/.../World_Imagery`), licence: Esri
    Community Maps.
  - **Light basemap** — CartoDB Positron (OSM-derived).

### Box — Project actions / cross-tab CTAs

- **Purpose.** Send the active ROI / layer selection to the 3D Modeling
  or Spectral Analytics tabs.
- **Outputs.** Internal reactive bridges; no disk artefacts.

### Box — Map measurement

- **Purpose.** Summarises the active polyline / polygon: length,
  perimeter, area.
- **Science.** For geographic CRS, area uses the WGS84 ellipsoid
  ([`sf::st_area()`](https://r-spatial.github.io/sf/reference/geos_measures.html)
  with `s2`); for projected CRS, planar geometry.
- **Precision.** Reported to 0.01 m or 0.01 m² but tied to the Leaflet
  click resolution (~0.5 px at the current zoom).

### Box — ROI comparison

- **Purpose.** Side-by-side per-ROI summary for every loaded raster
  (mean, sd, min, max, area).
- **Outputs.** `roi_comparison_table`.
- **Science.** `terra::extract(..., fun = ...)`; weighted by the
  intersection area when an ROI partially covers a cell.

### Box — Available processing products

- **Purpose.** Lists the rasters under the project’s `outputs/` folder
  that DroneBioR can map (orthomosaic, DSM, DTM, CHM, biomass proxies,
  panel ROI mask).
- **Outputs.** `product_table` with on-disk paths and resolutions.

------------------------------------------------------------------------

## Tab 3 — 3D Modeling

Sidebar accordions: **Scene source**, **GIS Workspace ROI**,
**Display**, **Classification**, **Tree detection**, **Volume math**.

### Top — Metric strip

Live counts for points loaded, points in active selection, trees
detected, surfaces drawn.

### Box — 3D viewer (Three.js, embedded)

- **Purpose.** WebGL point-cloud viewer with box / lasso / polygon
  selection.
- **Inputs.**
  - `viewer_cloud_source` — `"Full georeferenced LAS/LAZ/COPC sample"`
    or `"PLY preview fallback"` (default).
  - `viewer_max_points` — decimation target. Default **50 000** for PLY
    preview, configurable.
  - `show_orthomosaic_basemap` — toggles the ground-plane texture.
  - `show_dsm_drape` — drapes the DSM under the points.
- **Outputs.** `point_cloud_three`, `viewer_basemap`, selection events
  back to R (`selected_points`).
- **Science.**
  - Point cloud is sent as a packed Float32Array (binary) — not inline
    JSON — so 50 k points fit in ~1 MB on the wire.
  - LAS / LAZ / COPC paths use
    [`lidR::readLAS()`](https://rdrr.io/pkg/lidR/man/readLAS.html) with
    PDAL push-down filters: `-keep_xy <bbox>` (skips chunks outside the
    active ROI on COPC) and `-keep_random_fraction <f>` (decoder-level
    decimation, not post-load).
  - PLY preview is the ODM `point_cloud.ply`; binary little-endian
    layout (`x y z` float32 + `red blue green views` uchar, 16 B/vertex)
    read in one [`readBin()`](https://rdrr.io/r/base/readBin.html) pass
    (`R/pointcloud.R:28-117`).
- **Accuracy.**
  - LAS / LAZ / COPC: full sensor precision (LAS 1.2 / 1.4 XYZ as
    int32 + scale/offset; cm-level for typical drone surveys).
  - PLY preview from ODM: photogrammetric vertical RMSE ≈ 1.5–3 × GSD
    with GCPs (Dandois & Ellis 2013, *Remote Sens. Environ.*,
    <https://doi.org/10.1016/j.rse.2013.04.005>); without GCPs, vertical
    bias can be tens of cm to a few m.
- **Source.** Drone LiDAR (DJI L1 / L2, GreenValley, Riegl, etc.) or ODM
  photogrammetric cloud.

### Sub-tab — Selection

- **Outputs.** `selection_metrics` (count, footprint area, max crown
  diameter, Z min/mean/max, occupied volume), `classification_summary`.
- **Science.**
  [`compute_selection_metrics()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/compute_selection_metrics.md)
  (`R/selection_metrics.R:59-107`):
  - Footprint area: shoelace formula on the convex hull of XY.
  - Max crown diameter: max pairwise distance (subsampled to 1 000
    points if larger).
  - Occupied volume: voxel count × voxel_size³, default
    `voxel_size = 0.5 m`.
- **Precision.** Cell-level; occupied volume is approximate (no
  alpha-shape).

### Sub-tab — Survey volumes

- **Outputs.** `survey_volume_table`.
- **Science.**
  [`compute_survey_volumes()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/compute_survey_volumes.md)
  (`R/survey_volumes.R:103-224`). Volume = Σ (z_top − z_base) × A_cell,
  split into cut (+) and fill (−). The user picks the base reference:

| Base ref | Method | Recommended use |
|----|----|----|
| **DTM** | Top minus separate DTM (resampled to top grid if needed) | Canopy biomass — the correct choice when ground was filtered properly. |
| **Min Z** | Constant plane at min(top) inside ROI | Stockpile volume when no DTM is available. |
| **Mean Z** | Constant plane at mean(top) | Coarse baseline. |
| **Ground quantile** | Constant plane at quantile (default 5th percentile) | Robust ground proxy when noise contaminates min. |
| **User plane** | Constant plane at supplied `base_z` | Custom datum. |
| **Perimeter TIN** | Delaunay TIN interpolated from the ROI perimeter vertices | Industry standard for stockpiles (Pix4D, Bentley, Trimble). |

- **Accuracy.** Inherits the vertical accuracy of the DSM and the base
  reference. For photogrammetric DSM + DTM with GCPs, volumetric
  uncertainty is typically reported as ±2–5 % on regular stockpiles
  (Raeva, Filipova & Filipov 2016, *Int. Arch. Photogramm. Remote Sens.
  Spatial Inf. Sci.*, XLI-B1, 587-590) — actively variable on
  vegetation.
- **Precision.** Numbers shown to 0.01 m³; meaningful precision is at
  the cell-size level (cell_area = pixel² of the top raster).

### Sub-tab — Trees

- **Outputs.** `tree_metrics` (candidates), `selected_tree`.
- **Science.**
  [`derive_tree_candidates()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/derive_tree_candidates.md)
  (`R/pointcloud.R:141-191`):
  - Ground proxy = 5th percentile of Z.
  - Bin XY into 4 m grid (`grid_size`, configurable).
  - Per cell with ≥ 5 points (`min_points`) and tallest point above
    `min_height = 1.5 m`: crown diameter =
    `max(√(Δx² + Δy²), grid_size × 0.6)`; crown volume =
    `π × (D/2)² × h × 0.5`.
- **Accuracy.** Heuristic detector — no LMF / dalponte2016 / li2012
  algorithm yet. Sensitivity / precision against ground-truth trees is
  **to be measured** for this codebase.
- **Source.** Same point cloud as the viewer.

### Sub-tab — Spectral signature

- **Outputs.** `modeling_tree_spectral` — per-tree spectral means
  computed by the **Spectral Analytics** tab; this is the read-only
  consumer.

### Sub-tab — Vertical profile

- **Outputs.** `vertical_profile_plot` (ggplot histogram).
- **Science.** Heights binned with user `bin_size` (default 1 m) from 0
  to `ceiling(max_h / bin_size) × bin_size`.
- **Precision.** Bin width = chosen `bin_size`.

### Sub-tab — Manual crowns / ROIs

- **Outputs.** `manual_crowns_table`, `full_roi_status`.
- **Inputs.** ROIs drawn in the viewer or pulled from the GIS tab.

### Sub-tab — Distance

- **Outputs.** `distance_measurement`. Two-point 3D distance.
- **Precision.** Reported to 0.001 m; tied to viewer pick precision
  (~0.5 px screen-projected).

### Sub-tab — 2D context

- **Outputs.** `point_cloud_context_map` (Leaflet view centred on the
  point cloud bounds).

### Sub-tab — Export

- **Outputs.** `selection_export_paths` — paths of LAS / CSV / GeoJSON
  of the current selection
  ([`export_point_selection()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/export_point_selection.md)).

### Sub-tab — Tool reference

Static help: box selection, lasso, polygon, distance, etc.

------------------------------------------------------------------------

## Tab 4 — Spectral Analytics

Sidebar pipeline (collapsible accordion): **Load mosaic → Radiometric
scale → Panel ROI calibration → Preprocess → Index preview → Custom
index → Application map → Export**. Pipeline stepper at the top of the
main area tracks the current stage.

### Box — Orthomosaic metadata

- **Outputs.** `mosaic_meta` (CRS, extent, resolution, band count, data
  type).
- **Source.**
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  metadata of the loaded GeoTIFF.

### Box — Radiometric QA

- **Purpose.** Reports the maximum band value and the inferred scale
  factor used to convert to reflectance.
- **Science.**
  [`scale_to_reflectance()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/scale_to_reflectance.md)
  (`R/raster.R:103-166`) — three-step cascade:
  1.  GDAL cached `minmax` (zero pixel reads, ~10 ms);
  2.  mid-raster 50-row block (~100 ms on a 22k-wide ortho);
  3.  full `terra::global("max")` only as a last resort. Scale rule:
      `max ≤ 1.5` → already reflectance; `max > 10 000` → divide by 65
      535; else divide by 10 000. Output clamped to \[0, 1\].
- **Accuracy.** Auto-scaling is a sensible default; **for publication or
  absolute reflectance comparisons across flights, use the Panel ROI
  calibration step instead.**

### Box — Band histograms

- **Outputs.** `band_histogram_plot` (ggplot, one panel per band).
- **Science.**
  [`terra::values()`](https://rspatial.github.io/terra/reference/values.html)
  sampled to ≤ 1e6 cells for plotting.

### Box — Panel ROI calibration

- **Purpose.** Empirical-line reflectance calibration from a reflectance
  panel of known reflectance per band.
- **Inputs.** Panel ROI (drawn or loaded), per-band reflectance
  reference (`micasense_panel_reflectance_*` numeric inputs).
- **Outputs.** Per-band slope/intercept; calibrated reflectance written
  back into the in-memory mosaic.
- **Science.** Empirical-line method (Smith & Milton 1999, *Int. J.
  Remote Sens.* 20, <https://doi.org/10.1080/014311699212100>).
- **Accuracy.** With a clean MicaSense / Sequoia panel reading,
  band-mean reflectance is repeatable to **~1–3 %** under stable
  illumination (Mamaghani & Salvaggio 2019, *Drones* 3,
  <https://doi.org/10.3390/drones3040077>). Drift caused by changing
  illumination during the flight is not corrected here; for that you
  need the camera’s downwelling light sensor (DLS) record, which the app
  currently passes through ODM (`--radiometric-calibration camera+sun`)
  but does not re-apply per panel.

### Box — Index histogram

- **Outputs.** `index_histogram_plot`. Picker `index_choice` selects
  which index to plot.

### Box — Application map

- **Outputs.** `application_map_plot`, `application_summary` (area per
  class).
- **Science.** 5-class NDVI + CHM thresholding,
  [`classify_ground_vegetation()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/classify_ground_vegetation.md)
  (`R/classify.R:33-75`): `ndvi_bare_max = 0.20`,
  `ndvi_stress_max = 0.40`, `ndvi_vigorous_min = 0.65`,
  `chm_tall_min = 2.0 m`. Classes: Water/shadow, Bare soil, Stress,
  Moderate vigor, High vigor.
- **Accuracy.** Thresholds are **literature-typical defaults**, not
  field-calibrated for any specific crop on this codebase. Treat the
  application map as relative until you confirm the thresholds on your
  site (e.g. by sampling soil under the “bare” class and a reference
  crop under “high vigor”).

### Box — Tree / ROI spectral metrics

- **Outputs.** `tree_spectral_metrics`. Per-tree (from the 3D tab) or
  per-ROI (from the GIS tab) mean / sd / min / max for every loaded band
  and index.

### Box — Scientific reflectance preview

- **Outputs.** `mosaic_plot` — ggplot RGB or false-colour preview of the
  calibrated mosaic.

### Box — Index calculator preview

- **Outputs.** `index_plot` — ggplot of the selected index.

### Box — Index summary

- **Outputs.** `index_summary` table. mean / sd / min / max for every
  index, formatted to 2 dp.

### Box — Exported files

- **Outputs.** `export_paths`. List of GeoTIFFs written by
  [`write_dronebio_rasters()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/write_dronebio_rasters.md)
  (`R/raster.R:200-222`) — reflectance bands (FLT4S), indices (FLT4S),
  biomass proxy, optional valid-data mask (INT1U). All COG-style,
  DEFLATE + PREDICTOR=2.

------------------------------------------------------------------------

### Spectral index reference (built into the app)

Every index below is registered in the app’s layer reference table
(`inst/shiny/DroneBiomassStudio/app.R`); the same metadata feeds the “?”
help popovers and the layer manager.

| Index | Formula | Bands | Reference |
|----|----|----|----|
| NDVI | (NIR − Red) / (NIR + Red) | NIR, Red | Rouse et al. 1974, *3rd ERTS Symposium* |
| EVI | 2.5 × (NIR − Red) / (NIR + 6·Red − 7.5·Blue + 1) | NIR, Red, Blue | Huete et al. 2002, *RSE* 83 |
| SAVI | 1.5 × (NIR − Red) / (NIR + Red + 0.5) | NIR, Red | Huete 1988, *RSE* 25 |
| OSAVI | (NIR − Red) / (NIR + Red + 0.16) | NIR, Red | Rondeaux et al. 1996, *RSE* 55 |
| MSAVI2 | (2·NIR + 1 − √((2·NIR+1)² − 8·(NIR−Red))) / 2 | NIR, Red | Qi et al. 1994, *RSE* 48 |
| NDWI | (Green − NIR) / (Green + NIR) | Green, NIR | McFeeters 1996, *Int. J. Remote Sens.* 17 |
| GNDVI | (NIR − Green) / (NIR + Green) | NIR, Green | Gitelson et al. 1996, *RSE* 58 |
| GCI | NIR / Green − 1 | NIR, Green | Gitelson et al. 2003, *Geophys. Res. Lett.* 30 |
| RVI | NIR / Red | NIR, Red | Jordan 1969, *Ecology* 50 |
| DVI | NIR − Red | NIR, Red | Tucker 1979, *RSE* 8 |
| WDRVI | (0.2·NIR − Red) / (0.2·NIR + Red) | NIR, Red | Gitelson 2004, *J. Plant Physiol.* 161 |
| TVI | 0.5 × (120·(NIR − Green) − 200·(Red − Green)) | NIR, Green, Red | Broge & Leblanc 2001, *RSE* 76 |
| NDRE | (NIR − RedEdge) / (NIR + RedEdge) | NIR, RedEdge | Gitelson & Merzlyak 1994, *J. Photochem. Photobiol. B* 22 |
| CIrededge | NIR / RedEdge − 1 | NIR, RedEdge | Gitelson et al. 2003 |
| MCARI | ((RedEdge − Red) − 0.2·(RedEdge − Green)) × (RedEdge / Red) | RedEdge, Red, Green | Daughtry et al. 2000, *RSE* 74 |
| PSRI | (Red − Green) / RedEdge | Red, Green, RedEdge | Merzlyak et al. 1999, *Physiol. Plant.* 106 |
| VARI | (Green − Red) / (Green + Red − Blue) | Green, Red, Blue | Gitelson et al. 2002, *RSE* 80 |
| ExG | 2·Green − Red − Blue | Green, Red, Blue | Woebbecke et al. 1995, *Trans. ASAE* 38 |
| GLI | (2·Green − Red − Blue) / (2·Green + Red + Blue) | Green, Red, Blue | Louhaichi, Borman & Johnson 2001, *Geocarto Int.* 16 |
| TGI | −0.5 × (190·(Red − Green) − 120·(Red − Blue)) | Red, Green, Blue | Hunt et al. 2013, *Int. J. Appl. Earth Obs.* 21 |
| MGRVI | (Green² − Red²) / (Green² + Red²) | Green, Red | Bendig et al. 2015, *Int. J. Appl. Earth Obs.* 39 |
| RGBVI | (Green² − Red·Blue) / (Green² + Red·Blue) | Green, Red, Blue | Bendig et al. 2015 |

**Accuracy of an index value.** A single index pixel is as accurate as
the underlying reflectance pixel. Without panel-ROI calibration, the
absolute value is in DN-derived reflectance with ±5–15 % systematic bias
depending on illumination; **relative** comparisons within one mosaic
are stable to ~1 %. Cross-flight comparisons require the empirical-line
calibration step (Panel ROI box) — see Mamaghani & Salvaggio 2019 cited
above.

**Biomass proxies (DroneBioR-specific composites).**

| Name | Formula | Notes |
|----|----|----|
| Biomass spectral proxy | mean(NDVI, SAVI, NDRE) clamped to \[−1, 1\] | Image-only exploratory proxy, no kg/ha until calibrated. |
| Biomass = NDVI × CHM | NDVI × CHM | Volume-weighted; Lussem et al. 2019, *Eur. J. Remote Sens.* 52 |
| Biomass = NDRE × CHM | NDRE × CHM | Red-edge variant; preferred when NDVI saturates. |
| Biomass = SAVI × CHM | SAVI × CHM | Soil-adjusted variant. |
| Biomass = GNDVI × CHM | GNDVI × CHM | Green-NIR variant. |
| Biomass = VARI × CHM | VARI × CHM | RGB-only drone analogue of Bendig et al. 2015. |
| Biomass = ExG × CHM | ExG × CHM | RGB precision-ag pipelines. |
| Biomass = MGRVI × CHM | MGRVI × CHM | Best-validated RGB-only biomass surrogate for cereals (Bendig 2015). |
| Biomass = RGBVI × CHM | RGBVI × CHM | Bendig 2015 companion to MGRVI. |

All biomass proxies are **unitless surrogates** until calibrated against
field plots in the *Field Models* tab. The fit’s R², RMSE and residual
standard error are reported there.

------------------------------------------------------------------------

## Tab 5 — Field Models

Sidebar: CSV file input + a column-mapping wizard (`field_csv_mapping`)
that lets the user pick which column carries `sample_id`, `biomass`,
`coordinates` (x/y or lon/lat) and `CRS`.

### Box — CSV preview

- **Outputs.** `field_csv_preview` (first rows of the uploaded CSV).

### Box — Detected columns

- **Outputs.** `field_csv_diagnostics` (UI summarising the detected
  column types).

### Box — Extracted samples

- **Outputs.** `field_preview` — for each field point, the value of
  every loaded raster band / index at that location.
- **Science.**
  [`extract_field_spectral_data()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/extract_field_spectral_data.md)
  (`R/field.R:42-57`) — reprojects sample points to the predictors’ CRS,
  then
  [`terra::extract()`](https://rspatial.github.io/terra/reference/extract.html).
- **Accuracy.** Extraction is exact at the pixel scale; sample
  positional accuracy depends on the field GPS (consumer GPS ±3–5 m; RTK
  ±0.02 m). Mixed-pixel error at the orthomosaic resolution (e.g. 5 cm)
  is usually negligible compared to the GPS error.

### Box — Baseline model

- **Outputs.** `model_summary` — `summary(lm)` print-out.
- **Science.**
  [`fit_biomass_lm()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/fit_biomass_lm.md)
  (`R/field.R:76-93`). Ordinary least squares via
  [`stats::lm()`](https://rdrr.io/r/stats/lm.html). Default predictors
  when none are specified: NDVI, NDRE, EVI, SAVI, NDWI, NIR, RedEdge
  (whichever are present). Requires ≥ (n_predictors + 2) complete cases.
- **Accuracy & precision.** Report the model’s own R², residual standard
  error and per-coefficient SE. **There is no random forest, no glmnet,
  no cross-validation in the baseline model — it is intentionally
  simple.** For a defensible biomass estimate, validate the model with a
  held-out plot subset and report the RMSE on the holdout, not on the
  training residuals.

------------------------------------------------------------------------

## Tab 6 — Exports

Sidebar: `run_workflow`, `render_report`, `report_rerun_workflow`
checkbox (default OFF — reuses on-disk products), optional field CSV,
open output folder / manifest buttons.

### Box — Deliverables

- **Inputs.** `export_deliverables` checkbox group: reflectance bands,
  spectral indices, biomass proxy, application map, CHM, survey volumes
  table, HTML report, run manifest CSV.
- **Outputs.** Files under `output_dir`. Every export is appended to
  `dronebio_runs.csv` (engine, preset, resolution, image count,
  timestamp).
- **Science.**
  [`run_dronebio_workflow()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/run_dronebio_workflow.md)
  (`R/workflow.R`) is the single entry point.

### Box — Destination + format

- **Inputs.** Output directory, raster format (GeoTIFF / COG),
  compression, resolution override.

### Box — Workflow status / Output files / Report / Run manifest

- **Outputs.** Plain-text status lines, file paths, link to the rendered
  HTML report, link to the CSV manifest.
- **Source of the report.**
  [`render_dronebio_report()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/render_dronebio_report.md)
  (`R/report.R:50-125`) — knits `inst/report/biomass_report.Rmd` in an
  isolated environment.

------------------------------------------------------------------------

## Tab 7 — Time Series

Sidebar: registry path, **Add current project as flight** button, custom
flight entry (date / project_dir / notes), metric picker (NDVI mean /
Biomass proxy mean / CHM mean).

### Box — Flight manager

- **Outputs.** `ts_flight_manager` (table of registered flights with
  remove buttons).
- **Science.** Registry is a plain CSV
  ([`default_flight_registry()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/default_flight_registry.md)
  → `~/.dronebior/flight_registry.csv`) with columns `flight_id`,
  `date`, `project_dir`, `notes`. Editable by hand or
  version-controlled.

### Box — Time series plot

- **Outputs.** `ts_plot` (ggplot).
- **Science.** Per-flight aggregation by the chosen metric:
  - **NDVI mean** —
    [`flight_ndvi_mean()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/flight_summary_helpers.md)
    (`R/time_series.R:237-258`) reads only Red + NIR and computes a
    single NDVI mean; full-spectral path skipped for speed.
  - **Biomass proxy mean** — full pipeline:
    [`read_multispectral_orthomosaic()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/read_multispectral_orthomosaic.md)
    →
    [`scale_to_reflectance()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/scale_to_reflectance.md)
    →
    [`compute_spectral_indices()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/compute_spectral_indices.md)
    →
    [`compute_biomass_proxy()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/compute_biomass_proxy.md)
    → mean.
  - **CHM mean** — prefers the persisted `chm.tif` if fresher than
    DSM/DTM; otherwise builds CHM in memory.
- **Caching.** `~/.dronebior/flight_metrics_cache.rds` keyed by
  `(metric, path, size, mtime)`. Stale entries invalidate on file
  change.
- **Precision.** Means are reported to 4 dp; meaningful precision is
  limited by the per-flight calibration. Cross-flight NDVI is only
  comparable when the panel-ROI calibration was applied per flight.

------------------------------------------------------------------------

## External data sources used by the app

| Source | Role | Reference / link | Native accuracy |
|----|----|----|----|
| MicaSense RedEdge / Altum / Sequoia | Multispectral imagery | <https://eaglenxt.com/> | Per-band relative reflectance ±1–3 % after panel calibration |
| Generic RGB drone cameras (Sony, DJI, Phantom) | RGB imagery | sensor-specific | RGB radiometry is not absolute; use RGB-only indices (VARI, ExG, GLI, MGRVI, RGBVI) for relative comparisons |
| OpenDroneMap (local Docker) | Structure-from-Motion + MVS | <https://docs.opendronemap.org/> | Horizontal RMSE 2–5 × GSD with GCPs; vertical RMSE 1.5–3 × GSD |
| WebODM (REST) | Remote ODM | <https://github.com/WebODM/WebODM> | Same engine as local ODM |
| Pix4Dmapper / Agisoft Metashape | Optional external orthomosaics | vendor docs | Comparable to ODM with same GCP configuration |
| GeoScan GNSS metadata | Per-image position + heading for ODM-GEO | format documented in `R/odm_geo.R` | RTK ±0.02 m H / ±0.05 m V; PPK ±0.05 m H / ±0.10 m V; GPS-EXIF only ±2–5 m |
| LiDAR LAS / LAZ / COPC | Optional point cloud input | ASPRS LAS spec | Sensor-dependent (DJI L1 ±5 cm V, L2 ±3 cm V, Riegl VUX ±1.5 cm) |
| Esri ArcGIS World Imagery | Basemap (Satellite) | <https://www.arcgis.com/home/item.html?id=10df2279f9684e4a9f6a7f08febac2a9> | Tile resolution varies by region; check the Esri overview for sub-meter coverage |
| CartoDB Positron | Basemap (Light) | <https://carto.com/help/building-maps/basemap-list/> | OSM-derived |
| Three.js r128 | 3D viewer runtime | <https://threejs.org/> | N/A (rendering) |

**Not yet integrated** (planned for the Florida ag / hydrology
expansion):

- USDA NRCS SSURGO soil survey (<https://websoilsurvey.nrcs.usda.gov/>)
- USGS 3DEP 1 m DEM (<https://www.usgs.gov/3d-elevation-program>)
- NOAA / NWS weather grids (NDFD / NDGD)
- Florida Automated Weather Network (FAWN, <https://fawn.ifas.ufl.edu/>)

When these arrive, this reference will be extended with their native
accuracy and licence terms.

------------------------------------------------------------------------

## Caveats, in plain language

1.  **Index values without panel calibration are not absolute.** They
    are excellent for *relative* comparison inside one mosaic; treat
    absolute thresholds as defaults that need confirmation on site.
2.  **CHM and biomass × CHM products are only as good as the DTM.** A
    photogrammetric DTM under closed canopy is unreliable. Use a LiDAR
    DTM whenever you can.
3.  **Tree detection is heuristic, not validated.** It works as a “where
    to look” tool. For a count you can defend, validate against manual
    crowns on a sample plot.
4.  **Volumes need a base reference.** “Min Z” is convenient but biases
    low; “Perimeter TIN” matches the Pix4D / Bentley / Trimble standard
    and is the right choice when you have a clean ROI boundary. For
    canopy biomass, always use the DTM.
5.  **The baseline biomass model is an OLS regression.** Report the R²
    and the holdout RMSE — not the in-sample residuals — when you share
    kg/ha numbers.
