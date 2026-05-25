# DroneBioR (development version)

## Bug fixes

* **`run_docker_with_progress()` no longer errors with
  `unused argument (timeout = 1000)`.** The previous implementation
  called `processx::process$read_output(timeout = 1000)`, but
  `read_output()` does not accept a `timeout` argument (that
  parameter belongs to `poll_io()`). The helper now relies on
  `proc$wait(timeout = ms)` for non-blocking cadence and lets the
  subprocess inherit the parent R process's stdout/stderr, so the
  ODM output appears in the console exactly as it did with
  `system2()`. A real end-to-end test now drives the helper through
  a benign `sleep` subprocess so this class of API-shape regression
  fails the build before reaching main.

## Breaking changes

* **`default_dji_mavic_3m_band_map()` no longer exposes Blue.** The
  Mavic 3M does not capture a calibrated Blue MS band; the only blue
  available is the uncalibrated RGB JPG channel. Mixing it with the
  calibrated MS bands inside EVI / VARI / ExG / GLI / TGI / RGBVI
  produces a hybrid number that is not comparable to literature
  values and silently misleads downstream analysis. The default map
  now returns four entries — `Green`, `Red`, `RedEdge`, `NIR` — and
  `compute_spectral_indices()` automatically skips the six
  Blue-dependent indices, returning the 16 indices that the
  calibrated MS bands can support honestly. Users who specifically
  want the RGB JPG visible-band indices can pass an explicit
  `band_map = c(Blue = 3, Green = 2, Red = 1)` to
  `read_multispectral_orthomosaic()`.

## New features

* **Live ODM progress: stage, elapsed, ETA, percent.** ODM Docker
  runs can take 30-90 min per band and previously emitted only raw
  per-line log output, so users could not tell at a glance whether
  a run was progressing or hung. The CLI / batch path now runs
  docker under `processx` (a new Suggests dependency) and polls the
  ODM project directory every 15 s, printing a one-line status that
  combines:
    - the current ODM stage (e.g., `opensfm`, `openmvs`, `odm_dem`),
    - elapsed wall time in the run,
    - estimated time remaining (drawn from
      `~/.dronebior/odm_stage_history.csv`),
    - and a stage-count percentage.
  `run_odm_dji_mavic_3m()` additionally prints a banner before each
  of the 5 per-band runs with the up-front estimate and the batch's
  cumulative percent / ETA, plus a closing line with the band's
  actual duration. Without `processx` installed the helpers fall
  back to the existing blocking `system2()` call with a single
  pre-run banner.

## Breaking changes

* **`improve_dtm_csf()` no longer overwrites `dtm.tif` / `chm.tif`.**
  The function used to default to `dtm_filename = "dtm.tif"` and call
  `build_chm_raster(force = TRUE)`, which silently clobbered the ODM
  SMRF DTM and CHM in place — preventing any side-by-side comparison
  of SMRF vs CSF terrain. New defaults are `dtm_filename =
  "dtm_csf.tif"` and `chm_filename = "chm_csf.tif"`, both written
  alongside the SMRF originals so users can compare both methods.
  `odm_product_paths()` exposes the new files under the keys
  `dtm_csf` and `chm_csf`. Pass the old filenames explicitly to
  restore the pre-existing overwrite behaviour.

## Bug fixes

* **`run_odm_project()` and `run_odm_dji_mavic_3m()` no longer abort on
  report-stage failures.** ODM occasionally exits non-zero after
  writing the orthomosaic / DSM / DTM / point cloud — the most common
  cause is the `odm_report` stage's `gdal_translate` call failing on a
  numpy ABI mismatch inside the container (`ImportError: numpy.core.
  multiarray failed to import`). The PDF report dies, every
  geospatial product is intact. Both engines now check for the
  orthomosaic on disk after a non-zero exit and treat its presence
  as success-with-warning, so batch scripts (and the per-band DJI
  orchestrator) can keep processing the next band / next flight.

## New features

* **DJI Mavic 3M support — full multispectral pipeline.** New exported
  `run_odm_dji_mavic_3m()` orchestrates five per-band ODM runs (RGB
  JPGs for SfM + DSM + DTM + point cloud, then `--fast-orthophoto`
  reflectance runs on each of `_MS_G`, `_MS_R`, `_MS_RE`, `_MS_NIR`)
  and stacks the five orthomosaics into a single 7-band GeoTIFF
  (`Red, Green, Blue, MS_G, MS_R, MS_RE, MS_NIR`). Helper
  `list_dji_mavic_3m_images()` returns one manifest per camera band
  and `default_dji_mavic_3m_band_map()` exposes Blue (from the RGB
  JPG) plus the four calibrated MS bands to downstream index code.
  `read_multispectral_orthomosaic()` auto-detects the 7-layer stack
  and applies the DJI band map.
* **`list_aerial_images()` DJI Mavic 3M filtering.** When both
  `_D.JPG` (RGB visible) and `_MS_*.TIF` (multispectral) DJI Mavic 3M
  images are present in a folder, the function now drops the MS TIFs
  and tags the returned manifest with
  `attr(.,"dji_visible_multispectral") = TRUE`, so the existing
  `run_odm_project(camera_type = "rgb")` path stays viable for users
  who only want the RGB orthomosaic. For the full MS pipeline use the
  new `run_odm_dji_mavic_3m()`.

## Changes

* **`compute_spectral_indices()` now treats Blue as optional.** Strict
  minimum is Green + Red; Blue, RedEdge and NIR each unlock their own
  indices when present. DJI Mavic 3M (4-band MS, no calibrated Blue)
  now produces the full NDVI / NDRE / SAVI / OSAVI / MSAVI2 / NDWI /
  GNDVI / CIrededge / GCI / RVI / DVI / WDRVI / TVI / MCARI / PSRI /
  MGRVI stack without erroring; EVI, VARI, ExG, GLI, TGI and RGBVI
  are silently skipped because they require a Blue band. Pass
  `strict = TRUE` to keep the legacy "error when Blue is missing"
  behavior.
* **Reflectance output renamed.** `write_dronebio_rasters()` now writes
  the reflectance stack to `reflectance_bands.tif` (was
  `micasense_reflectance_bands.tif`). The filename is now camera-agnostic
  — DroneBioR runs on DJI Mavic 3M, Sony / DJI RGB and other
  non-MicaSense rigs, and the output name now reflects that. Downstream
  code that reads the path via `result$output_paths[["reflectance"]]`
  is unaffected.

## Documentation

* **New vignette `app-reference`.** End-to-end reference for every
  tab and box in `run_drone_biomass_studio()`: purpose, inputs,
  outputs, science, accuracy / precision and the external data
  source behind each number. Includes the full spectral-index
  citation table, the biomass-proxy formulas, the survey-volume
  base-reference options (DTM / min Z / mean Z / quantile /
  user plane / perimeter TIN) and an honest "to be measured"
  note where local validation data does not yet exist
  (tree detection sensitivity, application-map thresholds).
  Registered under Topics in `_pkgdown.yml` so it ships on the
  pkgdown site.

UX redesign of Drone Biomass Studio plus the GIS Workspace / 3D
Modeling stabilization pass. The Studio now feels like a guided
workflow tool rather than a wall of inputs: a sticky Project
Control Center summarises the project state across every tab, a
horizontal Workflow Stepper tracks the user's progress through
Process -> GIS -> Spectral -> 3D Modeling -> Field model ->
Export -> Time Series, and each tab carries its own CAD-style
toolbar, accordion sidebar and cross-tab CTA row.

## New features

* **Project Control Center.** Sticky top card visible from every
  tab. Shows project name + path, image count, engine, product
  status pills (Ortho / DSM / DTM / CHM / Point cloud), the
  last-update timestamp and a "Next action" link that jumps to the
  right tab. Driven by a new `project_snapshot()` reactive.

* **Workflow Stepper.** Seven horizontal chips above each panel:
  Process -> GIS -> Spectral -> 3D Modeling -> Field model ->
  Export -> Time Series. The active step is highlighted (matches
  `input$main_nav`); completed steps get a green checkmark via a
  new `workflow_completion()` reactive that checks
  `quick_outputs_check()` + side-effect outputs (biomass model
  summary, exports, flight registry, etc.). Click a step to jump
  to that tab via `updateNavbarPage()`.

* **Run-history manifest.** New internal helpers
  `dronebio_runs_path()`, `record_dronebio_run()` and
  `read_dronebio_runs()` in `R/products.R`. Every project gets a
  `dronebio_runs.csv` with a canonical schema (`timestamp`,
  `engine`, `preset`, `resolution_cm`, `image_count`, `bands`,
  `crs`, `orthomosaic`, `dsm`, `dtm`, `chm`, `point_cloud`,
  `textured_mesh`, `runtime_seconds`, `notes`) plus a JSON
  `extras` column for non-standard fields. Appended on every
  `run_dronebio_workflow()` call and on every Processing Engine
  launch. Surfaced in the Exports panel as a 20-row "Run
  manifest" table.

* **GIS Workspace -- accordion sidebar.** Five panels: Project
  paths / Map layers (open by default) plus Display options /
  Annotations / ROI comparison (collapsed). Replaces the
  forty-control linear sidebar.

* **GIS Workspace -- map toolbar.** CAD-style button strip above
  the leaflet canvas. Tool group: Navigate / Distance / Area-ROI /
  Volume-CHM / Annotation; Action group: Save ROI / Clear / Center
  map. Live status text on the right ("Measure area - 3 vertices
  placed", "Annotation mode - click to pin"). Active tool gets a
  green `.active` CSS class via a new `dronebior_gis_toolbar_active`
  custom message handler. Toolbar Save / Clear buttons re-fire
  existing sidebar handlers via a new `dronebior_click_button`
  handler so the server logic is not duplicated.

* **GIS Workspace -- Layer Manager card.** One row per active
  overlay with type (Raster product / Biomass proxy / Spectral
  index), band requirements and a Ready / Missing pill. Empty
  state when nothing is selected ("Tick layers in the sidebar's
  'Map layers' panel and click Load").

* **GIS Workspace -- cross-tab CTA row.** "Open in 3D Modeling
  -->", "Run Spectral QA -->", "Open Field Models -->", "Add to
  Time Series -->" buttons below the map. Each fires
  `updateNavbarPage("main_nav", ...)`.

* **3D Modeling -- accordion sidebar.** Seven panels: Scene source
  / GIS Workspace ROI (open) plus Display options / Classification
  / Tree detection / Volume & profile / Selection actions
  (collapsed). The "3D interaction tool" select moved to a hidden
  input; the new "Tool" group at the start of the modeling-toolbar
  drives it with CAD-style buttons (Inspect / Box / Lasso /
  Polygon / Measure / Crown). Reuses the GIS toolbar's
  active-state JS handler.

* **3D Modeling -- cross-tab CTA row.** "Back to GIS Workspace",
  "Run Spectral QA", "Open Field Models", "Open Exports" buttons
  below the metric tabs.

* **Spectral Analytics -- pipeline stepper.** Mini horizontal
  stepper just above the spectral workspace cards
  (`output$spectral_pipeline_stepper`): Mosaic -> Reflectance ->
  Indices -> App map -> Export. Each step ticks green when the
  corresponding reactive resolves; the next-to-be-done step is
  highlighted active. Shares the same CSS as the top-level
  workflow stepper.

* **Spectral Analytics -- accordion sidebar.** Six panels
  mirroring the calibration pipeline: Radiometric scale / Panel
  ROI calibration / Preprocessing / Index preview (open by
  default) / Custom index / Application map thresholds. Load
  mosaic and Export promoted to a compact button row at the top.

* **Spectral Analytics -- cross-tab CTA row.** "GIS Workspace",
  "Open in 3D Modeling", "Fit field model", "Export center"
  buttons at the bottom.

* **Field Models -- CSV mapping wizard.** Upload any CSV; the
  sidebar wizard shows column-role dropdowns auto-populated from
  detected column names (sample_id / biomass / x-longitude /
  y-latitude), plus a biomass units picker (kg/ha, Mg/ha, g/m^2)
  and a target CRS EPSG. The CSV preview and detected-column
  diagnostics show in the main area. The extract step renames
  the chosen columns into the canonical schema before calling
  `read_field_data()` so the existing pipeline keeps working.

* **Exports -- Export Center.** "Deliverables" checkboxGroup with
  eight export targets (reflectance / indices / biomass proxies /
  index CSV / refl CSV / application map / tree-ROI CSV / HTML
  report). "Destination + format" card with output folder, raster
  format (COG / GTiff deflate / GTiff LZW), target resolution and
  a checkbox to record the export in the runs manifest. Sidebar
  buttons for "Open output folder" and "Open run manifest"
  (system2 'open'). Runs-manifest card surfaces the
  dronebio_runs.csv contents (most-recent 20 rows).

* **Time Series -- Flight Manager.** "Add current project as
  flight" big primary button validates via
  `quick_outputs_check()` and calls `register_flight()` with
  today's date + the active project_dir. Custom flight entry
  fields collapse into an accordion. New
  `output$ts_flight_manager` renders one row per registered
  flight with a per-row Remove button (edits the registry CSV in
  place; underlying project directory is untouched).

## Performance and robustness (continued from prior unreleased work)

* Cache-aware path resolution in the 3D Modeling tab.
* PLY preview ~30-100x faster (single rawConnection).
* Selection updates via custom message - no more renderUI rebuild
  on every click.
* `outputOptions(point_cloud_viewer, suspendWhenHidden = FALSE)`.
* Decimal display rounded to 2 places everywhere.

## Bug fixes (continued from prior unreleased work)

* ROI vertices now show while drawing on the GIS Workspace map.
* 3D Modeling alignment when the point cloud and orthomosaic
  live in different coordinate frames (AABB-overlap + DSM-drape
  Z-range checks + basemap snap fallback).
* GIS Workspace map no longer renders multiple worlds (noWrap +
  tile bounds + deep-ocean CSS background).
* GIS ROI to 3D selection bridge uses the orthomosaic's CRS.
* Frame-mismatch warning latched per scene.

## New indices and biomass proxies

* `compute_spectral_indices()` now produces 22 layers from full
  multispectral inputs and 6 from RGB-only inputs (was 9 and 1).
* `compute_biomass_proxies()` returns a stack of greenness x CHM
  biomass surrogates (Biomass_NDVI_x_CHM, ..., Biomass_RGBVI_x_CHM)
  plus the legacy spectral proxy. Wired into the GIS gis_stack
  reactive: appended automatically when the project has a CHM.
* `product_metadata` now carries formula / bands / range / unit /
  reference / interpretation for every layer; the "?" modal next
  to each overlay renders all fields.

## New features

* **Expanded vegetation-index catalogue.** `compute_spectral_indices()`
  now produces up to 22 layers from a full multispectral input (was 9)
  and 6 from RGB-only inputs (was just VARI). Multispectral additions:
  OSAVI (Rondeaux 1996), GCI (Gitelson 2003), RVI / SR (Jordan 1969),
  DVI (Tucker 1979), WDRVI (Gitelson 2004), TVI (Broge 2001), MCARI
  (Daughtry 2000) and PSRI (Merzlyak 1999). RGB-only additions: ExG
  (Woebbecke 1995), GLI (Louhaichi 2001), TGI (Hunt 2013), MGRVI and
  RGBVI (Bendig 2015). The canonical-order vector and the GIS overlay
  registry, band-requirement table, semantic-domain helper and fixed
  index limits in the Shiny app were extended to cover every new
  index. RGB-only path tested explicitly.

* **Biomass-proxy stack via greenness x CHM.** New exported
  `compute_biomass_proxies(indices, chm)` returning a SpatRaster with
  the canonical spectral proxy plus eight greenness x CHM variants
  (NDVI / NDRE / SAVI / GNDVI / VARI / ExG / MGRVI / RGBVI multiplied
  by the canopy height model). The greenness x height product tracks
  per-pixel above-ground biomass for herbaceous and shrub canopies
  (Bendig et al. 2015; Lussem et al. 2019). The legacy single-layer
  `compute_biomass_proxy()` is preserved for back-compat. Wired into
  the GIS gis_stack reactive: when the project has a DSM + DTM the
  biomass proxies are automatically appended to the index stack and
  available in the overlay picker.

* **Detailed help modal for every map layer.** The "?" button next to
  each overlay / index in the GIS Workspace sidebar now shows the
  formula, the spectral bands required, the typical numeric range,
  the unit, the peer-reviewed reference (with author / year /
  journal) and a one-line interpretation. Every entry in
  `product_metadata` was rewritten - 30+ layers now carry citations
  (Rouse 1974, Huete 1988 / 2002, Rondeaux 1996, Qi 1994, Gitelson
  1994 / 1996 / 2002 / 2003 / 2004, Daughtry 2000, Merzlyak 1999,
  Bendig 2015, Hunt 2013, Woebbecke 1995, Louhaichi 2001, ...).

## Performance and robustness

* **Cache-aware path resolution in the 3D Modeling tab.** New internal
  `cache_aware_path()` helper resolves any project path to its local
  cache copy (`~/.dronebior/cache/<slug>/<basename>`) when one exists.
  The 3D tab routes the DSM, DTM, orthomosaic, point cloud and
  textured-mesh reads through this helper via a new `cached_products()`
  reactive, so OneDrive / iCloud Files-On-Demand stops being triggered
  on every reactive invocation once the user has run "Copy outputs to
  local cache".

* **PLY preview ~30-100x faster.** Rewrote `read_ply_point_cloud()` so
  it builds a single rawConnection over the entire selected vertex
  block and decodes XYZ + RGBV in bulk, instead of opening one
  rawConnection per point (which on the typical 35k-point ODM
  filterpoints PLY meant 35k file-handle round-trips).

* **Selection updates no longer rebuild the 3D scene.** Added a new
  Shiny custom message handler `dronebior_3d_set_selection` that
  rebuilds only the highlight BufferGeometry inside the existing
  three.js scene. The renderUI was isolated from `selected_point_ids()`,
  so clicking a point or pulling a GIS ROI into the 3D selection no
  longer re-encodes the full point cloud as JSON.

* **3D viewer survives tab switches.** Added
  `outputOptions(point_cloud_viewer, suspendWhenHidden = FALSE)` so
  Shiny does not unmount the renderUI when the user navigates away
  to another main tab - the "every minute the view redraws from a
  different plane" symptom is gone.

* **Decimal display rounded to 2 places everywhere.** Every
  user-facing numeric (renderTable digits, formatC calls, legend
  endpoints, hover labels for DTM / DSM / CHM / VARI / NDVI / NDRE /
  biomass proxies, etc.) now renders with 2 decimals. Bulk-applied
  across the 20 renderTable closures plus the format helpers.

## Bug fixes

* **ROI vertices now show while drawing on the GIS Workspace map.**
  The CircleMarker / polyline / polygon for in-progress ROI drawing
  was being added to the "Measurement" overlay group, which itself
  was listed in `addLayersControl(overlayGroups = ...)`. Because the
  group had no members on the map at the time the layers control
  was created, leaflet's L.Control.Layers started the group's
  checkbox UNCHECKED, and every marker we added later via
  leafletProxy landed in a hidden group. Fixed by removing
  "Measurement" (and "ROIs" / "Annotations") from overlayGroups so
  they have no togglable checkbox and are simply always visible.
  Vertices now appear instantly as the user clicks; the in-progress
  polyline forms after the second click and the closed polygon
  after the third. Numbered yellow vertex markers, dashed
  in-progress line and a "Drawing mode" badge make the canvas state
  unambiguous.

* **3D Modeling alignment when the point cloud and orthomosaic live
  in different coordinate frames.** Detect AABB-overlap mismatch on
  the JS side: when the basemap's XY bounding box does not intersect
  the point cloud's, snap the basemap plane to the points' bounding
  box. Separately check the Z range of the DSM drape against the
  point Z range and suppress the drape when the two are disjoint
  (typical for PLY previews in OpenSfM-local metres vs DSM in
  projected elevations). A one-shot warning toast tells the user
  the basemap was snapped and recommends switching to the LAS / LAZ
  / COPC source for an exact-georeferenced view.

* **GIS Workspace map no longer renders multiple worlds.** The
  satellite (Esri) and CartoDB tile layers now pass
  `noWrap = TRUE` plus an explicit `bounds = list(c(-85, -180),
  c(85, 180))`. Esri returns a real placeholder tile (grey image
  reading "Map data not yet available") with HTTP 200 when asked
  for out-of-range coordinates, which is why `errorTileUrl` could
  not fix the symptom on its own - leaflet has to be told not to
  request those tiles in the first place. The remaining canvas
  past +/-180 is painted a deep-ocean blue via a
  `.leaflet-container { background-color: ...; }` CSS rule so the
  margin reads as ocean on the Satellite basemap.

* **GIS ROI to 3D selection bridge uses the orthomosaic CRS.** The
  ROI polygon drawn on the GIS Workspace map is reprojected through
  `terra::crs(input$orthomosaic)` (was the DSM's CRS), so the
  selected 3D points land where the user drew the ROI on the map,
  even when the orthomosaic and DSM happen to be in different CRSs.

* **Periodic full rebuild of the 3D viewer.** A latch reactiveVal
  was added so the basemap-frame-mismatch warning toast does not
  repeat on every renderUI invocation. Combined with the tab-switch
  suspension fix above, the viewer now feels stable instead of
  redrawing under the user.

# DroneBioR 0.4.0

A second pass of feature work on top of 0.3.0, focused on the 3D Modeling
panel of Drone Biomass Studio and on the scientific volume math used by
the survey-grade workflow. Same scope rule as before: the package stays
out of SfM / MVS / mesh / texturing, and reaches the deliverables a
Pix4D Mapper or Metashape user would expect on the analysis side.

## New features

* **Survey-grade volume calculations.** New exported
  `compute_survey_volumes(top, roi, method, ...)` implements the six
  base-reference methods photogrammetric surveyors use - DTM (canopy
  biomass), min Z (classic stockpile), mean Z, ground quantile,
  user-defined plane, and Delaunay TIN through the perimeter vertices
  (the Pix4D / ContextCapture / Trimble standard for stockpiles). The
  result reports cut / fill / net volumes, planimetric area, the 3D
  draped surface area via the secant-of-slope formula, perimeter,
  cell area and count, and per-surface min / median / mean / max
  summaries. `interp` added to Suggests for the TIN method. Wired
  into the 3D Modeling panel as a new "Survey volumes" card driven
  by the convex hull of the currently-selected points.

* **3D Modeling panel redesign.** Renamed from "3D & Tree Metrics"
  (which buried everything that is not tree work). The 3D viewport
  now spans the full panel width at clamp(520px, 70vh, 820px)
  height; metrics moved into a `bslib::navset_card_tab` below with
  nine tabs (Selection / Survey volumes / Trees / Vertical profile
  / Manual crowns / Distance / 2D context / Export / Tool reference).
  New toolbar above the viewer: Browse files / Load 3D scene /
  Reset view / Zoom + / Zoom -. The Reset / Zoom buttons drive
  OrbitControls via a `window.__dronebior_viewer` global so R-side
  actionButtons can re-frame the camera.

* **Textured OBJ mesh loader for the 3D viewer.** New opt-in
  checkbox in the 3D Modeling sidebar registers the ODM textured
  output directory as a Shiny resource path and pulls
  `odm_textured_model_geo.obj` plus its MTL plus all referenced
  texture PNGs via three.js OBJLoader / MTLLoader. Vertex positions
  are rewritten into the viewer's local coordinate frame so the
  mesh and the point cloud share a single transform. Falls back to
  a plain MeshLambertMaterial if the MTL fails.

* **Live scale bar in the 3D viewer.** Projects two world points
  1 m apart at the OrbitControls target depth, measures the pixel
  distance, picks a nice round meter value (1 / 2 / 5 / 10 / 20
  / 50 / 100 / ...) so the bar lands near 110 px, and updates the
  bottom-left overlay DOM ~10 Hz from the animation loop. Re-scales
  smoothly as the user pans / zooms.

* **Corner XYZ orientation gizmo.** Separate three.js scene with
  an AxesHelper plus coloured sphere tips (X red, Y green, Z blue).
  A 112x112 transparent WebGL renderer is overlaid on the viewer
  bottom-right; its camera is reoriented every frame to match the
  main camera's view direction. Same convention Pix4D / Blender
  use; users get a fast orientation cue without a separate
  viewport.

* **Color stretch toggle on the GIS map.** New selector "Color
  stretch": "Fixed semantic" (default), "Data range", "Percentile
  2-98". Fixed semantic pins NDVI / NDRE / NDWI / GNDVI / VARI /
  Biomass_Index_Proxy to [-1, 1] (yellow = 0 is the biophysical
  vegetation boundary), MSAVI2 and reflectance bands to [0, 1],
  EVI / SAVI / CIrededge fall through to the data range (no
  canonical bounded range). Same value on two flights always
  produces the same colour, which is the convention for time-series
  panels in remote-sensing publications. The bottom-left legend
  reflects the active domain exactly.

* **GIS Workspace measurement and annotation tools.** "Measure
  volume (CHM)" picks polygon vertices, reprojects into the CHM
  CRS and routes through `compute_chm_roi_metrics()`. Annotation
  mode pins named markers on the basemap and persists them as
  GeoJSON. Multi-ROI comparison table averages NDVI / NDRE / EVI
  / SAVI / Biomass_Index_Proxy plus CHM mean / max / volume across
  named regions. Annotations and ROIs now both save under
  `<project_dir>/studio_assets/` for consistency, with the path
  displayed in the sidebar. ROIs auto-load on session start.

* **ROI delete / redraw workflow.** "Saved ROIs" dropdown plus
  "Redraw selected ROI" (deletes and re-enters draw mode with the
  same name) and "Delete selected ROI" (named-only delete that
  leaves the rest of the collection intact). Vertex-by-vertex
  editing is still not supported; the new redraw flow is the
  pragmatic replacement.

* **PDF / HTML report.** New exported `render_dronebio_report()`
  and a one-click "Render HTML report" button in the Exports panel.
  The bundled `inst/report/biomass_report.Rmd` template includes the
  ODM inventory, per-band reflectance summary, per-index histograms,
  CHM map, optional field-based biomass model.

* **Time-series tracking across flights.** New registry-based
  workflow: `register_flight()`, `list_flights()`,
  `flight_time_series()` with three stock summary helpers
  (`flight_ndvi_mean`, `flight_biomass_proxy_mean`,
  `flight_chm_mean`). New "Time Series" nav panel in the Shiny app
  with a metric selector and a plot across the registered dates.

* **Ground / vegetation classification.** New
  `classify_ground_vegetation()` for rule-based 5-class NDVI + CHM
  classification, and `classify_ground_csf()` bridging to
  `lidR::classify_ground()` with the Cloth Simulation Filter.

## Performance and robustness

* **Async workflow execution.** `run_dronebio_workflow()` runs in a
  background R session via `promises::future_promise()` when both
  `future` and `promises` are installed. UI stays responsive while
  the workflow writes products to disk.

* **COG-style raster tiling.** New `tile_raster_on_map()` private
  helper uses `leafem::addGeotiff()` when `leafem` is installed,
  with a process-scoped temp-file cache keyed by raster fingerprint.
  Pan/zoom on large orthomosaics is incremental rather than
  re-rendering a downsampled image.

* **Toast-based error UI.** New exported `with_error_toast()` helper
  catches errors inside Shiny reactives and renders them as
  notifications. Applied to the six event-driven reactives.
  `shiny.silent.error` re-thrown so existing `validate()` / `req()`
  inline messages keep working.

* **Reactive caching.** `bindCache()` applied to the IO reactives
  in the GIS Workspace (mosaic, gis_stack, point_cloud, hillshade).

## UX

* **Branded navbar.** New 400-px DroneBioR logo at 120 px height in
  the Drone Biomass Studio navbar. Matching `man/figures/logo.png`
  is auto-picked up by pkgdown for the doc site.

* **Per-panel help cards.** Each of the seven nav panels opens with
  a Bootstrap info card explaining what the panel does and linking
  to the relevant pkgdown vignette.

* **Hillshade overlay.** "Hillshade" added to the GIS overlay
  choices, computed from the DSM via `terra::terrain()` +
  `terra::shade()`, rendered in grayscale beneath the colour overlays.

* **Hover values formatted to 2 decimals.** Configured leafem's
  imagequery so the per-raster readout stops bleeding 15 decimal
  places of floating-point noise; moved to bottom-right so it
  stays clear of the top-right layers control. A custom message
  handler also sweeps the widgets on overlay reload / clear so
  they never accumulate across cycles.

## Bug fixes

* `bindCache()` must be called before `bindEvent()`, not after; the
  earlier ordering crashed the server function during init, which
  silently broke unrelated observers (file browser, etc.).
* `configure_proj_database()` no longer warns inside
  `run_dronebio_workflow()` when proj.db is reachable through other
  channels (Debian/Ubuntu apt PROJ paths added).
* `terra::extract()` on a matrix does not accept `ID = FALSE`;
  removed for the perimeter-TIN path.
* `compute_survey_volumes(top, roi)` returns an NA-filled object
  when the ROI does not intersect the raster, rather than the
  default `terra::crop()` error.

## New optional dependencies (Suggests)

* `interp` - perimeter-TIN base for `compute_survey_volumes()`.

# DroneBioR 0.3.0

This release focuses on making the Shiny app (Drone Biomass Studio) mature
enough for daily research use, and on closing the user-facing feature gap
versus Pix4D Mapper and Agisoft Metashape. The scope explicitly excludes
SfM / MVS / mesh / texture work (still delegated to ODM / WebODM /
Pix4Dmapper / Agisoft Metashape), and focuses on the scientific R layer
and the deliverables the user actually consumes from the app.

## New features

* **Sample project for first-run experience.** New
  `dronebio_sample_project()` and `run_drone_biomass_studio(sample = TRUE)`
  seed a clickable ODM-shaped project from the bundled fixtures so the
  app works immediately without flight data.
* **CHM volume measurement on the GIS map.** New "Measure volume (CHM)"
  tool: click polygon vertices on the basemap and the summary table
  reports CHM area, mean and max height, and surface volume - the
  Pix4D / Metashape volumetric measurement parity item.
* **GeoJSON map annotations layer.** New annotation mode in the GIS
  Workspace sidebar: pin named notes at coordinates and persist them
  as `annotations.geojson` in the project directory; reload them in a
  later session via the file picker.
* **Multi-ROI comparison table.** Save the current measurement polygon
  as a named ROI, then compute side-by-side NDVI / NDRE / EVI / SAVI /
  biomass-proxy means and CHM mean / max / volume for all saved ROIs.
* **Bundled HTML report + one-click render.** New
  `render_dronebio_report()` exported function and "Render HTML report"
  button in the Exports panel. The RMarkdown template at
  `inst/report/biomass_report.Rmd` includes the ODM inventory, reflectance
  summary, per-index histograms, the CHM, and an optional field-based
  biomass model.
* **Ground / vegetation classification.** New
  `classify_ground_vegetation()` for rule-based 5-class NDVI + CHM
  classification, and `classify_ground_csf()` bridging to
  `lidR::classify_ground()` with the Cloth Simulation Filter.
* **Time-series registry and Shiny panel.** Register multiple flights
  of the same site in a CSV registry, then plot NDVI / biomass /
  CHM means across dates from a new "Time Series" nav panel. New
  exported helpers: `default_flight_registry()`, `register_flight()`,
  `list_flights()`, `flight_time_series()`, `flight_ndvi_mean()`,
  `flight_biomass_proxy_mean()`, `flight_chm_mean()`.

## Performance and robustness

* **COG-style raster tiling.** New `tile_raster_on_map()` private helper
  in the app uses `leafem::addGeotiff()` when the `leafem` package is
  installed, with a process-scoped temp-file cache keyed by raster
  fingerprint. Pan/zoom on large orthomosaics is now incremental rather
  than re-rendering a downsampled image each interaction. Falls back
  cleanly to the existing `addRasterImage()` path when `leafem` is not
  available.
* **Async workflow execution.** `run_dronebio_workflow()` now runs in a
  background R session via `promises::future_promise()` when both
  `future` and `promises` are installed. The Shiny UI stays responsive
  while the workflow writes products to disk; falls back to synchronous
  execution otherwise.
* **Toast-based error UI.** New exported `with_error_toast()` helper
  catches errors inside Shiny reactives and renders them as
  notifications instead of the default red traceback. Applied to the
  six event-driven reactives (mosaic, gis_stack, point_cloud,
  extracted_field, model, workflow). `validate()` / `req()` inline
  messages still work because the helper re-throws `shiny.silent.error`.
* **Reactive caching.** `bindCache()` applied to the mosaic, gis_stack,
  point_cloud and hillshade reactives so re-loading the same inputs is
  an in-memory hit rather than a re-read.

## UX

* **Per-panel help cards.** Each of the (now seven) nav panels opens
  with a Bootstrap info card that explains what the panel does and
  links to the relevant pkgdown vignette.
* **Hillshade overlay.** "Hillshade" added to the GIS overlay choices,
  computed from the DSM via `terra::terrain()` + `terra::shade()`,
  rendered in grayscale beneath the colored overlays. `viridis`
  remains the default for non-vegetation layers; `YlGn` for the
  greenness indices.

## New optional dependencies (Suggests)

* `future`, `promises` - enable the async workflow path.
* `leafem` - enables COG-style raster tiling.

# DroneBioR 0.2.0

This release prepares the package for an open-source release for the academic
and research community.

## Major changes

* Documentation, README and metadata overhauled for a public release on
  GitHub.
* `DESCRIPTION`: declared `terra` and `sf` as `Imports` (they were used by
  exported functions but listed only in `Suggests`); added `URL`, `BugReports`,
  `SystemRequirements`, `VignetteBuilder` and `Language`; normalized
  `Authors@R` to a single canonical entry.
* License file `LICENSE.md` added (full GNU AGPL v3 text). License field
  standardized to `AGPL (>= 3)`.
* New `.gitignore` and updated `.Rbuildignore`.
* `convert_undistorted_tiffs_for_texturing()` now exported (its `.Rd` was
  orphaned in 0.1.0).
* `NAMESPACE` rebuilt with proper `importFrom()` directives for base packages.
* GitHub Actions workflow `R-CMD-check.yaml` added.
* New `CONTRIBUTING.md`, `CODE_OF_CONDUCT.md` and `inst/CITATION` files.
* Vignettes added under `vignettes/`:
  - `dronebior-overview` - end-to-end workflow.
  - `external-engines` - reading products from ODM, WebODM, Pix4Dmapper and
    Agisoft Metashape.
  - `spectral-indices` - the index catalogue and recommended use.
  - `point-clouds-and-chm` - dense cloud, DSM/DTM and CHM analyses.
* Runnable `@examples` block on every one of the 34 exported functions.
  Examples use small synthetic fixtures in `inst/extdata/` (multispectral
  orthomosaic subset, DSM, DTM and field CSV; 17 KB total) generated by
  `data-raw/build_fixtures.R`.
* `testthat` suite expanded from 5 to 45 tests, covering raster I/O,
  field data, project paths, image manifest, point cloud helpers and the
  full `run_dronebio_workflow()` pipeline against the bundled fixtures.
* `pkgdown` site configuration (`_pkgdown.yml`) grouping the reference
  by theme, plus a `pkgdown.yaml` GitHub Actions workflow that deploys
  the site to GitHub Pages on push to `main`.
* `CITATION.cff` (GitHub-readable software citation) added at the repo
  root.
* README overhauled with build/license/lifecycle badges, installation
  instructions, a quick-start snippet and a documentation index.
* Issue templates (`bug_report.yml`, `feature_request.yml`,
  `config.yml`) and a pull request template added under `.github/`.

## Internal

* Added a package-level `_PACKAGE` documentation with overview and engine
  references.

# DroneBioR 0.1.0

* Initial development release. Scope: read MicaSense images, drive ODM via
  Docker, read multispectral orthomosaics, scale reflectance, compute
  vegetation indices, extract field samples, fit a baseline biomass model,
  and provide a Shiny app prototype.
