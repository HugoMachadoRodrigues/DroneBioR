# Changelog

## DroneBioR (development version)

## DroneBioR 0.4.0

A second pass of feature work on top of 0.3.0, focused on the 3D
Modeling panel of Drone Biomass Studio and on the scientific volume math
used by the survey-grade workflow. Same scope rule as before: the
package stays out of SfM / MVS / mesh / texturing, and reaches the
deliverables a Pix4D Mapper or Metashape user would expect on the
analysis side.

### New features

- **Survey-grade volume calculations.** New exported
  `compute_survey_volumes(top, roi, method, ...)` implements the six
  base-reference methods photogrammetric surveyors use - DTM (canopy
  biomass), min Z (classic stockpile), mean Z, ground quantile,
  user-defined plane, and Delaunay TIN through the perimeter vertices
  (the Pix4D / ContextCapture / Trimble standard for stockpiles). The
  result reports cut / fill / net volumes, planimetric area, the 3D
  draped surface area via the secant-of-slope formula, perimeter, cell
  area and count, and per-surface min / median / mean / max summaries.
  `interp` added to Suggests for the TIN method. Wired into the 3D
  Modeling panel as a new “Survey volumes” card driven by the convex
  hull of the currently-selected points.

- **3D Modeling panel redesign.** Renamed from “3D & Tree Metrics”
  (which buried everything that is not tree work). The 3D viewport now
  spans the full panel width at clamp(520px, 70vh, 820px) height;
  metrics moved into a
  [`bslib::navset_card_tab`](https://rstudio.github.io/bslib/reference/navset.html)
  below with nine tabs (Selection / Survey volumes / Trees / Vertical
  profile / Manual crowns / Distance / 2D context / Export / Tool
  reference). New toolbar above the viewer: Browse files / Load 3D scene
  / Reset view / Zoom + / Zoom -. The Reset / Zoom buttons drive
  OrbitControls via a `window.__dronebior_viewer` global so R-side
  actionButtons can re-frame the camera.

- **Textured OBJ mesh loader for the 3D viewer.** New opt-in checkbox in
  the 3D Modeling sidebar registers the ODM textured output directory as
  a Shiny resource path and pulls `odm_textured_model_geo.obj` plus its
  MTL plus all referenced texture PNGs via three.js OBJLoader /
  MTLLoader. Vertex positions are rewritten into the viewer’s local
  coordinate frame so the mesh and the point cloud share a single
  transform. Falls back to a plain MeshLambertMaterial if the MTL fails.

- **Live scale bar in the 3D viewer.** Projects two world points 1 m
  apart at the OrbitControls target depth, measures the pixel distance,
  picks a nice round meter value (1 / 2 / 5 / 10 / 20 / 50 / 100 / …) so
  the bar lands near 110 px, and updates the bottom-left overlay DOM ~10
  Hz from the animation loop. Re-scales smoothly as the user pans /
  zooms.

- **Corner XYZ orientation gizmo.** Separate three.js scene with an
  AxesHelper plus coloured sphere tips (X red, Y green, Z blue). A
  112x112 transparent WebGL renderer is overlaid on the viewer
  bottom-right; its camera is reoriented every frame to match the main
  camera’s view direction. Same convention Pix4D / Blender use; users
  get a fast orientation cue without a separate viewport.

- **Color stretch toggle on the GIS map.** New selector “Color stretch”:
  “Fixed semantic” (default), “Data range”, “Percentile 2-98”. Fixed
  semantic pins NDVI / NDRE / NDWI / GNDVI / VARI / Biomass_Index_Proxy
  to \[-1, 1\] (yellow = 0 is the biophysical vegetation boundary),
  MSAVI2 and reflectance bands to \[0, 1\], EVI / SAVI / CIrededge fall
  through to the data range (no canonical bounded range). Same value on
  two flights always produces the same colour, which is the convention
  for time-series panels in remote-sensing publications. The bottom-left
  legend reflects the active domain exactly.

- **GIS Workspace measurement and annotation tools.** “Measure volume
  (CHM)” picks polygon vertices, reprojects into the CHM CRS and routes
  through
  [`compute_chm_roi_metrics()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/compute_chm_roi_metrics.md).
  Annotation mode pins named markers on the basemap and persists them as
  GeoJSON. Multi-ROI comparison table averages NDVI / NDRE / EVI / SAVI
  / Biomass_Index_Proxy plus CHM mean / max / volume across named
  regions. Annotations and ROIs now both save under
  `<project_dir>/studio_assets/` for consistency, with the path
  displayed in the sidebar. ROIs auto-load on session start.

- **ROI delete / redraw workflow.** “Saved ROIs” dropdown plus “Redraw
  selected ROI” (deletes and re-enters draw mode with the same name) and
  “Delete selected ROI” (named-only delete that leaves the rest of the
  collection intact). Vertex-by-vertex editing is still not supported;
  the new redraw flow is the pragmatic replacement.

- **PDF / HTML report.** New exported
  [`render_dronebio_report()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/render_dronebio_report.md)
  and a one-click “Render HTML report” button in the Exports panel. The
  bundled `inst/report/biomass_report.Rmd` template includes the ODM
  inventory, per-band reflectance summary, per-index histograms, CHM
  map, optional field-based biomass model.

- **Time-series tracking across flights.** New registry-based workflow:
  [`register_flight()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/register_flight.md),
  [`list_flights()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/list_flights.md),
  [`flight_time_series()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/flight_time_series.md)
  with three stock summary helpers (`flight_ndvi_mean`,
  `flight_biomass_proxy_mean`, `flight_chm_mean`). New “Time Series” nav
  panel in the Shiny app with a metric selector and a plot across the
  registered dates.

- **Ground / vegetation classification.** New
  [`classify_ground_vegetation()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/classify_ground_vegetation.md)
  for rule-based 5-class NDVI + CHM classification, and
  [`classify_ground_csf()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/classify_ground_csf.md)
  bridging to
  [`lidR::classify_ground()`](https://rdrr.io/pkg/lidR/man/classify.html)
  with the Cloth Simulation Filter.

### Performance and robustness

- **Async workflow execution.**
  [`run_dronebio_workflow()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/run_dronebio_workflow.md)
  runs in a background R session via
  [`promises::future_promise()`](https://rstudio.github.io/promises/reference/future_promise.html)
  when both `future` and `promises` are installed. UI stays responsive
  while the workflow writes products to disk.

- **COG-style raster tiling.** New `tile_raster_on_map()` private helper
  uses
  [`leafem::addGeotiff()`](https://r-spatial.github.io/leafem/reference/addGeotiff.html)
  when `leafem` is installed, with a process-scoped temp-file cache
  keyed by raster fingerprint. Pan/zoom on large orthomosaics is
  incremental rather than re-rendering a downsampled image.

- **Toast-based error UI.** New exported
  [`with_error_toast()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/with_error_toast.md)
  helper catches errors inside Shiny reactives and renders them as
  notifications. Applied to the six event-driven reactives.
  `shiny.silent.error` re-thrown so existing `validate()` / `req()`
  inline messages keep working.

- **Reactive caching.** `bindCache()` applied to the IO reactives in the
  GIS Workspace (mosaic, gis_stack, point_cloud, hillshade).

### UX

- **Branded navbar.** New 400-px DroneBioR logo at 120 px height in the
  Drone Biomass Studio navbar. Matching `man/figures/logo.png` is
  auto-picked up by pkgdown for the doc site.

- **Per-panel help cards.** Each of the seven nav panels opens with a
  Bootstrap info card explaining what the panel does and linking to the
  relevant pkgdown vignette.

- **Hillshade overlay.** “Hillshade” added to the GIS overlay choices,
  computed from the DSM via
  [`terra::terrain()`](https://rspatial.github.io/terra/reference/terrain.html) +
  [`terra::shade()`](https://rspatial.github.io/terra/reference/shade.html),
  rendered in grayscale beneath the colour overlays.

- **Hover values formatted to 2 decimals.** Configured leafem’s
  imagequery so the per-raster readout stops bleeding 15 decimal places
  of floating-point noise; moved to bottom-right so it stays clear of
  the top-right layers control. A custom message handler also sweeps the
  widgets on overlay reload / clear so they never accumulate across
  cycles.

### Bug fixes

- `bindCache()` must be called before `bindEvent()`, not after; the
  earlier ordering crashed the server function during init, which
  silently broke unrelated observers (file browser, etc.).
- [`configure_proj_database()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/configure_proj_database.md)
  no longer warns inside
  [`run_dronebio_workflow()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/run_dronebio_workflow.md)
  when proj.db is reachable through other channels (Debian/Ubuntu apt
  PROJ paths added).
- [`terra::extract()`](https://rspatial.github.io/terra/reference/extract.html)
  on a matrix does not accept `ID = FALSE`; removed for the
  perimeter-TIN path.
- `compute_survey_volumes(top, roi)` returns an NA-filled object when
  the ROI does not intersect the raster, rather than the default
  [`terra::crop()`](https://rspatial.github.io/terra/reference/crop.html)
  error.

### New optional dependencies (Suggests)

- `interp` - perimeter-TIN base for
  [`compute_survey_volumes()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/compute_survey_volumes.md).

## DroneBioR 0.3.0

This release focuses on making the Shiny app (Drone Biomass Studio)
mature enough for daily research use, and on closing the user-facing
feature gap versus Pix4D Mapper and Agisoft Metashape. The scope
explicitly excludes SfM / MVS / mesh / texture work (still delegated to
ODM / WebODM / Pix4Dmapper / Agisoft Metashape), and focuses on the
scientific R layer and the deliverables the user actually consumes from
the app.

### New features

- **Sample project for first-run experience.** New
  [`dronebio_sample_project()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/dronebio_sample_project.md)
  and `run_drone_biomass_studio(sample = TRUE)` seed a clickable
  ODM-shaped project from the bundled fixtures so the app works
  immediately without flight data.
- **CHM volume measurement on the GIS map.** New “Measure volume (CHM)”
  tool: click polygon vertices on the basemap and the summary table
  reports CHM area, mean and max height, and surface volume - the Pix4D
  / Metashape volumetric measurement parity item.
- **GeoJSON map annotations layer.** New annotation mode in the GIS
  Workspace sidebar: pin named notes at coordinates and persist them as
  `annotations.geojson` in the project directory; reload them in a later
  session via the file picker.
- **Multi-ROI comparison table.** Save the current measurement polygon
  as a named ROI, then compute side-by-side NDVI / NDRE / EVI / SAVI /
  biomass-proxy means and CHM mean / max / volume for all saved ROIs.
- **Bundled HTML report + one-click render.** New
  [`render_dronebio_report()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/render_dronebio_report.md)
  exported function and “Render HTML report” button in the Exports
  panel. The RMarkdown template at `inst/report/biomass_report.Rmd`
  includes the ODM inventory, reflectance summary, per-index histograms,
  the CHM, and an optional field-based biomass model.
- **Ground / vegetation classification.** New
  [`classify_ground_vegetation()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/classify_ground_vegetation.md)
  for rule-based 5-class NDVI + CHM classification, and
  [`classify_ground_csf()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/classify_ground_csf.md)
  bridging to
  [`lidR::classify_ground()`](https://rdrr.io/pkg/lidR/man/classify.html)
  with the Cloth Simulation Filter.
- **Time-series registry and Shiny panel.** Register multiple flights of
  the same site in a CSV registry, then plot NDVI / biomass / CHM means
  across dates from a new “Time Series” nav panel. New exported helpers:
  [`default_flight_registry()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/default_flight_registry.md),
  [`register_flight()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/register_flight.md),
  [`list_flights()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/list_flights.md),
  [`flight_time_series()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/flight_time_series.md),
  [`flight_ndvi_mean()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/flight_summary_helpers.md),
  [`flight_biomass_proxy_mean()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/flight_summary_helpers.md),
  [`flight_chm_mean()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/flight_summary_helpers.md).

### Performance and robustness

- **COG-style raster tiling.** New `tile_raster_on_map()` private helper
  in the app uses
  [`leafem::addGeotiff()`](https://r-spatial.github.io/leafem/reference/addGeotiff.html)
  when the `leafem` package is installed, with a process-scoped
  temp-file cache keyed by raster fingerprint. Pan/zoom on large
  orthomosaics is now incremental rather than re-rendering a downsampled
  image each interaction. Falls back cleanly to the existing
  `addRasterImage()` path when `leafem` is not available.
- **Async workflow execution.**
  [`run_dronebio_workflow()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/run_dronebio_workflow.md)
  now runs in a background R session via
  [`promises::future_promise()`](https://rstudio.github.io/promises/reference/future_promise.html)
  when both `future` and `promises` are installed. The Shiny UI stays
  responsive while the workflow writes products to disk; falls back to
  synchronous execution otherwise.
- **Toast-based error UI.** New exported
  [`with_error_toast()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/with_error_toast.md)
  helper catches errors inside Shiny reactives and renders them as
  notifications instead of the default red traceback. Applied to the six
  event-driven reactives (mosaic, gis_stack, point_cloud,
  extracted_field, model, workflow). `validate()` / `req()` inline
  messages still work because the helper re-throws `shiny.silent.error`.
- **Reactive caching.** `bindCache()` applied to the mosaic, gis_stack,
  point_cloud and hillshade reactives so re-loading the same inputs is
  an in-memory hit rather than a re-read.

### UX

- **Per-panel help cards.** Each of the (now seven) nav panels opens
  with a Bootstrap info card that explains what the panel does and links
  to the relevant pkgdown vignette.
- **Hillshade overlay.** “Hillshade” added to the GIS overlay choices,
  computed from the DSM via
  [`terra::terrain()`](https://rspatial.github.io/terra/reference/terrain.html) +
  [`terra::shade()`](https://rspatial.github.io/terra/reference/shade.html),
  rendered in grayscale beneath the colored overlays. `viridis` remains
  the default for non-vegetation layers; `YlGn` for the greenness
  indices.

### New optional dependencies (Suggests)

- `future`, `promises` - enable the async workflow path.
- `leafem` - enables COG-style raster tiling.

## DroneBioR 0.2.0

This release prepares the package for an open-source release for the
academic and research community.

### Major changes

- Documentation, README and metadata overhauled for a public release on
  GitHub.
- `DESCRIPTION`: declared `terra` and `sf` as `Imports` (they were used
  by exported functions but listed only in `Suggests`); added `URL`,
  `BugReports`, `SystemRequirements`, `VignetteBuilder` and `Language`;
  normalized `Authors@R` to a single canonical entry.
- License file `LICENSE.md` added (full GNU AGPL v3 text). License field
  standardized to `AGPL (>= 3)`.
- New `.gitignore` and updated `.Rbuildignore`.
- [`convert_undistorted_tiffs_for_texturing()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/convert_undistorted_tiffs_for_texturing.md)
  now exported (its `.Rd` was orphaned in 0.1.0).
- `NAMESPACE` rebuilt with proper `importFrom()` directives for base
  packages.
- GitHub Actions workflow `R-CMD-check.yaml` added.
- New `CONTRIBUTING.md`, `CODE_OF_CONDUCT.md` and `inst/CITATION` files.
- Vignettes added under `vignettes/`:
  - `dronebior-overview` - end-to-end workflow.
  - `external-engines` - reading products from ODM, WebODM, Pix4Dmapper
    and Agisoft Metashape.
  - `spectral-indices` - the index catalogue and recommended use.
  - `point-clouds-and-chm` - dense cloud, DSM/DTM and CHM analyses.
- Runnable `@examples` block on every one of the 34 exported functions.
  Examples use small synthetic fixtures in `inst/extdata/`
  (multispectral orthomosaic subset, DSM, DTM and field CSV; 17 KB
  total) generated by `data-raw/build_fixtures.R`.
- `testthat` suite expanded from 5 to 45 tests, covering raster I/O,
  field data, project paths, image manifest, point cloud helpers and the
  full
  [`run_dronebio_workflow()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/run_dronebio_workflow.md)
  pipeline against the bundled fixtures.
- `pkgdown` site configuration (`_pkgdown.yml`) grouping the reference
  by theme, plus a `pkgdown.yaml` GitHub Actions workflow that deploys
  the site to GitHub Pages on push to `main`.
- `CITATION.cff` (GitHub-readable software citation) added at the repo
  root.
- README overhauled with build/license/lifecycle badges, installation
  instructions, a quick-start snippet and a documentation index.
- Issue templates (`bug_report.yml`, `feature_request.yml`,
  `config.yml`) and a pull request template added under `.github/`.

### Internal

- Added a package-level `_PACKAGE` documentation with overview and
  engine references.

## DroneBioR 0.1.0

- Initial development release. Scope: read MicaSense images, drive ODM
  via Docker, read multispectral orthomosaics, scale reflectance,
  compute vegetation indices, extract field samples, fit a baseline
  biomass model, and provide a Shiny app prototype.
