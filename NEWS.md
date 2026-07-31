# DroneBioR (development version)

## 3D point cloud

* **"Build DSM, DTM and orthomosaic" (Stage 0, step 3) no longer looks dead.**
  It reruns ODM from `odm_meshing` — a docker job of several minutes — on the
  single R thread, with no feedback emitted before the block, so the click
  froze the whole UI silently (the only sign was the generic heartbeat banner
  after 2.5 s). It now runs in a background worker like the CSF refinement:
  an immediate "Building… in a background worker" notification and a live
  banner, a running-flag that blocks double-clicks, and a success/error toast
  plus a product-status refresh on completion. A synchronous fallback keeps it
  working where `future`/`promises` are unavailable.

* **A stopped Docker daemon now gives a clear message.** `run_odm_project()`
  only checked that the `docker` CLI exists, so a stopped Docker Desktop failed
  deep in the run as a cryptic "exit status 1". It now probes the daemon
  (`docker info`) up front and stops with "Docker is installed but its daemon
  is not responding. Start Docker Desktop…".

## 3D point cloud

* **Deleting / despiking points now actually leaves the viewer.** The 3D
  `point_cloud` reactive is `bindCache()`d on the cloud file *paths*, which do
  not change when an in-place edit (delete / despike / restore) rewrites the
  same file. So even after a successful edit — and even after clicking Load 3D
  again — the cache kept serving the pre-edit cloud, and the deleted points
  never disappeared (and a stale selection then tripped "the selection
  references vertex N but the cloud has M"). The cache key now includes a
  `(size, mtime)` fingerprint of the cloud file, so an edit invalidates it and
  the reload shows the cleaned cloud.

## Field Models

* **The Extract button is no longer a silent dead button.** It was hard-disabled
  until a readable `field_points()`, a ticked covariate AND a loaded mosaic all
  existed at once. Because reading the points needs the mosaic's CRS to reproject
  them, a project with products on disk but no mosaic loaded left Extract
  disabled with no hint why. Extract now enables as soon as a points file is
  staged, and the click's guards name the missing prerequisite in order (upload
  a file → load a mosaic → tick a covariate → fix the column/CRS mapping) with a
  toast, instead of a dead button. The mosaic is now checked before
  `field_points()` so a missing mosaic no longer aborts silently.

## Field Models

* **A projected-coordinate CSV no longer defaults to EPSG:4326 and extracts
  nothing.** The Field Models CRS input defaulted a plain CSV to 4326; a table
  of UTM eastings/northings (e.g. 530078 / 5647710) was then read as
  longitude/latitude, reprojected off the planet, and every sample fell outside
  the orthomosaic — so all covariates came back NA and the caret model "fit
  nothing" (a leaderboard row with "Covariate(s) with no data at any sample:
  ..."). `field_source_columns()` now reports `xy_geographic` by inspecting the
  guessed coordinate columns, and the CRS input defaults to 4326 only when the
  coordinates look geographic; projected coordinates default to the
  orthomosaic's CRS. The value is still editable if the guess is wrong.

## Field Models and 3D editing

* **The Train button is no longer a silent dead button.** The caret method
  picker was created empty (`choices = NULL`) and only filled after a
  catalogue round-trip; the Train button was hard-disabled (client-side
  `disabled`) while the picker was empty, so a click before the round-trip
  landed did nothing at all — no model, no toast. The picker now ships a static
  `lm / pls / ranger` default (the full 137-model catalogue still swaps in and
  preserves the selection), and the Train enable-gate no longer duplicates the
  method-count check, so if the picker is ever empty the click still reaches the
  handler and its "Pick at least one caret method" toast fires. (Training itself
  was never broken — verified R² ≈ 0.9 on the synthetic table.)

* **The 3D viewer refreshes after a cloud edit.** Despike / delete / restore
  rewrite `odm_filterpoints/point_cloud.ply` in place, but the viewer only
  rebuilt on a "Load 3D scene" click, so it kept showing the pre-edit cloud and
  a selection whose point-ids indexed it — the next edit then hit "The selection
  references vertex N but the cloud has M." Each edit now re-triggers the scene
  load (when the scene is mounted) so the cleaned cloud shows immediately and
  the selection resets against it.

## Point cloud despiking

* **A one-click despiker removes the reconstruction spikes from the 3D cloud.**
  ODM's `--pc-filter` only strips sparse statistical outliers; it cannot touch
  the coherent vertical "needles" that dense reconstruction of low-texture
  vegetation leaves behind, so the 3D view stayed spiky at any std-dev setting.
  New `despike_point_cloud()` / `despike_ply()` apply the two standard
  point-cloud denoisers (as in CloudCompare / PDAL / PCL / lidR): 3D
  **statistical outlier removal** for sparse floaters and needle tips, and a
  **height-above-surface** filter that estimates a robust local ground/canopy
  base on a grid (lower-envelope of a low per-cell quantile and its
  neighbourhood, so a whole clump of spike points is still measured against the
  real surface) and drops points standing more than a chosen height above it.
  On the sample MicaSense cloud this cut the height-above-surface tail from ~28
  m to ~3 m and roughly halved the DSM roughness, while leaving genuinely high
  terrain intact. Exposed in 3D Modeling → "Point Cloud step 2" as a **Despike**
  button with a "max height above local surface" slider; it writes the cleaned
  cloud with the same original-snapshot / restore / rerun-from-`odm_meshing`
  flow as manual editing, so the products rebuild from the cleaned cloud.
  Adds `RANN` to Suggests (the SOR pass degrades gracefully without it).

## Shiny responsiveness

* **The active tab now finalizes and renders on its own, instead of only after
  switching tabs and back.** The "now loading" banner helper pumped the libuv
  event loop with `httpuv::service(0)` from inside a running reactive, to force
  its banner to appear before a long synchronous block. Pumping the loop from
  inside a reactive is unsupported: it ran Shiny's deferred flush / `startCycle`
  / promise callbacks while the cycle was mid-flight, so outputs recomputed in
  that cycle stayed unsent and queued client inputs never started a new cycle —
  nothing updated until a fresh client event (a tab switch) drained everything
  at once. The heartbeat that clears the banner rode the same disrupted path, so
  the yellow "R is processing…" banner stuck too. Removed the pump;
  `sendCustomMessage()` already writes to the socket, and long synchronous
  blocks still surface via the client-side heartbeat watchdog. This was the root
  cause behind "the tab only updates when I click away and back" across GIS
  load, field extraction, caret training and CSF refinement. A test fails if any
  `httpuv::service()` / `later::run_now()` call reappears in the app.

## OneDrive / cloud-sync robustness

* **Product availability is read only from the project, never from a stale
  cache.** The app kept an optional local copy of the heavy outputs under
  `~/.dronebior/cache/<slug>/`, and product checks fell back to it. Because the
  slug was just the project folder name, loading a new set of images under the
  same root inherited a previous run's cached files: the status card lit
  Ortho / DSM / DTM green while "no products on disk yet" showed right beside
  it, and the app read products that did not belong to the new flight. The
  entire local-copy cache is removed — `sync_outputs_to_local_cache()`,
  `cache_aware_path()`, `local_cache_dir()`, the "Copy outputs to local cache"
  banner and every cache fallback in `quick_outputs_check()`,
  `validate_odm_outputs()`, `build_chm_raster()` and the CSF path. Reads and
  writes now go straight to the project's own files. (The in-memory raster-tile
  and PLY-header memos, keyed on file identity, are unaffected.)

* **Editing the point cloud on a cloud-synced project is no longer glacial.**
  `write_ply_subset()` streamed its chunks into a staging file *inside* the
  project folder; on a macOS OneDrive folder every read/write is proxied
  through the OS file provider, turning a ~0.3 s rewrite of a 300k-vertex cloud
  into minutes of an apparently frozen "Rewriting the point cloud…". It now
  detects a cloud-sync destination and streams to local disk, copying the
  finished cloud in once, and takes the original-snapshot with a cheap rename
  instead of a full-file copy. On a plain disk the behaviour is unchanged (it
  still stages beside the destination for an atomic rename). `write_ply_subset()`
  gains no new arguments.

* **CSF ground refinement fails fast when `RCSF` is missing.** lidR's
  cloth-simulation filter delegates to the separate `RCSF` package; without it
  the run died deep inside lidR with a terse "Package 'RCSF' needed".
  `classify_ground_csf()` now checks for it up front and names both packages,
  and `RCSF` is declared in `Suggests`.

## Point cloud editing (Stage 0)

* **Editing the cloud no longer overwrites the reconstruction.** Deleting
  points used to rewrite `odm_filterpoints/point_cloud.ply` in place, keeping
  only a hidden `point_cloud.ply.orig` dotfile. The edit now works on a copy:
  before the first deletion the untouched reconstruction is set aside under a
  visible, named `point_cloud.original.ply`, and the working cloud ODM meshes
  keeps the canonical name (ODM has no flag to point meshing at any other
  file). "Restore the original cloud" reads that snapshot; clouds edited before
  this change still restore from the legacy `.orig`. A fresh Stage-0 build
  clears a stale snapshot so the next edit captures the new reconstruction.
  `write_ply_subset()` gains a `backup_path` argument for this.

* **The point-cloud reconstruction controls live in one place.** The outlier
  filter (std. dev.) and "Rectify ground points" existed as independent copies
  on both the Point Cloud tab and the Processing Engine tab, with no sync, so
  the two drifted apart silently. They now live only on the Point Cloud tab
  (which already owned the detail level), and the Processing Engine run reads
  those values.

* **Cleaning the cloud is no longer a dead-end.** "2. Inspect and clean it in
  3D" now opens directly on the cleaning panel and explains the round-trip, the
  panel is framed as "Point Cloud step 2", and a "← Back to Point Cloud (build
  products)" button returns to the Stage-0 flow — where previously the user was
  dropped into the 3D Modeling tab with no signal to return for step 3. The
  button also refuses to open the editor when no cloud has been built yet.

## Bug fixes

* **Buttons that refused a guard no longer do nothing at all.** Twenty-four
  guards across the app were written as `validate(need(...))` inside an
  `observeEvent()`. `validate()` raises `shiny.silent.error`, which Shiny
  displays only in a `render*()` output; in an observer there is nothing to
  display it in, so the condition was discarded and the click produced no
  toast, no console message and no visible change. The most costly case was
  Field Models: **Train** looked completely dead whenever a prerequisite was
  missing. New `observer_need()` shows the reason first and then aborts the
  observer the same way, and every observer guard now uses it — including the
  three WebODM credential checks in `launch_odm_run()` and the two overlay
  checks in `render_gis_overlays()`, both of which are only ever reached from
  observers. A test parses `app.R` and fails if a `validate()` reappears in an
  observer.

* **Two warnings printed their own format string as data.** A long message was
  split across four string literals without `paste0()`, so `sprintf()` took the
  first fragment as the format and the other three as arguments: the user saw
  `ODM exited with status post-processing stage (PDF report, hillshade
  preview) failed; the  but orthomosaic ... is present` plus `3 arguments not
  used by format`. Fixed in `run_odm_project()` and `run_one_dji_band()`, with
  a test that walks every `sprintf()` in the package and the app — folding
  literal `paste0()` formats so the fixed shape stays covered — and checks the
  conversion count against the argument count.

* **"No CHM is available" now says which part is missing.** The message could
  not distinguish "no DEMs were discovered" from "the DSM and DTM are there but
  the subtraction failed", which need different fixes. Relatedly, `chm_raster()`
  called `file.exists()` on a product path that is `NULL` when the product was
  never discovered; `file.exists(NULL)` is `logical(0)`, so `if (!...)` raised
  "argument is of length zero", which the caller's `tryCatch()` swallowed and
  reported as a missing CHM.

* **`finalize_dronebio_products()` no longer destroys the point clouds and 3D
  models.** It copied out only the rasters — orthomosaic, DSM, DTM, CHM and the
  computed indices / biomass proxy — and then, with the default
  `remove_scaffolding = TRUE`, deleted the whole ODM tree. Everything else went
  with it: on a real 11-product project that was the 1.1 GB LAS, the LAZ and
  COPC clouds, the 883 MB filtered PLY, the 2.5D mesh, and both textured models
  including the 314 MB GLB the Shiny app's "3D Modeling" tab loads.
  `odm_product_paths()` had resolved every one of them all along; finalize just
  never copied them. It now collects every product that function resolves,
  under names that keep the real extension (`point_cloud.copc.laz` — a two-part
  extension `tools::file_ext()` cannot round-trip — plus `point_cloud.laz` /
  `.las` / `.ply`, `mesh.ply`, `textured_model.glb`, `report.pdf`, and the CSF
  terrain rasters). Multi-file products travel as folders rather than being
  flattened, because their internal references are by bare filename: the
  textured OBJ lands in `textured_model/` with its `.mtl` and all 44 texture
  PNGs under their original names, so `mtllib` still resolves and the Shiny 3D
  tab's "serve the OBJ's folder, probe for `<stem>.mtl`" layout still holds,
  and the 3D / map tile sets are copied whole (a `tileset.json` alone indexes
  payload that would no longer exist).

* **`finalize_dronebio_products()` never deletes a source on the strength of a
  copy that did not land.** Every copy is size-checked afterwards — `file.copy()`
  returning `TRUE` is not proof on a full disk or a stalled cloud-sync folder,
  and this function's next act is `unlink()`. If any copy fails, or if the new
  `products` argument excludes something that is on disk, the scaffolding is
  kept and a warning names what was at risk. The scaffolding sweep also skips
  any directory containing `out_dir`, instead of only one equal to it. Since
  the 3D deliverables are the biggest files a run makes (about 10 GB for the
  project above, copied rather than moved), `products` lets you narrow the set
  when space is tight; anything you drop that exists still blocks the delete.
  `metadata.json` now inventories the non-raster products too, sizing folder
  assets by the folder.

* **The ODM stage history no longer mixes RGB and multispectral runs.** Stage
  durations were pooled across every past run and scaled linearly by image
  count, so a 39-image multispectral run estimated against a history of
  300-image RGB runs put `opensfm` at 48s for a stage that went past 10
  minutes. Multispectral costs far more per image — 12-bit per-band TIFFs, and
  feature matching across NIR / red-edge bands over low-texture canopy — so the
  two are now recorded and read separately. `odm_stage_history.csv` gains a
  `camera` column, and estimates narrow in three tiers: rows from the same
  sensor, else rows whose sensor is unknown, else the hardcoded baseline. Rows
  from a sensor known to be *different* are never borrowed, in either
  direction. Histories written before this change have no `camera` column; the
  column is added as `NA` on read, so those runs stay usable as the unlabelled
  tier and estimates are unchanged until the first labelled run is recorded.
  The camera is taken from the sensor `run_odm_project()` detects, from the
  per-band label in the DJI Mavic 3M runner (`"RGB"`, `"MS_NIR"`, …), and from
  the camera selector in the Shiny app, pinned at run discovery.

* **A MicaSense set and a DJI Mavic 3M flight no longer share a stage
  history.** Both are "multispectral", so the tiers above pooled them — and
  their per-image cost is not comparable. On a 210-image MicaSense dataset
  whose `opensfm` takes about 90 seconds, history rows from 39-image DJI runs
  scaled linearly to 3.5 hours, and the run was quoted `5h 23m remaining`
  against a real cost near 90 seconds. `normalize_camera_type()` now resolves
  the sensor model (`"micasense"`, `"dji_mavic_3m"`, `"sequoia"`) rather than
  the class, and only an exact-sensor match is borrowed. When no history exists
  for the sensor, the fallback tier is rows that are genuinely ambiguous —
  unlabelled, or carrying the bare class label of the same class — so a
  MicaSense estimate can fall back on a generic multispectral run but never on
  a DJI or an RGB one. Rows written with the previous coarse labels stay
  usable in that ambiguous tier.

* **The ODM progress ETA now corrects itself when a stage overruns.** The
  remaining time was the active stage's `max(0, estimate - elapsed)` plus the
  untouched estimates of the pending stages. Once the active stage passed its
  estimate its term pinned to zero, so the ETA froze at "sum of the pending
  estimates" no matter how far the run overran — and those estimates came from
  the same history the run had just disproved. A 39-image multispectral run,
  predicted from a history of 300-image RGB runs, sat at `~3m 18s remaining`
  while a stage estimated at 48s went past 10 minutes. The active stage's
  overrun ratio is now carried over to the stages that have not started, and an
  overrunning stage is treated as half-done rather than as finished, so the same
  run reports about 50 minutes. Guards keep the correction sane: the ratio is
  ignored when the active stage's own estimate is under 30s (real histories put
  `odm_report` at ~0.02s, which would otherwise become a 100x multiplier) and is
  capped at 20x. Estimates behave exactly as before while a stage is inside its
  estimate, and the value is continuous across the boundary.

* **`register_flight()` no longer silently drops flights that share a date.**
  The `flight_id` embeds a hash of `project_dir`, and that hash accumulated in
  a 32-bit integer: it overflowed to `NA` after about nine characters, so every
  realistic project path produced the same digest and every flight on a given
  date got the id `<date>-NA`. Registering a second plot flown the same day
  therefore looked like a duplicate, and `register_flight()` returned without
  writing — the flight was missing from `list_flights()` and
  `flight_time_series()` with no error and no warning. The hash now runs in
  double precision reduced modulo `2^31 - 1` at every step, and the duplicate
  check matches on `date` + `project_dir` directly rather than on the derived
  id, so registries written by the old code do not gain duplicate rows the
  first time each flight is re-registered.

* **Documented examples no longer call an unexported function.** The
  `@examples` blocks of `register_flight()`, `flight_time_series()`, the
  `flight_*_mean()` helpers and `render_dronebio_report()` still called
  `dronebio_sample_project()`, which had been demoted to internal test
  infrastructure. Anyone copying an example out of a help page got
  `could not find function`, and `R CMD check` failed on it. The examples now
  use a temporary directory where only a path is needed, and `\dontrun{}` with
  a real project directory where actual ODM products are required.
  `_pkgdown.yml` no longer lists the helper in its reference index either,
  which is what had been breaking the pkgdown site build.

* **The pkgdown reference index covers the whole public API again.** Every
  function added since May was exported without being listed in
  `_pkgdown.yml`, and pkgdown treats an unlisted public topic as a fatal
  error, so the site build failed after the first problem was cleared. The
  46 missing topics are now filed under sections that match the modules they
  live in, including new ones for WebODM, for the DJI Mavic 3M / PPK /
  GeoScan geolocation helpers, and for field-calibrated biomass mapping.

* **The `app-reference` vignette builds again.** The line documenting the PLY
  vertex layout wrote the property names as `` `r g b views` ``, which knitr
  parses as inline R code and fails on. They are now spelled out as
  `red blue green views`, matching what `read_ply_point_cloud()` actually
  reads. Together with the two fixes above this restores `R CMD check`, which
  had failed on every platform since mid-May.

* **`harmonize_dem_products()` now removes isolated reconstruction spikes
  (the SfM "cones") the height filter missed.** Low-texture grazed pasture
  produces isolated few-metre cone/needle spikes in the DSM; because they are
  *shorter* than `canopy_ceiling`, the height-based tower filter kept them, so
  they showed up as cones in the 3D view. A new area-opening step flattens
  small isolated tall CHM patches (contiguous area `<= max_spike_area_m2`,
  default 10 m^2) to ground while preserving large contiguous canopy, keyed on
  patch *area* rather than height. On a real flight it dropped 91 cone spikes
  and kept a 2820 m^2 tree stand and the 21.8 m tallest canopy untouched. Tune
  with `spike_min_height` / `max_spike_area_m2` / `spike_dilate_cells`, or set
  `max_spike_area_m2 = 0` to disable.

* **The 7-band DJI orthomosaic no longer has a black border.**
  `stack_dji_ortho_from_ms()` dropped the ODM RGB alpha band (band 4) and
  hard-filled the transparent flight-edge border with `0,0,0`, which showed
  up as an ugly black frame (~27% of the frame on a typical pasture) while
  the DEMs and the original ODM ortho stayed clean. It now carries the alpha
  as a validity mask (falling back to "all RGB == 0" when no alpha band is
  present), so the stacked orthomosaic keeps a proper transparent nodata
  border across all seven bands.

* **`finalize_dronebio_products()` no longer silently ships an incomplete
  `products/` folder.** When the indices step crashed or never ran, finalize
  copied the orthomosaic + DEMs, skipped the missing `spectral_indices.tif` /
  `biomass_proxy.tif` without a word, and then deleted the intermediate
  `dronebior_analysis/` folder when `remove_scaffolding = TRUE` — so the
  absence was invisible and the inputs were gone. A new `expect` argument
  lets the caller declare which computed products it asked for; any that are
  missing now raise a clear warning instead of vanishing.

* **The vegetation-index step no longer OOM-crashes the R session on
  high-resolution orthomosaics.** A 3 cm 7-band DJI stack is ~3 GB per
  in-memory copy, and `run_dronebio_workflow()` scaled it to reflectance and
  chained 16 indices with no cap on terra's working memory, so terra tried to
  hold whole stacks in RAM (made worse when Docker is reserving a large share
  of system memory) and the session was killed mid-run. The workflow now caps
  terra's memory (new `max_memory_gb`, default 4 GB; opt out with `NULL` or
  `options(dronebior.skip_terra_memcap = TRUE)`) for the reflectance / index /
  summary / write steps so they stream to disk in blocks, and restores the
  previous terra settings on exit. On a real 9336x6297x7 ortho the 16 indices
  now complete in ~5.5 min instead of crashing.

* **The multispectral run no longer crashes when the DJI Mavic 3M drops
  band frames.** ODM's multispectral grouping requires every capture to
  carry all four bands; the Mavic 3M routinely skips a band frame on turns
  and at the flight ends, which made ODM die at the very first (`dataset`)
  stage ("Cannot match bands by filename ... no images are missing") before
  any product — or the finalize / cleanup step — could run, leaving the
  project folder full of intermediates. `run_odm_dji_mavic_3m()` now keeps
  only captures that have all four `_MS_*` bands (grouped by the shared
  capture key, which is 1:1 with the DJI `CaptureUUID`), reports how many
  incomplete captures were dropped, and rebuilds the MS `images/` folder
  each run so a previous failed run's unbalanced set can't linger and
  re-trigger the crash.

## New features

* **Field-calibrated biomass maps from drone products + sparse ground
  samples (`run_biomass_mapping()` and friends).** A new pipeline turns a
  handful of clipped biomass quadrats, many rising-plate / disc-meter
  heights and the drone CHM + spectral indices into a calibrated
  above-ground biomass map (kg/ha), following Page et al. (2025, *Rangeland
  Ecology & Management*) and Vahidi et al. (2023, *Remote Sensing*).
  `fit_plate_meter()` calibrates biomass against compressed plate height
  (the double-sampling multiplier that turns plate-only points into biomass
  points); `make_biomass_grid()` aggregates the indices + CHM onto a
  management grid (index means, CHM mean/median/max/sd/var, and the Page
  vegetation volume); `build_biomass_calibration()` joins the field points
  to that grid; `fit_biomass_model()` fits a staged model — a parsimonious
  Page linear model and (when `ranger` is available) a Vahidi random forest
  with a categorical pasture term — reporting leave-one-out R2/RMSE/MAE and
  the observed-vs-predicted 1:1-line stats, and keeping the lower-RMSE model
  under `method = "auto"`; and `predict_biomass_map()` writes the
  wall-to-wall `biomass_kgha.tif`. A `field_biomass_plate.csv` template
  ships in `inst/extdata`.

* **`finalize_dronebio_products()` collects a flight into one flat,
  documented folder.** A run otherwise leaves a deep, ODM-shaped tree
  plus raw DEM backups, a redundant RGB-only orthomosaic, the
  reflectance stack and logs. The new helper copies just the products
  you reuse — `orthomosaic.tif`, `dsm.tif`, `dtm.tif`, `chm.tif`,
  `spectral_indices.tif`, `biomass_proxy.tif` — into `<project>/products/`
  under simple names, writes a single `metadata.json` (generator
  version, run parameters, and per raster the CRS, resolution, extent,
  band names and per-band min/mean/max), and removes the intermediate
  scaffolding. The 7-band DJI stack is preferred as the orthomosaic
  when present. Pass `remove_scaffolding = FALSE` to keep the
  intermediates.

* **The DJI Mavic 3M pipeline harmonizes DEMs by default.**
  `run_odm_dji_mavic_3m()` now runs [harmonize_dem_products()] at the
  end of every flight (new `harmonize = TRUE`, `canopy_ceiling = 18`),
  so the DSM, DTM and CHM come out physically consistent (`CHM >= 0`,
  `DSM >= DTM`) and free of reconstruction towers / pits with no extra
  call. The raw ODM rasters are preserved as `dsm_raw.tif` /
  `dtm_raw.tif`, and the canonical `dsm.tif` / `dtm.tif` / `chm.tif`
  are overwritten with the clean versions, so every downstream
  consumer — `build_chm_raster()`, the spectral-index workflow, the
  Shiny app — transparently uses the harmonized products. The step is
  idempotent (it always re-derives from the `*_raw.tif` backups), so
  re-running a project does not compound the cleaning. Pass
  `harmonize = FALSE` to keep the raw ODM DEMs.

* **`harmonize_dem_products()` produces physically consistent DSM,
  DTM and CHM.** ODM builds the DSM and DTM by independent processes,
  so on bare ground the interpolated DTM often sits a few centimetres
  *above* the DSM — making `DSM − DTM` negative over ~15% of a
  short-canopy survey (observed in the raw data, not a despiking
  artifact). The new helper rebuilds all three so they obey
  `CHM >= 0` and `DSM >= DTM` everywhere by construction: it despikes
  the DTM, forms `CHM = DSM − DTM_clean` clamped to `>= 0` (which
  turns the DSM's downward pits back into bare ground) and despiked
  for canopy towers, then rebuilds `DSM = DTM_clean + CHM_clean`.
  Validated on a real flight: the cleaned DSM is never below the DTM
  (0 pixels) and the CHM is never negative, while genuine tree
  canopies survive. Pass a `dronebio_project` or explicit `dsm` /
  `dtm`; it writes `dsm_consistent.tif`, `dtm_consistent.tif` and
  `chm_consistent.tif`.

* **`despike_dem()` iterates to clear wide pits/towers (new
  `iterations`, default 2).** A single pass cannot fully remove a blob
  wider than the trend cell: while the blob is present it drags the
  local trend toward itself, so its deepest core hides (DEM − trend
  stays within threshold there). The function now runs the
  detect-and-fill pass `iterations` times — once the first pass
  replaces the bulk of the blob with surrounding ground, the
  recomputed trend is clean and the residual core stands out and is
  removed next pass. On the user's data the lone remaining downward
  pit (a 6 m-wide, −56 m cone) went from 32 residual sub-ground pixels
  after one pass to **zero** after two; the DSM finished at −3..26 m
  and the DTM at −3..6 m. The loop stops early when a pass changes
  nothing.

* **`despike_dem()` robustifies a spiked ground reference.** When the
  supplied `ground` (e.g. the DTM) carries the *same* spikes as the
  DEM being cleaned — common, since the user confirmed downward
  spikes appear in both the DSM and the DTM — using it directly made
  shared spikes invisible (DSM − DTM looks normal where both dip). The
  height-above-ground pass now (a) replaces any ground cell that
  departs from its own coarse median trend by more than 5 m with the
  trend, and (b) fills ground NoData gaps (where the DEM extends past
  the DTM at the boundary) with the DEM's own coarse trend, so edge
  pits no longer escape for lack of a reference. For DEMs whose ground
  is itself heavily spiked, passing `ground = NULL` (use the
  spike-robust coarse trend directly) is the most reliable choice; on
  a real flight it cut the DSM range from −56..133 m to −21..35 m and
  the DTM from −48..11 m to −7..10 m.

* **`despike_dem()` also fills downward pits.** The height-above-ground
  pass now flags cells more than `max_depth_below_ground` metres
  (default 2) *below* the ground as well as the towers above it — a
  DSM is the top surface, so a pixel well below the terrain is the
  downward-spike artifact visible as needles hanging beneath the
  surface in 3D. On a real flight this removed 1488 sub-ground pixels
  (down to −25 m) alongside the towers. Genuine edge tree canopies
  (smooth, a few metres tall — verified at 0.03 m local roughness)
  sit within the band and are correctly preserved.

* **`despike_dem()` gains a wide-tower (height-above-ground) pass.**
  Some reconstruction artifacts are not single-pixel needles but
  coherent blobs several metres across — a blurry patch ballooning to
  a 130 m tower over an otherwise <15 m surface. A small local-median
  window cannot see those (the tower's own pixels dominate it). New
  `max_height_above_ground` + `ground` arguments add a second detector
  that flags cells taller than a given height above the ground (pass
  the DTM, or let it build a coarse trend surface). On a real flight
  this dropped the DSM ceiling from 133 m to 31 m while the genuine
  ~14 m canopy survived. The local needle pass still runs by default.

* **`despike_dem()` removes isolated DSM / DTM spikes.** Surface
  models routinely sprout a handful of single-pixel "needle" spikes
  tens of metres tall from mis-reconstructed dense-cloud points where
  the imagery was blurry or low-texture — devastating for 3D
  visualisation and slope/volume stats, yet a vanishing fraction of
  pixels (observed: 286 of ~13M, deviating up to 45 m from an
  otherwise locally-smooth surface where 99.9% of pixels vary by
  under 0.3 m). The new exported `despike_dem()` is a local outlier
  filter: it compares each cell to its `window`x`window` neighbourhood
  median and replaces cells deviating more than `max_deviation` metres
  with the local median (or NA). Because real terrain and vegetation
  are spatially coherent, only the isolated spikes are caught — a
  global percentile clip cannot make that distinction. Apply it to a
  raw DSM before 3D rendering: `despike_dem("odm_dem/dsm.tif",
  out_path = "odm_dem/dsm_clean.tif")`.

* **DJI Mavic 3M multispectral bands now reconstructed together
  (clean spectral indices).** `run_odm_dji_mavic_3m()` gains
  `ms_mode = "multispectral"` (the new default). Instead of the legacy
  five independent ODM runs (one per MS band), it now does just two:
  one RGB run for geometry (DSM / DTM / RGB ortho) and one combined
  run on all four `_MS_*.TIF` bands. ODM reads each image's DJI band
  metadata (`BandName`, `RigCameraIndex`, `CentralWavelength`, which
  survive the MakerNote strip) to group the bands by capture,
  reconstructs once, and co-registers the bands onto a common grid.
  The result fixes two problems that plagued the per-band approach:
  the band orthomosaics are now pixel-aligned (so per-pixel index
  ratios like NDVI / NDRE are no longer corrupted by band
  mis-registration), and every index shares the same valid-data
  footprint (previously OSAVI covered ~12% of the area while NDRE
  covered ~54%, because the Red band reconstructed to a different
  extent than RedEdge). It is also faster — two ODM runs instead of
  five. The old behaviour is still available via
  `ms_mode = "per_band"`, and `primary_band` lets you override which
  band drives the multispectral reconstruction.

* **`build_chm_raster()` clips canopy-height outlier spikes.** New
  `outlier_percentile` argument (default `99.5`) sets CHM pixels above
  that percentile to `NA` after differencing, with a message reporting
  how many were dropped and the percentile value. Photogrammetric
  reconstructions routinely leave a sub-1% tail of physically
  impossible spikes (tens to hundreds of metres over short pasture)
  from mis-reconstructed points at edges, water and low-texture areas;
  even though they are rare they wreck colour ramps and contaminate
  biomass statistics. Set `outlier_percentile = 100` to keep every
  pixel.

## Bug fixes

* **`--auto-boundary` by default fixes the orthophoto OOM (exit 137).**
  On a 308-image DJI Mavic 3M flight the pipeline ran 47 minutes,
  wrote a correct 247 m x 484 m DSM and DTM, then the `odm_orthophoto`
  stage was OOM-killed — and the automatic retry died the same way.
  Root cause: a handful of stray, mis-registered points (low feature
  quality scatters them) spread the reconstruction's *Model bounds* to
  ~3.9 km x 6.4 km even though the real footprint was 0.12 km². The
  DSM / DTM are cropped so they survived, but the orthophoto stage
  tried to render the full ~25 km² canvas at 5 cm — billions of pixels
  — and exhausted the 48 GB Docker allocation. `run_odm_dji_mavic_3m()`
  now passes `--auto-boundary` by default (new `auto_boundary = TRUE`
  argument), which crops the reconstruction and the orthophoto to a
  polygon derived from the camera GPS, discarding the stray sprawl.
  The OOM auto-retry inherits the flag, so even a divergent run
  recovers. Set `auto_boundary = FALSE` only for surveys with no
  usable camera GPS.

* **ETA self-corrects + stops over-promising.** The pipeline ETA
  extrapolates recorded per-stage durations linearly by image count.
  Two problems made it wildly wrong: (1) the per-band/per-pipeline
  runs never *recorded* their durations, so the only history was from
  whatever seeded `~/.dronebior/odm_stage_history.csv` (often tiny
  test runs), and (2) the headline number was presented as if
  authoritative — a 39-image history extrapolated to 308 images
  produced a frightening "~35h" estimate even though the real run
  takes well under an hour at low quality. Now `run_docker_with_progress()`
  records each stage's measured duration against the run's image
  count on a clean exit, so the estimate tightens after the first run
  at a given scale; and the estimate banner is labelled as a rough
  extrapolation, naming the image counts it was derived from. The
  estimate still does not model `--feature-quality` / `--pc-quality`,
  so treat it as an upper bound, not a promise.

* **Progress poller now reads ODM's own stage markers.** The poller
  previously inferred the active stage from which stage *directories*
  existed on disk. ODM creates some stage dirs out of order — notably
  `odm_georeferencing/` is materialised early when `--geo` (PPK) is
  used — so the poller reported `odm_georeferencing` (stuck at "2/13
  stages") for the entire opensfm pass, even though opensfm was the
  stage actually running. The poller now parses the authoritative
  `Running <stage> stage` / `Finished <stage> stage` markers from the
  docker log and only falls back to the directory heuristic when the
  log has no markers yet. The stage count, ETA and "active stage"
  line now track reality.

## Performance

* **ODM concurrency auto-detects the machine's cores.** The default
  `max_concurrency` was a hardcoded 4, which left most of a modern
  multi-core machine idle (an Apple M1 Max has 10 cores; 4 workers
  used ~2 of them). `run_odm_dji_mavic_3m()` now defaults
  `max_concurrency = NULL`, resolved at run time to the physical core
  count (capped at 16). Pass an explicit integer to override — lower
  it on memory-constrained machines (each OpenSfM / OpenMVS worker
  uses ~1-2 GB).

* **`run_odm_dji_mavic_3m()` gains speed knobs for orthomosaic-only
  runs.** New `fast_orthophoto`, `build_dsm` and `build_dtm`
  parameters (defaults `FALSE`, `TRUE`, `TRUE`) let callers skip the
  dense MVS reconstruction — usually the single longest ODM stage —
  when only the orthomosaic + spectral indices are needed. Setting
  `fast_orthophoto = TRUE, build_dsm = FALSE, build_dtm = FALSE`
  roughly halves wall-clock per flight on large image sets. The MS
  bands already ran in fast-orthophoto mode; this exposes the same
  control for the RGB run. Combine with
  `rgb_extra_args = c("--feature-quality", "low", "--pc-quality", "low")`
  for the fastest possible pass.

## Bug fixes

* **DJI Mavic 3M: survive ODM's exifread / MakerNote crash.** ODM
  3.6.0 bundles a version of the `exifread` Python library that
  raises `IndexError: list index out of range` inside
  `decode_maker_note()` on some DJI Mavic 3M MakerNote tags, killing
  the run in the very first (`dataset`) stage within seconds. The DJI
  pipeline now strips the offending MakerNote with `exiftool` before
  ODM sees the images: when exiftool is on PATH it copies (never
  hardlinks — a hardlink would corrupt the originals) the band's
  images and removes the `MakerNotes` tag proactively; when exiftool
  is absent it runs as before and, if the crash is detected in the
  docker log, raises a clear "install exiftool (`brew install
  exiftool`)" error instead of an opaque `exit status 1`. Standard
  EXIF (camera model, focal length, dimensions, GPS) and the PPK
  geo.txt are untouched, so reconstruction quality is unaffected.
* **Drone Biomass Studio: progress card now follows the real ODM
  log.** For DJI Mavic 3M runs the "ODM run progress" card was
  pointed at the callr R-message log (`odm_run.log`), which does not
  contain the `Running X stage` / `Finished X stage` markers the card
  parses — so it sat at "0 / 13 stages" and "Total elapsed: --" even
  while ODM was working (or had already crashed). The card now
  watches the RGB band's `dronebior_odm.log`, which carries ODM's
  own stage markers, init timestamp and error tracebacks, so elapsed
  time, the active stage and any failure surface live.



* **Drone Biomass Studio: stop erroring on DJI Mavic 3M folders.**
  The Studio's "Run processing engine" button used to call
  `list_micasense_images()` which rejected DJI filenames with
  `Some image names do not match the expected MicaSense pattern
  'capture_band.tif'`. The Run button now auto-detects DJI Mavic 3M
  datasets via the new exported `has_djim3m_images()` and, when
  found, spawns the full per-band pipeline (`run_odm_dji_mavic_3m()`)
  in a `callr::r_bg()` background R session so the Shiny UI stays
  responsive while the 5 ODM runs + stack execute. callr's
  stdout / stderr are routed to `odm_run.log` so the existing
  "ODM run progress" card displays the run live. The same
  `has_djim3m_images()` gate now also fronts the `manifest` reactive
  so the image-count and flight overlay readouts no longer crash
  on a DJI folder. `callr` joins `Suggests` for the background
  execution; the Studio prints a clear install hint when it is
  missing.



* **Progress line no longer freezes on RStudio Console.** The
  previous release tried to render the per-poll status as a single
  carriage-return-updated sticky line. That worked in real terminals
  but rendered as a single frozen-looking line in RStudio Console
  on some R versions, hiding the per-15-s update entirely. The
  status now prints as a fresh `message()` line each poll. Because
  docker's verbose output is already redirected to
  `<project_dir>/dronebior_odm.log`, the console only has our
  poller writing to it, so a "one short line per poll" stream is
  uncluttered AND renders reliably everywhere R runs.
* **Final summary names the docker exit status and on-disk
  products.** `run_docker_with_progress()` now prints, right after
  the subprocess exits, a single line listing the docker exit code
  and a `ortho YES, dsm YES, dtm NO, las NO`-style breakdown of the
  four artefacts DroneBioR cares about. Surfaced after every band
  in the DJI Mavic 3M pipeline so the user can immediately tell
  whether the run actually delivered the expected products without
  having to inspect the project tree by hand.

## Changes

* **`run_odm_dji_mavic_3m()` PPK CLI is now opt-OUT, not opt-IN.**
  The `ppk_cli` argument now defaults to `"auto"`: on every run
  DroneBioR probes the system for `rnx2rtkp`, a DJI `.bin` -> RINEX
  converter (candidate command names: `klauppk_dji_to_rinex`,
  `klauppk`, `dji_to_rinex`, `djiparsekit`, `djirinexconverter`,
  `convbin`), and a base-station RINEX observation file located via
  the `DRONEBIOR_PPK_BASE_OBS` environment variable, the
  `dronebior.ppk_base_obs` R option, or `<images_dir>/base/*.obs`.
  When every piece is in place, a full PPK cycle (`.bin` -> RINEX
  -> rtklib `rnx2rtkp` -> .MRK rewrite) runs before ODM. When any
  piece is missing, DroneBioR emits a single message naming what is
  missing and falls back to the .MRK-as-shipped path. Pass `NULL` /
  `FALSE` to disable the CLI step explicitly, or pass an explicit
  [ppk_cli_rtklib_dji()] hook to override the auto-detect.

## New features

* **Native PPK / RTK support for DJI Mavic 3M.** When the source
  folder ships the `_Timestamp.MRK` sidecar(s) the drone writes
  alongside each mission's photos, `run_odm_dji_mavic_3m()` now
  auto-detects them, resolves every per-band image filename to its
  `photo_num` (DJI files follow `DJI_<datetime>_<NNNN>_<band>.<ext>`),
  and writes an ODM `geo.txt` so ODM uses the RTK / PPK positions
  instead of the EXIF GPS (which the Mavic 3M corrupts on
  altitude — the bug that was making OpenSfM diverge and
  `odm_orthophoto` OOM). ODM is then invoked with `--geo
  /datasets/<proj>/geo.txt --gps-accuracy 0.10`. The .MRK itself
  carries the receiver's fix-quality code; rows below the
  `ppk_min_fix_quality` threshold (default 4 = RTK Float) are
  dropped from the geo.txt so degraded positions cannot destabilise
  the bundle adjustment.
* **External PPK CLI hook.** New `ppk_cli` argument on
  `run_odm_dji_mavic_3m()` and a ready-made adapter
  `ppk_cli_rtklib_dji()` invoke a real PPK CLI **before** ODM:
  1. Convert the DJI `_PPKRAW.bin` rover observations to RINEX with
     a user-supplied converter (DJI's `.bin` format is proprietary,
     so this must be a tool the user installed separately — e.g.
     KlauPPK or any DJI-compatible converter; mainstream
     rtklib `convbin` does not handle DJI `.bin`).
  2. Run rtklib's `rnx2rtkp` against the rover RINEX, the
     `_PPKNAV.nav` ephemerides and the user-supplied base station
     RINEX, producing a positioning solution.
  3. Rewrite each `_Timestamp.MRK` in place by matching the .MRK
     GPS time-of-week column to the .pos epochs (within 1 s), then
     proceed with the regular .MRK -> geo.txt flow above. The hook
     fails loudly when its required CLI tools are missing from
     `PATH`; DroneBioR then falls back to the .MRK-as-shipped path.
* **New exported helpers** for direct use: `detect_djim3m_ppk_files()`,
  `parse_djim3m_mrk()`, `parse_djim3m_mrk_folder()`,
  `inspect_djim3m_mrk()`, `write_djim3m_geo_txt()`,
  `ppk_cli_rtklib_dji()`. Inspect the .MRK before plumbing it into
  ODM with `inspect_djim3m_mrk(images_dir)`.

## Documentation

* **More honest diagnosis when ODM exits 137 twice.** The previous
  release added an auto-retry for `exit status 137` (Docker OOM
  kill) and an error message that named Docker Desktop's memory
  cap as "almost always" the cause. Observed in the wild: a user
  with 48 GB allocated to Docker Desktop still hit 137 — the
  actual cause was the SfM bundle adjustment diverging on a
  Mavic 3M flight with bad EXIF altitudes, which made `odm_orthophoto`
  try to write a 17 km x 17 km orthomosaic at 5 cm resolution
  (~460 GB of pixels) and OOM regardless of cap. The "twice in
  a row" error message now names both failure modes explicitly:
    1. Docker memory cap (raise via Docker Desktop -> Resources)
    2. SfM divergence -> oversized orthophoto (check `log.json`
       for `Model bounds` / `Model area`; remedy is to apply PPK
       corrections to the EXIF GPS, or pass tighter SfM
       constraints via `extra_args`:
         `c("--gps-accuracy", "3",
            "--matcher-neighbors", "8",
            "--feature-quality", "medium")`).
  The auto-retry behaviour itself is unchanged — only the diagnostic
  copy.

## Bug fixes

* **Auto-retry on `exit status 137` (Docker OOM kill).** ODM Docker
  runs occasionally crash with `exit status 137 = 128 + SIGKILL(9)`,
  which means the host OS killed the container — almost always
  Docker Desktop's memory cap being too low for OpenSfM /
  OpenMVS peak usage on 300+ image flights. Observed on the MS_R
  band of a 308-image DJI Mavic 3M flight (\`ODM failed on band
  MS_R (exit status 137).\`). Both `run_odm_project()` and
  `run_one_dji_band()` now detect 137 specifically: they wipe any
  partial ODM state and retry once with `--max-concurrency 1` +
  `--feature-quality medium`, which roughly halves peak memory
  and usually fits inside an 8 GB Docker allocation. If the retry
  also dies with 137, the error message now spells out the actual
  cause and the remedy (Docker Desktop -> Settings -> Resources
  -> Memory, raise to >= 16 GB).

## Cleanup

* **`run_odm_dji_mavic_3m()` strips the canonical `dji/` project to
  its final products.** After the 7-band stack is on disk, the helper
  removes every directory and file inside `dji/` except `odm_dem/`
  (DSM/DTM/CHM), `odm_orthophoto/` (RGB ortho + 7-band stack),
  `log.json` (ODM log) and `dronebior_odm.log` (our docker output).
  Discarded: `images/` (input hardlinks), `opensfm/`, `openmvs/`,
  `odm_filterpoints/`, `odm_georeferencing/`, `odm_postprocess/`,
  `cameras.json`, `images.json`, `img_list.txt`, `benchmark.txt`.
  None of these are read by the downstream R workflow — the
  georeferencing information is already baked into the GeoTIFF
  headers. The previous `cleanup_ms_workspaces` argument is renamed
  to `cleanup_intermediates` and now controls both this RGB-project
  cleanup and the per-band MS workspace removal added in the
  previous release.



* **`run_odm_dji_mavic_3m()` removes the per-band MS workspaces
  after stacking.** Each MS band needs its own ODM project
  workspace (`dji_ms_g/`, `dji_ms_r/`, `dji_ms_re/`, `dji_ms_nir/`)
  because the DJI Mavic 3M visible camera and the MS camera array
  have different focal lengths and viewing geometry — they cannot
  share one ODM bundle adjustment. Each MS workspace produces a
  single-band orthomosaic that goes straight into the 7-band stack
  written under `dji/odm_orthophoto/`; nothing downstream reads
  the MS workspaces again. They are now deleted automatically once
  the stack is on disk, leaving the user with a single canonical
  `dji/` folder that holds every product worth keeping (RGB ortho,
  stacked 7-band ortho, DSM, DTM, CHM). Set
  `cleanup_ms_workspaces = FALSE` to keep them for debugging.

## Performance

* **`run_odm_dji_mavic_3m()` skips ODM stages DroneBioR does not
  consume by default.** The DJI Mavic 3M pipeline now passes
  `--skip-3dmodel --skip-report` to every ODM invocation and no
  longer exports the dense LAS point cloud. The skipped artifacts
  are the textured 3D model (`.obj` / `.glb`, produced by
  `odm_meshing` + `mvs_texturing`, 10-30 min on a 300-image flight),
  the PDF run report (~1-2 min, also the source of the known
  `gdal_translate` / numpy crash inside some ODM Docker images),
  and the LAS export (~10 s + ~640 MB on disk). The remaining
  ODM stages — `dataset`, `split`, `merge`, `opensfm`, `openmvs`,
  `odm_filterpoints`, `odm_georeferencing`, `odm_dem`,
  `odm_orthophoto`, `odm_postprocess` — are exactly what the
  pipeline needs to produce DSM, DTM, orthomosaic (and therefore
  CHM and vegetation indices). Expect ~15-30 min saved per flight.
  Users who want the textured 3D model or the LAS for
  `improve_dtm_csf()` can opt back in via the new `skip_3dmodel`,
  `skip_report` and `pc_las` arguments on `run_odm_dji_mavic_3m()`.
* **`build_odm_args()` gains `skip_3dmodel` and `skip_report`
  parameters** for the same flags, defaulting to `FALSE` to keep
  the existing `run_odm_project()` behaviour unchanged.

## Bug fixes

* **Stage poller no longer mis-identifies stale stage dirs as active.**
  Previous failed ODM runs leave downstream stage directories on disk
  (e.g. `odm_georeferencing/`). The poller picked the latest-by-
  canonical-order directory as the active stage, so even when the
  current run was actually grinding through `opensfm`, it would
  report "stage `odm_georeferencing` ... ~10 min remaining" — a
  wildly wrong ETA that made users think the run was wedged. The
  poller now compares each stage directory's mtime against the
  poller's start time and ignores anything older, so only stages
  actually touched by the current run count.

## Changes

* **Sticky one-line progress bar during ODM runs.** Previously
  `run_docker_with_progress()` emitted every poll as a fresh
  `message()` line and let docker's verbose stdout flow into the
  same console, so the status scrolled up and was lost between
  ODM's own log lines within seconds. The helper now redirects
  docker stdout / stderr to `<project_dir>/dronebior_odm.log`
  (`tail -f` it in another terminal if you want the raw output)
  and renders a single sticky status line that updates in place
  via `\r`. Stage-transition events (`-> stage 'opensfm' started`)
  still appear as ordinary lines above the sticky status, so the
  scrollback shows the milestones without burying the live ETA.

## Bug fixes

* **Auto-clean orphan OpenSfM state before re-launching ODM.** When a
  previous ODM run was interrupted between feature extraction and
  feature matching (Ctrl-C, crash, OOM, the `read_output(timeout=...)`
  bug fixed in the prior release, etc.) the project directory was
  left in a state where `opensfm/features/` existed but was missing
  some `*.features.npz` files. ODM's resume detection saw the
  features directory and skipped extraction, then crashed deep
  inside the joblib worker with

      FileNotFoundError: '/datasets/.../features/...features.npz'

  `run_odm_project()` and `run_one_dji_band()` now call a new
  internal helper `clean_incomplete_odm_state()` immediately before
  invoking docker. The helper detects the partial state
  (`opensfm/features/` present but `opensfm/reconstruction.json`
  absent) and wipes both the `opensfm/` directory and every
  downstream stage directory, so ODM starts cleanly. `images/` is
  preserved so the hardlinks don't have to be rebuilt.



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
