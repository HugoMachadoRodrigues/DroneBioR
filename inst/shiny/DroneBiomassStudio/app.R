library(DroneBioR)
library(shiny)
library(bslib)
library(leaflet)
library(terra)

# Configure async execution. With `future` and `promises` installed, the
# heavy workflow run (run_dronebio_workflow) executes in a background R
# session so the UI stays responsive. Falls back to synchronous execution
# when either package is missing.
.dronebior_async_available <- requireNamespace("future", quietly = TRUE) &&
  requireNamespace("promises", quietly = TRUE)
if (.dronebior_async_available) {
  if (!inherits(future::plan(), "multisession")) {
    future::plan(future::multisession,
                 workers = max(1L, future::availableCores() - 1L))
  }
}

default_project_dir <- getOption("dronebior.project_dir", getwd())
default_project <- dronebio_project(default_project_dir)
default_products <- odm_product_paths(default_project)

default_full_cloud <- function(products) {
  candidates <- unname(c(
    products[["point_cloud_las"]],
    products[["point_cloud_laz"]],
    products[["point_cloud_copc"]]
  ))
  existing <- candidates[file.exists(candidates)]
  if (length(existing) > 0) existing[[1]] else candidates[[1]]
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

downsample_raster <- function(x, size = 90000) {
  terra::spatSample(x, size = size, method = "regular", as.raster = TRUE, na.rm = FALSE)
}

# All user-created GIS Workspace artefacts (annotations and ROIs) are
# saved under a single subfolder of the project so the user has one
# obvious place to look in and version-control. Selection exports from
# the 3D & Tree Metrics tab still go to the user-chosen `output_dir`,
# because those are larger outputs the user usually wants pinned.
studio_assets_dir <- function(project_dir) {
  base <- if (nzchar(project_dir %||% "")) project_dir else tempdir()
  file.path(base, "studio_assets")
}
studio_assets_annotations_path <- function(project_dir) {
  file.path(studio_assets_dir(project_dir), "annotations.geojson")
}
studio_assets_rois_path <- function(project_dir) {
  file.path(studio_assets_dir(project_dir), "rois.geojson")
}

# Cache of (raster identity hash -> tile-friendly local path). Re-rendering
# the map with the same raster reuses the prior temp file so we are not
# rewriting GeoTIFFs on every load.
.dronebior_tile_cache <- new.env(parent = emptyenv())

# Cache of raster header reads (band count, extent, CRS) keyed by
# (path, mtime, size). Without this, every keystroke in the project_dir
# / orthomosaic textInput re-opens the same multi-GB orthomosaic from
# disk (or worse, from OneDrive Files-On-Demand), printing terra's
# ASCII progress bar to the console and freezing the UI for seconds.
# With the cache, the FIRST read of a given file is slow and visible
# (we wrap it in withProgress); every subsequent read is instant.
.dronebior_header_cache <- new.env(parent = emptyenv())

raster_header_key <- function(path) {
  if (!file.exists(path)) return(NULL)
  info <- file.info(path)
  paste0(normalizePath(path, mustWork = FALSE), "|", info$mtime, "|", info$size)
}

# Read selected raster header fields (nlyr, ext, crs) once per
# file-version. The first read shows a top-of-page "Now loading" banner
# with a client-side elapsed-time ticker; subsequent reads of the same
# (path, mtime, size) hit the cache and return instantly. Returns NULL
# when the file does not exist or terra fails to open it.
raster_header <- function(path, progress_msg = "Reading raster header") {
  if (!is.character(path) || !length(path) || !nzchar(path) || !file.exists(path)) {
    return(NULL)
  }
  key <- raster_header_key(path)
  if (!is.null(key)) {
    cached <- .dronebior_header_cache[[key]]
    if (!is.null(cached)) return(cached)
  }

  do_read <- function() {
    r <- tryCatch(terra::rast(path), error = function(e) NULL)
    if (is.null(r)) return(NULL)
    # Compute the WGS84 bounds here too so add_project_footprint and
    # fit_leaflet_to_orthomosaic do not have to re-open the file - all
    # downstream callers can use this single cached header.
    bounds_4326 <- tryCatch(raster_bounds_4326(r[[1]]), error = function(e) NULL)
    list(
      nlyr        = as.integer(terra::nlyr(r)),
      ext         = as.vector(terra::ext(r)),
      crs         = tryCatch(terra::crs(r, describe = TRUE), error = function(e) NULL),
      ncol        = as.integer(terra::ncol(r)),
      nrow        = as.integer(terra::nrow(r)),
      bounds_4326 = bounds_4326
    )
  }
  session <- if (requireNamespace("shiny", quietly = TRUE))
    shiny::getDefaultReactiveDomain() else NULL
  header <- with_gis_task(session,
                          name   = progress_msg,
                          detail = basename(path),
                          do_read())
  if (!is.null(header) && !is.null(key)) {
    .dronebior_header_cache[[key]] <- header
  }
  header
}

# Floating "Now loading" banner. R-side helpers push/pop task names
# via a Shiny custom message; the JS handler (in tags$head) creates
# a position:fixed <div> in document.body and updates it with a
# setInterval-driven elapsed-time counter, so the timer keeps ticking
# even while the R session is blocked on a synchronous terra::rast()
# call. Without the client-side ticker the elapsed display would
# freeze along with the rest of the UI.
#
# CRITICAL: session$sendCustomMessage() queues the message and Shiny
# only flushes the queue at the END of the current reactive flush
# cycle. If we just queue the "start" message and immediately enter
# a 5-minute synchronous terra::rast() call, the message sits in the
# queue for those 5 minutes - the banner never appears. We yield to
# the libuv event loop via httpuv::service(0) right after sending,
# which drains pending WebSocket writes and forces the banner to
# render BEFORE the heavy work blocks the R session.
gis_task_send <- function(session, payload) {
  if (is.null(session)) return(invisible(NULL))
  session$sendCustomMessage("dronebior_gis_task", payload)
  if (requireNamespace("httpuv", quietly = TRUE)) {
    # service(0) processes one iteration of the event loop without
    # blocking - just enough to flush queued WebSocket frames.
    tryCatch(httpuv::service(0), error = function(e) NULL)
  }
}
gis_task_start <- function(session, name, detail = NULL) {
  gis_task_send(session, list(action = "start",
                              name   = name,
                              detail = detail))
}
gis_task_stop <- function(session) {
  gis_task_send(session, list(action = "stop"))
}

# Wrap an expression with the top-banner ticker. The banner is the
# headline indicator ("Now: X · file.tif · 12s"). We do NOT also wrap
# in shiny::withProgress because that adds a redundant (and similarly
# buffered) corner notification - the banner is the better signal.
# Works fine when session is NULL (the wrapper falls through to plain
# evaluation outside Shiny).
with_gis_task <- function(session, name, detail = NULL, expr) {
  gis_task_start(session, name, detail)
  on.exit(gis_task_stop(session), add = TRUE)
  force(expr)
}

raster_tile_path <- function(x) {
  key <- digest_raster(x)
  cached <- .dronebior_tile_cache[[key]]
  if (!is.null(cached) && file.exists(cached)) return(cached)

  tmp <- tempfile(fileext = ".tif")
  written <- tryCatch(
    {
      terra::writeRaster(
        x, tmp,
        overwrite = TRUE,
        filetype  = "COG",
        gdal      = c("COMPRESS=DEFLATE", "BIGTIFF=IF_SAFER")
      )
      tmp
    },
    error = function(e) {
      # Older GDAL builds reject the COG driver. Plain GeoTIFF is universally
      # supported and addGeotiff still streams chunks efficiently from it.
      terra::writeRaster(
        x, tmp,
        overwrite = TRUE,
        filetype  = "GTiff",
        gdal      = c("COMPRESS=DEFLATE")
      )
      tmp
    }
  )
  .dronebior_tile_cache[[key]] <- written
  written
}

digest_raster <- function(x) {
  # Cheap fingerprint sufficient for our cache: extent + resolution +
  # layer count + first 200 sampled values. We are not chasing collisions
  # in adversarial settings, only avoiding redundant writes between renders.
  ext <- as.vector(terra::ext(x))
  res <- terra::res(x)
  n   <- terra::nlyr(x)
  sample_vals <- tryCatch(
    terra::spatSample(x, size = 200, method = "regular", na.rm = FALSE),
    error = function(e) NULL
  )
  paste(
    paste(ext, collapse = "-"),
    paste(res, collapse = "-"),
    n,
    paste(round(unlist(sample_vals), 6), collapse = ","),
    sep = "|"
  )
}

# Canonical domain for each layer when the user picks "Fixed semantic"
# stretch. NULL means "no canonical range, fall back to data range".
#   * NDVI, NDRE, NDWI, GNDVI, VARI, Biomass_Index_Proxy : [-1, 1]
#       Yellow at 0 marks the biophysical transition (no chlorophyll /
#       water vs vegetation). Same value on two flights gives the same
#       colour, which matters for time-series comparison.
#   * MSAVI2 : [0, 1]  (defined to be non-negative by construction)
#   * Raw reflectance bands : [0, 1]
#   * EVI / SAVI / CIrededge / Hillshade : NULL
#       No canonical bounded range; can exceed [-1, 1] in real imagery,
#       so we use the data range to avoid clipping the actual signal.
index_semantic_domain <- function(layer_name) {
  # Indices that live on a symmetric [-1, 1] domain: yellow at 0 marks
  # the biophysical transition (no chlorophyll / water vs vegetation).
  diverging_idx <- c(
    "NDVI", "NDRE", "NDWI", "GNDVI", "VARI", "OSAVI", "SAVI", "WDRVI",
    "GLI", "MGRVI", "RGBVI",
    "Biomass_Index_Proxy", "Biomass_Spectral"
  )
  # MSAVI2 is non-negative by construction and bands are [0,1] reflectance.
  unit_zero_one <- c("Red", "Green", "Blue", "NIR", "RedEdge", "MSAVI2")
  if (layer_name %in% diverging_idx) return(c(-1, 1))
  if (layer_name %in% unit_zero_one) return(c(0, 1))
  NULL
}

# Compute the colour domain (min, max) for a given layer under the user's
# selected stretch mode:
#   "Fixed semantic"   -> canonical range when known, else data range
#   "Data range"       -> linearly stretched to the layer's min/max
#   "Percentile 2-98"  -> robust stretch that ignores outliers
#
# Returns a numeric of length 2. Falls back to c(0, 1) when the raster
# has no finite values (entirely masked).
compute_color_domain <- function(layer_name, raster_values, mode = "Fixed semantic") {
  if (identical(mode, "Fixed semantic")) {
    sem <- index_semantic_domain(layer_name)
    if (!is.null(sem)) return(sem)
  }
  v <- raster_values[is.finite(raster_values)]
  if (length(v) < 2) return(c(0, 1))
  if (identical(mode, "Percentile 2-98")) {
    return(unname(quantile(v, probs = c(0.02, 0.98), na.rm = TRUE)))
  }
  range(v)
}

# Render a raster on a leafletProxy. Uses leafem::addGeotiff() (which streams
# from a local file as COG-style chunks) when leafem is installed, so users
# get smooth pan/zoom even on 1+ GB orthomosaics. Falls back to the existing
# downsample + addRasterImage path when leafem is not available, or when COG
# writing fails for any reason. The behaviour is identical from the user's
# perspective; only the under-the-hood serving differs.
#
# `domain` (numeric length 2) pins the colour scale endpoints, so that two
# flights of the same site produce the same colour for the same value.
# When NULL, the function falls back to the raster's own min/max.
tile_raster_on_map <- function(proxy, x, group,
                               opacity = 0.75,
                               palette_name = "viridis",
                               vals = NULL,
                               domain = NULL) {
  resolve_domain <- function() {
    if (!is.null(domain) && length(domain) == 2 && all(is.finite(domain))) {
      return(domain)
    }
    v <- if (is.null(vals)) terra::values(x, mat = FALSE) else vals
    finite_v <- v[is.finite(v)]
    if (length(finite_v) < 2) c(0, 1) else range(finite_v, na.rm = TRUE)
  }

  fallback <- function() {
    layer <- downsample_raster(x)
    d <- resolve_domain()
    pal <- colorNumeric(hcl.colors(100, palette_name), domain = d, na.color = "transparent")
    proxy |>
      addRasterImage(layer, colors = pal, opacity = opacity, project = TRUE, group = group)
  }

  if (!requireNamespace("leafem", quietly = TRUE)) {
    return(fallback())
  }

  result <- tryCatch({
    file <- raster_tile_path(x)
    color_options <- leafem::colorOptions(
      palette  = hcl.colors(100, palette_name),
      domain   = resolve_domain(),
      na.color = "transparent"
    )
    proxy |>
      leafem::addGeotiff(
        file               = file,
        opacity            = opacity,
        colorOptions       = color_options,
        group              = group,
        # Hover-value tooltip in the top-right. digits = 2 keeps it readable;
        # the default formatter would otherwise spill 15 decimal places of
        # floating-point noise across the screen.
        imagequeryOptions  = leafem::imagequeryOptions(
          digits   = 2,
          # Bottom-right keeps the hover readout clear of the top-right
          # layers control and the bottom-left legend.
          position = "bottomright",
          prefix   = "Layer"
        )
      )
  }, error = function(e) NULL)

  if (is.null(result)) fallback() else result
}

# A small info card that appears at the top of every nav_panel main area,
# explaining what the panel does and linking to the relevant vignette.
# Keeps onboarding consistent across panels without depending on each
# user reading the README first.
panel_intro_card <- function(title, body_text, vignette = NULL) {
  vignette_link <- NULL
  if (!is.null(vignette)) {
    vignette_link <- tags$a(
      href   = paste0("https://hugomachadorodrigues.github.io/DroneBioR/articles/", vignette, ".html"),
      target = "_blank",
      class  = "ms-1",
      "Open the vignette ↗"
    )
  }
  card(
    class = "border-info mb-3 panel-intro",
    card_header(class = "bg-light", tags$strong(title)),
    card_body(
      class = "py-2",
      tags$p(class = "small mb-0 text-muted", body_text, " ", vignette_link)
    )
  )
}

raster_bounds_4326 <- function(x) {
  e <- terra::ext(x)
  corners <- data.frame(
    x = c(e[1], e[2], e[2], e[1]),
    y = c(e[3], e[3], e[4], e[4])
  )
  s <- sf::st_as_sf(corners, coords = c("x", "y"), crs = terra::crs(x))
  ll <- sf::st_coordinates(sf::st_transform(s, 4326))
  c(
    lng1 = min(ll[, 1], na.rm = TRUE),
    lat1 = min(ll[, 2], na.rm = TRUE),
    lng2 = max(ll[, 1], na.rm = TRUE),
    lat2 = max(ll[, 2], na.rm = TRUE)
  )
}

list_browser_entries <- function(path) {
  if (!dir.exists(path)) {
    return(data.frame(label = character(), path = character(), type = character()))
  }

  entries <- list.files(path, all.files = FALSE, full.names = TRUE, no.. = TRUE)
  if (length(entries) == 0) {
    return(data.frame(label = character(), path = character(), type = character()))
  }

  is_dir <- dir.exists(entries)
  keep_file <- grepl("\\.(tif|tiff|csv|las|laz|ply|obj)$", entries, ignore.case = TRUE)
  entries <- entries[is_dir | keep_file]
  is_dir <- dir.exists(entries)

  if (length(entries) == 0) {
    return(data.frame(label = character(), path = character(), type = character()))
  }

  ord <- order(!is_dir, tolower(basename(entries)))
  entries <- entries[ord]
  is_dir <- is_dir[ord]

  data.frame(
    label = paste0(ifelse(is_dir, "[DIR]  ", "[FILE] "), basename(entries)),
    path = entries,
    type = ifelse(is_dir, "directory", "file"),
    stringsAsFactors = FALSE
  )
}

set_browser_dir <- function(path) {
  if (file.exists(path) && !dir.exists(path)) {
    path <- dirname(path)
  }
  normalizePath(path, mustWork = FALSE)
}

overlay_legend_html <- function(items) {
  if (length(items) == 0) {
    return("")
  }

  # Friendly per-layer unit suffix for the legend numbers. Pulls from
  # product_metadata when defined; falls back to no suffix.
  unit_for_layer <- function(layer_name) {
    if (is.null(layer_name)) return("")
    meta <- product_metadata[[layer_name]]
    if (is.null(meta) || is.null(meta$unit)) return("")
    if (grepl("etre|meter", meta$unit, ignore.case = TRUE)) return(" m")
    if (grepl("reflectance", meta$unit, ignore.case = TRUE)) return("")
    if (grepl("nitless|index", meta$unit, ignore.case = TRUE)) return("")
    ""
  }

  rows <- vapply(items, function(item) {
    gradient <- paste(item$colors, collapse = ",")
    suffix <- unit_for_layer(item$name)
    title_suffix <- if (nzchar(suffix)) paste0(" (", trimws(suffix), ")") else ""
    paste0(
      "<div class='db-legend-row'>",
      "<div class='db-legend-title'>", item$name, title_suffix, "</div>",
      "<div class='db-legend-bar' style='background: linear-gradient(to right,", gradient, ");'></div>",
      "<div class='db-legend-scale'><span>", format(round(item$min, 2), nsmall = 2),
      suffix, "</span><span>", format(round(item$max, 2), nsmall = 2),
      suffix, "</span></div>",
      "</div>"
    )
  }, character(1))

  paste0(
    "<div class='db-legend'><div class='db-legend-heading'>Overlay legends</div>",
    paste(rows, collapse = ""),
    "</div>"
  )
}

classification_legend_html <- function(class_counts, shown_n, total_n) {
  if (nrow(class_counts) == 0) {
    return("")
  }

  class_counts <- class_counts[class_counts$class %in% names(classification_palette), , drop = FALSE]
  class_counts <- class_counts[order(match(class_counts$class, names(classification_palette))), , drop = FALSE]
  rows <- vapply(seq_len(nrow(class_counts)), function(i) {
    cls <- class_counts$class[[i]]
    paste0(
      "<div class='class-legend-row'>",
      "<span class='class-swatch' style='background:", classification_palette[[cls]], ";'></span>",
      "<span class='class-name'>", htmltools::htmlEscape(cls), "</span>",
      "<span class='class-count'>", format(class_counts$n[[i]], big.mark = ","), "</span>",
      "</div>"
    )
  }, character(1))

  paste0(
    "<div class='class-legend'>",
    "<div class='class-legend-heading'>Point classes</div>",
    paste(rows, collapse = ""),
    "<div class='class-legend-footnote'>Showing ",
    format(shown_n, big.mark = ","),
    " of ",
    format(total_n, big.mark = ","),
    " classified preview points</div>",
    "</div>"
  )
}

sample_context_points <- function(points, max_points = 12000) {
  if (nrow(points) <= max_points) {
    return(points)
  }

  class_indices <- split(seq_len(nrow(points)), points$class)
  sampled <- unlist(lapply(class_indices, function(idx) {
    keep_n <- max(1, round(length(idx) / nrow(points) * max_points))
    keep_n <- min(length(idx), keep_n)
    idx[unique(round(seq(1, length(idx), length.out = keep_n)))]
  }), use.names = FALSE)

  sampled <- sort(unique(sampled))
  if (length(sampled) > max_points) {
    sampled <- sampled[seq_len(max_points)]
  }
  points[sampled, , drop = FALSE]
}

product_metadata <- list(
  `RGB Orthomosaic` = list(
    label   = "RGB Orthomosaic",
    formula = "Natural-color composite of the Red, Green, Blue reflectance bands.",
    unit    = "Visual composite (8-bit per channel)",
    range   = "0-255 per channel",
    bands   = "Red, Green, Blue",
    reference = "Standard ODM odm_orthophoto output.",
    use     = "True-colour view of the survey area as the camera saw it. The bread-and-butter deliverable for stakeholder maps."
  ),
  DSM = list(
    label   = "DSM",
    formula = "Digital Surface Model produced by ODM (odm_dem/dsm.tif).",
    unit    = "Metres above the projected vertical datum",
    range   = "Survey-dependent (typical 0 to a few hundred m)",
    bands   = "None - structure-from-motion product",
    reference = "ODM odm_dem step (SMRF non-ground + ground classification).",
    use     = "Top-of-canopy / structure surface. Use together with the DTM to compute canopy height (CHM)."
  ),
  DTM = list(
    label   = "DTM",
    formula = "Digital Terrain Model from ODM ground classification (odm_dem/dtm.tif).",
    unit    = "Metres above the projected vertical datum",
    range   = "Survey-dependent (typical 0 to a few hundred m)",
    bands   = "None - structure-from-motion product",
    reference = "ODM odm_dem step. Refine with the CSF filter (Zhang et al. 2016) when dense canopies bias the ground classification.",
    use     = "Bare-earth surface. Subtract from the DSM to get canopy height."
  ),
  CHM = list(
    label   = "CHM",
    formula = "CHM = DSM - DTM, then max(CHM, 0)",
    unit    = "Metres above local ground",
    range   = "0 to canopy height (typically 0-40 m)",
    bands   = "None - derived from DSM, DTM",
    reference = "Standard photogrammetric canopy-height workflow (Lim et al. 2003).",
    use     = "Vegetation height above the bare-earth surface. Click 'Build CHM' in the Project status card if this layer is missing."
  ),
  NDVI = list(
    label = "NDVI",
    formula = "(NIR - Red) / (NIR + Red)",
    unit = "Unitless ratio",
    range = "[-1, 1]; healthy vegetation typically 0.3-0.9",
    bands = "Red, NIR",
    reference = "Rouse et al. 1974, ERTS-1 symposium.",
    use = "General vegetation vigor and canopy greenness. Saturates over dense canopies."
  ),
  NDRE = list(
    label = "NDRE",
    formula = "(NIR - RedEdge) / (NIR + RedEdge)",
    unit = "Unitless ratio",
    range = "[-1, 1]; healthy dense canopy typically 0.2-0.6",
    bands = "RedEdge, NIR",
    reference = "Gitelson & Merzlyak 1994.",
    use = "Chlorophyll sensitive; better than NDVI for dense / mature canopies where NDVI saturates."
  ),
  EVI = list(
    label = "EVI",
    formula = "2.5 * (NIR - Red) / (NIR + 6*Red - 7.5*Blue + 1)",
    unit = "Unitless index",
    range = "Typically -1 to 2 in real imagery",
    bands = "Blue, Red, NIR",
    reference = "Huete et al. 2002, RSE 83.",
    use = "Vegetation vigor with atmospheric (Blue) and soil-background corrections; reduces NDVI saturation."
  ),
  SAVI = list(
    label = "SAVI",
    formula = "1.5 * (NIR - Red) / (NIR + Red + 0.5)",
    unit = "Unitless index",
    range = "[-1, 1]",
    bands = "Red, NIR",
    reference = "Huete 1988, RSE 25.",
    use = "Vegetation index with a soil-line correction (L=0.5). Use when canopy cover is partial."
  ),
  OSAVI = list(
    label = "OSAVI",
    formula = "(NIR - Red) / (NIR + Red + 0.16)",
    unit = "Unitless index",
    range = "[-1, 1]",
    bands = "Red, NIR",
    reference = "Rondeaux, Steven & Baret 1996, RSE 55.",
    use = "Optimised SAVI with L=0.16; favoured for agricultural and grassland canopies because it tracks LAI more linearly than SAVI."
  ),
  MSAVI2 = list(
    label = "MSAVI2",
    formula = "(2*NIR + 1 - sqrt((2*NIR + 1)^2 - 8*(NIR - Red))) / 2",
    unit = "Unitless index",
    range = "[-1, 1] (non-negative by construction in vegetation)",
    bands = "Red, NIR",
    reference = "Qi et al. 1994, RSE 48.",
    use = "Self-adjusting soil-line variant of SAVI - no L parameter; preferred for low-cover / arid scenes."
  ),
  NDWI = list(
    label = "NDWI",
    formula = "(Green - NIR) / (Green + NIR)",
    unit = "Unitless ratio",
    range = "[-1, 1]; open water > 0",
    bands = "Green, NIR",
    reference = "McFeeters 1996, IJRS 17.",
    use = "Highlights surface water / canopy moisture. Useful as a water mask."
  ),
  GNDVI = list(
    label = "GNDVI",
    formula = "(NIR - Green) / (NIR + Green)",
    unit = "Unitless ratio",
    range = "[-1, 1]",
    bands = "Green, NIR",
    reference = "Gitelson, Kaufman & Merzlyak 1996, RSE 58.",
    use = "Chlorophyll-sensitive analogue of NDVI; more responsive at mid-LAI canopy cover."
  ),
  CIrededge = list(
    label = "CI rededge",
    formula = "NIR / RedEdge - 1",
    unit = "Unitless ratio",
    range = "Typically 0-5",
    bands = "RedEdge, NIR",
    reference = "Gitelson, Vina, Ciganda et al. 2003, GRL 30.",
    use = "Red-edge Chlorophyll Index; strongly correlated with leaf chlorophyll content."
  ),
  GCI = list(
    label = "GCI",
    formula = "NIR / Green - 1",
    unit = "Unitless ratio",
    range = "Typically 0-10",
    bands = "Green, NIR",
    reference = "Gitelson, Gritz & Merzlyak 2003, J Plant Physiol 160.",
    use = "Green Chlorophyll Index; alternative to CIrededge when only Green + NIR are available."
  ),
  RVI = list(
    label = "RVI",
    formula = "NIR / Red",
    unit = "Unitless ratio",
    range = "Typically 1-30",
    bands = "Red, NIR",
    reference = "Jordan 1969, Ecology 50.",
    use = "Simple Ratio (also called SR). Maximum-sensitivity vegetation index for dense canopies."
  ),
  DVI = list(
    label = "DVI",
    formula = "NIR - Red",
    unit = "Reflectance difference",
    range = "Typically 0-0.7 reflectance units",
    bands = "Red, NIR",
    reference = "Tucker 1979, RSE 8.",
    use = "Difference Vegetation Index; simple but sensitive to brightness."
  ),
  WDRVI = list(
    label = "WDRVI",
    formula = "(0.2*NIR - Red) / (0.2*NIR + Red)",
    unit = "Unitless ratio",
    range = "[-1, 1]",
    bands = "Red, NIR",
    reference = "Gitelson 2004, J Plant Physiol 161.",
    use = "Wide Dynamic Range Vegetation Index; extends NDVI's linear response into dense canopy (a=0.2)."
  ),
  TVI = list(
    label = "TVI",
    formula = "0.5 * (120*(NIR - Green) - 200*(Red - Green))",
    unit = "Triangular area (unitless)",
    range = "Survey-dependent, typically -30 to +50",
    bands = "Green, Red, NIR",
    reference = "Broge & Leblanc 2001, RSE 76.",
    use = "Triangular Vegetation Index; tracks leaf chlorophyll and green LAI."
  ),
  MCARI = list(
    label = "MCARI",
    formula = "((RedEdge - Red) - 0.2*(RedEdge - Green)) * (RedEdge / Red)",
    unit = "Unitless index",
    range = "Typically -0.5 to 1",
    bands = "Green, Red, RedEdge",
    reference = "Daughtry, Walthall, Kim et al. 2000, RSE 74.",
    use = "Modified Chlorophyll Absorption Ratio Index; minimises soil and non-photosynthetic vegetation background."
  ),
  PSRI = list(
    label = "PSRI",
    formula = "(Red - Green) / RedEdge",
    unit = "Unitless ratio",
    range = "Typically -0.3 to 0.3",
    bands = "Green, Red, RedEdge",
    reference = "Merzlyak, Gitelson, Chivkunova et al. 1999, Physiol Plantarum 106.",
    use = "Plant Senescence Reflectance Index; detects senescent / stressed canopies (PSRI rises as chlorophyll drops)."
  ),
  VARI = list(
    label = "VARI",
    formula = "(Green - Red) / (Green + Red - Blue)",
    unit = "Unitless ratio",
    range = "Usually [-1, 1], centred around 0",
    bands = "Blue, Green, Red",
    reference = "Gitelson, Kaufman, Stark & Rundquist 2002, RSE 80.",
    use = "RGB-only vegetation index; the standard option for Sony / DJI / Phantom orthos without NIR."
  ),
  ExG = list(
    label = "ExG",
    formula = "2*Green - Red - Blue",
    unit = "Reflectance combination",
    range = "Approximately -1 to 2",
    bands = "Blue, Green, Red",
    reference = "Woebbecke, Meyer, Von Bargen & Mortensen 1995, Trans ASAE 38.",
    use = "Excess Green Index; RGB-only greenness used in precision agriculture and turf monitoring."
  ),
  GLI = list(
    label = "GLI",
    formula = "(2*Green - Red - Blue) / (2*Green + Red + Blue)",
    unit = "Unitless ratio",
    range = "[-1, 1]",
    bands = "Blue, Green, Red",
    reference = "Louhaichi, Borman & Johnson 2001, Geocarto Int 16.",
    use = "Green Leaf Index; normalised ExG, robust to overall illumination differences."
  ),
  TGI = list(
    label = "TGI",
    formula = "-0.5 * (190*(Red - Green) - 120*(Red - Blue))",
    unit = "Triangular area (unitless)",
    range = "Typically -50 to +50",
    bands = "Blue, Green, Red",
    reference = "Hunt, Doraiswamy, McMurtrey et al. 2013, Int J Appl Earth Obs 21.",
    use = "Triangular Greenness Index; sensitive to leaf chlorophyll on RGB-only sensors."
  ),
  MGRVI = list(
    label = "MGRVI",
    formula = "(Green^2 - Red^2) / (Green^2 + Red^2)",
    unit = "Unitless ratio",
    range = "[-1, 1]",
    bands = "Green, Red",
    reference = "Bendig, Yu, Aasen et al. 2015, Int J Appl Earth Obs 39.",
    use = "Modified Green-Red Vegetation Index; widely validated for RGB-UAV biomass estimation in cereal crops."
  ),
  RGBVI = list(
    label = "RGBVI",
    formula = "(Green^2 - Red*Blue) / (Green^2 + Red*Blue)",
    unit = "Unitless ratio",
    range = "[-1, 1]",
    bands = "Blue, Green, Red",
    reference = "Bendig, Yu, Aasen et al. 2015, Int J Appl Earth Obs 39.",
    use = "RGB Vegetation Index; companion to MGRVI for RGB-UAV biomass estimation."
  ),
  Biomass_Index_Proxy = list(
    label = "Biomass spectral proxy",
    formula = "mean(NDVI, SAVI, NDRE), clamped to [-1, 1]",
    unit = "Unitless exploratory proxy",
    range = "[-1, 1]",
    bands = "Red, RedEdge, NIR",
    reference = "Composite indicator (DroneBioR convention).",
    use = "Image-only exploratory biomass surface. Combines greenness signals from three indices. Not biomass in kg/ha without field calibration."
  ),
  Biomass_Spectral = list(
    label = "Biomass spectral",
    formula = "mean(NDVI, SAVI, NDRE) clamped to [-1, 1]; falls back to VARI on RGB-only orthos.",
    unit = "Unitless exploratory proxy",
    range = "[-1, 1]",
    bands = "Adaptive: Red+RedEdge+NIR if available, else Blue+Green+Red.",
    reference = "Composite spectral biomass (DroneBioR convention).",
    use = "Pure-spectral biomass surrogate. Use when no CHM is available."
  ),
  Biomass_NDVI_x_CHM = list(
    label = "Biomass = NDVI x CHM",
    formula = "NDVI * CHM (CHM clipped to >= 0)",
    unit = "Metres x greenness (unitless metres)",
    range = "0 to canopy-height * 1",
    bands = "Red, NIR, DSM, DTM",
    reference = "Greenness x height biomass surrogate; e.g. Lussem, Bolten, Gnyp et al. 2019, Eur J Remote Sens 52.",
    use = "Volume-weighted biomass surrogate. Tracks above-ground biomass for herbaceous / shrub canopies. Calibrate against field plots before reporting kg/ha."
  ),
  Biomass_NDRE_x_CHM = list(
    label = "Biomass = NDRE x CHM",
    formula = "NDRE * CHM (CHM clipped to >= 0)",
    unit = "Metres x greenness",
    range = "0 to canopy-height * 1",
    bands = "RedEdge, NIR, DSM, DTM",
    reference = "Red-edge variant of the greenness x height biomass surrogate; preferred for dense canopies where NDVI saturates."
  ),
  Biomass_SAVI_x_CHM = list(
    label = "Biomass = SAVI x CHM",
    formula = "SAVI * CHM (CHM clipped to >= 0)",
    unit = "Metres x greenness",
    range = "0 to canopy-height * 1",
    bands = "Red, NIR, DSM, DTM",
    reference = "Soil-corrected greenness x height surrogate; useful in mixed soil + canopy plots.",
    use = "Volume-weighted biomass surrogate. Soil-adjusted, good for partial canopy cover."
  ),
  Biomass_GNDVI_x_CHM = list(
    label = "Biomass = GNDVI x CHM",
    formula = "GNDVI * CHM (CHM clipped to >= 0)",
    unit = "Metres x greenness",
    range = "0 to canopy-height * 1",
    bands = "Green, NIR, DSM, DTM",
    reference = "Green-NIR greenness x height surrogate.",
    use = "Volume-weighted biomass surrogate using Green/NIR. Useful when red reflectance is unreliable."
  ),
  Biomass_VARI_x_CHM = list(
    label = "Biomass = VARI x CHM (RGB)",
    formula = "VARI * CHM (CHM clipped to >= 0)",
    unit = "Metres x greenness",
    range = "0 to canopy-height * 1",
    bands = "Blue, Green, Red, DSM, DTM",
    reference = "RGB-only biomass surrogate, drone analogue of Bendig et al. 2015.",
    use = "Volume-weighted biomass surrogate for RGB-only drones. The recommended biomass layer for Sony / Phantom / DJI surveys."
  ),
  Biomass_EXG_x_CHM = list(
    label = "Biomass = ExG x CHM (RGB)",
    formula = "ExG * CHM (CHM clipped to >= 0)",
    unit = "Metres x greenness",
    range = "0 to canopy-height * (max ExG)",
    bands = "Blue, Green, Red, DSM, DTM",
    reference = "ExG x height surrogate; common in precision-agriculture UAS pipelines.",
    use = "RGB-only volume biomass surrogate, weighted by excess-green response."
  ),
  Biomass_MGRVI_x_CHM = list(
    label = "Biomass = MGRVI x CHM (RGB)",
    formula = "MGRVI * CHM (CHM clipped to >= 0)",
    unit = "Metres x greenness",
    range = "0 to canopy-height * 1",
    bands = "Green, Red, DSM, DTM",
    reference = "Bendig, Yu, Aasen et al. 2015 (validated for cereal biomass on RGB UAS).",
    use = "Best-validated RGB-only biomass surrogate for cereals and grasses."
  ),
  Biomass_RGBVI_x_CHM = list(
    label = "Biomass = RGBVI x CHM (RGB)",
    formula = "RGBVI * CHM (CHM clipped to >= 0)",
    unit = "Metres x greenness",
    range = "0 to canopy-height * 1",
    bands = "Blue, Green, Red, DSM, DTM",
    reference = "Bendig, Yu, Aasen et al. 2015 (companion to MGRVI).",
    use = "RGB-only volume biomass surrogate combining all three visible bands."
  ),
  NIR = list(
    label = "NIR",
    formula = "ODM MicaSense NIR band after alpha masking and reflectance scaling.",
    unit = "Reflectance",
    range = "[0, 1] (clamped)",
    bands = "NIR",
    reference = "Source band - drives most chlorophyll-sensitive indices.",
    use = "Near-infrared canopy response. The single most important band for vegetation indices."
  ),
  RedEdge = list(
    label = "RedEdge",
    formula = "ODM MicaSense RedEdge band after alpha masking and reflectance scaling.",
    unit = "Reflectance",
    range = "[0, 1] (clamped)",
    bands = "RedEdge",
    reference = "Source band.",
    use = "Red-edge reflectance, useful for chlorophyll and dense canopies."
  ),
  Red = list(
    label = "Red",
    formula = "Red reflectance band after alpha masking and reflectance scaling.",
    unit = "Reflectance",
    range = "[0, 1] (clamped)",
    bands = "Red",
    reference = "Source band.",
    use = "Visible red reflectance used in NDVI, EVI and SAVI."
  ),
  Green = list(
    label = "Green",
    formula = "Green reflectance band after alpha masking and reflectance scaling.",
    unit = "Reflectance",
    range = "[0, 1] (clamped)",
    bands = "Green",
    reference = "Source band.",
    use = "Visible green reflectance used in NDWI and RGB visualization."
  ),
  Blue = list(
    label = "Blue",
    formula = "Blue reflectance band after alpha masking and reflectance scaling.",
    unit = "Reflectance",
    range = "[0, 1] (clamped)",
    bands = "Blue",
    reference = "Source band.",
    use = "Visible blue reflectance used in EVI and RGB visualization."
  ),
  Hillshade = list(
    label = "Hillshade",
    formula = "terra::shade(slope, aspect, angle = 45, direction = 315) on the DSM",
    unit = "Grayscale shading",
    range = "[0, 1]",
    bands = "None - derived from DSM",
    reference = "Standard analytical hillshading.",
    use = "Adds relief shading under colored overlays so terrain structure is readable."
  )
)

layer_input_id <- function(layer_name) {
  paste0("layer_", gsub("[^A-Za-z0-9]", "_", layer_name))
}

help_input_id <- function(layer_name) {
  paste0("help_", gsub("[^A-Za-z0-9]", "_", layer_name))
}

format_summary_table <- function(x, unit, digits = 2) {
  out <- x
  numeric_cols <- vapply(out, is.numeric, logical(1))
  out[numeric_cols] <- lapply(out[numeric_cols], function(v) formatC(v, format = "f", digits = digits))
  out$unit <- unit
  out
}

format_tree_table <- function(x) {
  if (nrow(x) == 0) {
    return(x)
  }
  data.frame(
    tree_id = x$tree_id,
    `height (m)` = formatC(x$height_m, format = "f", digits = 2),
    `crown diameter (m)` = formatC(x$crown_diameter_m, format = "f", digits = 2),
    `crown volume (m3)` = formatC(x$crown_volume_m3, format = "f", digits = 2),
    point_count = x$point_count,
    check.names = FALSE
  )
}

classification_palette <- c(
  Unclassified = "#94a3b8",
  Canopy = "#22c55e",
  Ground = "#a16207",
  `Tree crown` = "#facc15",
  `Stem/trunk` = "#7c2d12",
  `Low vegetation` = "#84cc16",
  Noise = "#ef4444",
  Exclude = "#64748b"
)

application_classes <- data.frame(
  class_id = 1:5,
  class = c("Water / shadow", "Bare soil", "Stress", "Moderate vigor", "High vigor"),
  color = c("#2563eb", "#a16207", "#ef4444", "#f59e0b", "#16a34a"),
  stringsAsFactors = FALSE
)

fixed_index_limits <- list(
  NDVI = c(-1, 1),
  NDRE = c(-1, 1),
  EVI = c(-1, 2),
  SAVI = c(-1, 1),
  OSAVI = c(-1, 1),
  NDWI = c(-1, 1),
  GNDVI = c(-1, 1),
  CIrededge = c(-1, 5),
  GCI       = c(-1, 10),
  RVI       = c(0, 30),
  DVI       = c(-0.2, 0.7),
  WDRVI     = c(-1, 1),
  TVI       = c(-50, 50),
  MCARI     = c(-0.5, 1),
  PSRI      = c(-0.3, 0.3),
  MSAVI2 = c(-1, 1),
  VARI = c(-1, 1),
  ExG    = c(-1, 2),
  GLI    = c(-1, 1),
  TGI    = c(-50, 50),
  MGRVI  = c(-1, 1),
  RGBVI  = c(-1, 1),
  Biomass_Index_Proxy = c(-1, 1),
  Biomass_Spectral     = c(-1, 1)
)

infer_radiometric_scale <- function(x) {
  max_value <- max(terra::global(x, "max", na.rm = TRUE)$max, na.rm = TRUE)
  p99 <- max(terra::global(x, function(v, ...) stats::quantile(v, 0.99, na.rm = TRUE), na.rm = TRUE)[, 1], na.rm = TRUE)
  if (!is.finite(max_value)) {
    return(list(label = "Unknown", scale_factor = NA_real_, max_value = NA_real_, p99 = NA_real_))
  }
  if (max_value <= 1.5) {
    list(label = "Reflectance 0-1", scale_factor = 1, max_value = max_value, p99 = p99)
  } else if (max_value <= 12000) {
    list(label = "Scaled DN 0-10000", scale_factor = 10000, max_value = max_value, p99 = p99)
  } else {
    list(label = "Scaled DN 0-65535", scale_factor = 65535, max_value = max_value, p99 = p99)
  }
}

radiometric_scale_factor <- function(x, mode) {
  if (identical(mode, "Already reflectance 0-1")) return(1)
  if (identical(mode, "Divide by 10000")) return(10000)
  if (identical(mode, "Divide by 65535")) return(65535)
  if (identical(mode, "Raw DN / no scaling")) return(1)
  infer_radiometric_scale(x)$scale_factor
}

scale_radiometric_bands <- function(x, mode) {
  factor <- radiometric_scale_factor(x, mode)
  if (!is.finite(factor) || factor == 0) {
    return(x)
  }
  x / factor
}

spectral_qa_summary <- function(raw, reflectance, alpha = NULL, scale_info = NULL) {
  total_cells <- terra::ncell(raw)
  scale_label <- if (is.null(scale_info)) infer_radiometric_scale(raw)$label else scale_info$label
  scale_factor <- if (is.null(scale_info)) infer_radiometric_scale(raw)$scale_factor else scale_info$scale_factor
  saturation_limit <- if (is.finite(scale_factor) && scale_factor > 1) scale_factor * 0.995 else 0.995

  rows <- lapply(names(raw), function(layer_name) {
    raw_layer <- raw[[layer_name]]
    refl_layer <- reflectance[[layer_name]]
    valid_n <- as.numeric(terra::global(!is.na(refl_layer), "sum", na.rm = TRUE)[1, 1])
    high_sat <- as.numeric(terra::global(raw_layer >= saturation_limit, "sum", na.rm = TRUE)[1, 1])
    low_sat <- as.numeric(terra::global(raw_layer <= 0, "sum", na.rm = TRUE)[1, 1])
    physical_invalid <- as.numeric(terra::global(refl_layer < 0 | refl_layer > 1, "sum", na.rm = TRUE)[1, 1])
    raw_stats <- terra::global(raw_layer, c("min", "mean", "max"), na.rm = TRUE)
    refl_stats <- terra::global(refl_layer, c("min", "mean", "max"), na.rm = TRUE)
    data.frame(
      band = layer_name,
      detected_scale = scale_label,
      valid_pixels = valid_n,
      nodata_or_masked_pixels = total_cells - valid_n,
      low_saturated_pixels = low_sat,
      high_saturated_pixels = high_sat,
      physical_invalid_pixels = physical_invalid,
      raw_min = raw_stats$min,
      raw_mean = raw_stats$mean,
      raw_max = raw_stats$max,
      reflectance_min = refl_stats$min,
      reflectance_mean = refl_stats$mean,
      reflectance_max = refl_stats$max,
      check.names = FALSE
    )
  })
  out <- do.call(rbind, rows)
  if (!is.null(alpha)) {
    alpha_valid <- as.numeric(terra::global(alpha != 0 & !is.na(alpha), "sum", na.rm = TRUE)[1, 1])
    attr(out, "alpha_valid_pixels") <- alpha_valid
  }
  out
}

apply_panel_calibration <- function(reflectance, coefficients) {
  out <- reflectance
  for (band_name in intersect(names(out), coefficients$band)) {
    row <- coefficients[coefficients$band == band_name, , drop = FALSE]
    out[[band_name]] <- out[[band_name]] * row$gain + row$offset
  }
  out
}

clean_valid_mask <- function(x, size = 3) {
  valid <- terra::app(!is.na(x), fun = function(...) as.integer(all(c(...))))
  w <- matrix(1, size, size)
  nearby <- terra::focal(valid, w = w, fun = sum, na.rm = TRUE, fillvalue = 0)
  nearby >= ceiling(sum(w) * 0.5)
}

preprocess_reflectance <- function(x,
                                   remove_invalid = TRUE,
                                   median_size = 0,
                                   clean_mask = FALSE,
                                   mask_size = 3,
                                   downsample_factor = 1,
                                   downsample_method = "None") {
  out <- x
  if (isTRUE(remove_invalid)) {
    out <- terra::ifel(out < 0 | out > 1, NA, out)
  }
  if (isTRUE(clean_mask)) {
    mask <- clean_valid_mask(out, size = mask_size)
    out <- terra::mask(out, mask, maskvalues = 0, updatevalue = NA)
  }
  if (median_size %in% c(3, 5)) {
    out <- terra::focal(out, w = matrix(1, median_size, median_size), fun = stats::median, na.rm = TRUE, fillvalue = NA)
  }
  if (downsample_factor > 1 && !identical(downsample_method, "None")) {
    method <- downsample_method
    if (identical(method, "Gaussian average")) {
      out <- terra::focal(out, w = matrix(c(1, 2, 1, 2, 4, 2, 1, 2, 1), 3, 3) / 16, fun = sum, na.rm = TRUE, fillvalue = NA)
      method <- "Average"
    }
    if (identical(method, "Average")) {
      out <- terra::aggregate(out, fact = downsample_factor, fun = mean, na.rm = TRUE)
    } else if (identical(method, "Median")) {
      out <- terra::aggregate(out, fact = downsample_factor, fun = stats::median, na.rm = TRUE)
    } else if (identical(method, "75% quantile")) {
      out <- terra::aggregate(out, fact = downsample_factor, fun = function(v, ...) stats::quantile(v, 0.75, na.rm = TRUE))
    }
  }
  out
}

stretch_layer_for_display <- function(layer, mode = "Percentile 2-98") {
  vals <- terra::values(layer, mat = FALSE)
  vals <- vals[is.finite(vals)]
  if (length(vals) == 0) return(layer)

  if (identical(mode, "None")) {
    return(layer)
  }
  if (identical(mode, "Percentile 2-98")) {
    limits <- stats::quantile(vals, c(0.02, 0.98), na.rm = TRUE)
    if (!is.finite(diff(limits)) || diff(limits) == 0) return(layer)
    return(terra::clamp((layer - limits[[1]]) / diff(limits), lower = 0, upper = 1, values = TRUE))
  }

  probs <- seq(0, 1, length.out = 256)
  breaks <- unique(as.numeric(stats::quantile(vals, probs, na.rm = TRUE)))
  if (length(breaks) < 2) return(layer)
  terra::app(layer, fun = function(v) stats::approx(breaks, seq(0, 1, length.out = length(breaks)), v, rule = 2)$y)
}

stretch_raster_for_display <- function(x, mode = "Percentile 2-98") {
  out <- x
  for (i in seq_len(terra::nlyr(out))) {
    out[[i]] <- stretch_layer_for_display(out[[i]], mode)
  }
  out
}

evaluate_custom_index <- function(reflectance, formula_text, output_name = "Custom_Index") {
  output_name <- gsub("[^A-Za-z0-9_]", "_", output_name)
  if (!nzchar(output_name)) output_name <- "Custom_Index"
  env <- new.env(parent = baseenv())
  for (band_name in names(reflectance)) {
    assign(band_name, reflectance[[band_name]], envir = env)
  }
  assign("safe_ratio", safe_ratio, envir = env)
  result <- eval(parse(text = formula_text), envir = env)
  if (!inherits(result, "SpatRaster")) {
    stop("Custom formula must return a terra SpatRaster.", call. = FALSE)
  }
  result <- result[[1]]
  names(result) <- output_name
  result
}

index_zlim <- function(layer_name, use_fixed = TRUE, x = NULL) {
  if (isTRUE(use_fixed) && layer_name %in% names(fixed_index_limits)) {
    return(fixed_index_limits[[layer_name]])
  }
  if (is.null(x)) return(NULL)
  vals <- terra::values(x, mat = FALSE)
  vals <- vals[is.finite(vals)]
  if (length(vals) == 0) return(NULL)
  as.numeric(stats::quantile(vals, c(0.02, 0.98), na.rm = TRUE))
}

build_application_map <- function(index_raster, thresholds) {
  thresholds <- sort(as.numeric(thresholds))
  rcl <- matrix(c(
    -Inf, thresholds[[1]], 1,
    thresholds[[1]], thresholds[[2]], 2,
    thresholds[[2]], thresholds[[3]], 3,
    thresholds[[3]], thresholds[[4]], 4,
    thresholds[[4]], Inf, 5
  ), ncol = 3, byrow = TRUE)
  out <- terra::classify(index_raster, rcl, include.lowest = TRUE, right = TRUE)
  names(out) <- "Application_Class"
  out
}

summarize_application_map <- function(class_raster) {
  freq <- terra::freq(class_raster)
  if (is.null(freq) || nrow(freq) == 0) {
    return(data.frame())
  }
  names(freq)[names(freq) == "value"] <- "class_id"
  freq <- freq[!is.na(freq$class_id), , drop = FALSE]
  if (nrow(freq) == 0) {
    return(data.frame())
  }
  freq <- merge(freq, application_classes, by = "class_id", all.x = TRUE, sort = FALSE)
  cell_area <- prod(abs(terra::res(class_raster)))
  freq$area_m2 <- freq$count * cell_area
  freq$area_ha <- freq$area_m2 / 10000
  freq[, c("class_id", "class", "count", "area_m2", "area_ha")]
}

spectral_values_summary <- function(values, prefix) {
  numeric_values <- values[is.finite(values)]
  if (length(numeric_values) == 0) {
    return(setNames(rep(NA_real_, 5), paste0(prefix, c("_mean", "_median", "_sd", "_p10", "_p90"))))
  }
  stats <- c(
    mean = mean(numeric_values),
    median = stats::median(numeric_values),
    sd = stats::sd(numeric_values),
    p10 = as.numeric(stats::quantile(numeric_values, 0.10, na.rm = TRUE)),
    p90 = as.numeric(stats::quantile(numeric_values, 0.90, na.rm = TRUE))
  )
  setNames(stats, paste0(prefix, "_", names(stats)))
}

summarize_predictors_in_polygon <- function(predictors, polygon) {
  vals <- terra::extract(predictors, polygon, ID = FALSE)
  if (is.null(vals) || nrow(vals) == 0) {
    return(setNames(rep(NA_real_, terra::nlyr(predictors) * 5), as.vector(t(outer(names(predictors), c("_mean", "_median", "_sd", "_p10", "_p90"), paste0)))))
  }
  unlist(lapply(names(predictors), function(layer_name) spectral_values_summary(vals[[layer_name]], layer_name)))
}

format_selection_metrics <- function(x) {
  if (nrow(x) == 0) {
    return(x)
  }
  out <- data.frame(
    measurement = c(
      "points",
      "footprint area",
      "max crown diameter",
      "z min",
      "z max",
      "height min",
      "height mean",
      "height max",
      "occupied volume"
    ),
    value = c(
      paste(format(x$n_points, big.mark = ","), "points"),
      paste(formatC(x$footprint_area_m2, format = "f", digits = 2), "m2"),
      paste(formatC(x$max_crown_diameter_m, format = "f", digits = 2), "m"),
      paste(formatC(x$z_min_m, format = "f", digits = 2), "m"),
      paste(formatC(x$z_max_m, format = "f", digits = 2), "m"),
      paste(formatC(x$height_min_m, format = "f", digits = 2), "m"),
      paste(formatC(x$height_mean_m, format = "f", digits = 2), "m"),
      paste(formatC(x$height_max_m, format = "f", digits = 2), "m"),
      paste(formatC(x$occupied_volume_m3, format = "f", digits = 2), "m3")
    ),
    check.names = FALSE
  )
  if ("chm_area_m2" %in% names(x) && is.finite(x$chm_area_m2)) {
    out <- rbind(out, data.frame(
      measurement = c("CHM area", "CHM mean height", "CHM max height", "CHM surface volume"),
      value = c(
        paste(formatC(x$chm_area_m2, format = "f", digits = 2), "m2"),
        paste(formatC(x$chm_height_mean_m, format = "f", digits = 2), "m"),
        paste(formatC(x$chm_height_max_m, format = "f", digits = 2), "m"),
        paste(formatC(x$chm_surface_volume_m3, format = "f", digits = 2), "m3")
      ),
      check.names = FALSE
    ))
  }
  if ("point_source" %in% names(x)) {
    out <- rbind(out, data.frame(measurement = "point source", value = x$point_source, check.names = FALSE))
  }
  if ("height_source" %in% names(x)) {
    out <- rbind(out, data.frame(measurement = "height source", value = x$height_source, check.names = FALSE))
  }
  out
}

local_to_raster_xy <- function(x, y, reference_points, raster) {
  e <- terra::ext(raster)
  xr <- range(reference_points$x, na.rm = TRUE)
  yr <- range(reference_points$y, na.rm = TRUE)

  if (!is.finite(diff(xr)) || diff(xr) == 0) {
    x_map <- rep(mean(c(e[1], e[2])), length(x))
  } else {
    x_map <- e[1] + ((x - xr[1]) / diff(xr)) * (e[2] - e[1])
  }

  if (!is.finite(diff(yr)) || diff(yr) == 0) {
    y_map <- rep(mean(c(e[3], e[4])), length(y))
  } else {
    y_map <- e[3] + ((y - yr[1]) / diff(yr)) * (e[4] - e[3])
  }

  data.frame(x_map = x_map, y_map = y_map)
}

points_are_georeferenced <- function(points) {
  identical(attr(points, "coordinate_source"), "full_georeferenced")
}

points_to_map_xy <- function(x, y, reference_points, raster) {
  if (points_are_georeferenced(reference_points)) {
    return(data.frame(x_map = x, y_map = y))
  }
  local_to_raster_xy(x, y, reference_points, raster)
}

transform_xy_to_wgs84 <- function(xy, raster) {
  pts <- sf::st_as_sf(xy, coords = c("x_map", "y_map"), crs = terra::crs(raster), remove = FALSE)
  coords <- sf::st_coordinates(sf::st_transform(pts, 4326))
  xy$lng <- coords[, 1]
  xy$lat <- coords[, 2]
  xy
}

status_badge <- function(available, ready_label = "Ready", missing_label = "Missing") {
  if (isTRUE(available)) {
    tags$span(class = "status-pill status-ok", tags$span(class = "status-icon", HTML("&#10003;")), ready_label)
  } else {
    tags$span(class = "status-pill status-bad", tags$span(class = "status-icon", HTML("&#10005;")), missing_label)
  }
}

# Sidebar grouping header used inside `layout_sidebar(sidebar = sidebar(...))`.
# `tone` colours the leading chip so related groups read as a unit at a glance
# (scene / select / trees / volume / actions / link). Falls back to the
# default green chip when no tone is given.
sidebar_section <- function(label, tone = NULL) {
  chip_class <- if (is.null(tone)) "sidebar-section-chip" else paste("sidebar-section-chip", tone)
  div(class = "sidebar-section",
      tags$span(class = chip_class),
      tags$span(label))
}

# A single tile in the "Scene sources" row at the top of the 3D Modeling
# main area. `output_id` is rendered server-side and produces a status_badge
# (Available / Missing); the tile name above it makes the layer explicit
# even when the badge is in its missing state.
scene_source_tile <- function(label, output_id) {
  div(class = "scene-source-tile",
      div(class = "scene-source-name", label),
      uiOutput(output_id))
}

haversine_m <- function(lon1, lat1, lon2, lat2) {
  r <- 6371008.8
  to_rad <- pi / 180
  phi1 <- lat1 * to_rad
  phi2 <- lat2 * to_rad
  dphi <- (lat2 - lat1) * to_rad
  dlambda <- (lon2 - lon1) * to_rad
  a <- sin(dphi / 2)^2 + cos(phi1) * cos(phi2) * sin(dlambda / 2)^2
  2 * r * atan2(sqrt(a), sqrt(1 - a))
}

lonlat_area_m2 <- function(lng, lat) {
  if (length(lng) < 3) {
    return(NA_real_)
  }
  mean_lat <- mean(lat, na.rm = TRUE) * pi / 180
  x <- (lng - mean(lng, na.rm = TRUE)) * pi / 180 * 6371008.8 * cos(mean_lat)
  y <- (lat - mean(lat, na.rm = TRUE)) * pi / 180 * 6371008.8
  idx_next <- c(seq_along(x)[-1], 1L)
  abs(sum(x * y[idx_next] - y * x[idx_next]) / 2)
}

bearing_degrees <- function(lon1, lat1, lon2, lat2) {
  to_rad <- pi / 180
  y <- sin((lon2 - lon1) * to_rad) * cos(lat2 * to_rad)
  x <- cos(lat1 * to_rad) * sin(lat2 * to_rad) -
    sin(lat1 * to_rad) * cos(lat2 * to_rad) * cos((lon2 - lon1) * to_rad)
  (atan2(y, x) * 180 / pi + 360) %% 360
}

flight_arrow_icon <- function(heading) {
  svg <- sprintf(
    "<svg xmlns='http://www.w3.org/2000/svg' width='34' height='34' viewBox='0 0 34 34'><g transform='rotate(%0.1f 17 17)'><circle cx='17' cy='17' r='12' fill='white' fill-opacity='0.92' stroke='#1f6f5b' stroke-width='2'/><path d='M17 6 L24 24 L17 20 L10 24 Z' fill='#1f6f5b'/></g></svg>",
    heading
  )
  paste0("data:image/svg+xml;utf8,", utils::URLencode(svg, reserved = TRUE))
}

read_odm_exif_flight_plan <- function(images_dir, odm_project_dir,
                                      max_files = 1200) {
  # Prefer geo.txt when the project has one (written either by the user
  # or by run_odm_project()'s GeoScan auto-detect). It contains the
  # camera positions ODM actually used, which is the ground truth even
  # when the original JPGs had no GPS in their EXIF. This path is cheap
  # (one small text file), so we do it before the heavy EXIF parsing.
  geo_path <- file.path(odm_project_dir, "geo.txt")
  if (file.exists(geo_path)) {
    raw_lines <- readLines(geo_path, warn = FALSE)
    raw_lines <- raw_lines[nzchar(trimws(raw_lines))]
    data_lines <- raw_lines[!grepl("^(EPSG|\\+proj|#)", raw_lines, ignore.case = TRUE)]
    parts <- strsplit(data_lines, "\\s+")
    parts <- parts[vapply(parts, function(p) length(p) >= 3L, logical(1))]
    if (length(parts)) {
      df <- data.frame(
        filename     = vapply(parts, `[`, character(1), 1L),
        longitude    = suppressWarnings(as.numeric(vapply(parts, `[`, character(1), 2L))),
        latitude     = suppressWarnings(as.numeric(vapply(parts, `[`, character(1), 3L))),
        altitude_m   = suppressWarnings(as.numeric(vapply(parts, function(p)
                       if (length(p) >= 4L) p[4L] else NA_character_, character(1)))),
        stringsAsFactors = FALSE
      )
      df <- df[is.finite(df$longitude) & is.finite(df$latitude), , drop = FALSE]
      if (nrow(df)) {
        df$capture_id   <- sub("\\.[A-Za-z0-9]+$", "", df$filename)
        df$capture_time <- as.numeric(seq_len(nrow(df)))
        df$band_id      <- 1L
        next_lng <- c(df$longitude[-1], df$longitude[nrow(df)])
        next_lat <- c(df$latitude[-1],  df$latitude[nrow(df)])
        df$heading <- bearing_degrees(df$longitude, df$latitude, next_lng, next_lat)
        if (nrow(df) > 1) {
          n <- nrow(df)
          prev_lng <- c(df$longitude[1], df$longitude[-n])
          prev_lat <- c(df$latitude[1],  df$latitude[-n])
          df$heading[n] <- bearing_degrees(prev_lng[n], prev_lat[n],
                                           df$longitude[n], df$latitude[n])
        }
        df$arrow_icon <- vapply(df$heading, flight_arrow_icon, character(1))
        df$sequence   <- seq_len(nrow(df))
        df$image_file <- file.path(images_dir, df$filename)
        return(df)
      }
    }
  }

  # Fallback: ODM's per-image EXIF JSONs (the original MicaSense path).
  # Permissive pattern catches both .tif.exif and .JPG.exif outputs.
  exif_dir <- file.path(odm_project_dir, "opensfm", "exif")
  if (!dir.exists(exif_dir)) {
    return(data.frame())
  }
  files <- list.files(exif_dir, pattern = "\\.exif$", full.names = TRUE,
                      ignore.case = TRUE)
  if (length(files) == 0) {
    return(data.frame())
  }

  # Subsample huge EXIF lists. MicaSense flights with thousands of
  # captures (5 bands * 1000+ captures) produce 5000+ EXIF JSONs, and
  # parsing them all synchronously on the main R thread blocks the
  # Shiny session for minutes - especially when the files live on a
  # cloud-synced drive (OneDrive, iCloud) where stat'ing each file
  # may trigger an on-demand download. A strided sample is more than
  # enough to draw a recognisable flight path on the map.
  if (length(files) > max_files) {
    stride <- ceiling(length(files) / max_files)
    files <- files[seq(1L, length(files), by = stride)]
  }

  # When called from a Shiny session, drive a progress bar so the user
  # sees the parsing advance rather than guessing whether the app
  # froze. Falls back to a no-op `setProgress` when called outside Shiny.
  has_progress <- requireNamespace("shiny", quietly = TRUE) &&
    !is.null(shiny::getDefaultReactiveDomain())
  if (has_progress) {
    shiny::setProgress(
      value = 0,
      message = "Reading flight metadata",
      detail = paste0("0 / ", length(files), " EXIF JSONs")
    )
  }

  parse_step <- max(1L, length(files) %/% 40L)
  rows <- vector("list", length(files))
  for (i in seq_along(files)) {
    path <- files[[i]]
    x <- tryCatch(jsonlite::fromJSON(path), error = function(e) NULL)
    if (!is.null(x) && !is.null(x$gps$latitude) && !is.null(x$gps$longitude)) {
      filename <- sub("\\.exif$", "", basename(path), ignore.case = TRUE)
      parsed <- regexec("^(.+)_([0-9]+)\\.[A-Za-z0-9]+$", filename)
      match  <- regmatches(filename, parsed)[[1]]
      if (length(match) == 3L) {
        cap_id  <- match[[2]]
        band_id <- as.integer(match[[3]])
      } else {
        # No band suffix (e.g. Sony / DJI single-band JPG). Treat the
        # whole name minus extension as the capture id, band 1.
        cap_id  <- sub("\\.[A-Za-z0-9]+$", "", filename)
        band_id <- 1L
      }
      rows[[i]] <- data.frame(
        capture_id   = cap_id,
        band_id      = band_id,
        filename     = filename,
        longitude    = as.numeric(x$gps$longitude),
        latitude     = as.numeric(x$gps$latitude),
        altitude_m   = as.numeric(x$gps$altitude %||% NA_real_),
        capture_time = as.numeric(x$capture_time %||% NA_real_),
        stringsAsFactors = FALSE
      )
    }
    if (has_progress && (i %% parse_step == 0L || i == length(files))) {
      shiny::setProgress(
        value = i / length(files),
        detail = paste0(format(i, big.mark = ","), " / ",
                        format(length(files), big.mark = ","), " EXIF JSONs")
      )
    }
  }
  raw <- do.call(rbind, rows[!vapply(rows, is.null, logical(1))])
  if (is.null(raw) || nrow(raw) == 0) {
    return(data.frame())
  }

  raw <- raw[order(raw$capture_time, raw$capture_id, raw$band_id), , drop = FALSE]
  raw <- raw[!duplicated(raw$capture_id), , drop = FALSE]
  raw <- raw[order(raw$capture_time, raw$capture_id), , drop = FALSE]
  raw$sequence <- seq_len(nrow(raw))
  next_lng <- c(raw$longitude[-1], raw$longitude[nrow(raw)])
  next_lat <- c(raw$latitude[-1], raw$latitude[nrow(raw)])
  prev_lng <- c(raw$longitude[1], raw$longitude[-nrow(raw)])
  prev_lat <- c(raw$latitude[1], raw$latitude[-nrow(raw)])
  raw$heading <- bearing_degrees(raw$longitude, raw$latitude, next_lng, next_lat)
  last <- nrow(raw)
  if (last > 1) {
    raw$heading[[last]] <- bearing_degrees(prev_lng[[last]], prev_lat[[last]], raw$longitude[[last]], raw$latitude[[last]])
  }
  raw$arrow_icon <- vapply(raw$heading, flight_arrow_icon, character(1))
  raw$image_file <- file.path(images_dir, raw$filename)
  raw
}

theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#1f6f5b",
  secondary = "#315f95"
)

ui <- page_navbar(
  title = tags$span(
    style = "display: inline-flex; align-items: center; gap: 14px;",
    tags$img(src = "logo.png", height = "120px",
             style = "vertical-align: middle;",
             alt = "DroneBioR logo"),
    tags$span(style = "font-size: 1.35rem; font-weight: 600;",
              "Drone Biomass Studio")
  ),
  theme = theme,
  header = tags$head(
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/three.js/r128/three.min.js"),
    tags$script(src = "https://cdn.jsdelivr.net/npm/three@0.128.0/examples/js/controls/OrbitControls.js"),
    tags$script(src = "https://cdn.jsdelivr.net/npm/three@0.128.0/examples/js/loaders/OBJLoader.js"),
    tags$script(src = "https://cdn.jsdelivr.net/npm/three@0.128.0/examples/js/loaders/MTLLoader.js"),
    tags$script(src = "https://cdn.jsdelivr.net/npm/three@0.128.0/examples/js/exporters/GLTFExporter.js"),
    tags$script(HTML("
      document.addEventListener('shown.bs.tab', function() {
        window.setTimeout(function() {
          window.dispatchEvent(new Event('resize'));
        }, 80);
      });
      // leafem's addGeotiff(imagequery = TRUE) injects DOM elements that
      // are not standard leaflet controls, so clearControls() leaves them
      // behind. We expose a custom message handler that R can call to
      // remove them explicitly when overlays are reloaded or cleared.
      Shiny.addCustomMessageHandler('dronebior_clear_imagequery', function(_msg) {
        document.querySelectorAll('.leaflet-control.imagequery').forEach(function(el) {
          el.remove();
        });
      });

      // Drawing-mode toggle for the GIS Workspace map. Adds a crosshair
      // cursor to the map and pins a yellow drawing-mode badge at the
      // top of the canvas so the user has obvious visual confirmation
      // that clicks are being captured as ROI vertices. Driven by an R
      // observer that watches input$gis_measure_tool.
      Shiny.addCustomMessageHandler('dronebior_gis_drawing_mode', function(msg) {
        var mapEl = document.getElementById('gis_map');
        if (!mapEl) return;
        if (msg && msg.active) {
          mapEl.classList.add('dronebior-drawing-mode');
          var badge = mapEl.querySelector('.dronebior-map-badge');
          if (!badge) {
            badge = document.createElement('div');
            badge.className = 'dronebior-map-badge';
            mapEl.appendChild(badge);
          }
          badge.textContent = (msg && msg.label) ? msg.label : 'Drawing mode';
        } else {
          mapEl.classList.remove('dronebior-drawing-mode');
          var oldBadge = mapEl.querySelector('.dronebior-map-badge');
          if (oldBadge) oldBadge.remove();
        }
      });

      // Now-loading banner driven by R-side gis_task_start/stop. The R
      // thread is single-threaded, so when it is blocked on a slow
      // terra::rast() open the UI cannot rerun any reactive output -
      // but the WebSocket pushes the start-message BEFORE entering the
      // block and the stop-message right after, so the banner appears
      // and disappears at the right moments. The elapsed counter
      // updates via a client-side setInterval that keeps ticking even
      // while R is blocked.
      Shiny.addCustomMessageHandler('dronebior_gis_task', function(msg) {
        // Banner is global and floats at the top of the viewport. We
        // create it lazily here (idempotent) so it works regardless of
        // which tab the user is on - the GIS Workspace, Processing
        // Engine, 3D Modeling, anywhere. Position: fixed in the CSS.
        var banner = document.getElementById('gis-task-banner');
        if (!banner) {
          banner = document.createElement('div');
          banner.id = 'gis-task-banner';
          banner.className = 'gis-task-banner';
          document.body.insertBefore(banner, document.body.firstChild);
        }
        if (window.__db_gis_task_timer) {
          clearInterval(window.__db_gis_task_timer);
          window.__db_gis_task_timer = null;
        }
        if (msg && msg.action === 'start' && msg.name) {
          var startedAt = Date.now();
          banner.dataset.startedAt = String(startedAt);
          var safe = String(msg.name).replace(/</g, '&lt;');
          var detail = msg.detail ?
            ('<span class=\"gis-task-detail\"> &middot; ' +
             String(msg.detail).replace(/</g, '&lt;') + '</span>') : '';
          banner.innerHTML =
            '<span class=\"gis-task-spinner\"></span>' +
            '<span class=\"gis-task-label\">Now: <strong>' + safe + '</strong>' +
              detail + '</span>' +
            '<span class=\"gis-task-elapsed\" id=\"gis-task-elapsed\">0s</span>';
          banner.style.display = 'flex';
          window.__db_gis_task_timer = setInterval(function() {
            var el = document.getElementById('gis-task-elapsed');
            if (!el) return;
            var s = Math.round((Date.now() - startedAt) / 1000);
            el.textContent = (s < 60) ? (s + 's')
                              : (Math.floor(s/60) + 'm ' + (s%60) + 's');
          }, 500);
        } else {
          // A 'stop' message clears ONLY task-specific banners. The
          // heartbeat watchdog below may immediately re-open the
          // banner if R is still busy.
          if (banner.dataset.kind !== 'heartbeat') {
            banner.style.display = 'none';
            banner.innerHTML = '';
            delete banner.dataset.kind;
          }
        }
        banner.dataset.kind = (msg && msg.action === 'start') ? 'task' : '';
      });

      // Heartbeat watchdog: the server sends a heartbeat every second
      // via dronebior_heartbeat. When R is blocked on synchronous
      // work, the heartbeats stop arriving. The client-side timer
      // below notices the gap and surfaces a generic R-is-processing
      // banner with a live elapsed-time counter. This is the safety
      // net for slow operations that we have not (or cannot) wrap in
      // with_gis_task individually - the user always sees that R is
      // busy, even if we cannot say exactly which reactive is running.
      //
      // We deliberately do NOT activate the watchdog until the first
      // heartbeat has actually been received. Otherwise the page-load
      // / WebSocket-handshake interval (which is usually > 2 s on a
      // complex Shiny app like this) would falsely trip the watchdog
      // before R has even had a chance to register the observer.
      window.__db_last_heartbeat = 0;
      window.__db_heartbeat_seen = false;
      Shiny.addCustomMessageHandler('dronebior_heartbeat', function(_msg) {
        window.__db_last_heartbeat = Date.now();
        window.__db_heartbeat_seen = true;
      });
      setInterval(function() {
        if (!window.__db_heartbeat_seen) return;
        var since = Date.now() - window.__db_last_heartbeat;
        var banner = document.getElementById('gis-task-banner');
        // 2.5 s threshold gives ~1.5 s of slack on the 1 s heartbeat
        // cadence, so brief stalls (~1 s render bursts) do not flash
        // the banner on/off. Anything genuinely stuck shows up.
        if (since > 2500) {
          if (!banner) {
            banner = document.createElement('div');
            banner.id = 'gis-task-banner';
            banner.className = 'gis-task-banner';
            document.body.insertBefore(banner, document.body.firstChild);
          }
          // Only take over the banner when there is no named task
          // already shown. Named tasks (with_gis_task) are the more
          // informative signal and stay visible until their stop.
          if (banner.dataset.kind !== 'task') {
            var secs = Math.round(since / 1000);
            banner.innerHTML =
              '<span class=\"gis-task-spinner\"></span>' +
              '<span class=\"gis-task-label\">R is processing in the background...</span>' +
              '<span class=\"gis-task-elapsed\">' + secs + 's</span>';
            banner.style.display = 'flex';
            banner.dataset.kind = 'heartbeat';
          }
        } else if (banner && banner.dataset.kind === 'heartbeat' &&
                   banner.style.display !== 'none') {
          banner.style.display = 'none';
          banner.innerHTML = '';
          banner.dataset.kind = '';
        }
      }, 500);

      // 3D Modeling viewer remote controls. The Shiny app exposes
      // window.__dronebior_viewer = { camera, controls, defaultPosition,
      // defaultTarget } from inside the three.js render function. These
      // handlers let sidebar buttons drive the camera without forcing the
      // user to learn the OrbitControls mouse gestures.
      Shiny.addCustomMessageHandler('dronebior_3d_reset', function(_msg) {
        var v = window.__dronebior_viewer;
        if (!v || !v.camera || !v.controls) return;
        v.camera.position.copy(v.defaultPosition);
        v.controls.target.copy(v.defaultTarget);
        v.controls.update();
      });
      Shiny.addCustomMessageHandler('dronebior_3d_zoom', function(msg) {
        var v = window.__dronebior_viewer;
        if (!v || !v.camera || !v.controls) return;
        var factor = (msg && typeof msg.factor === 'number') ? msg.factor : 0.8;
        // Dolly the camera toward / away from the controls target.
        var dir = v.camera.position.clone().sub(v.controls.target);
        dir.multiplyScalar(factor);
        v.camera.position.copy(v.controls.target.clone().add(dir));
        v.controls.update();
      });

      // Screenshot the WebGL canvas as PNG and trigger a browser download.
      // HTML overlays (legend, scale bar, gizmo) are NOT included - this
      // captures the actual 3D scene as the camera sees it, which is
      // what one would put in a paper figure.
      Shiny.addCustomMessageHandler('dronebior_3d_screenshot', function(msg) {
        var v = window.__dronebior_viewer;
        if (!v || !v.renderer) return;
        // Force a fresh render so the framebuffer is up to date even if
        // preserveDrawingBuffer was not set. Calling toDataURL() in the
        // same synchronous task right after render() works reliably.
        v.renderer.render(v.scene, v.camera);
        var dataUrl = v.renderer.domElement.toDataURL('image/png');
        var ts = new Date().toISOString().replace(/[^0-9]/g, '').slice(0, 14);
        var a = document.createElement('a');
        a.href = dataUrl;
        a.download = (msg && msg.label ? msg.label : 'dronebior_3d_view') + '_' + ts + '.png';
        document.body.appendChild(a);
        a.click();
        document.body.removeChild(a);
      });

      // Export the full three.js scene as a binary glTF (.glb). The
      // exporter walks the scene graph: textured OBJ mesh, point cloud,
      // basemap plane, axes, gizmo - everything visible in the viewer.
      // .glb opens in Blender / ContextCapture / MeshLab / web 3D
      // viewers without conversion.
      Shiny.addCustomMessageHandler('dronebior_3d_export_gltf', function(msg) {
        var v = window.__dronebior_viewer;
        if (!v || !v.scene || !THREE.GLTFExporter) {
          if (window.Shiny) {
            Shiny.setInputValue('point_cloud_export_status',
              'GLTFExporter not available in this browser session.',
              { priority: 'event' });
          }
          return;
        }
        var exporter = new THREE.GLTFExporter();
        exporter.parse(v.scene, function(result) {
          var ts = new Date().toISOString().replace(/[^0-9]/g, '').slice(0, 14);
          var blob, ext;
          if (result instanceof ArrayBuffer) {
            blob = new Blob([result], { type: 'application/octet-stream' });
            ext = '.glb';
          } else {
            blob = new Blob([JSON.stringify(result, null, 2)], { type: 'application/json' });
            ext = '.gltf';
          }
          var url = URL.createObjectURL(blob);
          var a = document.createElement('a');
          a.href = url;
          a.download = (msg && msg.label ? msg.label : 'dronebior_3d_scene') + '_' + ts + ext;
          document.body.appendChild(a);
          a.click();
          document.body.removeChild(a);
          setTimeout(function() { URL.revokeObjectURL(url); }, 1000);
        }, function(err) {
          console.warn('GLTFExporter failed:', err);
        }, { binary: true });
      });

      // 3D Modeling extended controls. These message handlers reach
      // into window.__dronebior_viewer (set up inside the viewer
      // script) and manipulate the live scene without forcing a
      // re-render. Cheap and instant: layer visibility toggles,
      // point-size slider, background theme, camera presets, and
      // tool-cancel (Esc) all go through here.
      Shiny.addCustomMessageHandler('dronebior_3d_set_layer', function(msg) {
        var v = window.__dronebior_viewer;
        if (!v || !v.setLayerVisible || !msg) return;
        v.setLayerVisible(msg.name, msg.visible);
      });
      Shiny.addCustomMessageHandler('dronebior_3d_set_point_size', function(msg) {
        var v = window.__dronebior_viewer;
        window.__dronebior_point_size_pct = msg && msg.pct;
        if (v && v.setPointSize) v.setPointSize(msg && msg.pct);
      });
      Shiny.addCustomMessageHandler('dronebior_3d_set_bg', function(msg) {
        var v = window.__dronebior_viewer;
        if (v && v.setBackground) v.setBackground(msg && msg.theme);
      });
      Shiny.addCustomMessageHandler('dronebior_3d_camera_preset', function(msg) {
        var v = window.__dronebior_viewer;
        if (v && v.cameraPreset) v.cameraPreset(msg && msg.name);
      });
      Shiny.addCustomMessageHandler('dronebior_3d_cancel', function(_msg) {
        var v = window.__dronebior_viewer;
        if (v && v.cancelSelection) v.cancelSelection();
        // Best-effort polygon abort: the viewer JS keeps `polygonPath`
        // in its own closure, so we fire a synthetic Escape key on the
        // canvas which the viewer's keydown handler picks up.
        var canvas = v && v.renderer && v.renderer.domElement;
        if (canvas) {
          canvas.dispatchEvent(new KeyboardEvent('keydown',
            { key: 'Escape', bubbles: true }));
        }
      });

      // Push the current selection IDs into the live three.js scene
      // without tearing down the points object. Reaches into the viewer's
      // setSelection (added in the viewer script) which rebuilds only the
      // small selection-highlight BufferGeometry from the existing point
      // records. This is what makes clicking points feel instant on the
      // 3D Modeling tab - the previous code re-encoded the full point
      // cloud as JSON and rebuilt the entire scene on every click.
      Shiny.addCustomMessageHandler('dronebior_3d_set_selection', function(msg) {
        var v = window.__dronebior_viewer;
        if (!v || !v.setSelection) return;
        var ids = (msg && Array.isArray(msg.ids)) ? msg.ids : [];
        v.setSelection(ids);
      });

      // Global keyboard shortcuts for the 3D viewer. These fire even
      // when the canvas does not have keyboard focus (we listen on
      // window). The viewer presets API gates them by checking that
      // a viewer is currently mounted.
      window.addEventListener('keydown', function(e) {
        var v = window.__dronebior_viewer;
        if (!v) return;
        // Do not steal keys when the user is typing into a textInput.
        var tag = (e.target && e.target.tagName || '').toLowerCase();
        if (tag === 'input' || tag === 'textarea' || tag === 'select') return;
        if (e.key === 'f' || e.key === 'F') {
          if (v.cameraPreset) v.cameraPreset('frame');
          e.preventDefault();
        } else if (e.key === '1') {
          if (v.cameraPreset) v.cameraPreset('front'); e.preventDefault();
        } else if (e.key === '3') {
          if (v.cameraPreset) v.cameraPreset('side');  e.preventDefault();
        } else if (e.key === '7') {
          if (v.cameraPreset) v.cameraPreset('top');   e.preventDefault();
        } else if (e.key === '5') {
          if (v.cameraPreset) v.cameraPreset('iso');   e.preventDefault();
        } else if (e.key === 'Escape') {
          if (v.cancelSelection) v.cancelSelection();
          if (window.Shiny) {
            Shiny.setInputValue('dronebior_3d_esc',
              Math.random(), { priority: 'event' });
          }
        }
      });
    ")),
    tags$style(HTML("
      :root {
        --db-panel: #ffffff;
        --db-border: #d9e2ec;
        --db-text-soft: #52606d;
        --db-workspace-height: calc(100vh - 96px);
      }
      html, body { height: auto !important; min-height: 100%; overflow-y: auto !important; }
      body { background: #f6f8fb; }
      .navbar { box-shadow: 0 1px 8px rgba(16, 24, 40, 0.08); }
      /* Crosshair cursor while the user is in a measurement / ROI
         drawing mode. The class is toggled on the leaflet container by
         a small JS handler watching input gis_measure_tool, so the
         click target is unambiguous. */
      .leaflet-container.dronebior-drawing-mode { cursor: crosshair !important; }
      .dronebior-map-badge {
        position: absolute;
        top: 8px;
        left: 50%;
        transform: translateX(-50%);
        z-index: 1000;
        background: rgba(15, 23, 42, 0.9);
        color: #facc15;
        padding: 4px 12px;
        border-radius: 999px;
        font-size: 12px;
        font-weight: 600;
        pointer-events: none;
        border: 1px solid rgba(250, 204, 21, 0.4);
        box-shadow: 0 2px 6px rgba(0,0,0,0.3);
      }
      /* Stops the leaflet container from showing a pale-grey 'void'
         outside the world bounds when the map tiles have noWrap = TRUE.
         The world stops cleanly at lng = +/-180 (no duplicate Australias)
         and any pixels past that read as ocean-blue instead of as a
         broken tile area. The colour was sampled from Esri World Imagery
         deep ocean so the join between tile and background is invisible
         on the Satellite basemap; on the Light basemap it reads as a
         dark frame. */
      .leaflet-container { background-color: #0e2c43 !important; }
      .tab-content, .tab-pane, .bslib-page-fill {
        height: auto !important;
        max-height: none !important;
        overflow: visible !important;
      }
      .main-scroll {
        height: var(--db-workspace-height);
        max-height: var(--db-workspace-height);
        overflow-y: auto;
        padding-right: 6px;
        padding-bottom: 36px;
      }
      .bslib-sidebar-layout > .sidebar {
        height: var(--db-workspace-height);
        max-height: var(--db-workspace-height);
        overflow-y: auto;
      }
      .spectral-page .bslib-sidebar-layout {
        align-items: stretch;
        height: auto !important;
        max-height: none !important;
      }
      .spectral-page .bslib-sidebar-layout > .sidebar {
        align-self: stretch;
        height: auto !important;
        max-height: none !important;
        overflow-y: visible !important;
      }
      .spectral-page .bslib-sidebar-layout > .sidebar,
      .spectral-page .bslib-sidebar-layout > .sidebar > * {
        min-height: 100%;
      }
      .spectral-workspace {
        min-height: var(--db-workspace-height);
      }
      .spectral-stack {
        display: grid;
        gap: 12px;
        align-content: start;
      }
      .spectral-stack .card {
        margin-bottom: 0;
      }
      .spectral-sidebar-fill {
        min-height: 260px;
        border-top: 1px solid #d9e2ec;
        margin-top: 12px;
        padding-top: 12px;
        color: #52606d;
        font-size: 0.82rem;
        line-height: 1.35;
        display: flex;
        align-items: flex-end;
      }
      .panel-calibration-status pre {
        white-space: pre-wrap;
        margin-bottom: 8px;
        background: #f8fafc;
        border: 1px solid #d9e2ec;
        border-radius: 6px;
        padding: 8px 10px;
        color: #334e68;
        font-size: 0.82rem;
      }
      .card { border: 1px solid var(--db-border); border-radius: 8px; box-shadow: none; }
      .card-header { font-weight: 700; color: #102a43; background: #fff; border-bottom: 1px solid var(--db-border); }
      .metric-strip {
        display: grid;
        grid-template-columns: repeat(4, minmax(120px, 1fr));
        gap: 10px;
        margin-bottom: 12px;
      }
      .metric {
        background: #ffffff;
        border: 1px solid var(--db-border);
        border-radius: 8px;
        padding: 10px 12px;
      }
      .metric .label { color: var(--db-text-soft); font-size: 0.82rem; }
      .metric .value { color: #102a43; font-size: 1.18rem; font-weight: 750; }
      .status-pill {
        display: inline-flex;
        align-items: center;
        gap: 7px;
        font-size: 0.92rem;
        font-weight: 750;
        color: #102a43;
      }
      .status-icon {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        width: 22px;
        height: 22px;
        border-radius: 5px;
        color: #ffffff;
        font-size: 0.82rem;
        line-height: 1;
      }
      .status-ok .status-icon { background: #168a5b; }
      .status-bad .status-icon { background: #c24135; }
      .layer-help-btn {
        width: 24px;
        height: 24px;
        padding: 0 !important;
        border-radius: 50% !important;
        font-size: 0.74rem !important;
        line-height: 1 !important;
        flex: 0 0 auto;
      }
      .map-card-header {
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 8px;
        width: 100%;
      }
      .map-center-btn {
        padding: 3px 9px !important;
        font-size: 0.78rem !important;
        line-height: 1.2 !important;
        white-space: nowrap;
      }
      .processing-steps {
        margin: 0;
        padding-left: 1.15rem;
      }
      .processing-steps li { margin-bottom: 0.42rem; }
      .processing-path {
        font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace;
        font-size: 0.82rem;
        color: #334155;
        word-break: break-word;
      }
      .map-frame, .viewer-frame {
        border: 1px solid var(--db-border);
        border-radius: 8px;
        overflow: hidden;
      }
      .map-frame { background: #e5ede8; }
      .viewer-frame { background: #101828; position: relative; }
      .viewer-frame { height: clamp(520px, 70vh, 820px); min-height: 520px; }
      .viewer-overlay {
        position: absolute;
        background: rgba(15, 23, 42, 0.82);
        color: #f8fafc;
        border-radius: 8px;
        padding: 8px 10px;
        font-size: 0.78rem;
        pointer-events: none;
        box-shadow: 0 2px 6px rgba(0,0,0,0.25);
        z-index: 5;
      }
      .viewer-overlay.legend {
        top: 10px;
        left: 10px;
        min-width: 130px;
        pointer-events: none;
      }
      .viewer-overlay.scale {
        bottom: 12px;
        left: 12px;
        padding: 4px 8px;
        font-size: 0.72rem;
      }
      .viewer-overlay.scale .scale-bar {
        display: inline-block;
        width: 60px;
        height: 3px;
        background: #f8fafc;
        margin-right: 6px;
        vertical-align: middle;
      }
      .viewer-overlay.gizmo {
        bottom: 12px;
        right: 12px;
        padding: 0;
        background: rgba(15, 23, 42, 0.55);
        width: 112px;
        height: 112px;
      }
      .viewer-overlay .legend-heading {
        font-weight: 700;
        margin-bottom: 5px;
      }
      .viewer-overlay .legend-gradient {
        height: 10px;
        border-radius: 4px;
        margin: 4px 0;
        background: linear-gradient(to right, #ffffe5, #f7fcb9, #addd8e, #41ab5d, #006837);
      }
      .viewer-overlay .legend-scale {
        display: flex;
        justify-content: space-between;
        font-size: 0.7rem;
      }
      .viewer-action-bar {
        display: flex;
        gap: 6px;
        position: absolute;
        top: 10px;
        right: 10px;
        z-index: 6;
        pointer-events: auto;
      }
      .viewer-action-bar .btn {
        padding: 4px 9px;
        font-size: 0.8rem;
        background: rgba(15, 23, 42, 0.88);
        color: #f8fafc;
        border: 1px solid rgba(255,255,255,0.18);
      }
      .viewer-action-bar .btn:hover {
        background: rgba(15, 23, 42, 1);
        border-color: rgba(255,255,255,0.35);
      }
      .modeling-toolbar {
        display: flex;
        flex-wrap: wrap;
        gap: 8px;
        align-items: center;
        margin-bottom: 10px;
      }
      .modeling-toolbar .viewer-status {
        margin-left: auto;
        color: var(--db-text-soft);
        font-size: 0.85rem;
      }
      .modeling-metrics-tabs .card {
        margin-bottom: 0;
      }
      #point-cloud-viewer canvas.selection-layer {
        position: absolute;
        left: 0;
        top: 0;
        width: 100%;
        height: 100%;
        pointer-events: none;
      }
      #point-cloud-viewer .tool-badge {
        position: absolute;
        left: 10px;
        top: 10px;
        z-index: 4;
        background: rgba(15, 23, 42, 0.82);
        color: white;
        padding: 5px 8px;
        border-radius: 6px;
        font-size: 0.78rem;
      }
      .file-browser-entries select { font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace; }
      .viewer-toolbar {
        display: flex;
        align-items: center;
        gap: 10px;
        margin-bottom: 12px;
        flex-wrap: wrap;
      }
      .viewer-status { color: var(--db-text-soft); font-size: 0.9rem; }
      .db-legend {
        background: rgba(255,255,255,0.94);
        border: 1px solid #cbd5e1;
        border-radius: 8px;
        padding: 10px 12px;
        min-width: 190px;
        max-width: 260px;
        max-height: 320px;
        overflow-y: auto;
        box-shadow: 0 3px 10px rgba(15, 23, 42, 0.15);
      }
      .db-legend-heading {
        font-weight: 800;
        color: #102a43;
        margin-bottom: 8px;
      }
      .db-legend-row { margin-bottom: 8px; }
      .db-legend-title { font-size: 0.82rem; font-weight: 700; color: #102a43; }
      .db-legend-bar {
        height: 9px;
        border-radius: 4px;
        border: 1px solid #cbd5e1;
        margin: 3px 0;
      }
      .db-legend-scale {
        display: flex;
        justify-content: space-between;
        font-size: 0.72rem;
        color: #52606d;
      }
      .db-class-legend-control {
        background: transparent !important;
        border: 0 !important;
        box-shadow: none !important;
      }
      .class-legend {
        background: rgba(255,255,255,0.94);
        border: 1px solid #cbd5e1;
        border-radius: 8px;
        padding: 9px 10px;
        min-width: 178px;
        max-width: 235px;
        box-shadow: 0 3px 10px rgba(15, 23, 42, 0.15);
      }
      .class-legend-heading {
        font-weight: 800;
        color: #102a43;
        margin-bottom: 6px;
      }
      .class-legend-row {
        display: grid;
        grid-template-columns: 12px minmax(0, 1fr) auto;
        align-items: center;
        gap: 6px;
        font-size: 0.76rem;
        color: #102a43;
        margin-bottom: 4px;
      }
      .class-swatch {
        width: 10px;
        height: 10px;
        border: 1px solid rgba(15, 23, 42, 0.35);
        border-radius: 50%;
      }
      .class-name {
        overflow: hidden;
        text-overflow: ellipsis;
        white-space: nowrap;
      }
      .class-count {
        color: #475569;
        font-variant-numeric: tabular-nums;
      }
      .class-legend-footnote {
        border-top: 1px solid #e2e8f0;
        color: #52606d;
        font-size: 0.68rem;
        line-height: 1.2;
        margin-top: 6px;
        padding-top: 6px;
      }
      .sidebar-note {
        color: var(--db-text-soft);
        font-size: 0.86rem;
        line-height: 1.3;
        margin-top: 8px;
      }
      /* Sidebar grouping: each section gets a small heading bar with a
         coloured chip that hints at the section role (source, scene,
         selection, trees, volumes, actions). The chip uses a CSS
         gradient on a 10x10 square so we do not depend on emoji fonts. */
      .sidebar-section {
        display: flex;
        align-items: center;
        gap: 8px;
        font-weight: 700;
        text-transform: uppercase;
        letter-spacing: 0.05em;
        font-size: 0.72rem;
        color: #102a43;
        margin: 18px 0 8px;
        padding-bottom: 5px;
        border-bottom: 1px solid var(--db-border);
      }
      .sidebar-section:first-child { margin-top: 2px; }
      .sidebar-section-chip {
        display: inline-block;
        width: 10px;
        height: 10px;
        border-radius: 3px;
        background: linear-gradient(135deg, #168a5b, #14b8a6);
        box-shadow: 0 0 0 2px rgba(20, 184, 166, 0.18);
      }
      .sidebar-section-chip.scene   { background: linear-gradient(135deg, #2563eb, #38bdf8); box-shadow: 0 0 0 2px rgba(56,189,248,0.18); }
      .sidebar-section-chip.select  { background: linear-gradient(135deg, #f59e0b, #facc15); box-shadow: 0 0 0 2px rgba(250,204,21,0.20); }
      .sidebar-section-chip.trees   { background: linear-gradient(135deg, #15803d, #84cc16); box-shadow: 0 0 0 2px rgba(132,204,22,0.20); }
      .sidebar-section-chip.volume  { background: linear-gradient(135deg, #7c2d12, #d97706); box-shadow: 0 0 0 2px rgba(217,119,6,0.20); }
      .sidebar-section-chip.actions { background: linear-gradient(135deg, #102a43, #475569); box-shadow: 0 0 0 2px rgba(71,85,105,0.18); }
      .sidebar-section-chip.link    { background: linear-gradient(135deg, #7c3aed, #c026d3); box-shadow: 0 0 0 2px rgba(192,38,211,0.20); }

      /* Now-loading banner: floats at the top-center of the viewport
         on every tab, so the user always sees what R is busy with -
         not just while they are on the GIS Workspace tab. The R-side
         heavy operations push a task name through the dronebior_gis_task
         custom-message channel; a JS-driven setInterval keeps the
         elapsed-time counter ticking even while the R thread is
         blocked on a synchronous terra::rast() open. */
      .gis-task-banner {
        display: none;
        position: fixed;
        top: 76px;
        left: 50%;
        transform: translateX(-50%);
        z-index: 10000;
        min-width: 320px;
        max-width: min(720px, 90vw);
        align-items: center;
        gap: 12px;
        padding: 10px 16px;
        border-radius: 999px;
        background: linear-gradient(90deg, #fef3c7, #fde68a);
        border: 1px solid #f59e0b;
        color: #78350f;
        font-size: 0.92rem;
        box-shadow: 0 8px 22px rgba(245, 158, 11, 0.25),
                    0 2px 4px rgba(15, 23, 42, 0.10);
      }
      .gis-task-banner .gis-task-spinner {
        display: inline-block;
        width: 14px;
        height: 14px;
        border: 2px solid rgba(120, 53, 15, 0.25);
        border-top-color: #b45309;
        border-radius: 50%;
        animation: dronebior-spin 0.8s linear infinite;
        flex: 0 0 auto;
      }
      .gis-task-banner .gis-task-label {
        flex: 1 1 auto;
        overflow: hidden;
        text-overflow: ellipsis;
        white-space: nowrap;
      }
      .gis-task-banner .gis-task-detail {
        color: #92400e;
        font-style: italic;
        opacity: 0.85;
      }
      .gis-task-banner .gis-task-elapsed {
        font-variant-numeric: tabular-nums;
        font-weight: 700;
        color: #92400e;
        flex: 0 0 auto;
      }
      @keyframes dronebior-spin {
        from { transform: rotate(0deg); }
        to   { transform: rotate(360deg); }
      }

      /* Scene sources strip: at-a-glance view of which ODM products are
         already available for the current project. Each pill is the same
         status-pill used elsewhere in the app, so the affordance is
         consistent across tabs. */
      .scene-sources-row {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(160px, 1fr));
        gap: 8px;
        padding: 4px 0 2px;
      }
      .scene-source-tile {
        display: flex;
        flex-direction: column;
        gap: 4px;
        padding: 9px 11px;
        background: #ffffff;
        border: 1px solid var(--db-border);
        border-radius: 8px;
      }
      .scene-source-tile .scene-source-name {
        font-size: 0.74rem;
        text-transform: uppercase;
        letter-spacing: 0.05em;
        color: var(--db-text-soft);
        font-weight: 700;
      }

      /* Modeling-tab metric strip: same .metric tile shape used in
         GIS Workspace, with a small mono-line second row for context
         (e.g. \"of 35,000 sampled\"). */
      .modeling-metric-strip {
        display: grid;
        grid-template-columns: repeat(4, minmax(150px, 1fr));
        gap: 10px;
        margin-bottom: 12px;
      }
      .modeling-metric-strip .metric { padding: 11px 13px; }
      .modeling-metric-strip .metric .label {
        display: flex;
        align-items: center;
        gap: 8px;
        text-transform: uppercase;
        letter-spacing: 0.04em;
        font-size: 0.74rem;
      }
      .modeling-metric-strip .metric .value {
        font-size: 1.24rem;
        font-weight: 760;
        color: #102a43;
        letter-spacing: -0.01em;
      }
      .modeling-metric-strip .metric .sublabel {
        color: var(--db-text-soft);
        font-size: 0.74rem;
        margin-top: 2px;
      }
      .metric-dot {
        display: inline-block;
        width: 8px;
        height: 8px;
        border-radius: 50%;
        background: #168a5b;
        box-shadow: 0 0 0 2px rgba(22, 138, 91, 0.18);
      }
      .metric-dot.warn   { background: #d97706; box-shadow: 0 0 0 2px rgba(217,119,6,0.20); }
      .metric-dot.muted  { background: #94a3b8; box-shadow: 0 0 0 2px rgba(148,163,184,0.20); }
      .metric-dot.select { background: #facc15; box-shadow: 0 0 0 2px rgba(250,204,21,0.20); }
      .metric-dot.tree   { background: #84cc16; box-shadow: 0 0 0 2px rgba(132,204,22,0.20); }
      .metric-dot.scene  { background: #38bdf8; box-shadow: 0 0 0 2px rgba(56,189,248,0.20); }

      /* Toolbar grouping above the 3D viewer: visually separate
         load / camera / export controls so the user can find the
         right control without reading every button label. */
      .modeling-toolbar .toolbar-group {
        display: inline-flex;
        align-items: center;
        gap: 6px;
      }
      .modeling-toolbar .toolbar-divider {
        width: 1px;
        height: 22px;
        background: var(--db-border);
        margin: 0 4px;
      }
      .modeling-toolbar .toolbar-group-label {
        font-size: 0.70rem;
        text-transform: uppercase;
        letter-spacing: 0.05em;
        color: var(--db-text-soft);
        margin-right: 2px;
      }
      .modeling-toolbar .viewer-status {
        background: #ffffff;
        border: 1px solid var(--db-border);
        border-radius: 999px;
        padding: 4px 12px;
        font-size: 0.82rem;
      }

      /* Live layer-toggle row beneath the toolbar. checkboxGroupInput
         renders one .form-check per layer; we make them sit on the
         same line as pill-styled chips so the user sees Points, Trees,
         Mesh, Grid etc. at a glance. Toggling any pill instantly
         shows/hides the corresponding object in the three.js scene
         (no point-cloud rebuild). */
      .modeling-layer-row {
        display: flex;
        flex-wrap: wrap;
        align-items: center;
        gap: 6px 10px;
        margin: 8px 0 12px;
        padding: 8px 12px;
        background: #f6f8fb;
        border: 1px solid var(--db-border);
        border-radius: 999px;
        font-size: 0.82rem;
      }
      .modeling-layer-row > .toolbar-group-label {
        margin-right: 4px;
      }
      .modeling-layer-row .shiny-options-group {
        display: inline-flex;
        flex-wrap: wrap;
        align-items: center;
        gap: 8px 14px;
        margin: 0;
      }
      .modeling-layer-row .form-check {
        display: inline-flex;
        align-items: center;
        gap: 4px;
        margin: 0;
        padding: 2px 8px;
        background: #ffffff;
        border: 1px solid var(--db-border);
        border-radius: 999px;
      }
      .modeling-layer-row .form-check-label {
        font-size: 0.80rem;
        color: #102a43;
      }
      .modeling-layer-row .form-check-input { margin: 0 4px 0 0; }

      /* Floating mode badge that lives in the top-left corner of the
         3D viewer. Reflects the active selection tool name plus the
         relevant keyboard shortcuts so the user never has to consult
         a separate help page to know what the mouse buttons do. */
      .viewer-mode-badge {
        position: absolute;
        top: 12px;
        left: 12px;
        background: rgba(15, 23, 42, 0.85);
        color: #f8fafc;
        padding: 8px 12px;
        border-radius: 8px;
        font-size: 0.78rem;
        z-index: 4;
        max-width: 320px;
        line-height: 1.35;
      }
      .viewer-mode-badge .mode-name { font-weight: 700; }
      .viewer-mode-badge .mode-hints {
        color: #cbd5e1;
        font-size: 0.70rem;
        margin-top: 4px;
      }
      .viewer-mode-badge kbd {
        background: rgba(255,255,255,0.12);
        border: 1px solid rgba(255,255,255,0.25);
        border-radius: 4px;
        padding: 1px 5px;
        font-size: 0.70rem;
        font-family: ui-monospace, SFMono-Regular, Menlo, monospace;
      }

      /* Viewer chrome polish: subtle inner shadow on the dark canvas
         frame plus a slightly larger, multi-row legend tile. The
         tooltip pill in the top-right shows which point cloud is
         loaded and which heightsource is active (CHM vs local-Z). */
      .viewer-frame {
        box-shadow: inset 0 0 0 1px rgba(255,255,255,0.04),
                    0 6px 14px rgba(15, 23, 42, 0.10);
      }
      .viewer-overlay.scene-info {
        top: 10px;
        right: 10px;
        max-width: 280px;
        background: rgba(15, 23, 42, 0.75);
        font-size: 0.72rem;
        line-height: 1.35;
      }
      .viewer-overlay.scene-info .info-row {
        display: flex;
        justify-content: space-between;
        gap: 10px;
      }
      .viewer-overlay.scene-info .info-key {
        color: #cbd5e1;
        text-transform: uppercase;
        letter-spacing: 0.05em;
        font-size: 0.66rem;
      }
      .viewer-overlay.scene-info .info-val {
        color: #f8fafc;
        font-weight: 600;
        text-align: right;
        overflow: hidden;
        text-overflow: ellipsis;
        white-space: nowrap;
        max-width: 180px;
      }

      pre { white-space: pre-wrap; }
      .btn { border-radius: 6px; }
      table { font-size: 0.88rem; }
    "))
  ),
  nav_panel(
    "Processing Engine",
    layout_sidebar(
      sidebar = sidebar(
        width = 380,
        # Project setup: must come first. These mirror the inputs in GIS
        # Workspace; both panels stay in sync via observers below.
        textInput("project_dir_pe", "Project root", value = default_project$project_dir),
        textInput("images_dir_pe", "Source images folder",
                  value = default_project$images_dir,
                  placeholder = "Folder containing the drone JPGs / TIFFs"),
        # Auto-detected ODM project picker: scans <project>/outputs/ for
        # any <subdir>/<name>/odm_orthophoto/odm_orthophoto.tif and lets
        # the user point the app at whichever one they want (handy after
        # multiple ODM runs in the same project root).
        uiOutput("odm_project_picker_ui"),
        div(class = "sidebar-note small text-muted",
            "Project root + source images appear in every tab. ODM outputs (orthomosaic, DEMs, point cloud) are derived from this picker."),
        selectInput(
          "processing_engine",
          "Engine",
          choices = c(
            "ODM Docker (local)" = "odm_docker",
            "WebODM REST (remote)" = "webodm"
          ),
          selected = "odm_docker"
        ),
        conditionalPanel(
          "input.processing_engine == 'webodm'",
          textInput("webodm_url", "WebODM URL", value = "http://localhost:8000",
                    placeholder = "e.g. http://localhost:8000 or https://webodm.example.org"),
          textInput("webodm_user", "WebODM username"),
          passwordInput("webodm_pass", "WebODM password"),
          numericInput("webodm_poll_seconds", "Status poll interval (s)",
                       value = 60, min = 15, max = 600, step = 15),
          div(class = "sidebar-note small text-muted",
              "WebODM runs the same opendronemap/odm engine remotely. Set up an instance at https://github.com/WebODM/WebODM or use the cloud service at https://webodm.net/.")
        ),
        selectInput(
          "camera_type",
          "Camera type",
          choices = c(
            "Multispectral (MicaSense / Sequoia)" = "multispectral",
            "RGB (Sony / DJI / Phantom / generic)" = "rgb"
          ),
          selected = "multispectral"
        ),
        uiOutput("camera_detected_note"),
        uiOutput("geoscan_detected_note"),
        selectInput(
          "processing_preset",
          "Processing preset",
          choices = c(
            "Scientific canopy model (recommended)",
            "Fast orthomosaic only",
            "Full 3D deliverables",
            "Custom"
          ),
          selected = "Scientific canopy model (recommended)"
        ),
        numericInput("resolution", "Orthophoto resolution (cm)", value = 5, min = 1, max = 30, step = 0.5),
        checkboxInput("fast_orthophoto", "Fast orthophoto", value = FALSE),
        checkboxInput("build_dsm", "Generate DSM", value = TRUE),
        checkboxInput("build_dtm", "Generate DTM", value = TRUE),
        checkboxInput("pc_las", "Export LAS point cloud", value = TRUE),
        checkboxInput("pc_copc", "Export COPC point cloud", value = FALSE),
        checkboxInput("pc_csv", "Export CSV point cloud", value = FALSE),
        checkboxInput("tiles", "Export 2D web map tiles", value = FALSE),
        checkboxInput("three_d_tiles", "Export 3D tiles", value = FALSE),
        checkboxInput("gltf", "Export glTF model", value = FALSE),
        actionButton("refresh_command", "Build command", class = "btn-primary"),
        actionButton("run_odm", "Run ODM", class = "btn-outline-danger"),
        div(class = "sidebar-note", "For full 3D textured products, turn off fast orthophoto. ODM uses fast orthophoto to prioritize rapid orthomosaic generation. RGB camera mode skips the radiometric-calibration flag (it only applies to MicaSense-style sun + reflectance sensors).")
      ),
      panel_intro_card(
        "Processing Engine",
        "First step for a fresh project: pick the camera type, the preset, and click 'Run ODM'. Drives OpenDroneMap inside Docker for both MicaSense multispectral and generic RGB flights. If you already have orthomosaics from WebODM, Pix4Dmapper or Metashape, skip this panel and load the existing GeoTIFFs from GIS Workspace.",
        vignette = "external-engines"
      ),
      card(card_header("Processing workflow"), uiOutput("processing_workflow")),
      card(card_header("What this preset creates"), tableOutput("preset_outputs")),
      card(card_header("ODM Docker command"), verbatimTextOutput("odm_command")),
      card(card_header("Processing guidance"), textOutput("engine_note")),
      card(card_header("ODM run progress"),
           textInput("odm_log_path", "Log file (live)",
                     value = "/tmp/dronebior_logs/odm.log",
                     placeholder = "Auto-filled when you click Run ODM"),
           uiOutput("odm_progress_ui")),
      card(card_header("Product status"), tableOutput("processing_products"))
    )
  ),
  nav_panel(
    "GIS Workspace",
    layout_sidebar(
      sidebar = sidebar(
        width = 380,
        textInput("project_dir", "Project directory", value = default_project$project_dir),
        textInput("images_dir", "Raw test image folder", value = default_project$images_dir),
        textInput("orthomosaic", "Multispectral orthomosaic", value = default_project$odm_orthomosaic),
        textInput("output_dir", "Analysis output folder", value = default_project$output_dir),
        actionButton("open_file_browser", "Browse project files", class = "btn-outline-secondary"),
        checkboxInput("use_alpha", "Use alpha band as valid-data mask", value = TRUE),
        checkboxInput("scale_reflectance", "Scale bands to reflectance", value = TRUE),
        div(class = "form-label", "Overlay products"),
        uiOutput("map_layer_controls"),
        sliderInput("map_opacity", "Layer opacity", min = 0, max = 1, value = 0.72, step = 0.05),
        selectInput(
          "gis_color_stretch",
          "Color stretch",
          choices  = c("Fixed semantic", "Data range", "Percentile 2-98"),
          selected = "Percentile 2-98"
        ),
        checkboxInput("show_raw_flight", "Show raw image flight plan", value = TRUE),
        selectInput(
          "gis_measure_tool",
          "Map measurement tool",
          choices = c("Navigate", "Measure distance", "Measure area", "Measure volume (CHM)")
        ),
        actionButton("clear_gis_measure", "Clear map measurement", class = "btn-outline-secondary"),
        actionButton("recenter_gis_map", "Center map", class = "btn-outline-secondary"),
        div(class = "form-label", "Map annotations"),
        div(class = "sidebar-note",
            "How to pin an annotation: (1) type the note text in the field below; (2) check 'Annotation mode'; (3) click a coordinate on the map - a pink marker drops with your text as the popup. Use 'Save annotations' to persist the layer to GeoJSON; 'Load annotations' brings it back later."),
        textInput("annotation_text", NULL, placeholder = "Annotation text..."),
        checkboxInput("annotation_mode", "Annotation mode (click map to pin)", value = FALSE),
        actionButton("save_annotations", "Save annotations (GeoJSON)", class = "btn-outline-secondary"),
        verbatimTextOutput("annotations_save_path"),
        fileInput("load_annotations", "Load annotations (GeoJSON)", accept = c(".geojson", ".json")),
        actionButton("clear_annotations", "Clear annotations", class = "btn-outline-secondary"),
        div(class = "form-label", "ROI comparison"),
        div(class = "sidebar-note",
            "How to draw an ROI: (1) click 'Draw new ROI' to switch into polygon mode; (2) click vertices on the map to outline the region; (3) click 'Save ROI' to add it to the comparison table. To delete a single ROI, pick its name in the dropdown and click 'Delete selected ROI'. Vertex-by-vertex editing is not supported yet; use 'Redraw selected ROI' to replace one in place."),
        textInput("roi_name", NULL, placeholder = "ROI name (e.g. plot_3)", value = "roi_1"),
        actionButton("start_drawing_roi", "Draw new ROI", class = "btn-primary"),
        actionButton("save_roi", "Save ROI", class = "btn-outline-secondary"),
        selectInput("selected_roi_name", "Saved ROIs", choices = character(0)),
        actionButton("redraw_selected_roi", "Redraw selected ROI", class = "btn-outline-secondary"),
        actionButton("delete_selected_roi", "Delete selected ROI", class = "btn-outline-danger"),
        actionButton("compute_roi_comparison", "Compute ROI comparison", class = "btn-outline-secondary"),
        actionButton("clear_rois", "Clear all ROIs", class = "btn-outline-danger"),
        verbatimTextOutput("rois_save_path"),
        actionButton("load_gis", "Load selected overlays", class = "btn-primary"),
        actionButton("clear_gis", "Clear overlays", class = "btn-outline-secondary"),
        div(class = "sidebar-note", "The basemap is visible immediately. Selected products are added as transparent overlays when they finish loading.")
      ),
      div(
        class = "main-scroll",
        panel_intro_card(
          "GIS Workspace",
          "Choose product overlays in the sidebar and click 'Load' to render them on the basemap. Use the measurement tool to draw distances or areas on the map; results land in the 'Map measurement' card below. New here? Try 'run_drone_biomass_studio(sample = TRUE)' to open a clickable demo project.",
          vignette = "dronebior-overview"
        ),
        div(
          class = "metric-strip",
          div(class = "metric", div(class = "label", "Image folder"), div(class = "value", uiOutput("metric_images", inline = TRUE))),
          div(class = "metric", div(class = "label", "Orthomosaic"), div(class = "value", uiOutput("metric_ortho", inline = TRUE))),
          div(class = "metric", div(class = "label", "Point cloud"), div(class = "value", uiOutput("metric_cloud", inline = TRUE))),
          div(class = "metric", div(class = "label", "DSM / DTM"), div(class = "value", uiOutput("metric_dem", inline = TRUE)))
        ),
        # Banner appears only when the project root is inside a cloud-
        # sync folder (OneDrive / Google Drive / Dropbox / iCloud). Lets
        # the user migrate big binary outputs to a fast local cache
        # so the app never re-triggers cloud downloads on Load.
        uiOutput("cloud_sync_banner"),
        card(card_header("Project status"),
             uiOutput("project_status_quick")),
        div(class = "map-frame", leafletOutput("gis_map", height = "58vh")),
        card(card_header("Map measurement"), tableOutput("gis_measure_summary")),
        card(card_header("ROI comparison"), tableOutput("roi_comparison_table")),
        card(card_header("Available processing products"), tableOutput("product_table"))
      )
    )
  ),
  nav_panel(
    "3D Modeling",
    layout_sidebar(
      sidebar = sidebar(
        width = 340,
        sidebar_section("Scene source", tone = "scene"),
        selectInput(
          "viewer_cloud_source",
          "3D viewer source",
          choices = c("Full georeferenced LAS/LAZ/COPC sample", "PLY preview fallback"),
          selected = if (file.exists(default_full_cloud(default_products))) "Full georeferenced LAS/LAZ/COPC sample" else "PLY preview fallback"
        ),
        textInput("full_cloud_path", "Full-resolution point cloud (LAS/LAZ/COPC)", value = default_full_cloud(default_products)),
        textInput("ply_path", "Preview point cloud (PLY)", value = file.path(default_project$odm_project_dir, "odm_filterpoints", "point_cloud.ply")),
        numericInput("max_points", "Maximum preview points", value = 35000, min = 1000, max = 150000, step = 1000),
        div(class = "sidebar-note", "Switch to the PLY preview for fast iteration; the LAS/LAZ/COPC source gives full georeferenced metrics and feeds ROI recalculation."),

        sidebar_section("Scene composition", tone = "scene"),
        checkboxInput("show_draped_dsm", "Show DSM as 3D draped orthomosaic (Pix4D-style)", value = TRUE),
        checkboxInput("show_textured_mesh", "Show textured 3D mesh (ODM OBJ)", value = FALSE),
        selectInput(
          "point_color_mode",
          "Point symbology",
          choices = c("RGB", "Classification", "Height"),
          selected = "RGB"
        ),
        sliderInput("point_size_pct", "Point size (%)",
                    min = 25, max = 300, value = 100, step = 25, ticks = FALSE),
        selectInput(
          "viewer_bg_theme",
          "Viewer background",
          choices = c("Dark (navy)" = "dark",
                      "Light (slate)" = "light",
                      "White" = "white"),
          selected = "dark"
        ),
        numericInput("min_point_height", "Point height filter min (m)", value = 0, min = 0, max = 100, step = 0.5),
        numericInput("max_point_height", "Point height filter max (m)", value = 100, min = 0, max = 150, step = 0.5),

        sidebar_section("Selection & classification", tone = "select"),
        selectInput(
          "selection_tool",
          "3D interaction tool",
          choices = c(
            "Inspect trees",
            "Box selection",
            "Lasso selection",
            "Polygon selection",
            "Measure distance",
            "Manual crown edit"
          ),
          selected = "Box selection"
        ),
        selectInput(
          "classification_label",
          "Class to assign",
          choices = c("Unclassified", "Canopy", "Ground", "Tree crown", "Stem/trunk", "Low vegetation", "Noise", "Exclude")
        ),
        actionButton("classify_selection", "Classify selected points", class = "btn-outline-secondary"),

        sidebar_section("Use GIS Workspace ROI", tone = "link"),
        selectInput("gis_roi_to_3d", "Saved ROI", choices = character(0),
                    selected = NULL),
        actionButton("apply_gis_roi_to_3d", "Pull ROI into 3D selection",
                     class = "btn-outline-secondary"),
        div(class = "sidebar-note",
            "Saved ROIs from the GIS Workspace tab show up here. Use them to drive 3D ROI metrics from polygons you already drew on the orthomosaic."),

        sidebar_section("Tree detection", tone = "trees"),
        numericInput("tree_grid", "Tree candidate grid size (m)", value = 4, min = 1, max = 15, step = 0.5),
        numericInput("min_tree_height", "Minimum canopy height (m)", value = 1.5, min = 0.1, max = 20, step = 0.1),
        numericInput("min_tree_points", "Minimum points per candidate", value = 5, min = 1, max = 100, step = 1),
        checkboxInput("use_full_roi_metrics", "Recalculate selected ROI with full cloud + CHM", value = TRUE),

        sidebar_section("Volume & profile", tone = "volume"),
        numericInput("voxel_size", "Volume voxel size (m)", value = 0.5, min = 0.05, max = 5, step = 0.05),
        numericInput("profile_bin_size", "Vertical profile bin size (m)", value = 1, min = 0.1, max = 10, step = 0.1),
        selectInput(
          "survey_volume_method",
          "Survey volumes - base reference",
          choices = c(
            "External DTM (canopy / true biomass)"      = "dtm",
            "Minimum Z inside ROI (classic stockpile)"  = "min_z",
            "Mean Z inside ROI"                         = "mean_z",
            "Ground quantile (robust min)"              = "ground_quantile",
            "User-defined plane (Z = base value)"       = "user_plane",
            "Perimeter TIN (Pix4D-style stockpile)"     = "perimeter_tin"
          ),
          selected = "dtm"
        ),
        numericInput("survey_user_plane_z", "User-plane Z (m)", value = 0, step = 0.1),
        numericInput("survey_ground_quantile", "Ground quantile (0..1)",
                     value = 0.05, min = 0, max = 0.5, step = 0.01),

        sidebar_section("Actions", tone = "actions"),
        textInput("selection_label", "Selection / ROI label", value = "roi_1"),
        actionButton("clear_point_selection", "Clear selection", class = "btn-outline-secondary"),
        actionButton("save_manual_crown", "Save/update crown ROI", class = "btn-outline-secondary"),
        actionButton("delete_manual_crown", "Delete crown ROI", class = "btn-outline-secondary"),
        actionButton("export_selection", "Export selected ROI", class = "btn-outline-secondary"),
        actionButton("open_file_browser_3d", "Browse project files", class = "btn-outline-secondary"),
        actionButton("load_3d_scene", "Load 3D scene", class = "btn-primary"),
        div(class = "sidebar-note", "Click a canopy marker in the 3D view to inspect approximate height, crown diameter and crown volume.")
      ),
      div(
        class = "main-scroll",
        panel_intro_card(
          "3D Modeling",
          "Loads a dense point cloud (LAS/LAZ/COPC or a PLY preview) and lets you select ROIs by box, lasso or polygon. Computes survey-grade volumes (canopy biomass via DTM, or Pix4D-style perimeter-TIN stockpile), per-selection metrics, vertical profile, and approximate tree candidates. Switch the source to PLY for fast iteration; LAS gives full georeferenced metrics.",
          vignette = "point-clouds-and-chm"
        ),
        # Top metric strip: live numeric counts for the current scene so
        # the user can see at a glance how much data is loaded, how big
        # the active selection is, how many trees were detected, and what
        # surfaces are being drawn in the viewer. Mirrors the GIS
        # Workspace metric strip so the layout feels consistent.
        div(
          class = "modeling-metric-strip",
          div(class = "metric",
              div(class = "label",
                  tags$span(class = "metric-dot scene"),
                  "Points"),
              div(class = "value", uiOutput("modeling_metric_points", inline = TRUE)),
              div(class = "sublabel", uiOutput("modeling_metric_points_sub", inline = TRUE))),
          div(class = "metric",
              div(class = "label",
                  tags$span(class = "metric-dot select"),
                  "Selection"),
              div(class = "value", uiOutput("modeling_metric_selected", inline = TRUE)),
              div(class = "sublabel", uiOutput("modeling_metric_selected_sub", inline = TRUE))),
          div(class = "metric",
              div(class = "label",
                  tags$span(class = "metric-dot tree"),
                  "Trees"),
              div(class = "value", uiOutput("modeling_metric_trees", inline = TRUE)),
              div(class = "sublabel", uiOutput("modeling_metric_trees_sub", inline = TRUE))),
          div(class = "metric",
              div(class = "label",
                  tags$span(class = "metric-dot muted"),
                  "Scene surface"),
              div(class = "value", uiOutput("modeling_metric_surface", inline = TRUE)),
              div(class = "sublabel", uiOutput("modeling_metric_surface_sub", inline = TRUE)))
        ),
        # Scene sources status card: cross-tab feedback. Each tile reflects
        # what the Processing Engine has produced and what GIS Workspace
        # is currently pointing at, so the user does not have to flip tabs
        # to know whether the 3D scene can be built.
        card(
          card_header(
            div(class = "map-card-header",
                tags$span("Scene sources"),
                tags$span(class = "viewer-status",
                          textOutput("scene_sources_status", inline = TRUE)))
          ),
          div(class = "scene-sources-row",
              scene_source_tile("Orthomosaic",  "scene_source_ortho"),
              scene_source_tile("DSM",          "scene_source_dsm"),
              scene_source_tile("DTM",          "scene_source_dtm"),
              scene_source_tile("Point cloud",  "scene_source_cloud"),
              scene_source_tile("Textured mesh","scene_source_mesh"))
        ),
        div(
          class = "modeling-toolbar",
          div(class = "toolbar-group",
              tags$span(class = "toolbar-group-label", "Load"),
              actionButton("open_file_browser_3d_main", "Browse files", class = "btn-outline-secondary"),
              actionButton("load_3d_scene_main", "Load 3D scene", class = "btn-primary")),
          tags$span(class = "toolbar-divider"),
          div(class = "toolbar-group",
              tags$span(class = "toolbar-group-label", "View"),
              actionButton("cam_top",   "Top (7)",    class = "btn-outline-secondary"),
              actionButton("cam_front", "Front (1)",  class = "btn-outline-secondary"),
              actionButton("cam_side",  "Side (3)",   class = "btn-outline-secondary"),
              actionButton("cam_iso",   "Iso (5)",    class = "btn-outline-secondary"),
              actionButton("cam_frame", "Frame all (F)", class = "btn-outline-secondary")),
          tags$span(class = "toolbar-divider"),
          div(class = "toolbar-group",
              tags$span(class = "toolbar-group-label", "Camera"),
              actionButton("reset_3d_view", "Reset",   class = "btn-outline-secondary"),
              actionButton("zoom_in_3d",  "Zoom +",   class = "btn-outline-secondary"),
              actionButton("zoom_out_3d", "Zoom -",   class = "btn-outline-secondary")),
          tags$span(class = "toolbar-divider"),
          div(class = "toolbar-group",
              tags$span(class = "toolbar-group-label", "Export"),
              actionButton("screenshot_3d",   "Screenshot",     class = "btn-outline-secondary"),
              actionButton("export_3d_gltf",  "Export glTF",    class = "btn-outline-secondary")),
          span(class = "viewer-status", textOutput("point_cloud_status", inline = TRUE))
        ),
        # Live layer toggles - operate on the existing three.js scene
        # via custom messages, so flipping these is INSTANT (no point
        # cloud rebuild). The sidebar checkboxes still control whether
        # the heavy DSM / textured-mesh data is loaded at all.
        div(
          class = "modeling-layer-row",
          tags$span(class = "toolbar-group-label", "Layers"),
          checkboxGroupInput(
            "modeling_layers", label = NULL, inline = TRUE,
            choices  = c("Points", "Selected", "DSM drape", "Textured mesh",
                         "Trees", "Grid", "Axes"),
            selected = c("Points", "Selected", "DSM drape", "Textured mesh",
                         "Trees", "Grid", "Axes")
          ),
          tags$span(class = "toolbar-divider"),
          actionButton("cancel_3d_selection", "Cancel selection (Esc)",
                       class = "btn-outline-secondary btn-sm")
        ),
        # Large 3D viewport with overlays (legend + scale bar + axis hint
        # + top-right scene info chip showing source + height source).
        div(
          class = "viewer-frame",
          # Top-left mode badge with the active tool name and the
          # mouse-button / keyboard hints the user needs while
          # interacting. Updated by an observer that watches
          # input$selection_tool; the JS keyboard handler also
          # references it on hover.
          tags$div(class = "viewer-mode-badge",
                   uiOutput("viewer_mode_badge_content")),
          uiOutput("point_cloud_viewer"),
          div(
            class = "viewer-overlay legend",
            div(class = "legend-heading", textOutput("viewer_legend_heading", inline = TRUE)),
            div(class = "legend-gradient"),
            div(class = "legend-scale",
                tags$span(textOutput("viewer_legend_min", inline = TRUE)),
                tags$span(textOutput("viewer_legend_max", inline = TRUE))
            ),
            tags$div(
              style = "margin-top: 6px; font-size: 0.72rem; color: #94a3b8;",
              "Axes: ", tags$span(style = "color:#ef4444;", "X"), " ",
              tags$span(style = "color:#22c55e;", "Y"), " ",
              tags$span(style = "color:#3b82f6;", "Z")
            )
          ),
          div(
            class = "viewer-overlay scene-info",
            div(class = "info-row",
                tags$span(class = "info-key", "Source"),
                tags$span(class = "info-val", textOutput("viewer_info_source", inline = TRUE))),
            div(class = "info-row",
                tags$span(class = "info-key", "Heights"),
                tags$span(class = "info-val", textOutput("viewer_info_heights", inline = TRUE))),
            div(class = "info-row",
                tags$span(class = "info-key", "CRS"),
                tags$span(class = "info-val", textOutput("viewer_info_crs", inline = TRUE)))
          ),
          div(
            class = "viewer-overlay scale",
            span(class = "scale-bar"),
            tags$span(id = "viewer_scale_label", "~10 m")
          ),
          # Bottom-right corner: XYZ orientation gizmo (mini three.js scene
          # sync'd to the main camera). Populated by the viewer JS.
          div(class = "viewer-overlay gizmo", id = "viewer_gizmo")
        ),
        # Tools / measurements / metrics in a compact tabset BELOW the
        # viewer so the 3D viewport itself owns most of the screen.
        div(
          class = "modeling-metrics-tabs mt-3",
          navset_card_tab(
            id = "modeling_tabs",
            nav_panel(
              "Selection",
              tableOutput("selection_metrics"),
              tableOutput("classification_summary")
            ),
            nav_panel(
              "Survey volumes",
              tableOutput("survey_volume_table"),
              div(class = "small text-muted",
                  "DSM - base over the convex hull of the selected points. Choose base reference in the sidebar (DTM / min Z / mean Z / quantile / user plane / perimeter TIN).")
            ),
            nav_panel(
              "Trees",
              card(card_header("Approximate tree candidates"), tableOutput("tree_metrics")),
              card(card_header("Selected tree"),               tableOutput("selected_tree"))
            ),
            nav_panel(
              "Spectral signature",
              div(class = "small text-muted mb-2",
                  "Per-tree (or per-ROI) spectral means computed in Spectral Analytics. ",
                  "Run \"Compute tree / ROI spectral metrics\" there to populate this table; ",
                  "selecting a tree in the 3D viewer highlights the corresponding row."),
              tableOutput("modeling_tree_spectral")
            ),
            nav_panel(
              "Vertical profile",
              plotOutput("vertical_profile_plot", height = "260px")
            ),
            nav_panel(
              "Manual crowns / ROIs",
              tableOutput("manual_crowns_table"),
              verbatimTextOutput("full_roi_status")
            ),
            nav_panel(
              "Distance",
              tableOutput("distance_measurement")
            ),
            nav_panel(
              "2D context",
              leafletOutput("point_cloud_context_map", height = "320px"),
              actionButton("recenter_context_map", "Center map", class = "btn-outline-secondary btn-sm mt-2")
            ),
            nav_panel(
              "Export",
              verbatimTextOutput("selection_export_paths")
            ),
            nav_panel(
              "Tool reference",
              tags$ul(
                tags$li(tags$strong("Box selection: "), "drag a rectangle over visible points."),
                tags$li(tags$strong("Lasso selection: "), "drag a freehand polygon over visible points."),
                tags$li(tags$strong("Polygon selection: "), "click vertices, double-click to close."),
                tags$li(tags$strong("Measure distance: "), "click two visible points; distance in meters."),
                tags$li(tags$strong("Manual crown edit: "), "lasso a crown, save with the Selection / ROI label."),
                tags$li(tags$strong("Volume: "), "reported as occupied voxel volume; see also the Survey volumes tab."),
                tags$li(tags$strong("Reset view / Zoom +/-: "), "in the toolbar above the viewer, controls the OrbitControls camera."),
                tags$li(tags$strong("Pull ROI into 3D selection: "), "in the sidebar, takes a polygon you saved in GIS Workspace and highlights the matching preview points here.")
              )
            )
          )
        )
      )
    )
  ),
  nav_panel(
    "Spectral Analytics",
    div(
      class = "spectral-page",
      layout_sidebar(
        sidebar = sidebar(
          width = 360,
          actionButton("load_mosaic", "Load orthomosaic", class = "btn-primary"),
          checkboxInput("spectral_use_alpha", "Use alpha / valid-data mask", value = TRUE),
          selectInput(
            "radiometric_scale_mode",
            "Radiometric scale",
            choices = c("Auto detect", "Already reflectance 0-1", "Divide by 10000", "Divide by 65535", "Raw DN / no scaling"),
            selected = "Auto detect"
          ),
          div(class = "form-label", "Panel calibration reflectance"),
          numericInput("panel_blue", "Blue panel reflectance", value = 0.50, min = 0, max = 1, step = 0.001),
          numericInput("panel_green", "Green panel reflectance", value = 0.50, min = 0, max = 1, step = 0.001),
          numericInput("panel_red", "Red panel reflectance", value = 0.50, min = 0, max = 1, step = 0.001),
          numericInput("panel_rededge", "RedEdge panel reflectance", value = 0.50, min = 0, max = 1, step = 0.001),
          numericInput("panel_nir", "NIR panel reflectance", value = 0.50, min = 0, max = 1, step = 0.001),
          actionButton("apply_panel_calibration", "Apply panel ROI calibration", class = "btn-outline-secondary"),
          actionButton("reset_panel_calibration", "Reset panel calibration", class = "btn-outline-secondary"),
          checkboxInput("remove_physical_invalid", "Mask reflectance outside 0-1", value = TRUE),
          selectInput("median_filter_size", "Median filter", choices = c("None" = 0, "3 x 3" = 3, "5 x 5" = 5), selected = 0),
          checkboxInput("clean_valid_mask", "Morphological valid-mask cleanup", value = FALSE),
          selectInput("downsample_method", "Scientific downsampling", choices = c("None", "Average", "Median", "Gaussian average", "75% quantile"), selected = "None"),
          numericInput("downsample_factor", "Downsampling factor", value = 1, min = 1, max = 10, step = 1),
          selectInput("display_stretch", "Display stretch", choices = c("Percentile 2-98", "Histogram equalization", "None"), selected = "Percentile 2-98"),
          checkboxInput("fixed_index_limits", "Use fixed scientific index limits", value = TRUE),
          selectInput("preview_mode", "Reflectance preview", choices = c("RGB", "NIR", "RedEdge", "Blue", "Green", "Red")),
          uiOutput("index_layer_ui"),
          textInput("custom_index_name", "Custom index name", value = "Custom_Index"),
          textAreaInput("custom_index_formula", "Custom index formula", value = "(NIR - Red) / (NIR + Red)", rows = 3),
          actionButton("compute_custom_index", "Compute custom index", class = "btn-outline-secondary"),
          uiOutput("application_index_ui"),
          numericInput("class_water_max", "Water/shadow max", value = 0.05, min = -1, max = 1, step = 0.01),
          numericInput("class_bare_max", "Bare soil max", value = 0.20, min = -1, max = 1, step = 0.01),
          numericInput("class_stress_max", "Stress max", value = 0.40, min = -1, max = 1, step = 0.01),
          numericInput("class_moderate_max", "Moderate vigor max", value = 0.65, min = -1, max = 1, step = 0.01),
          actionButton("compute_tree_spectral_metrics", "Compute tree / ROI spectral metrics", class = "btn-outline-secondary"),
          actionButton("export_products", "Export rasters", class = "btn-outline-secondary"),
          div(
            class = "spectral-sidebar-fill",
            "Scientific outputs use calibrated/preprocessed reflectance. Display stretch is used only for visualization and does not overwrite analysis rasters."
          )
        ),
        div(
          class = "spectral-workspace spectral-stack",
          panel_intro_card(
            "Spectral Analytics",
            "Loads the multispectral orthomosaic, applies your chosen radiometric scaling (auto-detect / divide by 32768 / etc.) and optional panel-ROI calibration, then computes the nine indices (NDVI, NDRE, EVI, SAVI, NDWI, GNDVI, CIrededge, MSAVI2, VARI) plus a biomass proxy. Use this panel to inspect band histograms and pick a meaningful threshold before exporting application maps.",
            vignette = "spectral-indices"
          ),
          layout_columns(
            col_widths = c(4, 8),
            card(card_header("Orthomosaic metadata"), tableOutput("mosaic_meta")),
            card(card_header("Radiometric QA"), tableOutput("radiometric_qa"))
          ),
          layout_columns(
            col_widths = c(6, 6),
            card(card_header("Band histograms"), plotOutput("band_histogram_plot", height = "360px")),
            card(
              card_header("Panel ROI calibration"),
              plotOutput("panel_roi_plot", height = "360px", brush = brushOpts(id = "panel_roi_brush", resetOnNew = TRUE)),
              div(class = "panel-calibration-status", verbatimTextOutput("panel_calibration_status")),
              tableOutput("panel_calibration_table")
            )
          ),
          layout_columns(
            col_widths = c(6, 6),
            card(card_header("Index histogram"), plotOutput("index_histogram_plot", height = "320px")),
            card(card_header("Application map"), plotOutput("application_map_plot", height = "380px"))
          ),
          layout_columns(
            col_widths = c(6, 6),
            card(card_header("Application map area summary"), tableOutput("application_summary")),
            card(card_header("Tree / ROI spectral metrics"), tableOutput("tree_spectral_metrics"))
          ),
          layout_columns(
            col_widths = c(6, 6),
            card(card_header("Scientific reflectance preview"), plotOutput("mosaic_plot", height = "430px")),
            card(card_header("Index calculator preview"), plotOutput("index_plot", height = "430px"))
          ),
          card(card_header("Index summary"), tableOutput("index_summary")),
          card(card_header("Exported files"), verbatimTextOutput("export_paths"))
        )
      )
    )
  ),
  nav_panel(
    "Field Models",
    layout_sidebar(
      sidebar = sidebar(
        width = 340,
        fileInput("field_file", "Field biomass CSV", accept = ".csv"),
        actionButton("extract_field", "Extract spectral values", class = "btn-primary"),
        actionButton("fit_model", "Fit baseline model", class = "btn-outline-secondary"),
        div(class = "sidebar-note", "Expected columns: sample_id, biomass_kgha, and either x/y or longitude/latitude.")
      ),
      panel_intro_card(
        "Field Models",
        "Upload your field biomass CSV, extract spectral values at each sample point, and fit a baseline linear model. The default model picks whichever of NDVI, NDRE, EVI, SAVI, NDWI, NIR and RedEdge are available - constrain the predictor set in R via fit_biomass_lm(predictors = ...) if you need a specific specification.",
        vignette = "dronebior-overview"
      ),
      card(card_header("Extracted samples"), tableOutput("field_preview")),
      card(card_header("Baseline model"), verbatimTextOutput("model_summary"))
    )
  ),
  nav_panel(
    "Exports",
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        actionButton("run_workflow", "Run R analysis workflow", class = "btn-primary"),
        actionButton("render_report", "Render HTML report", class = "btn-outline-secondary"),
        fileInput("report_field_csv", "Field CSV for report (optional)", accept = ".csv")
      ),
      panel_intro_card(
        "Exports",
        "Re-runs the full pipeline through run_dronebio_workflow() and writes the reflectance bands, spectral indices, biomass proxy and (when present) the valid-data mask to your project's output folder. Render HTML report produces a self-contained DroneBioR_report.html with the ODM inventory, index histograms, the CHM and an optional field-based biomass model.",
        vignette = "dronebior-overview"
      ),
      card(card_header("Workflow status"), verbatimTextOutput("workflow_status")),
      card(card_header("Output files"), verbatimTextOutput("workflow_outputs")),
      card(card_header("Report"), verbatimTextOutput("report_status"))
    )
  ),
  nav_panel(
    "Time Series",
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        textInput("ts_registry_path", "Registry path", value = default_flight_registry()),
        dateInput("ts_flight_date", "Flight date", value = Sys.Date()),
        textInput("ts_flight_project_dir", "Flight project dir",
                  placeholder = "/path/to/Drone_Biomass/2026-05-01"),
        textInput("ts_flight_notes", "Notes (optional)"),
        actionButton("ts_register", "Register flight", class = "btn-outline-secondary"),
        selectInput("ts_metric", "Metric",
                    choices = c("NDVI mean"            = "ndvi",
                                "Biomass proxy mean"   = "biomass",
                                "CHM mean (m)"         = "chm")),
        actionButton("ts_refresh", "Refresh plot", class = "btn-primary"),
        actionButton("ts_clear_registry", "Clear registry", class = "btn-outline-danger"),
        div(class = "sidebar-note", "Each registered flight is one row in the CSV at the registry path. Register a flight by entering its date and project directory, then refresh.")
      ),
      panel_intro_card(
        "Time Series",
        "Track NDVI, biomass proxy or canopy height across multiple flights of the same site. Register one row per flight (date + project directory), then pick a metric to plot it across time. The registry is a plain CSV under ~/.dronebior/flights.csv by default, so you can edit or version it manually.",
        vignette = "dronebior-overview"
      ),
      card(card_header("Registered flights"), tableOutput("ts_flights_table")),
      card(card_header("Time series plot"), plotOutput("ts_plot", height = "320px"))
    )
  )
)

server <- function(input, output, session) {
  # Sync the project-root and images-dir inputs between the Processing
  # Engine sidebar (`*_pe`) and the GIS Workspace sidebar. Each observer
  # only updates when the value actually differs, so the cross-update
  # is a no-op and the loop terminates after one hop.
  observeEvent(input$project_dir_pe, {
    if (!identical(input$project_dir_pe, input$project_dir)) {
      updateTextInput(session, "project_dir", value = input$project_dir_pe)
    }
  }, ignoreInit = TRUE)
  observeEvent(input$project_dir, {
    if (!identical(input$project_dir, input$project_dir_pe)) {
      updateTextInput(session, "project_dir_pe", value = input$project_dir)
    }
  }, ignoreInit = TRUE)
  observeEvent(input$images_dir_pe, {
    if (!identical(input$images_dir_pe, input$images_dir)) {
      updateTextInput(session, "images_dir", value = input$images_dir_pe)
    }
  }, ignoreInit = TRUE)
  observeEvent(input$images_dir, {
    if (!identical(input$images_dir, input$images_dir_pe)) {
      updateTextInput(session, "images_dir_pe", value = input$images_dir)
    }
  }, ignoreInit = TRUE)

  # Debounced shadows of the path inputs. Every keystroke in a textInput
  # fires the reactive graph; without debouncing, heavy follow-up work
  # (EXIF parsing for the flight overlay, raster header reads, etc.)
  # runs once per character typed and freezes the session. We keep the
  # raw `input$project_dir` etc. for immediate UI feedback (the file
  # browser opens against the path the user is editing right now), but
  # any expensive observer reads the debounced versions so the work
  # waits until typing settles.
  project_dir_debounced <- debounce(reactive(input$project_dir), 700)
  images_dir_debounced  <- debounce(reactive(input$images_dir),  700)

  # Heartbeat for the client-side watchdog. Every 1 s while the R
  # session is idle, we send a tiny custom message. The browser tracks
  # the time since the last heartbeat - when that gap exceeds 2.5 s
  # the watchdog assumes R is blocked on synchronous work and pops up
  # the "R is processing in the background..." banner with a live
  # elapsed-time counter. This is the safety net for slow operations
  # we did not wrap in with_gis_task individually: the user always
  # sees that something is running, even if we cannot name it.
  #
  # We do NOT call httpuv::service here on purpose. The observer body
  # is just a single sendCustomMessage; Shiny flushes the message
  # automatically when the observer finishes. Calling service(0) on
  # every tick would do unnecessary work and risk re-entrancy.
  observe({
    invalidateLater(1000, session)
    session$sendCustomMessage("dronebior_heartbeat",
                              list(t = as.numeric(Sys.time())))
  })

  # Discover existing ODM project layouts on disk so the user can
  # switch between e.g. odm_aerial_dataset/aerial_geoscan and
  # odm_micasense_dataset/micasense without manually editing paths.
  available_odm_projects <- reactive({
    # detect_odm_projects walks outputs/*/*/ to find existing ODM
    # project layouts. On OneDrive Files-On-Demand each list.dirs()
    # may round-trip metadata over the network, which freezes the UI
    # for seconds per keystroke in project_dir. Wrap in with_gis_task
    # so the floating banner tells the user what is running.
    pd <- input$project_dir %||% ""
    with_gis_task(session,
                  name   = "Scanning project for ODM outputs",
                  detail = basename(pd),
                  detect_odm_projects(pd))
  })

  output$odm_project_picker_ui <- renderUI({
    df <- available_odm_projects()
    if (!nrow(df)) {
      return(tags$div(class = "sidebar-note small text-muted",
                      "ODM project: (none on disk yet — defaults will be used until a run finishes)"))
    }
    labels <- vapply(seq_len(nrow(df)), function(i) {
      sprintf("%s/%s", df$dataset_subdir[i], df$project_name[i])
    }, character(1))
    selectInput("odm_project_pick", "ODM project (detected on disk)",
                choices = setNames(labels, labels),
                selected = labels[1L])
  })

  # Pick the right ODM dataset/project layout for the current root.
  # Precedence:
  #   1. the user's explicit selection in the ODM project picker;
  #   2. the first auto-detected ODM project on disk (covers users
  #      who never touched the picker - e.g. a Sony aerial run that
  #      writes to outputs/odm_aerial_dataset/aerial_geoscan/);
  #   3. the canonical MicaSense defaults via dronebio_project()
  #      (only when nothing has been detected yet).
  # The previous version of this reactive went straight from 1 to 3,
  # so any project that did not match the user's picker (which itself
  # might not have rendered yet on first paint) silently used the
  # MicaSense default - the banner / DTM / CHM all pointed at the
  # wrong odm_<subdir>/<project>/ tree.
  project <- reactive({
    pick <- input$odm_project_pick
    df <- available_odm_projects()
    chosen_row <- NULL
    if (!is.null(pick) && nzchar(pick) && !is.null(df) && nrow(df)) {
      match_idx <- which(paste0(df$dataset_subdir, "/", df$project_name) == pick)
      if (length(match_idx)) chosen_row <- df[match_idx[1L], , drop = FALSE]
    }
    if (is.null(chosen_row) && !is.null(df) && nrow(df) >= 1L) {
      chosen_row <- df[1L, , drop = FALSE]
    }
    p <- if (!is.null(chosen_row)) {
      dronebio_project(
        project_dir        = input$project_dir,
        odm_dataset_subdir = chosen_row$dataset_subdir,
        odm_project_name   = chosen_row$project_name
      )
    } else {
      dronebio_project(project_dir = input$project_dir)
    }
    p$images_dir <- input$images_dir
    p$output_dir <- input$output_dir
    p
  })

  # Cache-aware product paths. For every entry in odm_product_paths(),
  # returns the same path UNLESS a same-named file already lives in the
  # local cache (~/.dronebior/cache/<slug>/), in which case we point
  # readers at the cached copy. This is what stops the 3D Modeling tab
  # from re-pulling the DSM / point cloud from OneDrive Files-On-Demand
  # on every reactive invocation - the cache copy is local SSD and
  # never triggers a sync. The user must have run "Copy outputs to
  # local cache" once for any of this to matter; otherwise it
  # transparently returns the canonical paths.
  cached_products <- reactive({
    p <- project()
    paths <- odm_product_paths(p)
    for (k in names(paths)) {
      paths[[k]] <- DroneBioR:::cache_aware_path(unname(paths[[k]]), p)
    }
    paths
  })

  overlay_choices <- c(
    # Headline composite + the four canonical raster products
    "RGB Orthomosaic", "DSM", "DTM", "CHM",
    # Multispectral vegetation indices (need NIR; some also need RedEdge)
    "NDVI", "NDRE", "EVI", "SAVI", "OSAVI", "MSAVI2", "NDWI",
    "GNDVI", "CIrededge", "GCI", "RVI", "DVI", "WDRVI", "TVI",
    "MCARI", "PSRI",
    # RGB-only vegetation indices (work on any colour drone)
    "VARI", "ExG", "GLI", "TGI", "MGRVI", "RGBVI",
    # Biomass proxies. The spectral one (Biomass_Index_Proxy) is the
    # legacy pure-spectral surface; the *_x_CHM family combines greenness
    # with the canopy-height model for a volume-weighted biomass surrogate.
    "Biomass_Index_Proxy",
    "Biomass_NDVI_x_CHM", "Biomass_NDRE_x_CHM", "Biomass_SAVI_x_CHM",
    "Biomass_GNDVI_x_CHM", "Biomass_VARI_x_CHM", "Biomass_EXG_x_CHM",
    "Biomass_MGRVI_x_CHM", "Biomass_RGBVI_x_CHM",
    # Individual reflectance bands
    "NIR", "RedEdge", "Red", "Green", "Blue",
    # Relief shading from the DSM
    "Hillshade"
  )
  # Which bands each overlay needs in `reflectance()`. RGB orthos only
  # carry Blue/Green/Red, so anything requiring NIR/RedEdge is hidden.
  # The four canonical products (RGB Orthomosaic / DSM / DTM / CHM) are
  # stand-alone raster files, so they have no band requirements -- their
  # availability is checked separately via quick_outputs_check().
  # The Biomass_*_x_CHM layers need both their spectral inputs and the
  # CHM raster; we use the band requirements here to hide them on RGB
  # orthos that lack the bands - CHM availability is enforced inside
  # gis_stack() via compute_biomass_proxies().
  overlay_band_requirements <- list(
    `RGB Orthomosaic`   = c("Blue", "Green", "Red"),  # composite display
    DSM                 = character(),
    DTM                 = character(),
    CHM                 = character(),
    NDVI                = c("Red", "NIR"),
    NDRE                = c("RedEdge", "NIR"),
    EVI                 = c("Blue", "Red", "NIR"),
    SAVI                = c("Red", "NIR"),
    OSAVI               = c("Red", "NIR"),
    MSAVI2              = c("Red", "NIR"),
    NDWI                = c("Green", "NIR"),
    GNDVI               = c("Green", "NIR"),
    CIrededge           = c("RedEdge", "NIR"),
    GCI                 = c("Green", "NIR"),
    RVI                 = c("Red", "NIR"),
    DVI                 = c("Red", "NIR"),
    WDRVI               = c("Red", "NIR"),
    TVI                 = c("Green", "Red", "NIR"),
    MCARI               = c("Green", "Red", "RedEdge"),
    PSRI                = c("Green", "Red", "RedEdge"),
    VARI                = c("Blue", "Green", "Red"),
    ExG                 = c("Blue", "Green", "Red"),
    GLI                 = c("Blue", "Green", "Red"),
    TGI                 = c("Blue", "Green", "Red"),
    MGRVI               = c("Green", "Red"),
    RGBVI               = c("Blue", "Green", "Red"),
    Biomass_Index_Proxy = c("Red", "NIR", "RedEdge"),
    Biomass_NDVI_x_CHM  = c("Red", "NIR"),
    Biomass_NDRE_x_CHM  = c("RedEdge", "NIR"),
    Biomass_SAVI_x_CHM  = c("Red", "NIR"),
    Biomass_GNDVI_x_CHM = c("Green", "NIR"),
    Biomass_VARI_x_CHM  = c("Blue", "Green", "Red"),
    Biomass_EXG_x_CHM   = c("Blue", "Green", "Red"),
    Biomass_MGRVI_x_CHM = c("Green", "Red"),
    Biomass_RGBVI_x_CHM = c("Blue", "Green", "Red"),
    NIR                 = c("NIR"),
    RedEdge             = c("RedEdge"),
    Red                 = c("Red"),
    Green               = c("Green"),
    Blue                = c("Blue"),
    Hillshade           = character()  # depends on DSM, not on the ortho bands
  )
  # Cheap peek at the orthomosaic so the overlay panel can hide layers
  # the file cannot support, BEFORE the user clicks Load Mosaic. Returns
  # NA when no file is set yet. Goes through raster_header() so the
  # underlying terra::rast open is cached by (path, mtime, size) and
  # surfaced in the "Now loading" banner on its first hit - keystrokes
  # in input$orthomosaic do not re-open the same multi-GB file.
  overlay_orthomosaic_nlyr <- reactive({
    path <- input$orthomosaic
    if (!is.character(path) || !length(path) || !nzchar(path) || !file.exists(path)) {
      return(NA_integer_)
    }
    hdr <- raster_header(path, progress_msg = "Reading orthomosaic band count")
    if (is.null(hdr)) NA_integer_ else hdr$nlyr
  })
  available_overlays <- reactive({
    n <- overlay_orthomosaic_nlyr()
    # First pass: spectral filtering by band requirements.
    candidates <- if (is.na(n)) overlay_choices else {
      if (n <= 4) {
        keep <- vapply(overlay_choices, function(layer) {
          req_bands <- overlay_band_requirements[[layer]] %||% character()
          !any(c("NIR", "RedEdge") %in% req_bands)
        }, logical(1))
        overlay_choices[keep]
      } else overlay_choices
    }
    # Second pass: hide DSM / DTM / CHM rows when the file isn't on
    # disk yet (so users don't pick a layer that will fail to load).
    p <- tryCatch(project(), error = function(e) NULL)
    if (!is.null(p)) {
      qc <- tryCatch(quick_outputs_check(p), error = function(e) NULL)
      if (!is.null(qc)) {
        if (!isTRUE(qc[["dsm"]])) candidates <- setdiff(candidates, "DSM")
        if (!isTRUE(qc[["dtm"]])) candidates <- setdiff(candidates, "DTM")
        if (!isTRUE(qc[["chm"]])) candidates <- setdiff(candidates, "CHM")
        if (!isTRUE(qc[["orthomosaic"]])) candidates <- setdiff(candidates, "RGB Orthomosaic")
      }
    }
    candidates
  })

  browser_dir <- reactiveVal(set_browser_dir(default_project$project_dir))
  browser_selected <- reactiveVal(set_browser_dir(default_project$project_dir))
  gis_measure_points <- reactiveVal(data.frame(lng = numeric(), lat = numeric()))

  # Persistent map annotations. Stored as a tidy data frame in WGS84 so the
  # save format (GeoJSON) and the leaflet markers share the same coords.
  empty_annotations <- function() {
    data.frame(
      lng        = numeric(),
      lat        = numeric(),
      label      = character(),
      created_at = character(),
      stringsAsFactors = FALSE
    )
  }
  annotations <- reactiveVal(empty_annotations())

  # ROI collection for multi-region comparison. Each entry holds the polygon
  # vertices in WGS84 alongside the user-given name, so we can replay the
  # same ROI against different rasters / dates later.
  roi_collection <- reactiveVal(list())
  roi_comparison_request <- reactiveVal(0L)

  # Async state for the 'Refine DTM + CHM via CSF (lidR)' button.
  # `csf_running` gates the button so it cannot be fired twice in
  # parallel; `outputs_refresh_token` is a monotonically-incrementing
  # counter that downstream UI reads to know when on-disk products
  # have changed under it (CSF rewrites DTM and CHM, but no reactive
  # input changes, so we bump the token to force a re-render of the
  # project status card). `csf_cancelled` flips to TRUE when the user
  # clicks Cancel - finish_csf reads it to suppress the "failed" toast
  # that would otherwise show when the killed worker's promise rejects.
  csf_running            <- reactiveVal(FALSE)
  csf_cancelled          <- reactiveVal(FALSE)
  outputs_refresh_token  <- reactiveVal(0L)
  panel_coefficients <- reactiveVal(data.frame(
    band = c("Blue", "Green", "Red", "RedEdge", "NIR"),
    certified_reflectance = NA_real_,
    observed_roi_mean = NA_real_,
    gain = 1,
    offset = 0,
    model = "identity",
    stringsAsFactors = FALSE
  ))
  panel_calibration_status <- reactiveVal(
    "No panel calibration applied. Draw a ROI on the calibration panel preview, enter certified reflectance values in the sidebar, then click Apply panel ROI calibration."
  )
  custom_index_raster <- reactiveVal(NULL)
  spectral_export_paths <- reactiveVal(character())
  tree_spectral_metrics_value <- reactiveVal(data.frame())

  add_raw_flight_layers <- function(map, flight) {
    if (is.null(flight) || nrow(flight) == 0) {
      return(map)
    }
    map |>
      addPolylines(
        data = flight,
        lng = ~longitude,
        lat = ~latitude,
        color = "#1f6f5b",
        weight = 2,
        opacity = 0.8,
        group = "Raw flight path"
      ) |>
      addCircleMarkers(
        data = flight,
        lng = ~longitude,
        lat = ~latitude,
        radius = 5,
        stroke = TRUE,
        color = "#ffffff",
        weight = 1.5,
        fillColor = "#1f6f5b",
        fillOpacity = 0.88,
        group = "Raw image centers",
        popup = ~paste0(
          "<strong>", capture_id, "</strong>",
          "<br>Sequence: ", sequence,
          "<br>Altitude: ", round(altitude_m, 2), " m",
          "<br>Heading: ", round(heading, 1), "&deg;"
        )
      ) |>
      addMarkers(
        data = flight,
        lng = ~longitude,
        lat = ~latitude,
        icon = icons(iconUrl = flight$arrow_icon, iconWidth = 34, iconHeight = 34),
        group = "Flight direction",
        popup = ~paste0("Direction of flight at ", capture_id)
      )
  }

  flight_overlay_groups <- c("Raw flight path", "Raw image centers", "Flight direction")

  add_project_footprint <- function(map, orthomosaic_path, group = "Orthomosaic footprint") {
    if (!file.exists(orthomosaic_path)) {
      return(map)
    }
    # Pull bounds from the cached header so we never re-open the file.
    # First open shows the "Now loading" banner; subsequent calls hit
    # the cache and return microseconds. Saves the multi-second
    # terra::rast() trip every time renderLeaflet invalidates.
    hdr <- raster_header(orthomosaic_path,
                         progress_msg = "Reading orthomosaic footprint")
    bounds <- if (!is.null(hdr)) hdr$bounds_4326 else NULL
    if (is.null(bounds) || any(!is.finite(bounds))) {
      return(map)
    }
    map |>
      addRectangles(
        lng1 = bounds[["lng1"]],
        lat1 = bounds[["lat1"]],
        lng2 = bounds[["lng2"]],
        lat2 = bounds[["lat2"]],
        color = "#facc15",
        weight = 2,
        fill = FALSE,
        group = group,
        popup = group
      ) |>
      fitBounds(bounds[["lng1"]], bounds[["lat1"]], bounds[["lng2"]], bounds[["lat2"]])
  }

  fit_leaflet_to_orthomosaic <- function(map_id, orthomosaic_path, notify_missing = TRUE) {
    if (!file.exists(orthomosaic_path)) {
      if (isTRUE(notify_missing)) {
        showNotification("Orthomosaic file was not found, so the map cannot be centered.", type = "warning", duration = 4)
      }
      return(invisible(NULL))
    }

    hdr <- raster_header(orthomosaic_path,
                         progress_msg = "Centering map on orthomosaic")
    bounds <- if (!is.null(hdr)) hdr$bounds_4326 else NULL
    if (is.null(bounds) || any(!is.finite(bounds))) {
      if (isTRUE(notify_missing)) {
        showNotification("Orthomosaic bounds could not be read.", type = "warning", duration = 4)
      }
      return(invisible(NULL))
    }

    leafletProxy(map_id) |>
      fitBounds(bounds[["lng1"]], bounds[["lat1"]], bounds[["lng2"]], bounds[["lat2"]])
    invisible(bounds)
  }

  add_esri_imagery_tiles <- function(map, group = "Satellite") {
    transparent_tile <- "data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///ywAAAAAAQABAAACAUwAOw=="
    map |>
      addTiles(
        urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
        attribution = "Tiles &copy; Esri",
        group = group,
        options = tileOptions(
          maxZoom = 22,
          maxNativeZoom = 19,
          updateWhenIdle = FALSE,
          unloadInvisibleTiles = FALSE,
          errorTileUrl = transparent_tile,
          zIndex = 220,
          keepBuffer = 8,
          # noWrap: the world map stops at lng = +/-180, NO duplicate
          # copies of Australia / Americas appear past the dateline.
          noWrap = TRUE,
          # bounds: Esri's World Imagery server returns an actual grey
          # tile reading "Map data not yet available" when a tile is
          # requested past lng = +/-180. The errorTileUrl callback does
          # NOT save us here because Esri responds with HTTP 200 - the
          # tile is technically valid, it just has placeholder content.
          # Setting `bounds` here tells leaflet not to issue tile
          # requests outside the world bounds at all, so the area
          # beyond +/-180 shows the leaflet-container background colour
          # (painted ocean-blue by the CSS rule below).
          bounds = list(c(-85, -180), c(85, 180))
        )
      )
  }

  # Build a downsampled DSM heightfield for the "Show DSM as 3D draped
  # orthomosaic" feature. Returns a list with the Z grid (row-major, north
  # to south, west to east) plus geo bounds, ready for the JS three.js
  # PlaneGeometry displacement loop.
  #
  # When an orthomosaic with a 6th alpha band is supplied, the DSM is
  # masked by that alpha before sampling - which is what stops the
  # white margins of the ortho from rising into tall white columns
  # (ODM extrapolates the DSM to its bounding box, so the "no flight
  # coverage" pixels also carry valid-looking high Z values).
  build_dsm_heightmap <- function(dsm_path, orthomosaic_path = NULL, grid_size = 300) {
    if (!file.exists(dsm_path)) return(NULL)
    tryCatch({
      dsm <- terra::rast(dsm_path)
      if (terra::nlyr(dsm) > 1) dsm <- dsm[[1]]

      # --- 1) Crop to the orthomosaic footprint when both exist. The
      # DSM commonly extrapolates a few pixels beyond the ortho, and
      # those pixels carry extreme Z values (the "no-coverage spike"
      # that draws infinite white columns in the 3D scene).
      ortho_r <- NULL
      if (!is.null(orthomosaic_path) && file.exists(orthomosaic_path)) {
        ortho_r <- tryCatch(terra::rast(orthomosaic_path), error = function(e) NULL)
      }
      if (!is.null(ortho_r)) {
        dsm <- tryCatch(
          terra::crop(dsm, terra::ext(ortho_r), snap = "near"),
          error = function(e) dsm
        )
      }

      # --- 2) Alpha-band mask. Works for ODM MicaSense (band 6),
      # ODM RGB with alpha (band 4), and any other layout that puts
      # alpha as the LAST band. Feathered edges (alpha 1..254) still
      # produce spikes, so we treat anything below 200 as no-data.
      if (!is.null(ortho_r)) {
        nl <- terra::nlyr(ortho_r)
        alpha_idx <- if (nl == 6L) 6L else if (nl == 4L) 4L else NA_integer_
        if (!is.na(alpha_idx)) {
          alpha <- ortho_r[[alpha_idx]]
          if (!terra::compareGeom(dsm, alpha, stopOnError = FALSE)) {
            alpha <- terra::resample(alpha, dsm, method = "near")
          }
          dsm <- terra::mask(dsm, alpha >= 200, maskvalues = 0, updatevalue = NA)
        }
      }

      # --- 3) Outlier clipping by robust percentiles. RGB orthos
      # without a usable alpha band still spike; here we compute the
      # 0.5 / 99.5 percentile of the FINITE DSM values and mark
      # anything outside that range as NA. A 99% canopy + ground
      # signal sits well inside this window, but the 50 000 m no-data
      # sentinel does not.
      dsm_vals <- tryCatch(terra::values(dsm), error = function(e) NULL)
      if (!is.null(dsm_vals)) {
        ok <- is.finite(dsm_vals)
        if (any(ok)) {
          qs <- stats::quantile(dsm_vals[ok], probs = c(0.005, 0.995),
                                na.rm = TRUE, names = FALSE)
          if (is.finite(qs[1L]) && is.finite(qs[2L]) && qs[2L] > qs[1L]) {
            spread <- qs[2L] - qs[1L]
            # Allow a small margin below/above the central window so a
            # tall building / tree slightly above p99.5 is not cut off.
            lo <- qs[1L] - 0.10 * spread
            hi <- qs[2L] + 0.10 * spread
            dsm[dsm < lo | dsm > hi] <- NA
          }
        }
      }

      # --- 4) Final despike: 3x3 median respects NAs and removes
      # one-off extreme pixels that survived the percentile clip.
      dsm <- tryCatch(
        terra::focal(dsm, w = 3, fun = "median", na.rm = TRUE, na.policy = "all"),
        error = function(e) dsm
      )

      ds <- terra::spatSample(dsm, size = grid_size * grid_size,
                              method = "regular", as.raster = TRUE, na.rm = FALSE)
      z  <- terra::as.matrix(ds, wide = TRUE)

      # NaN is JSON-illegal; flag every missing/invalid cell as NA and
      # let the viewer JS skip the corresponding vertex (collapses it
      # to the scene floor instead of building a spike).
      bad <- !is.finite(z)
      if (all(bad)) return(NULL)
      # Use the 1st percentile of the GOOD pixels as the no-data
      # floor. This is below visible terrain but never -Inf.
      floor_z <- as.numeric(stats::quantile(z[!bad], probs = 0.01,
                                            na.rm = TRUE, names = FALSE))
      z[bad] <- floor_z

      e <- terra::ext(ds)
      list(
        z_rows = lapply(seq_len(nrow(z)), function(i) as.numeric(z[i, ])),
        nrow   = nrow(z),
        ncol   = ncol(z),
        xmin   = as.numeric(e[1]),
        xmax   = as.numeric(e[2]),
        ymin   = as.numeric(e[3]),
        ymax   = as.numeric(e[4])
      )
    }, error = function(e) NULL)
  }

  build_orthomosaic_texture <- function(orthomosaic_path, use_alpha = TRUE, scale_reflectance = TRUE) {
    if (!file.exists(orthomosaic_path)) {
      return(NULL)
    }

    tryCatch({
      ortho <- read_multispectral_orthomosaic(orthomosaic_path, use_alpha = use_alpha)
      rgb <- if (isTRUE(scale_reflectance)) scale_to_reflectance(ortho$bands) else ortho$bands
      rgb <- rgb[[c("Blue", "Green", "Red")]]
      rgb <- downsample_raster(rgb, size = 250000)
      e <- terra::ext(rgb)
      png_path <- tempfile(fileext = ".png")
      grDevices::png(png_path, width = 900, height = 900, bg = "transparent")
      old_par <- par(no.readonly = TRUE)
      device_open <- TRUE
      on.exit({
        if (isTRUE(device_open)) {
          par(old_par)
          grDevices::dev.off()
        }
        unlink(png_path)
      }, add = TRUE)
      par(mar = c(0, 0, 0, 0))
      terra::plotRGB(rgb, r = 3, g = 2, b = 1, stretch = "lin", axes = FALSE)
      par(old_par)
      grDevices::dev.off()
      device_open <- FALSE

      encoded <- jsonlite::base64_enc(readBin(png_path, what = "raw", n = file.info(png_path)$size))
      list(
        data_uri = paste0("data:image/png;base64,", encoded),
        xmin = as.numeric(e[1]),
        xmax = as.numeric(e[2]),
        ymin = as.numeric(e[3]),
        ymax = as.numeric(e[4])
      )
    }, error = function(e) {
      NULL
    })
  }

  build_context_orthomosaic_raster <- function(orthomosaic_path, use_alpha = TRUE, scale_reflectance = TRUE) {
    if (!file.exists(orthomosaic_path)) {
      return(NULL)
    }

    tryCatch({
      ortho <- read_multispectral_orthomosaic(orthomosaic_path, use_alpha = use_alpha)
      rgb <- scale_to_reflectance(ortho$bands)
      rgb <- rgb[[c("Blue", "Green", "Red")]]
      rgb <- downsample_raster(rgb, size = 90000)
      for (i in seq_len(terra::nlyr(rgb))) {
        vals <- terra::values(rgb[[i]], mat = FALSE)
        vals <- vals[is.finite(vals)]
        if (length(vals) > 20) {
          limits <- stats::quantile(vals, probs = c(0.02, 0.98), na.rm = TRUE, names = FALSE)
          if (all(is.finite(limits)) && diff(limits) > 0) {
            rgb[[i]] <- terra::clamp((rgb[[i]] - limits[[1]]) / diff(limits), lower = 0, upper = 1, values = TRUE)
          }
        }
      }
      rgb <- round(terra::clamp(rgb, lower = 0, upper = 1, values = TRUE) * 255)
      terra::RGB(rgb) <- c(3, 2, 1)
      rgb
    }, error = function(e) {
      NULL
    })
  }

  output$map_layer_controls <- renderUI({
    # Hide overlays the loaded orthomosaic cannot support (e.g. NDVI/NDRE
    # on a 3-band RGB ortho). Default-on layer adapts: NDVI when present,
    # else VARI (the RGB equivalent).
    avail <- available_overlays()
    default_on <- if ("NDVI" %in% avail) "NDVI" else if ("VARI" %in% avail) "VARI" else NA_character_
    rows <- lapply(avail, function(layer_name) {
      meta <- product_metadata[[layer_name]]
      tags$div(
        class = "d-flex align-items-center justify-content-between gap-2 mb-1",
        checkboxInput(
          inputId = layer_input_id(layer_name),
          label = meta$label,
          value = identical(layer_name, default_on)
        ),
        actionButton(
          inputId = help_input_id(layer_name),
          label = "?",
          class = "btn btn-outline-secondary layer-help-btn",
          title = paste("Show", meta$label, "formula")
        )
      )
    })
    tags$div(rows)
  })

  selected_overlay_layers <- reactive({
    overlay_choices[vapply(overlay_choices, function(layer_name) {
      isTRUE(input[[layer_input_id(layer_name)]])
    }, logical(1))]
  })

  lapply(overlay_choices, function(layer_name) {
    local({
      product_name <- layer_name
      observeEvent(input[[help_input_id(product_name)]], {
        meta <- product_metadata[[product_name]]
        if (is.null(meta)) {
          showModal(modalDialog(
            title = product_name,
            tags$p("No documentation registered for this layer."),
            easyClose = TRUE,
            footer = modalButton("Close")
          ))
          return()
        }
        # Show every documented field. Each entry is hidden when the
        # metadata entry does not define that field, so the modal stays
        # compact for source bands (which have no 'range' or 'reference')
        # but expands for the indices that do.
        field_row <- function(label, value) {
          if (is.null(value) || !nzchar(as.character(value))) return(NULL)
          tags$p(tags$strong(paste0(label, ": ")), value)
        }
        showModal(modalDialog(
          title = meta$label %||% product_name,
          field_row("Formula",        meta$formula),
          field_row("Bands needed",   meta$bands),
          field_row("Typical range",  meta$range),
          field_row("Unit",           meta$unit),
          field_row("Reference",      meta$reference),
          field_row("Interpretation", meta$use),
          easyClose = TRUE,
          size = "m",
          footer = modalButton("Close")
        ))
      }, ignoreInit = TRUE)
    })
  })

  show_file_browser <- function(start_path = NULL) {
    if (!is.null(start_path) && nzchar(start_path)) {
      browser_dir(set_browser_dir(start_path))
      browser_selected(normalizePath(start_path, mustWork = FALSE))
    }

    showModal(modalDialog(
      title = "Project file browser",
      size = "l",
      easyClose = TRUE,
      textInput("browser_path_text", "Current folder", value = browser_dir()),
      div(
        class = "d-flex gap-2 mb-2",
        actionButton("browser_go", "Go", class = "btn-outline-secondary"),
        actionButton("browser_up", "Up one level", class = "btn-outline-secondary"),
        actionButton("browser_open", "Open selected", class = "btn-primary")
      ),
      uiOutput("file_browser_entries"),
      tableOutput("browser_selected_path"),
      footer = tagList(
        actionButton("use_selected_project", "Use as project"),
        actionButton("use_selected_images", "Use as image folder"),
        actionButton("use_selected_orthomosaic", "Use as orthomosaic"),
        actionButton("use_selected_output", "Use as output folder"),
        actionButton("use_selected_ply", "Use as PLY"),
        actionButton("use_selected_full_cloud", "Use as LAS/LAZ/COPC"),
        modalButton("Close")
      )
    ))
  }

  observeEvent(input$open_file_browser, {
    show_file_browser(input$project_dir)
  })

  observeEvent(input$open_file_browser_3d, {
    show_file_browser(input$full_cloud_path %||% input$ply_path)
  })

  observeEvent(input$open_file_browser_3d_main, {
    show_file_browser(input$full_cloud_path %||% input$ply_path)
  })

  observeEvent(browser_dir(), {
    updateTextInput(session, "browser_path_text", value = browser_dir())
  }, ignoreInit = TRUE)

  output$file_browser_entries <- renderUI({
    entries <- list_browser_entries(browser_dir())
    if (nrow(entries) == 0) {
      return(div(class = "sidebar-note", "No directories or supported files were found in this folder."))
    }

    choices <- stats::setNames(entries$path, entries$label)
    div(
      class = "file-browser-entries",
      selectInput(
        "browser_entry",
        "Folder contents",
        choices = choices,
        selected = choices[[1]],
        size = min(18, max(8, length(choices))),
        selectize = FALSE
      )
    )
  })

  observeEvent(input$browser_entry, {
    browser_selected(input$browser_entry)
  }, ignoreInit = TRUE)

  output$browser_selected_path <- renderTable({
    data.frame(selected_path = browser_selected(), stringsAsFactors = FALSE)
  }, digits = 2)

  observeEvent(input$browser_go, {
    path <- input$browser_path_text
    validate(need(nzchar(path), "Enter a folder path."))
    if (dir.exists(path)) {
      browser_dir(normalizePath(path, mustWork = FALSE))
      browser_selected(normalizePath(path, mustWork = FALSE))
    } else if (file.exists(path)) {
      browser_dir(dirname(normalizePath(path, mustWork = FALSE)))
      browser_selected(normalizePath(path, mustWork = FALSE))
    } else {
      showNotification("Path not found.", type = "error")
    }
  })

  observeEvent(input$browser_up, {
    parent <- dirname(browser_dir())
    browser_dir(normalizePath(parent, mustWork = FALSE))
    browser_selected(normalizePath(parent, mustWork = FALSE))
  })

  observeEvent(input$browser_open, {
    path <- browser_selected()
    if (dir.exists(path)) {
      browser_dir(normalizePath(path, mustWork = FALSE))
    }
  })

  observeEvent(input$use_selected_project, {
    path <- browser_selected()
    if (!dir.exists(path)) path <- dirname(path)
    updateTextInput(session, "project_dir", value = normalizePath(path, mustWork = FALSE))
  })

  observeEvent(input$use_selected_images, {
    path <- browser_selected()
    if (!dir.exists(path)) path <- dirname(path)
    updateTextInput(session, "images_dir", value = normalizePath(path, mustWork = FALSE))
  })

  observeEvent(input$use_selected_orthomosaic, {
    path <- browser_selected()
    if (!file.exists(path)) {
      showNotification("Select a GeoTIFF file for the orthomosaic.", type = "warning")
      return()
    }
    updateTextInput(session, "orthomosaic", value = normalizePath(path, mustWork = FALSE))
  })

  observeEvent(input$use_selected_output, {
    path <- browser_selected()
    if (!dir.exists(path)) path <- dirname(path)
    updateTextInput(session, "output_dir", value = normalizePath(path, mustWork = FALSE))
  })

  observeEvent(input$use_selected_ply, {
    path <- browser_selected()
    if (!file.exists(path) || !grepl("\\.ply$", path, ignore.case = TRUE)) {
      showNotification("Select a PLY file for the 3D preview.", type = "warning")
      return()
    }
    updateTextInput(session, "ply_path", value = normalizePath(path, mustWork = FALSE))
  })

  observeEvent(input$use_selected_full_cloud, {
    path <- browser_selected()
    if (!file.exists(path) || !grepl("\\.(las|laz)$", path, ignore.case = TRUE)) {
      showNotification("Select a LAS, LAZ or COPC LAZ file for full-resolution ROI metrics.", type = "warning")
      return()
    }
    updateTextInput(session, "full_cloud_path", value = normalizePath(path, mustWork = FALSE))
  })

  # Cheap fast outputs: file.exists() on a single path. Render
  # immediately without project() dependency so they're visible even
  # while the heavier reactives downstream are still warming up.
  output$metric_images <- renderUI({
    status_badge(dir.exists(input$images_dir %||% ""), "Ready", "Missing")
  })

  output$metric_ortho <- renderUI({
    status_badge(file.exists(input$orthomosaic %||% ""), "Available", "Missing")
  })

  # Slower outputs: depend on project() (which depends on the disk-
  # scanning available_odm_projects()). We pre-render a placeholder
  # spinner so the user sees activity instead of an empty box during
  # the 1-2s warmup window.
  output$metric_cloud <- renderUI({
    products <- tryCatch(odm_product_paths(project()), error = function(e) NULL)
    if (is.null(products)) {
      return(tags$span(class = "status-pill status-bad",
                       tags$span(class = "status-icon", HTML("&#8230;")),
                       "Checking..."))
    }
    status_badge(any(file.exists(unname(products[c("point_cloud_las", "point_cloud_laz", "point_cloud_copc")]))),
                 "Available", "Missing")
  })

  output$metric_dem <- renderUI({
    products <- tryCatch(odm_product_paths(project()), error = function(e) NULL)
    if (is.null(products)) {
      return(tags$span(class = "status-pill status-bad",
                       tags$span(class = "status-icon", HTML("&#8230;")),
                       "Checking..."))
    }
    ok <- file.exists(products[["dsm"]]) && file.exists(products[["dtm"]])
    status_badge(ok, "DSM/DTM OK", "Missing")
  })

  products <- reactive({
    summarize_odm_products(project())
  })

  # Detect cloud-sync project roots (OneDrive / Google Drive / etc.)
  # and offer a one-click migration of the heavy binary outputs to a
  # local cache directory outside any cloud sync. Once migrated, the
  # app repoints every path input at the local copy and never touches
  # the cloud folder again for analysis.
  cloud_sync_provider <- reactive({
    is_cloud_sync_path(input$project_dir %||% "")
  })

  output$cloud_sync_banner <- renderUI({
    provider <- cloud_sync_provider()
    if (is.na(provider)) return(NULL)
    p <- tryCatch(project(), error = function(e) NULL)
    if (is.null(p)) return(NULL)
    cache_dir <- DroneBioR:::local_cache_dir(p)
    already_migrated <- dir.exists(cache_dir) &&
      length(list.files(cache_dir, pattern = "\\.(tif|laz|las|obj|glb)$",
                        ignore.case = TRUE)) > 0L
    if (already_migrated) {
      tags$div(
        style = paste("background:#ecfdf5; color:#065f46; padding:10px 14px;",
                      "border-radius:6px; margin-bottom:12px;"),
        tags$strong("✓ Outputs cached locally"),
        tags$br(),
        sprintf("Reads come from %s — no more %s traffic.",
                cache_dir, provider),
        tags$br(),
        actionButton("repoint_to_cloud", "Switch reads back to cloud folder",
                     class = "btn-link btn-sm",
                     style = "padding:0; margin-top:4px;"))
    } else {
      tags$div(
        style = paste("background:#fef3c7; color:#92400e; padding:10px 14px;",
                      "border-radius:6px; margin-bottom:12px;"),
        tags$strong("⚠ Project lives inside ", provider),
        tags$br(),
        "Reading the orthomosaic / DSM / point cloud from a cloud-synced ",
        "folder can trigger background up/downloads. Copy the heavy outputs ",
        "to a local cache once and the app reads from there from then on.",
        tags$br(), tags$br(),
        actionButton("migrate_to_local_cache",
                     paste0("Copy outputs to local cache (",
                            "~/.dronebior/cache/...)"),
                     class = "btn-warning btn-sm"))
    }
  })

  observeEvent(input$migrate_to_local_cache, {
    p <- tryCatch(project(), error = function(e) NULL)
    if (is.null(p)) return()
    withProgress(message = "Copying outputs to local cache",
                 detail = "this is a one-time copy",
                 value = 0.1, {
      res <- tryCatch(sync_outputs_to_local_cache(p),
                      error = function(e) {
                        showNotification(paste("Migration failed:", conditionMessage(e)),
                                         type = "error", duration = 10)
                        NULL
                      })
      if (is.null(res)) return()
      incProgress(0.7, detail = "Repointing path inputs")
      # Repoint each input at the cached copy if we have one.
      ck <- res$paths
      if (!is.na(ck["orthomosaic"]))      updateTextInput(session, "orthomosaic",     value = unname(ck["orthomosaic"]))
      pc <- ck[c("point_cloud_copc", "point_cloud_laz")]
      pc <- pc[!is.na(pc)]
      if (length(pc))                     updateTextInput(session, "full_cloud_path", value = unname(pc[1L]))
      incProgress(0.2, detail = "Done")
    })
    showNotification(
      paste0("Copied ", length(res$paths), " files to ", res$cache_dir,
             ". App now reads from local cache."),
      type = "message", duration = 10)
  })

  observeEvent(input$repoint_to_cloud, {
    p <- tryCatch(project(), error = function(e) NULL)
    if (is.null(p)) return()
    paths <- odm_product_paths(p)
    updateTextInput(session, "orthomosaic",     value = unname(p$odm_orthomosaic))
    updateTextInput(session, "full_cloud_path", value = pick_best_point_cloud(p))
    showNotification("Reads switched back to the cloud-synced project folder.",
                     type = "message", duration = 6)
  })

  # Lightweight always-on summary: file existence + size only. Cheap.
  # Shows up the moment the user lands on the GIS Workspace tab.
  # Reads outputs_refresh_token() so the card re-renders automatically
  # after a background job (CSF refinement, etc.) finishes rewriting
  # products on disk - file mtimes do not invalidate reactives, so we
  # explicitly bump the token from the job's onFulfilled callback.
  output$project_status_quick <- renderUI({
    outputs_refresh_token()  # invalidates this render when CSF finishes
    p <- tryCatch(project(), error = function(e) NULL)
    if (is.null(p)) {
      return(tags$div(class = "text-muted", "Project not configured."))
    }
    qc <- tryCatch(quick_outputs_check(p), error = function(e) NULL)
    n_imgs <- tryCatch({
      length(list.files(p$images_dir, pattern = "\\.(jpe?g|tif?f)$",
                        ignore.case = TRUE, recursive = FALSE))
    }, error = function(e) NA_integer_)

    # Detect RGB vs Multispectral ortho without forcing a pixel read.
    ortho_label <- "Orthomosaic"
    paths_for_ortho <- odm_product_paths(p)
    cache_dir       <- DroneBioR:::local_cache_dir(p)
    ortho_path_canonical <- unname(paths_for_ortho[["orthomosaic"]])
    ortho_path_cached    <- file.path(cache_dir, basename(ortho_path_canonical))
    ortho_path_resolved  <- if (file.exists(ortho_path_cached)) ortho_path_cached
                            else ortho_path_canonical
    if (file.exists(ortho_path_resolved)) {
      # Goes through the cached raster_header() so re-renders of this
      # output (e.g. when the user types in project_dir/output_dir)
      # do not re-open the same orthomosaic file. The "Now loading"
      # banner appears on the FIRST open while terra reads the header.
      hdr <- raster_header(ortho_path_resolved,
                           progress_msg = "Detecting orthomosaic type")
      if (!is.null(hdr) && !is.na(hdr$nlyr)) {
        ortho_label <- if (hdr$nlyr >= 5L) "Multispectral Orthomosaic"
                       else "RGB Orthomosaic"
      }
    }

    # Canonical product list (the five rows the boss asked for).
    product_status <- function(label, ok, hint = NULL) {
      tags$div(
        style = "padding:2px 0;",
        tags$span(style = "display:inline-block; width:18px;",
                  if (isTRUE(ok)) "✓" else "—"),
        tags$strong(label),
        if (!isTRUE(ok) && !is.null(hint))
          tags$span(style = "color:#6b7280; margin-left:8px; font-size:0.85em;", hint))
    }

    chm_existing <- isTRUE(qc[["chm"]])
    chm_buildable <- isTRUE(qc[["dsm"]]) && isTRUE(qc[["dtm"]]) && !chm_existing

    rows <- list(
      tags$div(tags$strong("Images: "), if (is.na(n_imgs)) "—" else n_imgs),
      tags$div(tags$strong("Project root: "), tags$code(p$project_dir),
               tags$br(),
               tags$strong("Layout: "),
               tags$code(basename(dirname(p$odm_project_dir)), " / ",
                         basename(p$odm_project_dir))),
      tags$hr(style = "margin:6px 0;"),
      tags$div(tags$strong("Products"),
               style = "margin-bottom:4px;"),
      product_status(ortho_label,
                     isTRUE(qc[["orthomosaic"]]),
                     hint = "missing — needs an ODM run"),
      product_status("DSM", isTRUE(qc[["dsm"]]), "missing"),
      product_status("DTM", isTRUE(qc[["dtm"]]), "missing"),
      product_status("CHM", chm_existing,
                     if (chm_buildable) "buildable from DSM - DTM"
                     else "needs DSM + DTM")
    )
    if (chm_buildable) {
      rows[[length(rows) + 1L]] <- tags$div(
        style = "margin-top:6px;",
        actionButton("build_chm", "Build CHM (DSM - DTM)",
                     class = "btn-outline-primary btn-sm"))
    }
    # Always offer the CSF refinement when we have a point cloud +
    # DSM. ODM's default ground classification is conservative on
    # dense canopy; CSF often dramatically improves the CHM.
    if (isTRUE(qc[["point_cloud"]]) && isTRUE(qc[["dsm"]])) {
      csf_busy   <- isTRUE(csf_running())
      btn_label  <- if (csf_busy) "CSF refinement running..."
                    else "Refine DTM + CHM via CSF (lidR)"
      btn_extra  <- if (csf_busy) list(disabled = "disabled") else list()
      rows[[length(rows) + 1L]] <- tags$div(
        style = "margin-top:6px;",
        do.call(actionButton,
                c(list(inputId = "refine_dtm_csf",
                       label   = btn_label,
                       class   = "btn-outline-success btn-sm"),
                  btn_extra)),
        # While the worker is running, expose a Cancel button so the
        # user can abort if it looks stuck. Cancel kills the worker
        # by resetting future::plan(); the in-flight future rejects
        # but finish_csf swallows the toast because csf_cancelled is
        # TRUE at that point.
        if (csf_busy) tags$div(
          style = "margin-top:4px;",
          actionButton("cancel_csf", "Cancel CSF refinement",
                       class = "btn-outline-danger btn-sm")),
        tags$div(style = "color:#6b7280; font-size:0.8em; margin-top:4px;",
                 if (csf_busy)
                   paste0("Running in a separate R process (future::multisession ",
                          "worker). The main R console you see in RStudio stays ",
                          "idle because the heavy lidR + terra work happens in a ",
                          "child process - check Activity Monitor for an R ",
                          "process with high CPU to confirm. Click Cancel if it ",
                          "looks stuck.")
                 else
                   paste0("Re-classifies the point cloud with Cloth Simulation Filter ",
                          "(handles dense vegetation better than ODM's SMRF default), ",
                          "writes a new DTM and rebuilds the CHM. Runs in a ",
                          "background worker so the rest of the app stays ",
                          "responsive while it takes a few minutes.")))
    }
    tags$div(style = "margin-bottom:8px;", rows)
  })

  # Observer for the Build CHM button -- runs build_chm_raster() with
  # cache awareness and a visible progress indicator, then notifies
  # so the user sees it land in the canonical 5-item list above.
  observeEvent(input$build_chm, {
    p <- tryCatch(project(), error = function(e) NULL)
    if (is.null(p)) return()
    withProgress(message = "Building CHM", value = 0.1,
                 detail = "DSM - DTM, clamping negatives", {
      out <- tryCatch(build_chm_raster(p, force = FALSE, cache_aware = TRUE),
                      error = function(e) {
                        showNotification(paste("Build CHM failed:",
                                                conditionMessage(e)),
                                         type = "error", duration = 10)
                        NULL
                      })
      incProgress(0.8, detail = "Done")
      if (!is.null(out)) {
        showNotification(paste0("CHM written to ", out),
                         type = "message", duration = 8)
      }
    })
  })

  # Observer for the 'Refine DTM + CHM via CSF (lidR)' button.
  #
  # CSF on a real ODM cloud (millions of points) takes a few minutes,
  # so running it on the main R thread freezes the entire app (every
  # other tab, every slider, every leaflet pan). The observer instead
  # ships the work into a background `future_promise` (multisession
  # worker initialised at the top of this file) and returns
  # immediately. The user gets:
  #   * a persistent in-app notification that the job is running,
  #     stays up until the worker resolves or rejects;
  #   * a disabled button so a double-click does not enqueue two CSF
  #     jobs against the same cloud;
  #   * a follow-up notification when the worker finishes (success
  #     reports the new DTM/CHM basenames; failure surfaces the
  #     error message);
  #   * an automatic refresh of the Project status card via
  #     `outputs_refresh_token`, because CSF writes new files on
  #     disk but does not change any reactive input.
  #
  # When `future` / `promises` are not available (e.g. an old env),
  # we fall back to the previous synchronous behaviour so the button
  # still works - just blocking again.
  observeEvent(input$refine_dtm_csf, {
    p <- tryCatch(project(), error = function(e) NULL)
    if (is.null(p)) return()
    if (!requireNamespace("lidR", quietly = TRUE)) {
      showNotification(paste("lidR is not installed. Install it from R:",
                              "install.packages('lidR')"),
                       type = "error", duration = 12)
      return()
    }
    if (isTRUE(csf_running())) {
      showNotification(
        "CSF refinement is already running in the background. Wait for it to finish before starting another.",
        type = "warning", duration = 6
      )
      return()
    }

    finish_csf <- function(res = NULL, err = NULL) {
      cancelled <- isTRUE(csf_cancelled())
      csf_running(FALSE)
      csf_cancelled(FALSE)  # reset for the next attempt
      shiny::removeNotification("csf_progress")
      gis_task_stop(session)
      # When the user clicked Cancel, the worker's eventual rejection
      # is the EXPECTED side-effect of plan() reset - swallow the
      # "CSF refinement failed: ..." toast in that case.
      if (cancelled) return(invisible(NULL))
      if (!is.null(err)) {
        showNotification(
          paste("CSF refinement failed:", conditionMessage(err)),
          type = "error", duration = 12, id = "csf_result"
        )
        return(invisible(NULL))
      }
      if (is.null(res)) return(invisible(NULL))
      outputs_refresh_token(outputs_refresh_token() + 1L)
      showNotification(
        paste0("CSF complete. New DTM: ", basename(res$dtm),
               " | New CHM: ", basename(res$chm),
               ". Reload the GIS Workspace overlays to see them."),
        type = "message", duration = 12, id = "csf_result"
      )
    }

    # Snapshot the project list so the worker has a self-contained
    # value (futures serialise their globals - the reactive `project()`
    # would not survive the trip otherwise).
    p_snapshot <- p

    # Capture the on-disk DroneBioR location from the MAIN session so
    # the worker can find the package no matter how it was loaded
    # here. `find.package("DroneBioR")` returns:
    #   * the installed library path when the user did install.packages
    #     / R CMD INSTALL, OR
    #   * the repo source path when the user did pkgload::load_all().
    # That value travels into the worker via `globals` and the worker
    # falls back to pkgload::load_all(dronebior_pkg_path) when the
    # package is not in its own library.
    dronebior_pkg_path <- tryCatch(
      find.package("DroneBioR"),
      error = function(e) NA_character_
    )

    if (isTRUE(.dronebior_async_available)) {
      csf_running(TRUE)
      # Two-track visibility for the user:
      #   * a corner notification ("Refining DTM via CSF...") that
      #     stays up until finish_csf removes it, and
      #   * the floating top-of-viewport banner ("Now: CSF ground
      #     classification...") whose client-side timer ticks the
      #     elapsed seconds in real time even though the worker is
      #     in a separate R process (so the watchdog heartbeat is
      #     happily ticking in the main session and would otherwise
      #     hide its own generic banner). Without this banner the
      #     user perceives nothing happening - main R is idle, the
      #     worker is doing the heavy lifting in a child process.
      gis_task_start(session,
                     name   = "CSF ground classification (background worker)",
                     detail = basename(p_snapshot$odm_project_dir))
      showNotification(
        "Refining DTM via CSF in a background worker. The UI stays responsive - this notification will go away when it finishes.",
        type     = "message",
        duration = NULL,
        closeButton = FALSE,
        id       = "csf_progress"
      )
      fut <- promises::future_promise({
        # The worker is a fresh R session - it has not loaded
        # DroneBioR or lidR. Try the installed library first; when
        # the user is running from source via pkgload::load_all
        # (no installed copy), bootstrap DroneBioR in the worker
        # by calling pkgload::load_all() against the captured repo
        # path so the same edits visible to the main session are
        # also visible to the worker.
        if (!requireNamespace("DroneBioR", quietly = TRUE)) {
          if (is.na(dronebior_pkg_path) || !nzchar(dronebior_pkg_path) ||
              !dir.exists(dronebior_pkg_path)) {
            stop("DroneBioR is not installed and no source path is available ",
                 "for the worker (try install.packages locally, ",
                 "or run from the package repo with pkgload::load_all()).",
                 call. = FALSE)
          }
          if (!requireNamespace("pkgload", quietly = TRUE)) {
            stop("DroneBioR is not installed in this worker and pkgload is ",
                 "not available to load it from source.", call. = FALSE)
          }
          pkgload::load_all(dronebior_pkg_path, quiet = TRUE,
                            attach = FALSE, helpers = FALSE)
        }
        if (!requireNamespace("lidR", quietly = TRUE)) {
          stop("lidR package not installed in worker. Install with ",
               "install.packages('lidR') from R.", call. = FALSE)
        }
        DroneBioR::improve_dtm_csf(
          p_snapshot,
          resolution       = 0.5,
          class_threshold  = 0.5,
          cloth_resolution = 0.5,
          rigidness        = 1L,
          rebuild_chm      = TRUE
        )
      }, seed = TRUE,
         globals = list(p_snapshot         = p_snapshot,
                        dronebior_pkg_path = dronebior_pkg_path))

      promises::then(
        fut,
        onFulfilled = function(res) finish_csf(res = res),
        onRejected  = function(err) finish_csf(err = err)
      )
      invisible(NULL)
    } else {
      # Synchronous fallback when async is not available.
      withProgress(message = "Refining DTM via CSF", value = 0.05, {
        incProgress(0.10, detail = "Reading point cloud")
        res <- tryCatch(
          improve_dtm_csf(p_snapshot, resolution = 0.5,
                          class_threshold = 0.5,
                          cloth_resolution = 0.5,
                          rigidness = 1L,
                          rebuild_chm = TRUE),
          error = function(e) { finish_csf(err = e); NULL }
        )
        incProgress(0.9, detail = "Done")
        if (!is.null(res)) finish_csf(res = res)
      })
    }
  })

  # Cancel CSF refinement: tears down all multisession workers via
  # future::plan() reset and immediately rebuilds the pool, so the
  # in-flight future is killed and the next CSF click gets a clean
  # worker. The killed worker's promise eventually rejects, but
  # finish_csf checks csf_cancelled() and swallows the toast.
  observeEvent(input$cancel_csf, {
    if (!isTRUE(csf_running())) return()
    csf_cancelled(TRUE)
    csf_running(FALSE)
    shiny::removeNotification("csf_progress")
    gis_task_stop(session)
    showNotification(
      paste0("CSF cancellation requested. The background worker is being ",
             "killed; if it had already written partial outputs, they may ",
             "still be on disk. Click Refine again to start a fresh run."),
      type = "warning", duration = 10, id = "csf_cancel"
    )
    if (isTRUE(.dronebior_async_available)) {
      tryCatch({
        # Force-rebuild the worker pool. This terminates any future
        # in flight in the previous workers.
        future::plan(future::sequential)
        future::plan(future::multisession,
                     workers = max(1L, future::availableCores() - 1L))
      }, error = function(e) {
        showNotification(
          paste("Could not reset background worker pool:", conditionMessage(e)),
          type = "warning", duration = 8
        )
      })
    }
  })

  output$product_table <- renderTable({
    products()
  }, digits = 2)

  output$processing_products <- renderTable({
    products()
  }, digits = 2)

  observeEvent(input$processing_preset, {
    preset <- input$processing_preset
    if (identical(preset, "Fast orthomosaic only")) {
      updateCheckboxInput(session, "fast_orthophoto", value = TRUE)
      updateCheckboxInput(session, "build_dsm", value = FALSE)
      updateCheckboxInput(session, "build_dtm", value = FALSE)
      updateCheckboxInput(session, "pc_las", value = FALSE)
      updateCheckboxInput(session, "pc_copc", value = FALSE)
      updateCheckboxInput(session, "pc_csv", value = FALSE)
      updateCheckboxInput(session, "tiles", value = FALSE)
      updateCheckboxInput(session, "three_d_tiles", value = FALSE)
      updateCheckboxInput(session, "gltf", value = FALSE)
    } else if (identical(preset, "Scientific canopy model (recommended)")) {
      updateCheckboxInput(session, "fast_orthophoto", value = FALSE)
      updateCheckboxInput(session, "build_dsm", value = TRUE)
      updateCheckboxInput(session, "build_dtm", value = TRUE)
      updateCheckboxInput(session, "pc_las", value = TRUE)
      updateCheckboxInput(session, "pc_copc", value = FALSE)
      updateCheckboxInput(session, "pc_csv", value = FALSE)
      updateCheckboxInput(session, "tiles", value = FALSE)
      updateCheckboxInput(session, "three_d_tiles", value = FALSE)
      updateCheckboxInput(session, "gltf", value = FALSE)
    } else if (identical(preset, "Full 3D deliverables")) {
      updateCheckboxInput(session, "fast_orthophoto", value = FALSE)
      updateCheckboxInput(session, "build_dsm", value = TRUE)
      updateCheckboxInput(session, "build_dtm", value = TRUE)
      updateCheckboxInput(session, "pc_las", value = TRUE)
      updateCheckboxInput(session, "pc_copc", value = TRUE)
      updateCheckboxInput(session, "pc_csv", value = FALSE)
      updateCheckboxInput(session, "tiles", value = TRUE)
      updateCheckboxInput(session, "three_d_tiles", value = TRUE)
      updateCheckboxInput(session, "gltf", value = TRUE)
    }
  }, ignoreInit = FALSE)

  output$preset_outputs <- renderTable({
    data.frame(
      output = c("Orthomosaic", "DSM", "DTM", "LAS point cloud", "COPC point cloud", "CSV point cloud", "Web tiles", "3D tiles", "glTF model"),
      status = c(
        "always",
        if (isTRUE(input$build_dsm)) "enabled" else "off",
        if (isTRUE(input$build_dtm)) "enabled" else "off",
        if (isTRUE(input$pc_las)) "enabled" else "off",
        if (isTRUE(input$pc_copc)) "enabled" else "off",
        if (isTRUE(input$pc_csv)) "enabled" else "off",
        if (isTRUE(input$tiles)) "enabled" else "off",
        if (isTRUE(input$three_d_tiles)) "enabled" else "off",
        if (isTRUE(input$gltf)) "enabled" else "off"
      ),
      use = c(
        "main raster used by GIS Workspace and Spectral Analytics",
        "canopy surface used by CHM and tree-height metrics",
        "terrain surface used to convert DSM to CHM",
        "full-resolution ROI metrics in 3D & Tree Metrics",
        "streaming cloud format for large projects and external GIS/LiDAR tools",
        "tabular point export for external analysis",
        "fast web map display products",
        "web 3D streaming and external viewers",
        "portable textured 3D model for external viewers"
      ),
      saved_under = file.path(project()$odm_project_dir, c(
        "odm_orthophoto",
        "odm_dem",
        "odm_dem",
        "odm_georeferencing",
        "odm_georeferencing",
        "odm_georeferencing",
        "odm_orthophoto/tiles",
        "3d_tiles",
        "odm_texturing_25d"
      )),
      check.names = FALSE
    )
  }, digits = 2)

  output$processing_workflow <- renderUI({
    products <- odm_product_paths(project())
    tags$div(
      tags$ol(
        class = "processing-steps",
        tags$li(tags$strong("Choose a preset. "), "The preset sets a scientifically sensible group of ODM outputs."),
        tags$li(tags$strong("Adjust output checkboxes only if needed. "), "DSM + DTM + LAS are required for CHM and full-resolution ROI metrics in the 3D tab."),
        tags$li(tags$strong("Click Build command. "), "This refreshes the Docker command so you can inspect exactly what ODM will run."),
        tags$li(tags$strong("Click Run ODM. "), "ODM writes the selected products to the project output folders below."),
        tags$li(tags$strong("Use the other tabs. "), "GIS Workspace, Spectral Analytics and 3D & Tree Metrics read these generated products directly.")
      ),
      tags$p(tags$strong("ODM project folder:"), class = "mb-1"),
      tags$div(class = "processing-path", project()$odm_project_dir),
      tags$p(tags$strong("Key products:"), class = "mt-3 mb-1"),
      tags$div(class = "processing-path", paste(
        paste("Orthomosaic:", products[["orthomosaic"]]),
        paste("DSM:", products[["dsm"]]),
        paste("DTM:", products[["dtm"]]),
        paste("LAS:", products[["point_cloud_las"]]),
        sep = "\n"
      ))
    )
  })

  mosaic <- reactive({
    with_error_toast("Load orthomosaic", {
      validate(need(file.exists(input$orthomosaic), paste("Orthomosaic not found:", input$orthomosaic)))
      masked <- read_multispectral_orthomosaic(input$orthomosaic, use_alpha = isTRUE(input$spectral_use_alpha))
      unmasked <- read_multispectral_orthomosaic(input$orthomosaic, use_alpha = FALSE)
      masked$raw_bands <- unmasked$bands
      masked
    })
  }) |>
    bindCache(input$orthomosaic, input$spectral_use_alpha) |>
    bindEvent(input$load_mosaic)

  radiometric_scale_info <- reactive({
    req(mosaic())
    inferred <- infer_radiometric_scale(mosaic()$raw_bands)
    mode <- input$radiometric_scale_mode %||% "Auto detect"
    factor <- radiometric_scale_factor(mosaic()$raw_bands, mode)
    label <- if (identical(mode, "Auto detect")) inferred$label else mode
    list(
      label = label,
      inferred_label = inferred$label,
      scale_factor = factor,
      max_value = inferred$max_value,
      p99 = inferred$p99
    )
  })

  base_reflectance <- reactive({
    req(mosaic())
    scale_radiometric_bands(mosaic()$bands, input$radiometric_scale_mode %||% "Auto detect")
  })

  reflectance <- reactive({
    req(base_reflectance())
    calibrated <- apply_panel_calibration(base_reflectance(), panel_coefficients())
    preprocess_reflectance(
      calibrated,
      remove_invalid = isTRUE(input$remove_physical_invalid),
      median_size = as.integer(input$median_filter_size %||% 0),
      clean_mask = isTRUE(input$clean_valid_mask),
      mask_size = 3,
      downsample_factor = as.integer(input$downsample_factor %||% 1),
      downsample_method = input$downsample_method %||% "None"
    )
  })

  indices <- reactive({
    req(reflectance())
    compute_spectral_indices(reflectance())
  })

  all_indices <- reactive({
    req(indices())
    custom <- custom_index_raster()
    if (is.null(custom)) {
      indices()
    } else {
      c(indices(), custom)
    }
  })

  biomass_proxy <- reactive({
    req(all_indices())
    compute_biomass_proxy(all_indices())
  })

  output$index_layer_ui <- renderUI({
    choices <- tryCatch(names(all_indices()), error = function(e) c("NDVI", "NDRE", "EVI", "SAVI", "NDWI", "GNDVI", "CIrededge", "MSAVI2", "VARI"))
    selectInput("index_layer", "Index preview", choices = choices, selected = choices[[1]])
  })

  output$application_index_ui <- renderUI({
    choices <- tryCatch(names(all_indices()), error = function(e) c("NDVI", "NDRE", "EVI", "SAVI", "NDWI", "GNDVI", "CIrededge", "MSAVI2", "VARI"))
    selectInput("application_index", "Application map index", choices = choices, selected = "NDVI")
  })

  gis_stack <- reactive({
    with_error_toast("Load GIS stack", {
      validate(need(file.exists(input$orthomosaic), paste("Orthomosaic not found:", input$orthomosaic)))
      withProgress(message = "Loading GIS stack", value = 0, {
        incProgress(0.1, detail = "Reading orthomosaic header")
        ortho <- read_multispectral_orthomosaic(input$orthomosaic, use_alpha = input$use_alpha)

        # Aggregate bands BEFORE radiometric scaling + index math. Full-res
        # 5 cm/px orthos can be 20k x 20k pixels (437 M pixels per band) --
        # computing VARI / NDVI etc. on that takes minutes even on a fast
        # machine. Aggregating to ~4000 px max dimension gives ~25-50 cm/px
        # for display, which is more than enough for any zoom the leaflet
        # map will reach. Full-res versions are still readable from disk
        # for the Analytics / Exports tabs.
        nc <- terra::ncol(ortho$bands); nr <- terra::nrow(ortho$bands)
        fact <- max(1, floor(max(nc, nr) / 4000))
        if (fact > 1) {
          incProgress(0.2, detail = sprintf("Aggregating %dx%d -> %dx%d (display scale)",
                                            nc, nr,
                                            as.integer(nc / fact),
                                            as.integer(nr / fact)))
          bands_for_display <- terra::aggregate(ortho$bands, fact = fact,
                                                fun = mean, na.rm = TRUE)
        } else {
          bands_for_display <- ortho$bands
        }

        incProgress(0.2, detail = "Scaling to reflectance")
        refl <- if (isTRUE(input$scale_reflectance)) {
          scale_to_reflectance(bands_for_display)
        } else bands_for_display

        incProgress(0.15, detail = "Computing spectral indices")
        idx <- compute_spectral_indices(refl)

        # Biomass proxies. compute_biomass_proxies() returns the legacy
        # Biomass_Spectral surface (mean of NDVI/SAVI/NDRE, or VARI on
        # RGB-only orthos) plus a family of greenness x CHM layers when
        # a CHM is available. The CHM is cheap to read - already cached
        # by chm_raster() - so we always offer the multiplicative proxies
        # when the project has DSM + DTM. Skips silently otherwise.
        incProgress(0.15, detail = "Building biomass proxies")
        proxy_stack <- tryCatch({
          ch <- tryCatch(chm_raster(), error = function(e) NULL)
          compute_biomass_proxies(idx, chm = ch)
        }, error = function(e) NULL)

        # Keep backwards compatibility: the historical Biomass_Index_Proxy
        # layer name is still emitted alongside the new Biomass_Spectral
        # one. Downstream code (legend mapping, time-series, product
        # metadata) keys on the old name in places, and renaming would
        # silently break those callers.
        legacy_proxy <- NULL
        if (all(c("NDVI", "SAVI", "NDRE") %in% names(idx))) {
          legacy_proxy <- compute_biomass_proxy(idx)
        }

        incProgress(0.2, detail = "Stacking final layers")
        out <- c(refl, idx)
        if (!is.null(legacy_proxy)) out <- c(out, legacy_proxy)
        if (!is.null(proxy_stack))  out <- c(out, proxy_stack)
        incProgress(0.1, detail = "Done")
        out
      })
    })
  }) |>
    bindCache(input$orthomosaic, input$use_alpha, input$scale_reflectance) |>
    bindEvent(input$load_gis)

  # Hillshade is derived from the DSM, independently of the gis_stack. We
  # compute it on demand and cache by project_dir so repeated map renders
  # do not redo the terrain math.
  hillshade_raster <- reactive({
    with_error_toast("Compute hillshade", {
      p <- project()
      products <- odm_product_paths(p)
      dsm_default <- unname(products[["dsm"]])
      # Prefer the local-cache copy of the DSM when migration ran;
      # otherwise fall back to the project's canonical (often
      # cloud-synced) path.
      cache_dir  <- DroneBioR:::local_cache_dir(p)
      dsm_cached <- file.path(cache_dir, basename(dsm_default))
      dsm_path   <- if (file.exists(dsm_cached)) dsm_cached else dsm_default
      validate(need(
        file.exists(dsm_path),
        "DSM not available - hillshade needs odm_dem/dsm.tif."
      ))
      withProgress(message = "Computing hillshade", value = 0.05, {
        incProgress(0.1, detail = sprintf("Reading %s", basename(dsm_path)))
        dsm <- terra::rast(dsm_path)[[1]]
        # Aggregate down to ~3000 px max so the slope/aspect/shade
        # math runs in seconds even on a 22k-pixel-wide 5 cm DSM.
        nc <- terra::ncol(dsm); nr <- terra::nrow(dsm)
        fact <- max(1, floor(max(nc, nr) / 3000))
        if (fact > 1) {
          incProgress(0.25, detail = sprintf("Aggregating %dx%d -> %dx%d",
                                              nc, nr,
                                              as.integer(nc / fact),
                                              as.integer(nr / fact)))
          dsm <- terra::aggregate(dsm, fact = fact, fun = mean, na.rm = TRUE)
        }
        incProgress(0.2, detail = "Computing slope")
        slope  <- terra::terrain(dsm, "slope",  unit = "radians", neighbors = 8)
        incProgress(0.2, detail = "Computing aspect")
        aspect <- terra::terrain(dsm, "aspect", unit = "radians", neighbors = 8)
        incProgress(0.15, detail = "Shading")
        h <- terra::shade(slope, aspect, angle = 45, direction = 315)
        names(h) <- "Hillshade"
        incProgress(0.1, detail = "Done")
        h
      })
    })
  }) |>
    bindCache(input$project_dir)

  output$gis_map <- renderLeaflet({
    # leafletOptions: minZoom = 2 keeps the user from zooming out far
    # enough to see neighbouring world copies. worldCopyJump stays
    # FALSE because the tile layers also pass noWrap = TRUE - the
    # world stops cleanly at lng = +/-180 with no duplicates. The
    # resulting empty area outside the world is hidden by the
    # `.leaflet-container { background-color: ... }` rule loaded in
    # the head of the page.
    m <- leaflet(options = leafletOptions(
        worldCopyJump = FALSE,
        minZoom = 2
      )) |>
      # Explicit map panes pinning measurement / annotation / ROI
      # layers ABOVE every raster overlay. Without this, leafem's
      # addGeotiff and the orthomosaic raster image both render in
      # leaflet's default overlayPane (zIndex 400), and on some
      # browsers the layer stacking order placed CircleMarkers
      # behind them - which is why the user saw nothing while
      # drawing a ROI even with the markers technically rendered.
      addMapPane("dbMeasurementPane", zIndex = 700) |>
      addMapPane("dbAnnotationPane",  zIndex = 690) |>
      addMapPane("dbRoiPane",         zIndex = 680) |>
      add_esri_imagery_tiles(group = "Satellite") |>
      addProviderTiles(providers$CartoDB.Positron, group = "Light basemap",
                       options = providerTileOptions(
                         noWrap = TRUE,
                         bounds = list(c(-85, -180), c(85, 180))
                       )) |>
      addScaleBar(position = "bottomleft") |>
      setView(lng = 0, lat = 0, zoom = 2)

    m <- add_project_footprint(m, input$orthomosaic)

    # NB: we INTENTIONALLY do NOT include "Measurement", "ROIs" or
    # "Annotations" in overlayGroups. When leaflet's LayersControl is
    # built with a named overlay group that has no members on the map
    # at the time the control is added, the corresponding checkbox
    # starts UNCHECKED - and subsequent CircleMarkers / polygons we
    # add to that empty group via leafletProxy stay invisible until
    # the user manually ticks it. That was the root cause of the
    # "I draw an ROI and no vertex appears" bug. Groups not listed
    # in overlayGroups have no toggle and are simply always visible.
    overlay_groups <- c(
      "Orthomosaic footprint",
      if (isTRUE(input$show_raw_flight)) flight_overlay_groups else character()
    )
    m |>
      addLayersControl(
        baseGroups = c("Satellite", "Light basemap"),
        overlayGroups = overlay_groups,
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  outputOptions(output, "gis_map", suspendWhenHidden = FALSE)

  refresh_gis_flight_layers <- function(fit_to_flight = FALSE) {
    proxy <- leafletProxy("gis_map") |>
      clearGroup("Raw flight path") |>
      clearGroup("Raw image centers") |>
      clearGroup("Flight direction")

    show_raw <- isolate(isTRUE(input$show_raw_flight))
    if (!show_raw) {
      return(invisible(proxy))
    }

    flight <- isolate(flight_plan())
    if (nrow(flight) == 0) {
      return(invisible(proxy))
    }

    proxy <- add_raw_flight_layers(proxy, flight)
    if (isTRUE(fit_to_flight)) {
      proxy <- proxy |>
        fitBounds(
          min(flight$longitude, na.rm = TRUE),
          min(flight$latitude, na.rm = TRUE),
          max(flight$longitude, na.rm = TRUE),
          max(flight$latitude, na.rm = TRUE)
        )
    }
    invisible(proxy)
  }

  session$onFlushed(function() {
    refresh_gis_flight_layers(fit_to_flight = TRUE)
  }, once = TRUE)

  observeEvent(input$show_raw_flight, {
    refresh_gis_flight_layers(fit_to_flight = isTRUE(input$show_raw_flight))
  }, ignoreInit = TRUE)

  # Refresh the flight overlay after the user finishes typing a new
  # project_dir or images_dir. Reads the *debounced* values so the
  # observer waits ~700 ms of quiet before firing - one parse per
  # path change, not one parse per keystroke.
  observeEvent(list(images_dir_debounced(), project_dir_debounced()), {
    refresh_gis_flight_layers(fit_to_flight = isTRUE(input$show_raw_flight))
  }, ignoreInit = TRUE)

  observeEvent(input$recenter_gis_map, {
    fit_leaflet_to_orthomosaic("gis_map", input$orthomosaic)
  }, ignoreInit = TRUE)

  observeEvent(input$clear_gis, {
    proxy <- leafletProxy("gis_map")
    for (group in overlay_choices) {
      proxy <- proxy |> clearGroup(group)
    }
    proxy |>
      clearControls() |>
      addScaleBar(position = "bottomleft") |>
      addLayersControl(
        baseGroups = c("Satellite", "Light basemap"),
        overlayGroups = c(
          "Orthomosaic footprint",
          if (isTRUE(input$show_raw_flight)) flight_overlay_groups else character(),
          "Measurement"
        ),
        options = layersControlOptions(collapsed = FALSE)
      )
    # Wipe leafem imagequery widgets too - clearControls() does not touch
    # them, so without this they pile up indefinitely across loads/clears.
    session$sendCustomMessage("dronebior_clear_imagequery", list())
    gis_loaded(FALSE)
  })

  # Tracks whether the GIS overlays have been loaded at least once in this
  # session. Used by the opacity-slider observer to decide whether it has
  # any layers to re-render.
  gis_loaded <- reactiveVal(FALSE)

  # Core render path. Factored out so we can call it from two places:
  #   - the "Load selected overlays" button (fits the map to the overlays);
  #   - the layer-opacity slider (re-renders existing overlays at the new
  #     opacity, without resetting pan/zoom).
  render_gis_overlays <- function(all_selected, opacity, fit_to_bounds = TRUE,
                                  stretch_mode = "Fixed semantic") {
    validate(need(length(all_selected) > 0, "Select at least one overlay product."))

    # Hillshade + the four canonical raster products (RGB / DSM / DTM /
    # CHM) live outside gis_stack() -- they read from external files.
    # Peel them off first; everything left is a spectral index / band.
    hillshade_selected <- "Hillshade" %in% all_selected
    external_products  <- intersect(c("RGB Orthomosaic", "DSM", "DTM", "CHM"),
                                    all_selected)
    spectral_selected  <- setdiff(all_selected,
                                  c("Hillshade", external_products))

    x <- if (length(spectral_selected) > 0) gis_stack() else NULL
    if (!is.null(x)) {
      spectral_selected <- intersect(spectral_selected, names(x))
    }
    validate(need(
      hillshade_selected || length(external_products) > 0 ||
        length(spectral_selected) > 0,
      "Selected products are not available in the current raster stack."
    ))

    # Helper: resolve a product key to its on-disk path, preferring the
    # local cache (~/.dronebior/cache/<slug>/) when migration ran.
    p_active <- tryCatch(project(), error = function(e) NULL)
    resolve_product_path <- function(key) {
      if (is.null(p_active)) return(NULL)
      paths <- odm_product_paths(p_active)
      default <- unname(paths[[key]])
      if (!nzchar(default)) return(NULL)
      cache_dir <- DroneBioR:::local_cache_dir(p_active)
      cached    <- file.path(cache_dir, basename(default))
      if (file.exists(cached))  return(cached)
      if (file.exists(default)) return(default)
      NULL
    }

    proxy <- leafletProxy("gis_map")
    for (group in overlay_choices) {
      proxy <- proxy |> clearGroup(group)
    }
    proxy <- proxy |> clearControls()

    # leafem imagequery widgets are not standard leaflet controls, so
    # clearControls() leaves them behind. Sweep them out via the custom
    # JS handler before adding the new layers, so we never accumulate
    # stale "Layer X" boxes from previous renders.
    session <- shiny::getDefaultReactiveDomain()
    if (!is.null(session)) {
      session$sendCustomMessage("dronebior_clear_imagequery", list())
    }

    first_layer <- NULL
    legend_items <- list()

    # Render external rasters (RGB Orthomosaic / DSM / DTM / CHM) before
    # the spectral overlays so they form a visual base. We use
    # terra::spatSample(method = "regular") instead of terra::aggregate
    # to subsample big rasters for display -- aggregate reads every
    # pixel + computes means (10+ minutes on a 4-band 22k x 20k ortho)
    # while regular sampling reads roughly target_px pixels and
    # finishes in seconds. The visual result is indistinguishable at
    # the zoom levels leaflet renders, and the original COG on disk
    # is untouched so the Analytics tab can still hit full res.
    if (length(external_products) > 0L) {
      withProgress(message = "Rendering raster products",
                   value = 0, {
        for (layer_name in external_products) {
          tryCatch({
          incProgress(0.05, detail = sprintf("Loading %s", layer_name))
          path_key <- switch(layer_name,
                             `RGB Orthomosaic` = "orthomosaic",
                             DSM = "dsm",
                             DTM = "dtm",
                             CHM = "chm")
          raster_path <- resolve_product_path(path_key)
          if (is.null(raster_path)) next
          r <- tryCatch(terra::rast(raster_path), error = function(e) NULL)
          if (is.null(r)) next
          nc <- terra::ncol(r); nr <- terra::nrow(r)
          total_px <- as.numeric(nc) * as.numeric(nr)
          # RGB Orthomosaic gets a smaller target: leaflet's
          # addRasterImage (used internally by addRasterRGB) caps
          # the rendered raster at 4 MB. A 3-band 8-bit raster at
          # 1100x1100 is 3.6 MB, which fits. DSM / DTM / CHM are
          # single-band so 2.5 M px ~ 2.5 MB and never hits the cap.
          target_px <- if (identical(layer_name, "RGB Orthomosaic")) 1.1e6 else 2.5e6
          if (total_px > target_px) {
            incProgress(0.1, detail = sprintf("Subsampling %s (%d x %d)",
                                              layer_name, nc, nr))
            r <- terra::spatSample(r, size = target_px,
                                   method = "regular",
                                   as.raster = TRUE,
                                   na.rm = FALSE)
          }
          incProgress(0.05, detail = sprintf("Adding %s to map", layer_name))

          if (identical(layer_name, "RGB Orthomosaic")) {
            # Natural-colour composite via leafem::addRasterRGB. ODM
            # writes the orthomosaic as (Red, Green, Blue, Alpha); the
            # alpha band turns the scene-border white pixels into NA
            # so they render transparent against the basemap.
            if (terra::nlyr(r) >= 3L) {
              rgb_stack <- r[[1:3]]
              if (terra::nlyr(r) >= 4L) {
                rgb_stack <- terra::mask(rgb_stack, r[[4L]],
                                         maskvalues = 0, updatevalue = NA)
              }
              proxy <- proxy |>
                leafem::addRasterRGB(
                  rgb_stack,
                  r = 1, g = 2, b = 3,
                  group     = "RGB Orthomosaic",
                  opacity   = opacity,
                  project   = TRUE,
                  quantiles = c(0.02, 0.98))
            }
          } else {
            # DSM / DTM in metres -> viridis; CHM uses BuGn so 0 is
            # white and tall canopy is dark green.
            palette_name <- if (identical(layer_name, "CHM")) "BuGn" else "viridis"
            layer <- r[[1L]]
            raster_vals <- terra::values(layer, mat = FALSE)
            domain <- compute_color_domain(layer_name, raster_vals,
                                           mode = stretch_mode)
            proxy <- tile_raster_on_map(
              proxy, layer,
              group        = layer_name,
              opacity      = opacity,
              palette_name = palette_name,
              vals         = raster_vals,
              domain       = domain
            )
            if (length(domain) == 2 && all(is.finite(domain))) {
              legend_items[[layer_name]] <- list(
                name   = layer_name,
                min    = domain[1],
                max    = domain[2],
                colors = hcl.colors(9, palette_name)
              )
            }
          }
          if (is.null(first_layer)) first_layer <- r
          }, error = function(e) {
            showNotification(
              sprintf("Failed to render '%s': %s", layer_name,
                      conditionMessage(e)),
              type = "warning", duration = 8)
          })
        }  # end for(layer_name in external_products)
      })   # end withProgress
    }      # end if (length(external_products) > 0L)

    # Render hillshade first so it sits beneath color overlays. We use a
    # neutral grayscale ramp and slightly reduced opacity so colored
    # indices on top remain readable.
    if (hillshade_selected) {
      h <- hillshade_raster()
      if (!is.null(h)) {
        proxy <- tile_raster_on_map(
          proxy, h,
          group        = "Hillshade",
          opacity      = min(0.75, 0.85 * opacity),
          palette_name = "Grays"
        )
        if (is.null(first_layer)) first_layer <- h
      }
    }

    # Diverging Red-Yellow-Green palette for any layer that conceptually
    # spans negative -> 0 -> positive: red at the low end (water/bare/soil
    # for negative NDVI etc.), yellow at zero (transition), dark green at
    # the high end (vigorous vegetation). Raw reflectance bands stay on
    # viridis since they are bounded 0..1 with no semantic midpoint.
    index_palette_layers <- c(
      "NDVI", "NDRE", "EVI", "SAVI", "NDWI",
      "GNDVI", "CIrededge", "MSAVI2", "VARI", "Biomass_Index_Proxy"
    )
    for (layer_name in spectral_selected) {
      raster <- x[[layer_name]]
      palette_name <- if (layer_name %in% index_palette_layers) "RdYlGn" else "viridis"

      # Domain controls both the rendered colour scale AND the legend, so
      # what the user sees on the map matches the gradient bar in the
      # bottom-left legend exactly.
      raster_vals <- terra::values(raster, mat = FALSE)
      domain <- compute_color_domain(layer_name, raster_vals, mode = stretch_mode)

      proxy <- tile_raster_on_map(
        proxy, raster,
        group        = layer_name,
        opacity      = opacity,
        palette_name = palette_name,
        vals         = raster_vals,
        domain       = domain
      )

      if (length(domain) == 2 && all(is.finite(domain))) {
        legend_items[[layer_name]] <- list(
          name   = layer_name,
          min    = domain[1],
          max    = domain[2],
          colors = hcl.colors(9, palette_name)
        )
      }
      if (is.null(first_layer)) {
        first_layer <- raster
      }
    }
    selected_layers <- c(external_products, spectral_selected)
    if (hillshade_selected) selected_layers <- c(selected_layers, "Hillshade")

    proxy <- proxy |>
      addScaleBar(position = "bottomleft") |>
      # Legend on bottomleft (above the scale bar) - keeps it clear of the
      # layers control which lives in the top-right corner.
      addControl(html = HTML(overlay_legend_html(legend_items)), position = "bottomleft")

    if (isTRUE(fit_to_bounds) && !is.null(first_layer)) {
      bounds <- raster_bounds_4326(first_layer)
      proxy <- proxy |>
        fitBounds(bounds[["lng1"]], bounds[["lat1"]], bounds[["lng2"]], bounds[["lat2"]])
    }

    proxy |>
      addLayersControl(
        baseGroups = c("Satellite", "Light basemap"),
        # Drop "Measurement" / "ROIs" / "Annotations" from overlayGroups
        # so they have no togglable checkbox - keeps them always
        # visible (see comment in the original renderLeaflet output
        # for the empty-group-starts-unchecked rationale).
        overlayGroups = c(
          selected_layers,
          "Orthomosaic footprint",
          if (isTRUE(input$show_raw_flight)) flight_overlay_groups else character()
        ),
        options = layersControlOptions(collapsed = FALSE)
      )
  }

  observeEvent(input$load_gis, {
    note_id <- showNotification(
      "Loading overlay products. The basemap remains visible while rasters are processed.",
      type = "message",
      duration = NULL
    )
    on.exit(removeNotification(note_id), add = TRUE)

    render_gis_overlays(
      all_selected   = intersect(selected_overlay_layers(), overlay_choices),
      opacity        = input$map_opacity,
      fit_to_bounds  = TRUE,
      stretch_mode   = input$gis_color_stretch %||% "Fixed semantic"
    )
    gis_loaded(TRUE)
  })

  # When the opacity slider or the stretch mode change after the user has
  # already loaded overlays, re-render at the new settings. We skip
  # fit_to_bounds so the user's pan/zoom is preserved.
  observeEvent(list(input$map_opacity, input$gis_color_stretch), {
    if (!isTRUE(gis_loaded())) return()
    render_gis_overlays(
      all_selected   = intersect(isolate(selected_overlay_layers()), overlay_choices),
      opacity        = input$map_opacity,
      fit_to_bounds  = FALSE,
      stretch_mode   = input$gis_color_stretch %||% "Fixed semantic"
    )
  }, ignoreInit = TRUE)

  observeEvent(input$gis_map_click, {
    click <- input$gis_map_click

    # Annotation mode takes precedence over measurement: a single click in
    # annotation mode pins the current text to that coordinate, regardless
    # of which measurement tool is selected.
    if (isTRUE(input$annotation_mode)) {
      text <- input$annotation_text %||% ""
      if (!nzchar(text)) {
        showNotification("Type annotation text first.", type = "warning", duration = 4)
        return()
      }
      annotations(rbind(
        annotations(),
        data.frame(
          lng        = click$lng,
          lat        = click$lat,
          label      = text,
          created_at = as.character(Sys.time()),
          stringsAsFactors = FALSE
        )
      ))
      return()
    }

    if (identical(input$gis_measure_tool, "Navigate")) {
      return()
    }
    pts <- gis_measure_points()
    pts <- rbind(pts, data.frame(lng = click$lng, lat = click$lat))
    gis_measure_points(pts)
    # Brief confirmation toast so the user sees the click was registered
    # even before the marker appears. Helps diagnose the "I clicked but
    # nothing happened" case (which usually meant the marker rendered
    # under another raster pane) and gives positive feedback when
    # things ARE working.
    showNotification(
      paste0("Vertex ", nrow(pts), " placed at ",
             formatC(click$lat, format = "f", digits = 4), ", ",
             formatC(click$lng, format = "f", digits = 4)),
      type = "default", duration = 2
    )
  }, ignoreInit = TRUE)

  # Render annotations as their own leaflet group so they are independent
  # of the measurement layer and survive overlay reloads.
  observe({
    df <- annotations()
    proxy <- leafletProxy("gis_map") |> clearGroup("Annotations")
    if (nrow(df) == 0) return()
    proxy |>
      addCircleMarkers(
        data         = df,
        lng          = ~lng,
        lat          = ~lat,
        radius       = 6,
        color        = "#0f172a",
        weight       = 1.5,
        fillColor    = "#f472b6",
        fillOpacity  = 0.9,
        label        = ~label,
        popup        = ~paste0(
          "<strong>", htmltools::htmlEscape(label), "</strong><br>",
          "<small>", htmltools::htmlEscape(created_at), "</small>"
        ),
        group        = "Annotations"
      )
  })

  observeEvent(input$save_annotations, {
    with_error_toast("Save annotations", {
      df <- annotations()
      validate(need(nrow(df) > 0, "No annotations to save."))
      out_path <- studio_assets_annotations_path(input$project_dir)
      dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
      sf_obj <- sf::st_as_sf(df, coords = c("lng", "lat"), crs = 4326)
      sf::st_write(sf_obj, out_path, delete_dsn = TRUE, quiet = TRUE)
      showNotification(
        paste("Annotations saved to", out_path),
        type     = "message",
        duration = 6
      )
    })
  })

  # Side-by-side path display: the user always sees where Save will write
  # before clicking, so there is no ambiguity about the destination.
  output$annotations_save_path <- renderText({
    paste0("Save target:\n", studio_assets_annotations_path(input$project_dir))
  })

  output$rois_save_path <- renderText({
    paste0("ROIs persisted to:\n", studio_assets_rois_path(input$project_dir))
  })

  observeEvent(input$load_annotations, {
    with_error_toast("Load annotations", {
      file <- input$load_annotations
      validate(need(!is.null(file), "Select a GeoJSON file."))
      g <- sf::st_read(file$datapath, quiet = TRUE)
      validate(need(nrow(g) > 0, "GeoJSON file is empty."))
      coords <- sf::st_coordinates(sf::st_transform(g, 4326))
      label_col <- if ("label" %in% names(g)) as.character(g$label) else rep("", nrow(g))
      created_col <- if ("created_at" %in% names(g)) as.character(g$created_at) else rep("", nrow(g))
      new_df <- data.frame(
        lng        = coords[, 1],
        lat        = coords[, 2],
        label      = label_col,
        created_at = created_col,
        stringsAsFactors = FALSE
      )
      annotations(rbind(annotations(), new_df))
      showNotification(
        paste("Loaded", nrow(new_df), "annotations."),
        type     = "message",
        duration = 4
      )
    })
  })

  observeEvent(input$clear_annotations, {
    annotations(empty_annotations())
    leafletProxy("gis_map") |> clearGroup("Annotations")
  })

  # Keep the "Saved ROIs" dropdown in sync with the actual collection.
  observe({
    rois <- roi_collection()
    updateSelectInput(
      session,
      "selected_roi_name",
      choices = if (length(rois) == 0) character(0) else names(rois),
      selected = isolate(input$selected_roi_name) %||% NULL
    )
  })

  # Auto-persist the ROI collection to studio_assets/rois.geojson so the
  # set survives a session restart. Writes a feature collection in WGS84
  # with one polygon per ROI plus a `name` attribute.
  observe({
    rois <- roi_collection()
    out_path <- studio_assets_rois_path(input$project_dir)
    if (length(rois) == 0) {
      if (file.exists(out_path)) unlink(out_path)
      return()
    }
    tryCatch({
      dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
      geoms <- lapply(rois, function(r) {
        coords <- cbind(c(r$lng, r$lng[[1]]), c(r$lat, r$lat[[1]]))
        sf::st_polygon(list(coords))
      })
      sf_obj <- sf::st_sf(
        name     = names(rois),
        geometry = sf::st_sfc(geoms, crs = 4326)
      )
      sf::st_write(sf_obj, out_path, delete_dsn = TRUE, quiet = TRUE)
    }, error = function(e) {
      message("ROI auto-save failed: ", conditionMessage(e))
    })
  })

  # Auto-load ROIs from disk on session start (one-shot).
  observe({
    in_path <- studio_assets_rois_path(input$project_dir)
    if (!file.exists(in_path)) return()
    if (length(isolate(roi_collection())) > 0) return()  # do not overwrite in-session work
    tryCatch({
      g <- sf::st_read(in_path, quiet = TRUE)
      g <- sf::st_transform(g, 4326)
      loaded <- list()
      for (i in seq_len(nrow(g))) {
        nm <- if ("name" %in% names(g)) as.character(g$name[[i]]) else paste0("roi_", i)
        coords <- sf::st_coordinates(g$geometry[[i]])
        loaded[[nm]] <- list(name = nm, lng = coords[, 1], lat = coords[, 2])
      }
      roi_collection(loaded)
      showNotification(
        paste("Loaded", length(loaded), "saved ROIs from", in_path),
        type = "message", duration = 5
      )
    }, error = function(e) {
      message("ROI auto-load failed: ", conditionMessage(e))
    })
  })

  observeEvent(input$delete_selected_roi, {
    name <- input$selected_roi_name %||% ""
    if (!nzchar(name)) {
      showNotification("Select an ROI in the dropdown first.", type = "warning", duration = 4)
      return()
    }
    current <- roi_collection()
    if (!name %in% names(current)) return()
    current[[name]] <- NULL
    roi_collection(current)
    showNotification(paste("Deleted ROI:", name), type = "message", duration = 3)
  })

  # Redraw selected ROI: deletes the existing entry and puts the user back
  # into polygon-drawing mode with the same ROI name pre-filled, so the
  # next Save ROI replaces it. The closest thing to vertex editing we can
  # offer without bringing in a JS draw plugin.
  observeEvent(input$redraw_selected_roi, {
    name <- input$selected_roi_name %||% ""
    if (!nzchar(name)) {
      showNotification("Select an ROI in the dropdown first.", type = "warning", duration = 4)
      return()
    }
    current <- roi_collection()
    if (name %in% names(current)) {
      current[[name]] <- NULL
      roi_collection(current)
    }
    updateTextInput(session, "roi_name", value = name)
    updateSelectInput(session, "gis_measure_tool", selected = "Measure area")
    gis_measure_points(data.frame(lng = numeric(), lat = numeric()))
    leafletProxy("gis_map") |> clearGroup("Measurement")
    showNotification(
      paste0("Redraw mode for '", name, "'. Click polygon vertices on the map, then 'Save ROI'."),
      type = "message", duration = 6
    )
  })

  # "Draw new ROI" puts the user in polygon-drawing mode and tells them
  # what to do next. This is the discoverable entry point for the ROI
  # comparison workflow; the existing measurement tools still work for
  # users who want to come at it the long way.
  observeEvent(input$start_drawing_roi, {
    updateSelectInput(session, "gis_measure_tool", selected = "Measure area")
    gis_measure_points(data.frame(lng = numeric(), lat = numeric()))
    leafletProxy("gis_map") |> clearGroup("Measurement")
    showNotification(
      ui = "ROI drawing mode active. Click polygon vertices on the map, then click 'Save ROI'.",
      type = "message", duration = 6
    )
  })

  observeEvent(input$save_roi, {
    pts <- gis_measure_points()
    if (nrow(pts) < 3) {
      showNotification(
        ui = "No polygon drawn yet. Click 'Draw new ROI' and place at least 3 vertices on the map first.",
        type = "warning", duration = 6
      )
      return()
    }
    raw_name <- input$roi_name %||% ""
    name <- if (nzchar(raw_name)) raw_name else paste0("roi_", length(roi_collection()) + 1L)
    existing <- roi_collection()
    existing[[name]] <- list(
      name = name,
      lng  = pts$lng,
      lat  = pts$lat
    )
    roi_collection(existing)
    showNotification(
      paste0("Saved ROI '", name, "' with ", nrow(pts), " vertices."),
      type     = "message",
      duration = 4
    )
  })

  observeEvent(input$clear_rois, {
    roi_collection(list())
    leafletProxy("gis_map") |> clearGroup("ROIs")
  })

  observeEvent(input$compute_roi_comparison, {
    roi_comparison_request(roi_comparison_request() + 1L)
  })

  # Persist ROI shapes on the map so users can see what they have saved.
  observe({
    rois <- roi_collection()
    proxy <- leafletProxy("gis_map") |> clearGroup("ROIs")
    if (length(rois) == 0) return()
    palette <- hcl.colors(max(length(rois), 3), "Dark 3")
    for (i in seq_along(rois)) {
      roi <- rois[[i]]
      poly_df <- data.frame(
        lng = c(roi$lng, roi$lng[[1]]),
        lat = c(roi$lat, roi$lat[[1]])
      )
      proxy <- proxy |>
        addPolygons(
          data        = poly_df,
          lng         = ~lng,
          lat         = ~lat,
          color       = palette[i],
          weight      = 2,
          fillColor   = palette[i],
          fillOpacity = 0.15,
          label       = roi$name,
          group       = "ROIs"
        )
    }
  })

  output$roi_comparison_table <- renderTable({
    # Recompute when the user presses the button. Reading the reactiveVal
    # also forces invalidation when ROIs are added or cleared.
    roi_comparison_request()
    rois <- roi_collection()
    if (length(rois) == 0) {
      return(data.frame(
        roi    = "(no ROIs)",
        action = "Draw a polygon, then 'Save current polygon as ROI'.",
        stringsAsFactors = FALSE
      ))
    }

    gs <- tryCatch(gis_stack(), error = function(e) NULL)
    ch <- tryCatch(chm_raster(), error = function(e) NULL)
    if (is.null(gs) && is.null(ch)) {
      return(data.frame(
        roi    = "(no rasters)",
        action = "Click 'Load selected overlays' to compute the spectral stack first.",
        stringsAsFactors = FALSE
      ))
    }

    rows <- list()
    for (roi in rois) {
      ll <- data.frame(lng = roi$lng, lat = roi$lat)
      row <- list(roi = roi$name, n_vertices = nrow(ll))

      # Spectral means per ROI.
      if (!is.null(gs)) {
        proj_xy <- tryCatch({
          ll_sf  <- sf::st_as_sf(ll, coords = c("lng", "lat"), crs = 4326)
          proj_s <- sf::st_transform(ll_sf, terra::crs(gs))
          sf::st_coordinates(proj_s)
        }, error = function(e) NULL)
        if (!is.null(proj_xy)) {
          closed <- rbind(proj_xy, proj_xy[1, , drop = FALSE])
          vect_poly <- terra::vect(list(closed), type = "polygons", crs = terra::crs(gs))
          stat_layers <- intersect(c("NDVI", "NDRE", "EVI", "SAVI", "Biomass_Index_Proxy"), names(gs))
          if (length(stat_layers) > 0) {
            ext_mean <- terra::extract(gs[[stat_layers]], vect_poly, fun = mean, na.rm = TRUE)
            ext_mean$ID <- NULL
            for (lyr in stat_layers) {
              row[[paste0(lyr, "_mean")]] <- as.numeric(ext_mean[[lyr]])
            }
          }
        }
      }

      # CHM stats per ROI.
      if (!is.null(ch)) {
        proj_xy_chm <- tryCatch({
          ll_sf  <- sf::st_as_sf(ll, coords = c("lng", "lat"), crs = 4326)
          proj_s <- sf::st_transform(ll_sf, terra::crs(ch))
          sf::st_coordinates(proj_s)
        }, error = function(e) NULL)
        if (!is.null(proj_xy_chm)) {
          roi_poly <- data.frame(x = proj_xy_chm[, 1], y = proj_xy_chm[, 2])
          metrics <- compute_chm_roi_metrics(ch, roi_poly)
          row$chm_mean_m   <- metrics$chm_height_mean_m
          row$chm_max_m    <- metrics$chm_height_max_m
          row$chm_volume_m3 <- metrics$chm_surface_volume_m3
        }
      }

      rows[[roi$name]] <- row
    }

    # Pad rows to a consistent set of columns so do.call(rbind, ...) works.
    all_cols <- unique(unlist(lapply(rows, names)))
    rows_padded <- lapply(rows, function(r) {
      missing <- setdiff(all_cols, names(r))
      for (m in missing) r[[m]] <- NA
      r[all_cols]
    })
    do.call(rbind, lapply(rows_padded, as.data.frame, stringsAsFactors = FALSE))
  }, digits = 2)

  observeEvent(input$gis_measure_tool, {
    gis_measure_points(data.frame(lng = numeric(), lat = numeric()))
    leafletProxy("gis_map") |> clearGroup("Measurement")
  }, ignoreInit = TRUE)

  # Toggle a crosshair cursor + on-map badge while the user is in any
  # of the measurement / ROI-drawing tools, so it is OBVIOUS the map
  # is in click-to-place mode. The badge is built inside the gis_map
  # DOM element via a custom message handler; toggling the class on
  # the same element flips the cursor instantly without re-rendering
  # the map.
  observe({
    tool   <- input$gis_measure_tool %||% "Navigate"
    drawing <- !identical(tool, "Navigate")
    label   <- switch(tool,
      "Measure distance"     = "Drawing mode - click to place distance points",
      "Measure area"         = "Drawing mode - click polygon vertices, then 'Save ROI'",
      "Measure volume (CHM)" = "Drawing mode - click polygon vertices, then compute CHM volume",
      ""
    )
    session$sendCustomMessage("dronebior_gis_drawing_mode",
                              list(active = drawing, label = label))
  })

  observeEvent(input$clear_gis_measure, {
    gis_measure_points(data.frame(lng = numeric(), lat = numeric()))
    leafletProxy("gis_map") |> clearGroup("Measurement")
  })

  observe({
    pts <- gis_measure_points()
    proxy <- leafletProxy("gis_map") |> clearGroup("Measurement")
    if (nrow(pts) == 0) {
      return()
    }
    # Pick the colour by tool so the user can tell distance / area /
    # volume measurements apart at a glance.
    accent <- if (identical(input$gis_measure_tool, "Measure volume (CHM)"))
                "#f97316"                       # orange (volume)
              else "#38bdf8"                    # sky blue (distance / area / ROI)
    # Use plain addCircleMarkers with NO custom labelOptions / pane -
    # we mirror exactly what the saved-ROI observer does (which the
    # user has confirmed renders correctly). The previous attempt
    # used labelOptions(permanent = TRUE, ...) and pathOptions(pane =
    # "dbMeasurementPane"); one of those caused leaflet to silently
    # drop the circles. Keep big yellow-on-navy dots so they read on
    # any basemap.
    proxy <- proxy |>
      addCircleMarkers(
        data = pts,
        lng = ~lng,
        lat = ~lat,
        radius = 8,
        color = "#0f172a",
        weight = 2,
        fillColor = "#facc15",
        fillOpacity = 1,
        opacity = 1,
        group = "Measurement"
      )
    # In-progress polyline: as soon as there are >= 2 vertices, draw
    # the line segments connecting them. For Measure area / volume
    # this shows the polygon forming BEFORE the third vertex closes
    # it; for Measure distance this is the actual measurement line.
    if (nrow(pts) >= 2) {
      proxy <- proxy |>
        addPolylines(
          data = pts,
          lng = ~lng,
          lat = ~lat,
          color = accent,
          weight = 3,
          opacity = 0.95,
          dashArray = if (identical(input$gis_measure_tool, "Measure distance")) NULL else "6,4",
          group = "Measurement"
        )
    }
    if (identical(input$gis_measure_tool, "Measure area") && nrow(pts) >= 3) {
      closed <- rbind(pts, pts[1, , drop = FALSE])
      proxy <- proxy |>
        addPolygons(
          data = closed,
          lng = ~lng,
          lat = ~lat,
          color = accent,
          weight = 3,
          fillColor = accent,
          fillOpacity = 0.20,
          group = "Measurement"
        )
    } else if (identical(input$gis_measure_tool, "Measure volume (CHM)") && nrow(pts) >= 3) {
      closed <- rbind(pts, pts[1, , drop = FALSE])
      proxy <- proxy |>
        addPolygons(
          data = closed,
          lng = ~lng,
          lat = ~lat,
          color = accent,
          weight = 3,
          fillColor = accent,
          fillOpacity = 0.22,
          group = "Measurement"
        )
    }
  })

  output$gis_measure_summary <- renderTable({
    pts <- gis_measure_points()
    if (nrow(pts) == 0 || identical(input$gis_measure_tool, "Navigate")) {
      return(data.frame(metric = "tool", value = input$gis_measure_tool, unit = "mode"))
    }
    if (identical(input$gis_measure_tool, "Measure distance")) {
      distance <- 0
      if (nrow(pts) >= 2) {
        distance <- sum(haversine_m(pts$lng[-nrow(pts)], pts$lat[-nrow(pts)], pts$lng[-1], pts$lat[-1]), na.rm = TRUE)
      }
      return(data.frame(
        metric = c("points", "distance"),
        value = c(as.character(nrow(pts)), formatC(distance, format = "f", digits = 2)),
        unit = c("count", "m")
      ))
    }
    area <- if (nrow(pts) >= 3) lonlat_area_m2(pts$lng, pts$lat) else NA_real_
    perimeter <- 0
    if (nrow(pts) >= 2) {
      closed <- rbind(pts, pts[1, , drop = FALSE])
      perimeter <- sum(haversine_m(closed$lng[-nrow(closed)], closed$lat[-nrow(closed)], closed$lng[-1], closed$lat[-1]), na.rm = TRUE)
    }
    if (identical(input$gis_measure_tool, "Measure volume (CHM)")) {
      chm <- tryCatch(chm_raster(), error = function(e) NULL)
      if (is.null(chm)) {
        return(data.frame(
          metric = c("points", "footprint area", "perimeter", "CHM status"),
          value  = c(
            as.character(nrow(pts)),
            formatC(area, format = "f", digits = 2),
            formatC(perimeter, format = "f", digits = 2),
            "CHM not available - generate DSM and DTM first"
          ),
          unit   = c("count", "m2", "m", "info")
        ))
      }
      # Reproject the lng/lat polygon vertices into the CHM CRS so the
      # downstream zonal stats use the same coordinate system as the raster.
      closed <- rbind(pts, pts[1, , drop = FALSE])
      proj_xy <- tryCatch({
        ll_sf <- sf::st_as_sf(closed, coords = c("lng", "lat"), crs = 4326)
        proj_sf <- sf::st_transform(ll_sf, terra::crs(chm))
        sf::st_coordinates(proj_sf)
      }, error = function(e) NULL)
      if (is.null(proj_xy)) {
        return(data.frame(
          metric = "status",
          value  = "Could not project polygon into the CHM CRS.",
          unit   = "info"
        ))
      }
      roi_poly <- data.frame(x = proj_xy[, 1], y = proj_xy[, 2])
      metrics <- compute_chm_roi_metrics(chm, roi_poly)
      return(data.frame(
        metric = c("points", "footprint area (lon/lat)", "CHM area",
                   "CHM mean height", "CHM max height", "CHM volume"),
        value  = c(
          as.character(nrow(pts)),
          formatC(area, format = "f", digits = 2),
          formatC(metrics$chm_area_m2, format = "f", digits = 2),
          formatC(metrics$chm_height_mean_m, format = "f", digits = 2),
          formatC(metrics$chm_height_max_m, format = "f", digits = 2),
          formatC(metrics$chm_surface_volume_m3, format = "f", digits = 2)
        ),
        unit   = c("count", "m2", "m2", "m", "m", "m3")
      ))
    }
    data.frame(
      metric = c("points", "area", "perimeter"),
      value = c(as.character(nrow(pts)), formatC(area, format = "f", digits = 2), formatC(perimeter, format = "f", digits = 2)),
      unit = c("count", "m2", "m")
    )
  }, digits = 2)

  manifest <- reactive({
    validate(need(dir.exists(input$images_dir), paste("Image folder not found:", input$images_dir)))
    list_micasense_images(input$images_dir)
  })

  # Flight overlay (image-centre markers + arrow icons on the GIS map).
  # Reads thousands of per-image EXIF JSONs from ODM's opensfm cache.
  # On large flights this is the slowest user-visible operation in the
  # app, so we are aggressive about avoiding it:
  #   * key on the debounced inputs, so mid-typing keystrokes never
  #     trigger a parse;
  #   * skip entirely when the "Show raw image flight plan" overlay is
  #     off - no point parsing 5000 JSONs to populate a layer that is
  #     not shown;
  #   * wrap in withProgress so the user sees concrete progress rather
  #     than a frozen window;
  #   * bindCache on (images_dir, odm_project_dir) so revisiting the
  #     same project is free after the first parse.
  flight_plan <- reactive({
    images_dir <- images_dir_debounced()
    proj_dir   <- project_dir_debounced()
    pick       <- input$odm_project_pick
    if (!isTRUE(input$show_raw_flight)) {
      return(data.frame())
    }
    if (!nzchar(images_dir) || !dir.exists(images_dir)) {
      return(data.frame())
    }
    if (!nzchar(proj_dir)) {
      return(data.frame())
    }
    # Same precedence as the main project() reactive: user pick
    # first, then first auto-detected ODM project on disk, then
    # MicaSense defaults. Picking the first auto-detected layout
    # matters for Sony / GeoScan aerial runs that write to
    # outputs/odm_aerial_dataset/<project>/.
    df <- isolate(available_odm_projects())
    chosen_row <- NULL
    if (!is.null(pick) && nzchar(pick) && !is.null(df) && nrow(df)) {
      match_idx <- which(paste0(df$dataset_subdir, "/", df$project_name) == pick)
      if (length(match_idx)) chosen_row <- df[match_idx[1L], , drop = FALSE]
    }
    if (is.null(chosen_row) && !is.null(df) && nrow(df) >= 1L) {
      chosen_row <- df[1L, , drop = FALSE]
    }
    p <- if (!is.null(chosen_row)) {
      dronebio_project(
        project_dir        = proj_dir,
        odm_dataset_subdir = chosen_row$dataset_subdir,
        odm_project_name   = chosen_row$project_name
      )
    } else {
      dronebio_project(project_dir = proj_dir)
    }
    # Use the floating GIS task banner (with_gis_task) so the user
    # actually sees what is running - the corner withProgress
    # notification on its own sits in the message queue and never
    # appears until the work completes. We still establish a
    # withProgress context around the call because the inner
    # read_odm_exif_flight_plan() calls setProgress() to publish
    # incremental "X / N EXIF JSONs" detail; without the wrapping
    # withProgress those would emit warnings.
    withProgress(message = "Reading flight metadata",
                 detail  = "preparing...",
                 value   = 0.05, {
      with_gis_task(
        session,
        name   = "Reading flight metadata",
        detail = basename(images_dir),
        read_odm_exif_flight_plan(images_dir, p$odm_project_dir)
      )
    })
  }) |>
    bindCache(images_dir_debounced(),
              project_dir_debounced(),
              input$odm_project_pick %||% "",
              isTRUE(input$show_raw_flight))

  output$odm_command <- renderText({
    input$refresh_command
    isolate({
      args <- build_odm_args(
        dataset_dir = project()$odm_dataset_dir,
        project_name = project()$odm_project_name,
        camera_type = input$camera_type %||% "multispectral",
        orthophoto_resolution_cm = input$resolution,
        fast_orthophoto = input$fast_orthophoto,
        build_dsm = input$build_dsm,
        build_dtm = input$build_dtm,
        pc_las = input$pc_las,
        pc_copc = input$pc_copc,
        pc_csv = input$pc_csv,
        tiles = input$tiles,
        three_d_tiles = input$three_d_tiles,
        gltf = input$gltf
      )
      paste("docker", paste(shQuote(args), collapse = " "))
    })
  })

  # ODM stages, in pipeline order. Used by the live progress card.
  odm_stage_order <- c("dataset", "split", "merge", "opensfm", "openmvs",
                       "odm_filterpoints", "odm_meshing", "mvs_texturing",
                       "odm_georeferencing", "odm_dem", "odm_orthophoto",
                       "odm_report", "odm_postprocess")

  format_duration <- function(seconds) {
    if (!is.finite(seconds) || seconds < 0) return("--")
    h <- floor(seconds / 3600)
    m <- floor((seconds %% 3600) / 60)
    s <- floor(seconds %% 60)
    if (h > 0) sprintf("%dh %02dm %02ds", h, m, s)
    else if (m > 0) sprintf("%dm %02ds", m, s)
    else sprintf("%ds", s)
  }

  # Parse sub-stage progress from the tail of the log. Returns a list
  # with `percent` (0-100 or NA) and a one-line `label` describing what
  # the stage is currently doing. ODM's progress hints are stage-specific:
  #
  # * openmvs        "Fused depth-maps 19 (4.28%, 16s, ETA 6m)"
  # * mvs_texturing  "Loading odm_textured_model_geo_material0030_map_Kd.png"
  #                  + the final "Writing..." line
  # * opensfm        "Extracting ... features for image X" + matching
  # * odm_dem        "Step X of Y"
  # * odm_orthophoto "Building tile X/Y"
  parse_odm_sub_progress <- function(lines, active_stage) {
    if (is.na(active_stage) || !length(lines)) return(list(percent = NA_real_, label = NA_character_))
    tail_lines <- tail(lines, 400L)

    if (identical(active_stage, "openmvs")) {
      hits <- grep("Fused depth-maps", tail_lines, value = TRUE)
      if (length(hits)) {
        last <- tail(hits, 1L)
        m <- regmatches(last, regexec("Fused depth-maps ([0-9]+)[^(]*\\(([0-9.]+)%", last))[[1L]]
        if (length(m) >= 3L) {
          return(list(percent = as.numeric(m[3L]),
                      label = sprintf("Fused depth-maps %s (%s%%)", m[2L], m[3L])))
        }
      }
    } else if (identical(active_stage, "mvs_texturing")) {
      mat_lines <- grep("material[0-9]+_map_Kd", tail_lines, value = TRUE)
      if (length(mat_lines)) {
        nums <- as.integer(sub(".*material([0-9]+)_map_Kd.*", "\\1", mat_lines))
        max_n <- suppressWarnings(max(nums, na.rm = TRUE))
        if (is.finite(max_n)) {
          return(list(percent = NA_real_,
                      label = sprintf("Loading texture atlas #%d", max_n + 1L)))
        }
      }
      if (any(grepl("Writing", tail_lines))) {
        return(list(percent = 95, label = "Writing final textured model"))
      }
    } else if (identical(active_stage, "opensfm")) {
      n_extract <- sum(grepl("Extracting .* features for image", tail_lines))
      n_match   <- sum(grepl("^\\d{4}-\\d{2}-\\d{2}.*Matching .* and .*Matches:", tail_lines))
      if (n_match > 0) {
        return(list(percent = NA_real_,
                    label = sprintf("Feature matching (~%d pairs in tail)", n_match)))
      }
      if (n_extract > 0) {
        return(list(percent = NA_real_,
                    label = sprintf("Feature extraction (~%d images in tail)", n_extract)))
      }
    } else if (identical(active_stage, "odm_dem")) {
      hits <- grep("Step ([0-9]+) of ([0-9]+)", tail_lines, value = TRUE)
      if (length(hits)) {
        last <- tail(hits, 1L)
        m <- regmatches(last, regexec("Step ([0-9]+) of ([0-9]+)", last))[[1L]]
        if (length(m) >= 3L) {
          p <- 100 * as.numeric(m[2L]) / max(1, as.numeric(m[3L]))
          return(list(percent = p, label = sprintf("Step %s of %s", m[2L], m[3L])))
        }
      }
    } else if (identical(active_stage, "odm_orthophoto")) {
      hits <- grep("tile ([0-9]+)/([0-9]+)", tail_lines, value = TRUE)
      if (length(hits)) {
        last <- tail(hits, 1L)
        m <- regmatches(last, regexec("tile ([0-9]+)/([0-9]+)", last))[[1L]]
        if (length(m) >= 3L) {
          p <- 100 * as.numeric(m[2L]) / max(1, as.numeric(m[3L]))
          return(list(percent = p, label = sprintf("Tile %s / %s", m[2L], m[3L])))
        }
      }
    }
    list(percent = NA_real_, label = NA_character_)
  }

  progress_bar <- function(percent, label = NULL,
                           color = "#2563eb", height = "8px") {
    pct <- if (is.na(percent)) 0 else max(0, min(100, percent))
    tags$div(
      style = "margin: 4px 0;",
      tags$div(
        style = sprintf("background:#e5e7eb; border-radius:4px; height:%s; overflow:hidden;", height),
        tags$div(style = sprintf(
          "background:%s; width:%.1f%%; height:100%%; transition:width 0.5s;",
          color, pct))
      ),
      if (!is.null(label))
        tags$div(style = "font-size:0.75em; color:#6b7280; margin-top:2px;", label)
    )
  }

  parse_odm_init_time <- function(lines) {
    # Match `Initializing ODM 3.6.0 - Tue May 12 00:30:51  2026` (ODM uses
    # two spaces before the year because the day is %e not %d).
    hit <- grep("Initializing ODM", lines, value = TRUE)
    if (!length(hit)) return(NA_real_)
    m <- regmatches(hit[1L], regexec(" - (.+)$", hit[1L]))[[1L]]
    if (length(m) < 2L) return(NA_real_)
    raw <- gsub("\\s+", " ", trimws(m[2L]))
    # Try common formats and locales.
    fmts <- c("%a %b %d %H:%M:%S %Y", "%a %b %e %H:%M:%S %Y")
    for (f in fmts) {
      t <- suppressWarnings(strptime(raw, f, tz = "UTC"))
      if (!is.na(t)) return(as.numeric(as.POSIXct(t)))
    }
    NA_real_
  }

  # Session-local map of (run_id, stage) -> wall-clock first-seen timings.
  # Stored so we can compute per-stage durations and persist them to history
  # when the user has the Progress card open across stage transitions.
  stage_timing <- reactiveVal(list(run_id = NA_character_, image_count = NA_integer_,
                                   stages = list()))

  output$odm_progress_ui <- renderUI({
    invalidateLater(3000, session)
    path <- input$odm_log_path
    if (!is.character(path) || !nzchar(path) || !file.exists(path)) {
      return(tags$div(class = "text-muted",
                      "No ODM log found yet. Click Run ODM, or paste the path of a log written by docker."))
    }
    lines <- tryCatch(readLines(path, warn = FALSE), error = function(e) character())
    if (!length(lines)) {
      return(tags$div(class = "text-muted", "Log file is empty — ODM may still be initializing."))
    }
    running_stages  <- regmatches(lines, regexpr("Running [a-z_]+ stage", lines))
    finished_stages <- regmatches(lines, regexpr("Finished [a-z_]+ stage", lines))
    running_names   <- sub("Running ([a-z_]+) stage", "\\1", running_stages)
    finished_names  <- sub("Finished ([a-z_]+) stage", "\\1", finished_stages)

    started_but_not_finished <- setdiff(running_names, finished_names)
    active_stage <- if (length(started_but_not_finished))
      started_but_not_finished[length(started_but_not_finished)] else NA_character_
    done_flag <- any(grepl("MMMMMMMMMM", lines))
    err_lines <- grep("\\[ERROR\\]|Traceback|out of memory|Killed", lines, value = TRUE)

    # Outputs-on-disk completion check: a run that crashed in
    # odm_report (numpy/gdal bug) is still effectively complete if the
    # critical products exist on disk. We use the lightweight
    # existence + size check here so the every-3-seconds refresh of
    # the Progress card never touches the raster headers (which on
    # OneDrive Files-On-Demand can force minute-long downloads).
    p_for_status <- tryCatch(project(), error = function(e) NULL)
    outputs_complete <- FALSE
    if (!is.null(p_for_status)) {
      qc_status <- tryCatch(quick_outputs_check(p_for_status),
                            error = function(e) NULL)
      if (!is.null(qc_status) && isTRUE(qc_status[["outputs_complete"]])) {
        outputs_complete <- TRUE
      }
    }
    effectively_done <- done_flag || outputs_complete

    init_time <- parse_odm_init_time(lines)
    now_time  <- as.numeric(Sys.time())
    total_elapsed <- if (is.finite(init_time)) now_time - init_time else NA_real_

    # Parse image count from "Loading N images" / "Found N usable images".
    image_count <- NA_integer_
    img_hit <- grep("Loading [0-9]+ images|Found [0-9]+ usable images", lines, value = TRUE)
    if (length(img_hit)) {
      m <- regmatches(img_hit[1L], regexpr("[0-9]+", img_hit[1L]))
      if (length(m)) image_count <- as.integer(m)
    }

    # Update session state: reset on new run, record first-seen timings.
    # On run discovery, mark stages already past Running as `pre_observed` —
    # we missed their true start, so their measured duration is unreliable
    # and gets excluded from the persistent history.
    cur <- stage_timing()
    run_id <- if (!is.na(init_time)) format(as.POSIXct(init_time, origin = "1970-01-01", tz = "UTC")) else NA_character_
    if (!identical(cur$run_id, run_id)) {
      cur <- list(run_id       = run_id,
                  image_count  = image_count,
                  pre_observed = union(running_names, finished_names),
                  stages       = list())
    } else if (is.na(cur$image_count) && !is.na(image_count)) {
      cur$image_count <- image_count
    }
    for (stg in running_names) {
      if (is.null(cur$stages[[stg]])) cur$stages[[stg]] <- list()
      if (is.null(cur$stages[[stg]]$running_at)) {
        cur$stages[[stg]]$running_at <- now_time
      }
    }
    for (stg in finished_names) {
      if (is.null(cur$stages[[stg]])) cur$stages[[stg]] <- list()
      if (is.null(cur$stages[[stg]]$finished_at)) {
        cur$stages[[stg]]$finished_at <- now_time
        # Persist to history only when we observed the WHOLE run live
        # (stage wasn't already in flight when the card opened).
        if (!is.null(cur$stages[[stg]]$running_at) &&
            !isTRUE(cur$stages[[stg]]$history_written) &&
            !(stg %in% (cur$pre_observed %||% character()))) {
          dur <- cur$stages[[stg]]$finished_at - cur$stages[[stg]]$running_at
          if (is.finite(dur) && dur > 2) {
            DroneBioR:::record_odm_stage_completion(
              run_started_at  = cur$run_id %||% "unknown",
              image_count     = cur$image_count %||% NA_integer_,
              stage           = stg,
              duration_seconds = dur
            )
          }
          cur$stages[[stg]]$history_written <- TRUE
        }
      }
    }
    stage_timing(cur)

    # ETA: active-stage remaining + sum of pending estimates.
    pending_stages <- setdiff(odm_stage_order,
                              union(finished_names, na.omit(active_stage)))
    active_elapsed <- if (!is.na(active_stage) &&
                          !is.null(cur$stages[[active_stage]]$running_at)) {
      max(0, now_time - cur$stages[[active_stage]]$running_at)
    } else 0
    eta_seconds <- if (done_flag) 0 else {
      DroneBioR:::estimate_remaining_seconds(
        active_stage           = if (is.na(active_stage)) NULL else active_stage,
        pending_stages         = pending_stages,
        active_elapsed_seconds = active_elapsed,
        image_count            = cur$image_count
      )
    }

    rows <- lapply(odm_stage_order, function(stg) {
      icon <- if (stg %in% finished_names) "✅"
              else if (identical(stg, active_stage)) "\U0001F501"
              else "⬜"
      label <- if (identical(stg, active_stage)) tags$strong(stg) else stg

      # Time column: duration for done, running-for/est for active, est for pending.
      time_text <- if (stg %in% finished_names &&
                       !is.null(cur$stages[[stg]]$running_at) &&
                       !is.null(cur$stages[[stg]]$finished_at)) {
        dur <- cur$stages[[stg]]$finished_at - cur$stages[[stg]]$running_at
        if (is.finite(dur) && dur > 0) format_duration(dur) else "—"
      } else if (identical(stg, active_stage)) {
        est <- DroneBioR:::estimate_odm_stage_seconds(stg, cur$image_count)
        paste0(format_duration(active_elapsed), " / est ", format_duration(est))
      } else if (stg %in% pending_stages) {
        est <- DroneBioR:::estimate_odm_stage_seconds(stg, cur$image_count)
        paste0("est ", format_duration(est))
      } else "—"

      tags$tr(
        tags$td(icon, style = "font-size:1.1em; padding-right:8px;"),
        tags$td(label),
        tags$td(time_text, class = "text-muted small", style = "text-align:right;")
      )
    })

    header_eta <- if (effectively_done) {
      label <- if (done_flag) "✓ Pipeline complete"
               else "✓ Outputs validated (pipeline aborted at a non-critical stage)"
      tags$span(style = "color:#16a34a; margin-left:12px;", label)
    } else if (!is.na(active_stage)) {
      tagList(
        tags$span(style = "margin-left:12px;",
                  "· Active: ", tags$code(active_stage)),
        tags$span(style = "margin-left:12px;",
                  "· ETA: ", tags$strong(format_duration(eta_seconds)))
      )
    } else {
      tags$span(style = "margin-left:12px;", "· No active stage yet")
    }

    # Big success banner when outputs are validated even though the
    # log shows historical errors (e.g. odm_report numpy crash).
    success_banner <- if (outputs_complete && !done_flag) {
      tags$div(
        style = paste("background:#dcfce7; color:#14532d; padding:10px 14px;",
                      "border-radius:6px; margin-bottom:8px; font-size:0.95em;"),
        tags$div(tags$strong("✓ Pipeline outputs validated on disk")),
        tags$div(style = "margin-top:4px;",
                 "ODM crashed in a non-critical stage (odm_report — known ",
                 "numpy/gdal Docker-image bug) but orthomosaic, DSM, DTM, ",
                 "point cloud and textured mesh are intact and georeferenced. ",
                 "Go to ", tags$strong("GIS Workspace"), " to use them. ",
                 "Error details below are historical."))
    } else NULL

    # Overall pipeline progress: how many of the 13 stages are done.
    n_total  <- length(odm_stage_order)
    n_done   <- sum(odm_stage_order %in% finished_names)
    overall_percent <- 100 * n_done / n_total
    overall_label <- sprintf("Pipeline: %d / %d stages complete", n_done, n_total)

    # Sub-stage progress: parses ODM hints like 'Fused depth-maps X (Y%)'
    # or 'Step X of Y'. Falls back to a stage label if no % available.
    sub <- if (done_flag) list(percent = 100, label = "Complete")
           else parse_odm_sub_progress(lines, active_stage)

    header <- tags$div(
      style = "margin-bottom:8px;",
      tags$div(tags$strong("Total elapsed: "), format_duration(total_elapsed),
               header_eta,
               if (!is.na(image_count))
                 tags$span(style = "margin-left:12px; color:#6b7280;",
                          "· ", image_count, " images")),
      progress_bar(overall_percent, overall_label, color = "#16a34a", height = "10px"),
      if (!is.na(active_stage) && !done_flag) {
        if (is.finite(sub$percent)) {
          progress_bar(sub$percent,
                       paste0(active_stage, " — ",
                              ifelse(is.na(sub$label), "", sub$label)),
                       color = "#2563eb")
        } else if (!is.na(sub$label)) {
          tags$div(style = "font-size:0.8em; color:#6b7280; margin-top:4px;",
                   tags$code(active_stage), " — ", sub$label)
        }
      }
    )

    last_info <- tail(grep("\\[INFO\\]|\\[ERROR\\]", lines, value = TRUE), 1L)
    last_info_clean <- gsub("\033\\[[0-9;]*m", "", last_info)
    footer <- tags$div(style = "margin-top:8px; font-size:0.85em; color:#6b7280;",
                       tags$em("Last log line: "),
                       tags$code(substr(last_info_clean, 1L, 160L)),
                       tags$div(style = "margin-top:4px;",
                                tags$em("ETA uses history at ~/.dronebior/odm_stage_history.csv (or hardcoded baseline)")))

    err_block <- if (length(err_lines)) {
      body <- tags$ul(lapply(tail(err_lines, 5L),
                              function(x) tags$li(tags$code(gsub("\033\\[[0-9;]*m", "", x)))))
      if (outputs_complete) {
        # Outputs are good -- demote errors to a collapsed disclosure.
        tags$details(
          tags$summary(
            style = "cursor:pointer; color:#6b7280; font-size:0.85em; margin-top:8px;",
            "Historical log errors (the pipeline outputs are still valid — click to expand)"),
          tags$div(style = "color:#dc2626; font-size:0.85em; margin-top:4px;", body))
      } else {
        tags$div(style = "margin-top:8px; color:#dc2626; font-size:0.85em;",
                 tags$strong("Errors detected:"), body)
      }
    } else NULL

    tagList(
      success_banner,
      header,
      tags$table(class = "table table-sm", style = "margin-bottom:4px;",
                 tags$tbody(rows)),
      footer,
      err_block
    )
  })

  # Auto-detected camera type based on the contents of the images folder.
  # Drives the badge below the camera_type selector and the run-time guard.
  detected_camera <- reactive({
    DroneBioR:::detect_camera_from_folder(input$images_dir %||% "")
  })

  # Auto-correct the Camera type selector on the first detection of a
  # mismatch for a given images_dir. After the auto-correction the path
  # is remembered, so if the user manually switches the dropdown back
  # we never fight them. Pasting a different path that also mismatches
  # triggers another auto-correction.
  auto_corrected_image_paths <- reactiveVal(character(0))
  observe({
    imgs <- input$images_dir %||% ""
    if (!nzchar(imgs) || !dir.exists(imgs)) return()
    if (imgs %in% auto_corrected_image_paths()) return()
    d   <- detected_camera()
    sel <- input$camera_type %||% "multispectral"
    if (!is.na(d) && !identical(d, sel)) {
      updateSelectInput(session, "camera_type", selected = d)
      auto_corrected_image_paths(c(auto_corrected_image_paths(), imgs))
      showNotification(
        paste0("Camera type auto-set to ", sQuote(d),
               " based on the contents of the Source images folder. ",
               "Change the dropdown if this is wrong."),
        type = "message", duration = 8
      )
    } else if (!is.na(d) && identical(d, sel)) {
      # Already matching: still remember the path so a later
      # spurious mismatch (e.g. user toggles the dropdown then
      # toggles back) doesn't force another auto-correction.
      auto_corrected_image_paths(c(auto_corrected_image_paths(), imgs))
    }
  })

  output$camera_detected_note <- renderUI({
    d <- detected_camera()
    sel <- input$camera_type %||% "multispectral"
    if (is.na(d)) {
      tags$div(class = "small text-muted",
               style = "margin-top:-8px; margin-bottom:8px;",
               "Auto-detect: no images found in 'Source images folder' yet.")
    } else if (identical(d, sel)) {
      tags$div(class = "small",
               style = "margin-top:-8px; margin-bottom:8px; color:#16a34a;",
               sprintf("Auto-detect: %s ✓ matches selection", d))
    } else {
      tags$div(class = "small",
               style = "margin-top:-8px; margin-bottom:8px; color:#d97706;",
               sprintf("Auto-detect: %s ⚠ differs from selection (%s)", d, sel))
    }
  })

  # GeoScan sidecar detection — surfaces when run_odm_project() would
  # auto-attach --geo. The badge tells the user the override is in
  # effect before they hit Run.
  geoscan_detected <- reactive({
    DroneBioR::detect_geoscan_metadata(input$images_dir %||% "")
  })

  output$geoscan_detected_note <- renderUI({
    meta <- geoscan_detected()
    if (is.null(meta)) return(NULL)
    n_cams <- tryCatch(
      nrow(DroneBioR::read_geoscan_cameras(meta$cameras_path)),
      error = function(e) NA_integer_
    )
    n_label <- if (is.na(n_cams)) "?" else as.character(n_cams)
    tags$div(class = "small",
             style = "margin-top:-4px; margin-bottom:8px; color:#2563eb;",
             sprintf("GeoScan metadata detected: %s camera positions in %s",
                     n_label, basename(meta$metadata_dir)),
             tags$br(),
             tags$em("Will auto-attach --geo and --matcher-neighbors 8 on Run."))
  })

  # ------------------------------------------------------------------ #
  # Auto-load ODM outputs on pipeline completion + active-run recovery #
  # ------------------------------------------------------------------ #

  # Set of run_ids we've already auto-filled, so the side effect fires
  # exactly once per run.
  autoloaded_runs <- reactiveVal(character(0))

  # On server startup: if a recent run record exists, repoint the
  # Progress card at it so the user keeps visibility across reloads.
  observe({
    rec <- DroneBioR:::read_active_run_record()
    if (!is.null(rec) && !is.null(rec$log_path) && nzchar(rec$log_path) &&
        file.exists(rec$log_path)) {
      updateTextInput(session, "odm_log_path", value = rec$log_path)
      showNotification(
        paste0("Recovered active ODM run from ~/.dronebior/active_runs.json. ",
               "Log: ", rec$log_path),
        type = "message", duration = 8
      )
    }
  }, priority = 1000)

  # Helper that pre-fills the path inputs across tabs to point at the
  # canonical ODM outputs of the current project.
  autoload_odm_outputs <- function() {
    p <- tryCatch(project(), error = function(e) NULL)
    if (is.null(p)) return(invisible(NULL))
    paths <- odm_product_paths(p)
    filled <- character(0)

    if (file.exists(paths[["orthomosaic"]])) {
      updateTextInput(session, "orthomosaic", value = unname(paths[["orthomosaic"]]))
      filled <- c(filled, "orthomosaic")
    }
    pc <- pick_best_point_cloud(p)
    if (file.exists(pc)) {
      updateTextInput(session, "full_cloud_path", value = pc)
      filled <- c(filled, paste0("point cloud (", basename(pc), ")"))
    }
    if (file.exists(paths[["point_cloud_ply"]])) {
      updateTextInput(session, "ply_path", value = unname(paths[["point_cloud_ply"]]))
      filled <- c(filled, "PLY preview")
    }
    if (file.exists(pick_best_textured_obj(p))) {
      updateCheckboxInput(session, "show_textured_mesh", value = TRUE)
      filled <- c(filled, "textured mesh enabled")
    }
    if (file.exists(paths[["dsm"]])) {
      updateCheckboxInput(session, "show_draped_dsm", value = TRUE)
      filled <- c(filled, "draped DSM enabled")
    }
    filled
  }

  # Auto-load fires on EITHER signal:
  #   (a) Log banner `MMMMMMMMMM` (the canonical ODM-complete marker)
  #   (b) Valid orthomosaic + DSM already on disk for the current project
  #       (recovers from runs that crashed in odm_report -- numpy/gdal bug
  #       in the latest opendronemap/odm Docker image -- without losing
  #       the actually-good upstream outputs).
  # Each unique run_id fires the side-effects exactly once per session.
  observe({
    invalidateLater(3000, session)
    p <- tryCatch(project(), error = function(e) NULL)
    if (is.null(p)) return()

    log_done   <- FALSE
    log_run_id <- NA_character_
    path <- input$odm_log_path
    if (is.character(path) && nzchar(path) && file.exists(path)) {
      lines <- tryCatch(readLines(path, warn = FALSE), error = function(e) character())
      if (length(lines)) {
        log_done <- any(grepl("MMMMMMMMMM", lines))
        init_time <- parse_odm_init_time(lines)
        if (!is.na(init_time)) {
          log_run_id <- format(as.POSIXct(init_time, origin = "1970-01-01", tz = "UTC"))
        }
      }
    }

    outputs_done <- FALSE
    outputs_run_id <- NA_character_
    # Use the lightweight existence + size check here -- the full
    # validate_odm_outputs() opens rasters via terra::rast(), which on
    # OneDrive Files-On-Demand folders can trigger minute-long
    # downloads of header/COG-overview blocks. We don't need raster
    # geometry to decide autoload should fire; existence + non-zero
    # size is enough.
    qc <- tryCatch(quick_outputs_check(p), error = function(e) NULL)
    if (!is.null(qc) && isTRUE(qc[["outputs_complete"]])) {
      outputs_done <- TRUE
      ortho_path <- odm_product_paths(p)[["orthomosaic"]]
      outputs_run_id <- paste0("ortho-mtime-",
                               format(file.info(ortho_path)$mtime,
                                      "%Y-%m-%dT%H:%M:%S"))
    }

    if (!(log_done || outputs_done)) return()
    chosen_run_id <- if (log_done && !is.na(log_run_id)) log_run_id else outputs_run_id
    if (is.na(chosen_run_id)) return()
    if (chosen_run_id %in% autoloaded_runs()) return()

    filled <- autoload_odm_outputs()
    autoloaded_runs(c(autoloaded_runs(), chosen_run_id))
    DroneBioR:::clear_active_run_record()
    reason <- if (log_done) "MMMMMMMM banner" else "valid outputs on disk (ortho + DSM)"
    showNotification(
      paste0("ODM outputs detected (", reason, "). Auto-filled: ",
             paste(filled, collapse = ", ")),
      type = "message", duration = 15
    )
  })

  # Path reuse: when the project root or source images folder changes,
  # update every derived path input across all tabs to the canonical
  # ODM paths of the new project. Users can still override; switching
  # projects intentionally resets the overrides. We also do this whether
  # the files exist yet or not, so the user can see what the next ODM
  # run will produce.
  observeEvent(list(input$project_dir, input$images_dir, input$output_dir,
                    input$odm_project_pick), {
    p <- tryCatch(project(), error = function(e) NULL)
    if (is.null(p)) return()
    # Prefer local-cache copies of heavy products when they exist, so a
    # previously migrated project keeps reading from the fast local disk
    # instead of OneDrive after the user edits any path input.
    cache_dir <- DroneBioR:::local_cache_dir(p)
    use_cache <- function(filename) {
      candidate <- file.path(cache_dir, filename)
      if (file.exists(candidate)) candidate else NULL
    }
    ortho_default <- unname(p$odm_orthomosaic)
    ortho_cached  <- use_cache(basename(ortho_default))
    updateTextInput(session, "orthomosaic",
                    value = if (!is.null(ortho_cached)) ortho_cached else ortho_default)

    pc_default <- pick_best_point_cloud(p)
    pc_cached  <- use_cache(basename(pc_default))
    updateTextInput(session, "full_cloud_path",
                    value = if (!is.null(pc_cached)) pc_cached else pc_default)

    paths <- odm_product_paths(p)
    ply_default <- unname(paths[["point_cloud_ply"]])
    ply_cached  <- use_cache(basename(ply_default))
    updateTextInput(session, "ply_path",
                    value = if (!is.null(ply_cached)) ply_cached else ply_default)
  })

  output$engine_note <- renderText({
    if (isTRUE(input$fast_orthophoto) && (isTRUE(input$three_d_tiles) || isTRUE(input$gltf))) {
      "Fast orthophoto prioritizes rapid orthomosaic generation and ODM can skip full textured 3D outputs. Disable fast orthophoto for commercial-style 3D deliverables."
    } else if (isTRUE(input$build_dsm) && isTRUE(input$build_dtm)) {
      "DSM and DTM are enabled. This supports canopy height modeling and later tree segmentation."
    } else {
      "Enable DSM and DTM if you want scientifically defensible tree height, crown and volume estimates."
    }
  })

  # Factor out the actual launch so we can call it from the modal
  # confirmation paths as well as the direct Run button.
  launch_odm_run <- function(cam) {
    engine <- input$processing_engine %||% "odm_docker"
    common_args <- list(
      project()                            ,
      camera_type             = cam        ,
      orthophoto_resolution_cm = input$resolution,
      fast_orthophoto         = input$fast_orthophoto,
      build_dsm               = input$build_dsm,
      build_dtm               = input$build_dtm,
      pc_las                  = input$pc_las,
      pc_copc                 = input$pc_copc,
      pc_csv                  = input$pc_csv,
      tiles                   = input$tiles,
      three_d_tiles           = input$three_d_tiles,
      gltf                    = input$gltf
    )

    with_error_toast("Run processing engine", {
      if (identical(engine, "webodm")) {
        validate(need(nzchar(input$webodm_url  %||% ""), "WebODM URL is required."))
        validate(need(nzchar(input$webodm_user %||% ""), "WebODM username is required."))
        validate(need(nzchar(input$webodm_pass %||% ""), "WebODM password is required."))
        showNotification(
          paste0("Submitting to WebODM at ", input$webodm_url,
                 ". This can take many hours; status updates appear in the R console."),
          type = "message", duration = 10
        )
        do.call(run_webodm_project, c(
          common_args,
          list(
            base_url     = input$webodm_url,
            username     = input$webodm_user,
            password     = input$webodm_pass,
            poll_seconds = input$webodm_poll_seconds %||% 60
          )
        ))
        showNotification("WebODM task completed; outputs downloaded.",
                         type = "message", duration = 8)
      } else {
        # Non-blocking dispatch: stage images, build docker args, then
        # system2(wait = FALSE) so the Shiny session stays responsive
        # and the ODM run progress card can refresh live.
        if (!nzchar(Sys.which("docker"))) {
          stop("Docker not found in PATH. Install/start Docker Desktop.", call. = FALSE)
        }
        p <- project()
        manifest <- switch(cam,
          multispectral = list_micasense_images(p$images_dir),
          rgb           = list_aerial_images(p$images_dir))
        copy_images_for_odm(manifest, p$odm_images_dir)

        # Mirror run_odm_project()'s GeoScan auto-detection: if the user's
        # source images folder has a Metadata/Cameras_WGS84.txt sibling,
        # generate <project>/geo.txt and append --geo + --matcher-neighbors.
        auto_extra <- character()
        if (identical(cam, "rgb")) {
          geo_meta <- DroneBioR::detect_geoscan_metadata(p$images_dir)
          if (!is.null(geo_meta)) {
            dir.create(p$odm_project_dir, recursive = TRUE, showWarnings = FALSE)
            DroneBioR::convert_geoscan_to_odm_geo(
              cameras_path = geo_meta$cameras_path,
              geo_txt_path = file.path(p$odm_project_dir, "geo.txt"),
              gnss_offset  = geo_meta$gnss_offset_path
            )
            auto_extra <- c(
              "--geo",
              paste0("/datasets/", p$odm_project_name, "/geo.txt"),
              "--matcher-neighbors", "8"
            )
            showNotification(
              paste0("GeoScan metadata detected. Auto-attached --geo + --matcher-neighbors 8."),
              type = "message", duration = 8
            )
          }
        }

        args <- build_odm_args(
          dataset_dir              = p$odm_dataset_dir,
          project_name             = p$odm_project_name,
          camera_type              = cam,
          orthophoto_resolution_cm = input$resolution,
          fast_orthophoto          = input$fast_orthophoto,
          build_dsm                = input$build_dsm,
          build_dtm                = input$build_dtm,
          pc_las                   = input$pc_las,
          pc_copc                  = input$pc_copc,
          pc_csv                   = input$pc_csv,
          tiles                    = input$tiles,
          three_d_tiles            = input$three_d_tiles,
          gltf                     = input$gltf,
          extra_args               = auto_extra
        )

        log_path <- file.path(p$odm_dataset_dir, "odm_run.log")
        dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
        writeLines(character(), log_path)
        updateTextInput(session, "odm_log_path", value = log_path)

        # wait = FALSE means system2 returns immediately; ODM keeps running.
        system2("docker", args = args, stdout = log_path, stderr = log_path, wait = FALSE)

        # Count source images so the Progress card / ETA know the scale,
        # and persist an active-run record so a browser refresh recovers.
        image_count <- tryCatch(nrow(manifest), error = function(e) NA_integer_)
        DroneBioR:::write_active_run_record(
          run_id      = paste0("pending-", format(Sys.time(), "%Y%m%dT%H%M%S")),
          log_path    = log_path,
          project_dir = p$project_dir,
          image_count = image_count
        )

        showNotification(
          paste0("ODM started in background. Watch the 'ODM run progress' card. ",
                 "Log: ", log_path),
          type = "message", duration = 12
        )
      }
    })
  }

  # Main Run button: if the user-selected camera type matches the
  # auto-detected one (or detection is ambiguous), launch directly.
  # Otherwise pop a modal so the user can confirm or switch.
  observeEvent(input$run_odm, {
    cam <- input$camera_type %||% "multispectral"
    det <- detected_camera()
    if (!is.na(det) && !identical(det, cam)) {
      showModal(modalDialog(
        title = "Camera type mismatch",
        sprintf(paste0("You selected %s but the source images folder looks like %s. ",
                       "Running with the wrong camera type can add ODM warnings or ",
                       "(for multispectral with no reflectance panel) skip radiometric ",
                       "calibration silently.\n\nWhat do you want to do?"),
                sQuote(cam), sQuote(det)),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("run_odm_confirm_keep", sprintf("Continue with %s", cam),
                       class = "btn-warning"),
          actionButton("run_odm_confirm_switch", sprintf("Switch to %s and run", det),
                       class = "btn-primary")
        ),
        easyClose = TRUE
      ))
    } else {
      launch_odm_run(cam)
    }
  })
  observeEvent(input$run_odm_confirm_keep, {
    removeModal()
    launch_odm_run(input$camera_type %||% "multispectral")
  })
  observeEvent(input$run_odm_confirm_switch, {
    removeModal()
    d <- detected_camera()
    if (!is.na(d)) updateSelectInput(session, "camera_type", selected = d)
    launch_odm_run(if (is.na(d)) (input$camera_type %||% "multispectral") else d)
  })

  output$mosaic_meta <- renderTable({
    req(mosaic())
    x <- mosaic()$bands
    scale <- radiometric_scale_info()
    data.frame(
      metric = c("source", "layers", "columns", "rows", "crs", "detected scale", "applied scale factor", "alpha mask"),
      value = c(
        mosaic()$source,
        terra::nlyr(x),
        terra::ncol(x),
        terra::nrow(x),
        terra::crs(x, describe = TRUE)$name,
        scale$inferred_label,
        scale$scale_factor,
        if (!is.null(mosaic()$alpha) && isTRUE(input$spectral_use_alpha)) "enabled" else "not used"
      )
    )
  }, digits = 2)

  output$radiometric_qa <- renderTable({
    req(mosaic(), base_reflectance())
    qa <- spectral_qa_summary(mosaic()$raw_bands, base_reflectance(), mosaic()$alpha, radiometric_scale_info())
    numeric_cols <- vapply(qa, is.numeric, logical(1))
    qa[numeric_cols] <- lapply(qa[numeric_cols], function(v) ifelse(abs(v) >= 1000, format(round(v), big.mark = ","), formatC(v, format = "f", digits = 2)))
    qa
  }, digits = 2)

  output$band_histogram_plot <- renderPlot({
    req(base_reflectance())
    x <- base_reflectance()
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par), add = TRUE)
    par(mfrow = c(2, 3), mar = c(3.2, 3.2, 2.3, 0.8))
    for (band_name in names(x)) {
      vals <- terra::spatSample(x[[band_name]], size = 50000, method = "regular", na.rm = TRUE, values = TRUE)[, 1]
      hist(vals, breaks = 60, col = "#1f6f5b", border = NA, main = band_name, xlab = "Reflectance")
      abline(v = c(0, 1), col = "#ef4444", lty = 2)
    }
    plot.new()
    legend("center", legend = c("Physical bounds 0-1"), lty = 2, col = "#ef4444", bty = "n")
  })

  output$panel_roi_plot <- renderPlot({
    req(base_reflectance())
    x <- downsample_raster(base_reflectance(), size = 120000)
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par), add = TRUE)
    par(mar = c(0, 0, 2.2, 0))
    display <- stretch_raster_for_display(x[[c("Blue", "Green", "Red")]], input$display_stretch %||% "Percentile 2-98")
    terra::plotRGB(display, r = 3, g = 2, b = 1, axes = FALSE, stretch = "lin")
    title(main = "Draw panel ROI with the mouse", line = 0.6)
  })

  observeEvent(input$apply_panel_calibration, {
    req(base_reflectance())
    brush <- input$panel_roi_brush
    if (is.null(brush)) {
      showNotification("Draw a panel ROI before applying calibration.", type = "warning", duration = 4)
      return()
    }
    roi_ext <- terra::ext(
      min(brush$xmin, brush$xmax),
      max(brush$xmin, brush$xmax),
      min(brush$ymin, brush$ymax),
      max(brush$ymin, brush$ymax)
    )
    roi <- terra::crop(base_reflectance(), roi_ext)
    observed <- terra::global(roi, "mean", na.rm = TRUE)
    observed_mean <- observed$mean
    names(observed_mean) <- rownames(observed)
    roi_cells <- as.numeric(terra::global(!is.na(roi[[1]]), "sum", na.rm = TRUE)[1, 1])
    certified <- c(
      Blue = input$panel_blue,
      Green = input$panel_green,
      Red = input$panel_red,
      RedEdge = input$panel_rededge,
      NIR = input$panel_nir
    )
    coefficients <- data.frame(
      band = names(certified),
      certified_reflectance = as.numeric(certified),
      observed_roi_mean = as.numeric(observed_mean[names(certified)]),
      gain = as.numeric(certified) / as.numeric(observed_mean[names(certified)]),
      offset = 0,
      model = "single-panel gain; offset fixed at 0",
      stringsAsFactors = FALSE
    )
    coefficients$gain[!is.finite(coefficients$gain)] <- 1
    panel_coefficients(coefficients)
    panel_calibration_status(paste(
      "Panel ROI calibration applied.",
      paste0("ROI cells used: ", format(roi_cells, big.mark = ","), "."),
      paste0(
        "ROI extent: xmin=", formatC(roi_ext[1], format = "f", digits = 2),
        ", xmax=", formatC(roi_ext[2], format = "f", digits = 2),
        ", ymin=", formatC(roi_ext[3], format = "f", digits = 2),
        ", ymax=", formatC(roi_ext[4], format = "f", digits = 2), "."
      ),
      "Result table below shows certified reflectance, observed ROI mean, gain and offset for each band.",
      "The scientific reflectance raster is corrected downstream; display stretch remains visualization-only.",
      sep = "\n"
    ))
    showNotification("Panel ROI calibration applied to the scientific reflectance raster.", type = "message", duration = 5)
  })

  observeEvent(input$reset_panel_calibration, {
    panel_coefficients(data.frame(
      band = c("Blue", "Green", "Red", "RedEdge", "NIR"),
      certified_reflectance = NA_real_,
      observed_roi_mean = NA_real_,
      gain = 1,
      offset = 0,
      model = "identity",
      stringsAsFactors = FALSE
    ))
    panel_calibration_status(
      "No panel calibration applied. Draw a ROI on the calibration panel preview, enter certified reflectance values in the sidebar, then click Apply panel ROI calibration."
    )
    showNotification("Panel calibration reset.", type = "message", duration = 4)
  })

  output$panel_calibration_status <- renderText({
    panel_calibration_status()
  })

  output$panel_calibration_table <- renderTable({
    x <- panel_coefficients()
    numeric_cols <- vapply(x, is.numeric, logical(1))
    x[numeric_cols] <- lapply(x[numeric_cols], function(v) formatC(v, format = "f", digits = 2))
    x
  }, digits = 2)

  output$mosaic_plot <- renderPlot({
    req(reflectance())
    x <- downsample_raster(reflectance(), size = 160000)
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par), add = TRUE)
    if (identical(input$preview_mode, "RGB")) {
      par(mar = c(0, 0, 2.2, 0))
      display <- stretch_raster_for_display(x[[c("Blue", "Green", "Red")]], input$display_stretch %||% "Percentile 2-98")
      terra::plotRGB(display, r = 3, g = 2, b = 1, stretch = "lin", axes = FALSE)
      title(main = "RGB display raster", line = 0.6)
    } else {
      par(mar = c(4.2, 4.2, 3, 4.2))
      display <- stretch_layer_for_display(x[[input$preview_mode]], input$display_stretch %||% "Percentile 2-98")
      terra::plot(
        display,
        col = hcl.colors(80, "viridis"),
        main = paste(input$preview_mode, "display stretch"),
        xlab = "Easting (m)",
        ylab = "Northing (m)"
      )
    }
  })

  output$index_summary <- renderTable({
    req(all_indices())
    format_summary_table(
      summarize_spatraster(all_indices(), c("min", "mean", "max", "sd")),
      unit = "unitless index",
      digits = 2
    )
  }, digits = 2)

  observeEvent(input$compute_custom_index, {
    req(reflectance())
    tryCatch({
      custom <- evaluate_custom_index(reflectance(), input$custom_index_formula, input$custom_index_name)
      custom_index_raster(custom)
      showNotification(paste("Custom index computed:", names(custom)), type = "message", duration = 4)
    }, error = function(e) {
      showNotification(paste("Custom index failed:", conditionMessage(e)), type = "error", duration = 6)
    })
  })

  output$index_plot <- renderPlot({
    req(all_indices(), input$index_layer)
    layer <- all_indices()[[input$index_layer]]
    display_layer <- if (isTRUE(input$fixed_index_limits)) layer else stretch_layer_for_display(layer, input$display_stretch %||% "Percentile 2-98")
    zlim <- index_zlim(input$index_layer, isTRUE(input$fixed_index_limits), layer)
    terra::plot(
      display_layer,
      # Diverging Red-Yellow-Green palette: red at low values (negative
      # vegetation indices), yellow near zero, dark green at the high end.
      col = hcl.colors(100, "RdYlGn"),
      main = input$index_layer,
      xlab = "Easting (m)",
      ylab = "Northing (m)",
      range = zlim
    )
  })

  output$index_histogram_plot <- renderPlot({
    req(all_indices(), input$index_layer)
    vals <- terra::spatSample(all_indices()[[input$index_layer]], size = 80000, method = "regular", na.rm = TRUE, values = TRUE)[, 1]
    hist(vals, breaks = 80, col = "#1f6f5b", border = NA, main = paste(input$index_layer, "histogram"), xlab = "Index value")
    if (isTRUE(input$fixed_index_limits) && input$index_layer %in% names(fixed_index_limits)) {
      abline(v = fixed_index_limits[[input$index_layer]], col = "#ef4444", lty = 2)
    }
  })

  application_map <- reactive({
    req(all_indices(), input$application_index)
    thresholds <- c(input$class_water_max, input$class_bare_max, input$class_stress_max, input$class_moderate_max)
    validate(need(all(diff(sort(thresholds)) > 0), "Application thresholds must be distinct."))
    build_application_map(all_indices()[[input$application_index]], thresholds)
  })

  output$application_map_plot <- renderPlot({
    req(application_map())
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par), add = TRUE)
    layout(matrix(c(1, 2), nrow = 2), heights = c(4.3, 0.9))
    par(mar = c(3.4, 4.0, 2.4, 1.0))
    terra::plot(
      application_map(),
      col = application_classes$color,
      breaks = seq(0.5, 5.5, by = 1),
      legend = FALSE,
      axes = TRUE,
      main = paste(input$application_index, "application classes")
    )
    par(mar = c(0, 0, 0, 0))
    plot.new()
    legend(
      "center",
      legend = application_classes$class,
      fill = application_classes$color,
      border = NA,
      bty = "n",
      horiz = FALSE,
      ncol = 3,
      cex = 0.82,
      x.intersp = 0.65,
      y.intersp = 0.9
    )
  })

  output$application_summary <- renderTable({
    req(application_map())
    out <- summarize_application_map(application_map())
    if (nrow(out) == 0) return(data.frame(message = "No classified cells."))
    out$area_m2 <- formatC(out$area_m2, format = "f", digits = 2)
    out$area_ha <- formatC(out$area_ha, format = "f", digits = 2)
    out
  }, digits = 2)

  observeEvent(input$compute_tree_spectral_metrics, {
    req(reflectance(), all_indices())
    predictors <- c(reflectance(), all_indices())
    chm <- tryCatch(chm_raster(), error = function(e) NULL)
    if (!is.null(chm)) {
      if (!terra::compareGeom(predictors[[1]], chm, stopOnError = FALSE)) {
        chm <- terra::resample(chm, predictors[[1]], method = "bilinear")
      }
      names(chm) <- "CHM"
      predictors <- c(predictors, chm)
    }

    rows <- list()
    if (point_cloud_event() > 0) {
      trees <- tryCatch(tree_candidates(), error = function(e) data.frame())
      if (nrow(trees) > 0) {
        mapped <- points_to_map_xy(trees$x, trees$y, point_cloud(), predictors[[1]])
        for (i in seq_len(nrow(trees))) {
          center <- terra::vect(
            data.frame(x = mapped$x_map[[i]], y = mapped$y_map[[i]]),
            geom = c("x", "y"),
            crs = terra::crs(predictors)
          )
          crown <- terra::buffer(center, width = max(trees$crown_diameter_m[[i]] / 2, terra::res(predictors)[[1]]))
          rows[[length(rows) + 1]] <- data.frame(
            feature_type = "tree_candidate",
            feature_id = trees$tree_id[[i]],
            crown_diameter_m = trees$crown_diameter_m[[i]],
            t(summarize_predictors_in_polygon(predictors, crown)),
            check.names = FALSE
          )
        }
      }
    }

    roi <- tryCatch(selection_roi_analysis(), error = function(e) data.frame())
    if (nrow(roi) >= 3) {
      coords <- as.matrix(rbind(roi[, c("x", "y")], roi[1, c("x", "y")]))
      current_roi <- terra::vect(list(coords), type = "polygons", crs = terra::crs(predictors))
      rows[[length(rows) + 1]] <- data.frame(
        feature_type = "current_roi",
        feature_id = input$selection_label %||% "current_roi",
        crown_diameter_m = NA_real_,
        t(summarize_predictors_in_polygon(predictors, current_roi)),
        check.names = FALSE
      )
    }

    if (length(rows) == 0) {
      showNotification("No tree candidates or current ROI are available. Load the 3D scene or select an ROI first.", type = "warning", duration = 6)
      tree_spectral_metrics_value(data.frame())
      return()
    }
    tree_spectral_metrics_value(do.call(rbind, rows))
    showNotification("Tree / ROI spectral metrics computed.", type = "message", duration = 4)
  })

  output$tree_spectral_metrics <- renderTable({
    x <- tree_spectral_metrics_value()
    if (nrow(x) == 0) {
      return(data.frame(message = "No tree or ROI spectral metrics computed yet."))
    }
    numeric_cols <- vapply(x, is.numeric, logical(1))
    x[numeric_cols] <- lapply(x[numeric_cols], function(v) formatC(v, format = "f", digits = 2))
    x
  }, digits = 2)

  observeEvent(input$export_products, {
    req(reflectance(), all_indices(), biomass_proxy())
    paths <- write_dronebio_rasters(input$output_dir, reflectance(), all_indices(), biomass_proxy(), mosaic()$alpha)
    if (!is.null(custom_index_raster())) {
      custom_path <- file.path(input$output_dir, paste0(names(custom_index_raster()), ".tif"))
      terra::writeRaster(custom_index_raster(), custom_path, overwrite = TRUE, datatype = "FLT4S")
      paths <- c(paths, custom_index = custom_path)
    }
    app_raster <- application_map()
    app_path <- file.path(input$output_dir, "application_map_classes.tif")
    terra::writeRaster(app_raster, app_path, overwrite = TRUE, datatype = "INT1U")
    paths <- c(paths, application_map = app_path)

    app_summary <- summarize_application_map(app_raster)
    app_csv <- file.path(input$output_dir, "application_map_area_summary.csv")
    utils::write.csv(app_summary, app_csv, row.names = FALSE)
    paths <- c(paths, application_summary = app_csv)

    gpkg_path <- file.path(input$output_dir, "application_map_classes.gpkg")
    tryCatch({
      vector_raster <- app_raster
      if (terra::ncell(vector_raster) > 2000000) {
        fact <- ceiling(sqrt(terra::ncell(vector_raster) / 2000000))
        vector_raster <- terra::aggregate(vector_raster, fact = fact, fun = function(v, ...) {
          v <- v[!is.na(v)]
          if (length(v) == 0) return(NA_real_)
          as.numeric(names(which.max(table(v))))
        })
      }
      polygons <- terra::as.polygons(vector_raster, dissolve = TRUE, values = TRUE, na.rm = TRUE)
      names(polygons) <- "class_id"
      polygons$class <- application_classes$class[match(polygons$class_id, application_classes$class_id)]
      terra::writeVector(polygons, gpkg_path, overwrite = TRUE, filetype = "GPKG")
      paths <- c(paths, application_classes_gpkg = gpkg_path)
    }, error = function(e) {
      showNotification(paste("GeoPackage export skipped:", conditionMessage(e)), type = "warning", duration = 6)
    })

    metrics <- tree_spectral_metrics_value()
    if (nrow(metrics) > 0) {
      metrics_path <- file.path(input$output_dir, "tree_roi_spectral_metrics.csv")
      utils::write.csv(metrics, metrics_path, row.names = FALSE)
      paths <- c(paths, tree_roi_spectral_metrics = metrics_path)
    }

    spectral_export_paths(paths)
    showNotification("Spectral products exported.", type = "message", duration = 5)
  })

  output$export_paths <- renderText({
    paths <- spectral_export_paths()
    if (length(paths) == 0) return("")
    paste(paths, collapse = "\n")
  })

  point_cloud_event <- reactive({
    input$load_3d_scene + input$load_3d_scene_main
  })

  point_classes <- reactiveVal(data.frame(point_id = integer(), class = character()))
  manual_crowns <- reactiveVal(data.frame())
  selection_export_paths <- reactiveVal(character())
  selected_ids_value <- reactiveVal(integer())
  full_roi_status_value <- reactiveVal("No ROI selected yet.")

  chm_raster <- reactive({
    products <- cached_products()
    if (!file.exists(products[["dsm"]]) || !file.exists(products[["dtm"]])) {
      return(NULL)
    }
    build_chm_from_dsm_dtm(products[["dsm"]], products[["dtm"]])
  })

  dsm_raster <- reactive({
    products <- cached_products()
    if (!file.exists(products[["dsm"]])) return(NULL)
    terra::rast(products[["dsm"]])[[1]]
  })

  dtm_raster <- reactive({
    products <- cached_products()
    if (!file.exists(products[["dtm"]])) return(NULL)
    terra::rast(products[["dtm"]])[[1]]
  })

  viewer_basemap <- reactive({
    build_orthomosaic_texture(
      input$orthomosaic,
      use_alpha = input$use_alpha,
      scale_reflectance = input$scale_reflectance
    )
  }) |>
    bindCache(input$orthomosaic %||% "", input$use_alpha, input$scale_reflectance)

  context_orthomosaic <- reactive({
    build_context_orthomosaic_raster(
      input$orthomosaic,
      use_alpha = input$use_alpha,
      scale_reflectance = input$scale_reflectance
    )
  }) |>
    bindCache(input$orthomosaic %||% "", input$use_alpha, input$scale_reflectance)

  # Cache the draped-DSM heightfield. Building it is multi-second
  # work (crop to ortho, alpha-mask, percentile-clip, 3x3 median,
  # spatSample to a regular grid) and the previous code called it
  # inline inside output$point_cloud_viewer - so EVERY selection,
  # tool change, height-filter tweak rebuilt it from scratch. The
  # user perceived "the 3D plot is processing non-stop." Cached on
  # (dsm path, ortho path, draped-mode flag); recomputes only when
  # one of those actually changes. The dsm_path goes through
  # cached_products() so reads come from the local cache instead of
  # OneDrive once the user has run the migration.
  draped_dsm_heightmap <- reactive({
    if (!isTRUE(input$show_draped_dsm)) return(NULL)
    dsm_path <- cached_products()[["dsm"]]
    if (is.null(dsm_path) || !nzchar(dsm_path) || !file.exists(dsm_path)) {
      return(NULL)
    }
    build_dsm_heightmap(dsm_path, input$orthomosaic, grid_size = 180)
  }) |>
    bindCache(
      cached_products()[["dsm"]] %||% "",
      input$orthomosaic %||% "",
      isTRUE(input$show_draped_dsm)
    )

  # Cheap cached handle on the orthomosaic raster (header-only).
  # Used by the 2D context map's observers to project x/y point
  # positions back to WGS84 without re-opening the GeoTIFF on every
  # selection - terra::rast() is fast on local files but can be
  # multiple seconds on OneDrive Files-On-Demand, and the
  # observers fire frequently as the user clicks around.
  orthomosaic_raster_cached <- reactive({
    path <- input$orthomosaic %||% ""
    if (!nzchar(path) || !file.exists(path)) return(NULL)
    tryCatch(terra::rast(path)[[1]], error = function(e) NULL)
  }) |>
    bindCache(input$orthomosaic %||% "")

  point_cloud_reference_raster <- reactive({
    products <- cached_products()
    if (file.exists(products[["dsm"]])) {
      return(terra::rast(products[["dsm"]])[[1]])
    }
    if (file.exists(input$orthomosaic)) {
      return(terra::rast(input$orthomosaic)[[1]])
    }
    NULL
  })

  add_cloud_runtime_attributes <- function(points, source_path, coordinate_source, height_source) {
    attr(points, "point_cloud_source") <- normalizePath(source_path, mustWork = FALSE)
    attr(points, "coordinate_source") <- coordinate_source
    attr(points, "height_source") <- height_source
    points
  }

  # Cache-aware path resolvers for the two scene-source inputs. Both
  # textInputs let the user type any path; if a file with the same
  # basename also exists in ~/.dronebior/cache/<slug>/, we transparently
  # read from there instead. The biggest practical win is on OneDrive
  # projects where the canonical .laz file lives behind Files-On-Demand
  # but a local copy already sits in the cache from a previous "Copy
  # outputs to local cache" pass.
  resolved_full_cloud_path <- reactive({
    raw <- input$full_cloud_path %||% ""
    if (!nzchar(raw)) return("")
    DroneBioR:::cache_aware_path(raw, project())
  })
  resolved_ply_path <- reactive({
    raw <- input$ply_path %||% ""
    if (!nzchar(raw)) return("")
    DroneBioR:::cache_aware_path(raw, project())
  })

  point_cloud <- reactive({
    with_error_toast("Load point cloud", {
      full_path <- resolved_full_cloud_path()
      ply_path  <- resolved_ply_path()
      use_full_sample <- identical(input$viewer_cloud_source, "Full georeferenced LAS/LAZ/COPC sample") &&
        nzchar(full_path) && file.exists(full_path)

      if (isTRUE(use_full_sample)) {
        pts <- read_full_point_cloud(full_path, max_points = input$max_points)
        chm <- chm_raster()
        if (!is.null(chm)) {
          pts <- add_chm_heights(pts, chm)
          pts <- add_cloud_runtime_attributes(pts, full_path, "full_georeferenced", "DSM-DTM CHM")
        } else {
          pts <- add_point_heights(pts)
          pts <- add_cloud_runtime_attributes(pts, full_path, "full_georeferenced", "local low-Z proxy")
        }
      } else {
        validate(need(nzchar(ply_path) && file.exists(ply_path),
                      paste("PLY file not found:", ply_path)))
        pts <- read_ply_point_cloud(ply_path, max_points = input$max_points)
        pts <- add_point_heights(pts)
        pts <- add_cloud_runtime_attributes(pts, ply_path, "local_preview", "local low-Z proxy")
      }

      point_classes(data.frame(point_id = pts$point_id, class = "Unclassified"))
      selected_ids_value(integer())
      manual_crowns(data.frame())
      selection_export_paths(character())
      full_roi_status_value("No ROI selected yet.")
      pts
    })
  }) |>
    bindCache(
      input$viewer_cloud_source,
      resolved_full_cloud_path(),
      resolved_ply_path(),
      input$max_points
    ) |>
    bindEvent(point_cloud_event())

  observeEvent(input$selected_point_ids, {
    ids <- input$selected_point_ids %||% integer()
    selected_ids_value(unique(as.integer(unlist(ids))))
  }, ignoreInit = TRUE)

  observeEvent(input$clear_point_selection, {
    selected_ids_value(integer())
    showNotification("Selection cleared.", type = "message", duration = 3)
  })

  # Surface the JS-detected coordinate-frame mismatch as a one-shot
  # warning. Triggers when the orthomosaic / DSM and the loaded point
  # cloud are in different coordinate frames - typically the PLY
  # preview from odm_filterpoints/ (OpenSfM-local metres) vs the
  # orthomosaic / DSM (projected UTM, SIRGAS, etc.). The viewer snaps
  # the basemap plane to the points' XY bounds and disables the DSM
  # drape; we tell the user so they know to switch to LAS/LAZ/COPC.
  # last_basemap_mismatch_shown tracks whether the warning has fired in
  # this session, because the JS sends the boolean once per scene build
  # (priority="event"), so without the latch the user would see one
  # toast per renderUI run.
  last_basemap_mismatch_shown <- reactiveVal(FALSE)
  observeEvent(point_cloud_event(), {
    last_basemap_mismatch_shown(FALSE)
  })
  observeEvent(input$basemap_frame_mismatch, {
    if (!isTRUE(input$basemap_frame_mismatch)) return()
    if (isTRUE(last_basemap_mismatch_shown())) return()
    last_basemap_mismatch_shown(TRUE)
    showNotification(
      paste0("Orthomosaic / DSM and point cloud are in different coordinate frames. ",
             "The basemap was snapped to the points' bounding box and the DSM ",
             "drape was disabled. For exact georeferencing, switch the 3D ",
             "viewer source to the LAS/LAZ/COPC sample (it is in projected ",
             "metres and matches the orthomosaic CRS)."),
      type = "warning", duration = 10
    )
  }, ignoreInit = TRUE)

  # Push selection updates to the live three.js scene without rebuilding
  # it. Fires on every change to selected_ids_value (from JS clicks, from
  # the GIS Workspace ROI bridge, from the Clear selection button). The
  # viewer must already be mounted - point_cloud_event() > 0 - otherwise
  # the message is a no-op on the JS side.
  observe({
    ids <- selected_ids_value()
    if (point_cloud_event() == 0) return()
    payload <- list(ids = if (length(ids) == 0L) list() else as.integer(ids))
    session$sendCustomMessage("dronebior_3d_set_selection", payload)
  })

  selected_point_ids <- reactive({
    selected_ids_value()
  })

  empty_classified_points <- function() {
    data.frame(
      point_id = integer(),
      x = numeric(),
      y = numeric(),
      z = numeric(),
      height_m = numeric(),
      color = character(),
      class = character(),
      class_color = character(),
      stringsAsFactors = FALSE
    )
  }

  classified_points <- reactive({
    req(point_cloud())
    pts <- point_cloud()
    classes <- point_classes()
    if (nrow(classes) > 0) {
      pts <- merge(pts, classes, by = "point_id", all.x = TRUE, sort = FALSE)
    } else {
      pts$class <- "Unclassified"
    }
    pts$class[is.na(pts$class)] <- "Unclassified"
    pts$class_color <- unname(classification_palette[pts$class])
    pts$class_color[is.na(pts$class_color)] <- classification_palette[["Unclassified"]]
    pts
  })

  display_points <- reactive({
    req(classified_points())
    pts <- classified_points()
    pts <- pts[pts$height_m >= input$min_point_height & pts$height_m <= input$max_point_height, , drop = FALSE]
    pts
  })

  selected_points <- reactive({
    if (point_cloud_event() == 0) {
      return(empty_classified_points())
    }
    req(classified_points())
    ids <- selected_point_ids()
    if (length(ids) == 0) {
      return(classified_points()[0, , drop = FALSE])
    }
    classified_points()[classified_points()$point_id %in% ids, , drop = FALSE]
  })

  selected_geometry_points <- reactive({
    if (point_cloud_event() == 0) {
      return(empty_classified_points())
    }
    req(point_cloud())
    ids <- selected_point_ids()
    pts <- point_cloud()
    if (length(ids) == 0) {
      return(pts[0, , drop = FALSE])
    }
    pts[pts$point_id %in% ids, , drop = FALSE]
  })

  selection_roi_local <- reactive({
    pts <- selected_geometry_points()
    if (nrow(pts) < 3) {
      return(data.frame(x = numeric(), y = numeric()))
    }
    method <- if (identical(input$selection_method, "box")) "bbox" else "hull"
    build_roi_polygon(pts, method = method)
  })

  selection_roi_analysis <- reactive({
    roi <- selection_roi_local()
    if (nrow(roi) < 3) {
      return(roi)
    }
    reference_points <- point_cloud()
    reference_raster <- point_cloud_reference_raster()
    if (points_are_georeferenced(reference_points) || is.null(reference_raster)) {
      return(roi)
    }
    mapped <- local_to_raster_xy(roi$x, roi$y, reference_points, reference_raster)
    data.frame(x = mapped$x_map, y = mapped$y_map)
  })

  full_selection_points <- reactive({
    if (!isTRUE(input$use_full_roi_metrics)) {
      full_roi_status_value("Full-resolution recalculation is off; using the viewer preview sample.")
      return(selected_geometry_points()[0, , drop = FALSE])
    }
    pts_preview <- selected_geometry_points()
    if (nrow(pts_preview) == 0) {
      full_roi_status_value("No ROI selected yet.")
      return(pts_preview[0, , drop = FALSE])
    }
    roi <- selection_roi_analysis()
    if (nrow(roi) < 3) {
      full_roi_status_value("Select at least three points to build a polygon ROI.")
      return(pts_preview[0, , drop = FALSE])
    }
    full_path <- resolved_full_cloud_path()
    if (!nzchar(full_path) || !file.exists(full_path)) {
      full_roi_status_value("Full point cloud path is missing; using the viewer preview sample.")
      return(pts_preview[0, , drop = FALSE])
    }

    tryCatch({
      pts <- read_full_point_cloud(full_path, roi_polygon = roi, max_points = Inf)
      if (nrow(pts) == 0) {
        full_roi_status_value("The ROI polygon did not intersect any full-resolution points; using the viewer preview sample.")
        return(pts_preview[0, , drop = FALSE])
      }

      chm <- chm_raster()
      if (!is.null(chm)) {
        pts <- add_chm_heights(pts, chm)
        pts <- add_cloud_runtime_attributes(pts, full_path, "full_georeferenced", "DSM-DTM CHM")
      } else {
        pts <- add_point_heights(pts)
        pts <- add_cloud_runtime_attributes(pts, full_path, "full_georeferenced", "local low-Z proxy")
      }

      full_roi_status_value(paste0(
        "Full-resolution metrics active. ROI uses ",
        format(nrow(pts), big.mark = ","),
        " points from ",
        basename(attr(pts, "point_cloud_source")),
        if (!is.null(chm)) " with DSM-DTM CHM heights." else " with local low-Z height fallback."
      ))
      pts
    }, error = function(e) {
      full_roi_status_value(paste("Full-resolution recalculation failed; using viewer preview sample. Reason:", conditionMessage(e)))
      pts_preview[0, , drop = FALSE]
    })
  })

  analysis_points <- reactive({
    if (point_cloud_event() == 0) {
      return(empty_classified_points())
    }
    full <- full_selection_points()
    if (nrow(full) > 0) {
      return(full)
    }
    selected_geometry_points()
  })

  chm_roi_metrics <- reactive({
    roi <- selection_roi_analysis()
    chm <- chm_raster()
    if (is.null(chm) || nrow(roi) < 3 || nrow(selected_geometry_points()) == 0) {
      return(data.frame())
    }
    compute_chm_roi_metrics(chm, roi)
  })

  selection_metrics <- reactive({
    if (point_cloud_event() == 0) {
      metrics <- compute_selection_metrics(empty_classified_points(), voxel_size = input$voxel_size)
      metrics$point_source <- "No 3D scene loaded"
      metrics$height_source <- "No 3D scene loaded"
      return(metrics)
    }
    pts <- analysis_points()
    metrics <- compute_selection_metrics(pts, voxel_size = input$voxel_size)
    chm_metrics <- chm_roi_metrics()
    if (nrow(chm_metrics) > 0) {
      metrics <- cbind(metrics, chm_metrics)
    }
    metrics$point_source <- if (nrow(full_selection_points()) > 0) {
      paste0("Full cloud: ", basename(input$full_cloud_path))
    } else if (point_cloud_event() > 0) {
      paste0("Viewer sample: ", basename(attr(point_cloud(), "point_cloud_source") %||% input$ply_path))
    } else {
      "No 3D scene loaded"
    }
    metrics$height_source <- attr(pts, "height_source") %||% "local low-Z proxy"
    metrics
  })

  vertical_profile <- reactive({
    compute_vertical_profile(analysis_points(), bin_size = input$profile_bin_size)
  })

  tree_candidates <- reactive({
    req(display_points())
    derive_tree_candidates(
      display_points(),
      grid_size = input$tree_grid,
      min_height = input$min_tree_height,
      min_points = input$min_tree_points
    )
  })

  output$point_cloud_viewer <- renderUI({
    basemap <- viewer_basemap()
    scene_loaded <- point_cloud_event() > 0
    if (isTRUE(scene_loaded)) {
      req(display_points())
      points <- display_points()
      trees <- if (nrow(points) > 0) tree_candidates() else data.frame()
    } else {
      points <- empty_classified_points()
      trees <- data.frame()
    }

    if (identical(input$point_color_mode, "Classification")) {
      points$display_color <- points$class_color
    } else if (identical(input$point_color_mode, "Height")) {
      pal <- grDevices::colorRampPalette(hcl.colors(9, "YlGn"))
      z <- points$height_m
      z_range <- range(z, na.rm = TRUE)
      scaled <- if (diff(z_range) == 0) rep(0.5, length(z)) else (z - z_range[1]) / diff(z_range)
      points$display_color <- pal(101)[pmax(1, pmin(101, round(scaled * 100) + 1))]
    } else {
      points$display_color <- points$color
    }
    classified_mask <- !is.na(points$class) & points$class != "Unclassified"
    points$display_color[classified_mask] <- points$class_color[classified_mask]
    # Isolate the selection read so that clicking points / pulling in an
    # ROI does not invalidate the renderUI and tear down the whole three.js
    # scene from scratch. The initial render still uses the current
    # selection IDs (zero on a fresh load, restored after a basemap-driven
    # rebuild). Subsequent selection changes flow through the
    # dronebior_3d_set_selection custom message instead.
    points$selected <- points$point_id %in% isolate(selected_point_ids())

    point_json <- jsonlite::toJSON(
      points[, c("point_id", "x", "y", "z", "height_m", "display_color", "selected", "class", "class_color")],
      dataframe = "rows",
      digits = 7,
      auto_unbox = TRUE
    )
    tree_json <- jsonlite::toJSON(trees, dataframe = "rows", digits = 7, auto_unbox = TRUE)
    mode_json <- jsonlite::toJSON(input$selection_tool, auto_unbox = TRUE)
    camera_state_json <- jsonlite::toJSON(isolate(input$point_cloud_camera_state) %||% NULL, auto_unbox = TRUE)
    basemap_json <- jsonlite::toJSON(basemap %||% list(), auto_unbox = TRUE, null = "null")

    # ODM textured-mesh URL. We expose the directory holding the OBJ +
    # MTL + texture PNGs as a Shiny resource path so the browser can
    # GET them; OBJLoader / MTLLoader pull in the related files via
    # relative URLs the loaders compute themselves.
    mesh_url <- NULL
    if (isTRUE(input$show_textured_mesh)) {
      mesh_path <- cached_products()[["textured_obj"]]
      if (file.exists(mesh_path)) {
        shiny::addResourcePath("dronebior_obj", dirname(mesh_path))
        mesh_url <- paste0("dronebior_obj/", basename(mesh_path))
      }
    }
    mesh_url_json <- jsonlite::toJSON(mesh_url, auto_unbox = TRUE, null = "null")

    # Draped DSM heightfield comes from the cached reactive (see
    # draped_dsm_heightmap above) so re-renders triggered by
    # selection / tool / height-filter changes do NOT rebuild it.
    draped_dsm <- draped_dsm_heightmap()
    draped_dsm_json <- jsonlite::toJSON(draped_dsm %||% list(),
                                        auto_unbox = TRUE, null = "null", na = "null")

    tags$div(
      id = "point-cloud-viewer",
      style = "width:100%; height:100%; position:relative;",
      tags$script(HTML({
        viewer_script <- "
        (function() {
          const container = document.getElementById('point-cloud-viewer');
          if (!container) return;
          container.innerHTML = '';
          if (!window.THREE) {
            container.innerHTML = '<div style=\"color:white;padding:20px;\">Three.js did not load. Check internet access or bundle Three.js locally.</div>';
            return;
          }

          const points = __POINT_JSON__;
          const trees = __TREE_JSON__;
          const mode = __MODE_JSON__;
          const savedCameraState = __CAMERA_STATE_JSON__;
          const basemap = __BASEMAP_JSON__;
          const meshUrl = __MESH_URL_JSON__;
          const drapedDsm = __DRAPED_DSM_JSON__;
          const width = container.clientWidth;
          const height = container.clientHeight || 560;
          const scene = new THREE.Scene();
          // Apply the user's selected background theme up front; if the
          // theme input has not been processed yet, fall back to the
          // dark navy default. The viewer also exposes setBackground()
          // so toggling the sidebar select updates the colour live.
          const initialTheme = window.__dronebior_bg_theme || 'dark';
          if (initialTheme === 'light') {
            scene.background = new THREE.Color(0xe9eef5);
          } else if (initialTheme === 'white') {
            scene.background = new THREE.Color(0xffffff);
          } else {
            scene.background = new THREE.Color(0x0f172a);
          }

          const camera = new THREE.PerspectiveCamera(55, width / height, 0.01, 1000);
          // preserveDrawingBuffer = true keeps the WebGL framebuffer
          // intact between renders so the Screenshot toolbar button can
          // do renderer.domElement.toDataURL() reliably across browsers.
          // Small perf hit, acceptable for this app's use case.
          const renderer = new THREE.WebGLRenderer({ antialias: true, preserveDrawingBuffer: true });
          renderer.setSize(width, height);
          renderer.setPixelRatio(Math.min(window.devicePixelRatio || 1, 2));
          renderer.domElement.style.position = 'absolute';
          renderer.domElement.style.left = '0';
          renderer.domElement.style.top = '0';
          container.appendChild(renderer.domElement);
          const overlay = document.createElement('canvas');
          overlay.width = width;
          overlay.height = height;
          overlay.className = 'selection-layer';
          container.appendChild(overlay);
          const overlayCtx = overlay.getContext('2d');

          const hasBasemap = basemap &&
            Number.isFinite(basemap.xmin) &&
            Number.isFinite(basemap.xmax) &&
            Number.isFinite(basemap.ymin) &&
            Number.isFinite(basemap.ymax);
          const hasPoints = points.length > 0;
          const xs = hasPoints ? points.map(p => p.x) : [];
          const ys = hasPoints ? points.map(p => p.y) : [];
          const zs = hasPoints ? points.map(p => p.z) : [];
          const minX = hasPoints ? Math.min(...xs) : (hasBasemap ? basemap.xmin : -1);
          const maxX = hasPoints ? Math.max(...xs) : (hasBasemap ? basemap.xmax : 1);
          const minY = hasPoints ? Math.min(...ys) : (hasBasemap ? basemap.ymin : -1);
          const maxY = hasPoints ? Math.max(...ys) : (hasBasemap ? basemap.ymax : 1);
          const minZ = hasPoints ? Math.min(...zs) : 0;
          const maxZ = hasPoints ? Math.max(...zs) : 1;
          const cx = (minX + maxX) / 2, cy = (minY + maxY) / 2;
          const scale = Math.max(maxX - minX, maxY - minY, maxZ - minZ, 1);

          // Coordinate-frame mismatch guard. The point cloud (especially the
          // PLY preview, which can be in OpenSfM local metres) and the
          // basemap (always in the orthomosaic's projected CRS - UTM /
          // SIRGAS / WGS84) sometimes live in different frames. When that
          // happens, the basemap's bounding box does not overlap the
          // points' bounding box AT ALL, so its plane lands far away from
          // the points and the two surfaces appear on visually disjoint
          // planes. Detection: AABB-overlap. We DO NOT trigger the snap
          // for partial overlap (small ROIs inside a wider ortho), only
          // for fully disjoint bounding boxes.
          let basemapXmin = hasBasemap ? basemap.xmin : minX;
          let basemapXmax = hasBasemap ? basemap.xmax : maxX;
          let basemapYmin = hasBasemap ? basemap.ymin : minY;
          let basemapYmax = hasBasemap ? basemap.ymax : maxY;
          let basemapFrameMismatch = false;
          if (hasBasemap && hasPoints) {
            const xOverlap = !(maxX < basemapXmin || minX > basemapXmax);
            const yOverlap = !(maxY < basemapYmin || minY > basemapYmax);
            if (!xOverlap || !yOverlap) {
              basemapFrameMismatch = true;
              basemapXmin = minX; basemapXmax = maxX;
              basemapYmin = minY; basemapYmax = maxY;
            }
          }

          // Separate Z-frame check for the DSM drape. The drape's heightmap
          // values are absolute elevations (e.g. 50-80 m above mean sea
          // level), while the point cloud's Z is whatever the source file
          // gave us. If those ranges do not overlap (typical when the
          // points are in OpenSfM-local metres centred on 0 but the DSM is
          // in real-world EGM2008 elevations), draping anchors the surface
          // far above or below the points. In that case we suppress the
          // drape and fall back to the flat basemap_plane.
          let drapedZmin = Infinity, drapedZmax = -Infinity;
          if (drapedDsm && Array.isArray(drapedDsm.z_rows)) {
            for (let row = 0; row < drapedDsm.nrow; row++) {
              const z_row = drapedDsm.z_rows[row];
              if (!z_row) continue;
              for (let col = 0; col < drapedDsm.ncol; col++) {
                const zv = z_row[col];
                if (Number.isFinite(zv)) {
                  if (zv < drapedZmin) drapedZmin = zv;
                  if (zv > drapedZmax) drapedZmax = zv;
                }
              }
            }
          }
          const drapedFrameMismatch = hasPoints &&
            Number.isFinite(drapedZmin) && Number.isFinite(drapedZmax) &&
            (drapedZmax < minZ || drapedZmin > maxZ);

          if (window.Shiny) {
            Shiny.setInputValue('basemap_frame_mismatch',
              basemapFrameMismatch || drapedFrameMismatch,
              { priority: 'event' });
          }

          function hexToRgb(hex) {
            const h = hex.replace('#', '');
            return [
              parseInt(h.substring(0, 2), 16) / 255,
              parseInt(h.substring(2, 4), 16) / 255,
              parseInt(h.substring(4, 6), 16) / 255
            ];
          }
          function pos(p) {
            return [(p.x - cx) / scale * 10, (p.z - minZ) / scale * 10, -(p.y - cy) / scale * 10];
          }
          function originalDistance(a, b) {
            return Math.sqrt(Math.pow(a.x - b.x, 2) + Math.pow(a.y - b.y, 2) + Math.pow(a.z - b.z, 2));
          }

          // Basemap rendering. Two paths:
          //  * Draped DSM (Pix4D-style): build a PlaneGeometry subdivided
          //    to match the DSM height grid, displace vertex Z by the
          //    DSM value, and apply the orthomosaic as the texture.
          //    Adds directional + ambient lights so the slope-driven
          //    Lambert shading is visible.
          //  * Flat plane fallback (the previous behaviour): used when
          //    the user has not enabled draped mode or no DSM is
          //    available.
          // DSM drape is only meaningful when the basemap and the points
          // share a coordinate frame. When they do not (XY-bounds
          // disjoint, OR DSM-Z range disjoint from point-Z range), the
          // drape would float hundreds of scene units above/below the
          // points. Fall back to the flat basemap plane in either case.
          const hasDrapedDsm = drapedDsm &&
            Array.isArray(drapedDsm.z_rows) &&
            drapedDsm.nrow > 1 && drapedDsm.ncol > 1 &&
            !basemapFrameMismatch && !drapedFrameMismatch;

          if (hasDrapedDsm && hasBasemap && basemap.data_uri) {
            const textureLoader = new THREE.TextureLoader();
            textureLoader.load(basemap.data_uri, function(texture) {
              if ('colorSpace' in texture && THREE.SRGBColorSpace) {
                texture.colorSpace = THREE.SRGBColorSpace;
              }
              texture.anisotropy = renderer.capabilities.getMaxAnisotropy();
              const planeWidth  = Math.max((basemapXmax - basemapXmin) / scale * 10, 0.1);
              const planeHeight = Math.max((basemapYmax - basemapYmin) / scale * 10, 0.1);
              const geom = new THREE.PlaneGeometry(
                planeWidth, planeHeight,
                drapedDsm.ncol - 1, drapedDsm.nrow - 1
              );
              const positions = geom.attributes.position;
              // PlaneGeometry vertices are row-major, top-to-bottom in
              // its local Y. After rotateX(-PI/2) the original local Z
              // becomes scene Y (up), so we displace Z here.
              let idx = 0;
              for (let row = 0; row < drapedDsm.nrow; row++) {
                const z_row = drapedDsm.z_rows[row];
                for (let col = 0; col < drapedDsm.ncol; col++) {
                  const z_world = z_row[col];
                  positions.setZ(idx, (z_world - minZ) / scale * 10);
                  idx++;
                }
              }
              positions.needsUpdate = true;
              geom.computeVertexNormals();

              // MeshStandardMaterial responds to physically-based
              // lights and tone mapping, which together with the
              // hemisphere light below produce a soft natural look
              // similar to a Pix4D 3D model view (not the harsh
              // direct-sun look the previous Lambert + single
              // directional gave).
              const material = new THREE.MeshStandardMaterial({
                map: texture,
                side: THREE.DoubleSide,
                roughness: 0.95,
                metalness: 0.0
              });
              const mesh = new THREE.Mesh(geom, material);
              mesh.rotation.x = -Math.PI / 2;
              const cxScene = (((basemapXmin + basemapXmax) / 2) - cx) / scale * 10;
              const czScene = -((((basemapYmin + basemapYmax) / 2) - cy) / scale * 10);
              mesh.position.set(cxScene, 0, czScene);
              mesh.name = 'dsm_drape';
              scene.add(mesh);

              // Soft outdoor lighting: hemisphere (sky -> ground)
              // for ambient fill that varies vertically, plus a gentle
              // directional to keep slope shading. ACES tone mapping
              // on the renderer rounds off the highlights.
              const hemi = new THREE.HemisphereLight(0xddeeff, 0x445544, 0.65);
              scene.add(hemi);
              const sun = new THREE.DirectionalLight(0xffffff, 0.55);
              sun.position.set(60, 120, 50);
              scene.add(sun);
              if ('ACESFilmicToneMapping' in THREE) {
                renderer.toneMapping = THREE.ACESFilmicToneMapping;
                renderer.toneMappingExposure = 1.0;
              }
              if ('outputColorSpace' in renderer && THREE.SRGBColorSpace) {
                renderer.outputColorSpace = THREE.SRGBColorSpace;
              }
            });
          } else if (hasBasemap && basemap.data_uri) {
            const textureLoader = new THREE.TextureLoader();
            textureLoader.load(basemap.data_uri, function(texture) {
              if ('colorSpace' in texture && THREE.SRGBColorSpace) {
                texture.colorSpace = THREE.SRGBColorSpace;
              }
              texture.anisotropy = renderer.capabilities.getMaxAnisotropy();
              const planeWidth = Math.max((basemapXmax - basemapXmin) / scale * 10, 0.1);
              const planeHeight = Math.max((basemapYmax - basemapYmin) / scale * 10, 0.1);
              const planeX = (((basemapXmin + basemapXmax) / 2) - cx) / scale * 10;
              const planeZ = -((((basemapYmin + basemapYmax) / 2) - cy) / scale * 10);
              const plane = new THREE.Mesh(
                new THREE.PlaneGeometry(planeWidth, planeHeight),
                new THREE.MeshBasicMaterial({ map: texture, transparent: true, opacity: 0.88, side: THREE.DoubleSide })
              );
              plane.rotation.x = -Math.PI / 2;
              plane.position.set(planeX, -0.08, planeZ);
              plane.name = 'basemap_plane';
              scene.add(plane);
            });
          }

          // ODM textured-mesh loader. ODM writes odm_textured_model_geo.obj
          // with an MTL alongside it and one or more texture PNGs in the
          // same folder. OBJLoader + MTLLoader resolve those relative
          // paths automatically; all we have to do is rewrite the loaded
          // geometry into the viewer's local coord system (the same
          // affine transform we apply to point positions).
          if (meshUrl && THREE.OBJLoader && THREE.MTLLoader) {
            const baseDir = meshUrl.substring(0, meshUrl.lastIndexOf('/') + 1);
            const objName = meshUrl.substring(meshUrl.lastIndexOf('/') + 1);
            const mtlName = objName.replace(/\\.obj$/i, '.mtl');
            const mtlLoader = new THREE.MTLLoader();
            mtlLoader.setPath(baseDir);
            mtlLoader.load(mtlName, function(materials) {
              materials.preload();
              const objLoader = new THREE.OBJLoader();
              objLoader.setMaterials(materials);
              objLoader.setPath(baseDir);
              objLoader.load(objName, function(obj) {
                obj.traverse(function(child) {
                  if (child.isMesh && child.geometry && child.geometry.attributes && child.geometry.attributes.position) {
                    const positions = child.geometry.attributes.position;
                    for (let i = 0; i < positions.count; i++) {
                      const ox = positions.getX(i);
                      const oy = positions.getY(i);
                      const oz = positions.getZ(i);
                      // ODM OBJ is in projected metres, +Z up. Match
                      // the same transform used for point cloud
                      // positions so points and mesh share a frame.
                      positions.setX(i,  (ox - cx)    / scale * 10);
                      positions.setY(i,  (oz - minZ)  / scale * 10);
                      positions.setZ(i, -(oy - cy)    / scale * 10);
                    }
                    positions.needsUpdate = true;
                    if (child.geometry.attributes.normal) {
                      child.geometry.deleteAttribute('normal');
                    }
                    child.geometry.computeVertexNormals();
                    child.geometry.computeBoundingSphere();
                  }
                });
                obj.name = 'textured_mesh';
                scene.add(obj);
              }, undefined, function(err) {
                console.warn('OBJLoader failed:', err);
              });
            }, undefined, function(err) {
              console.warn('MTLLoader failed (loading OBJ without materials):', err);
              const objLoader = new THREE.OBJLoader();
              objLoader.setPath(baseDir);
              objLoader.load(objName, function(obj) {
                obj.traverse(function(child) {
                  if (child.isMesh && child.geometry && child.geometry.attributes && child.geometry.attributes.position) {
                    const positions = child.geometry.attributes.position;
                    for (let i = 0; i < positions.count; i++) {
                      positions.setX(i,  (positions.getX(i) - cx)   / scale * 10);
                      positions.setY(i,  (positions.getZ(i) - minZ) / scale * 10);
                      positions.setZ(i, -(positions.getY(i) - cy)   / scale * 10);
                    }
                    positions.needsUpdate = true;
                    child.material = new THREE.MeshLambertMaterial({ color: 0xaaaaaa, side: THREE.DoubleSide });
                  }
                });
                obj.name = 'textured_mesh';
                scene.add(obj);
              });
            });
          }

          const positions = [];
          const colors = [];
          const selectedPositions = [];
          const selectedColors = [];
          points.forEach(p => {
            const pp = pos(p);
            positions.push(...pp);
            colors.push(...hexToRgb(p.display_color || '#66cc66'));
            if (p.selected) {
              selectedPositions.push(...pp);
              selectedColors.push(...hexToRgb((p.class && p.class !== 'Unclassified') ? (p.class_color || p.display_color) : '#ef4444'));
            }
          });
          const geometry = new THREE.BufferGeometry();
          geometry.setAttribute('position', new THREE.Float32BufferAttribute(positions, 3));
          geometry.setAttribute('color', new THREE.Float32BufferAttribute(colors, 3));
          const initialPointSize = (window.__dronebior_point_size_pct || 100) / 100 * 0.035;
          const material = new THREE.PointsMaterial({ size: initialPointSize, vertexColors: true });
          const pointsObject = new THREE.Points(geometry, material);
          pointsObject.name = 'points';
          scene.add(pointsObject);
          // Always create the selected_points mesh, even if currently empty,
          // so the dronebior_3d_set_selection handler can update it later
          // without having to rebuild the scene from scratch. The selection
          // is no longer part of the renderUI dependency graph.
          const selectedGeometry = new THREE.BufferGeometry();
          selectedGeometry.setAttribute('position', new THREE.Float32BufferAttribute(selectedPositions, 3));
          selectedGeometry.setAttribute('color', new THREE.Float32BufferAttribute(selectedColors, 3));
          const selectedPointMaterial = new THREE.PointsMaterial({ size: initialPointSize * 2.3, vertexColors: true });
          const selectedPointsObject = new THREE.Points(selectedGeometry, selectedPointMaterial);
          selectedPointsObject.name = 'selected_points';
          scene.add(selectedPointsObject);

          // Wireframe AABB drawn around the current selection. Drawn as
          // 12 line segments (THREE.LineSegments) of a yellow material so
          // it stays visible against any background theme. The user sees
          // this in the viewer the moment a box / lasso / polygon select
          // closes, so they no longer have to flip to the 2D context
          // sub-tab to confirm the ROI was captured.
          const selectionBoxGeometry = new THREE.BufferGeometry();
          selectionBoxGeometry.setAttribute('position',
            new THREE.Float32BufferAttribute(new Float32Array(72), 3));
          const selectionBoxMaterial = new THREE.LineBasicMaterial({
            color: 0xfacc15,
            transparent: true,
            opacity: 0.95,
            depthTest: false
          });
          const selectionBoxObject = new THREE.LineSegments(
            selectionBoxGeometry, selectionBoxMaterial);
          selectionBoxObject.name = 'selection_bbox';
          selectionBoxObject.visible = false;
          selectionBoxObject.renderOrder = 999;
          scene.add(selectionBoxObject);

          // Build a lookup from point_id to the original point record so
          // setSelection can rebuild the highlight geometry without walking
          // the full array every time.
          const pointById = new Map();
          points.forEach(p => pointById.set(p.point_id, p));
          function rebuildSelectionMesh(ids) {
            const selPos = [];
            const selCol = [];
            const idArr = (ids && ids.length) ? ids : [];
            let bxMin = Infinity, byMin = Infinity, bzMin = Infinity;
            let bxMax = -Infinity, byMax = -Infinity, bzMax = -Infinity;
            for (let i = 0; i < idArr.length; i++) {
              const p = pointById.get(idArr[i]);
              if (!p) continue;
              const pp = pos(p);
              selPos.push(pp[0], pp[1], pp[2]);
              if (pp[0] < bxMin) bxMin = pp[0]; if (pp[0] > bxMax) bxMax = pp[0];
              if (pp[1] < byMin) byMin = pp[1]; if (pp[1] > byMax) byMax = pp[1];
              if (pp[2] < bzMin) bzMin = pp[2]; if (pp[2] > bzMax) bzMax = pp[2];
              const c = (p.class && p.class !== 'Unclassified')
                ? (p.class_color || p.display_color)
                : '#ef4444';
              const rgb = hexToRgb(c);
              selCol.push(rgb[0], rgb[1], rgb[2]);
            }
            selectedGeometry.setAttribute('position',
              new THREE.Float32BufferAttribute(selPos, 3));
            selectedGeometry.setAttribute('color',
              new THREE.Float32BufferAttribute(selCol, 3));
            selectedGeometry.attributes.position.needsUpdate = true;
            selectedGeometry.attributes.color.needsUpdate = true;
            selectedGeometry.computeBoundingSphere();

            // Pump the 12 edges of the AABB into the LineSegments geometry.
            // Pad zero-width axes (single point selected, or a perfectly
            // co-planar lasso) by a small fraction of the scene radius so
            // the box stays a visible cuboid rather than a degenerate
            // line / quad.
            if (idArr.length === 0 || !Number.isFinite(bxMin)) {
              selectionBoxObject.visible = false;
            } else {
              const pad = Math.max((maxX - minX), (maxY - minY), (maxZ - minZ), 1) / scale * 10 * 0.005;
              if (bxMax - bxMin < pad) { const m = (bxMin + bxMax)/2; bxMin = m - pad; bxMax = m + pad; }
              if (byMax - byMin < pad) { const m = (byMin + byMax)/2; byMin = m - pad; byMax = m + pad; }
              if (bzMax - bzMin < pad) { const m = (bzMin + bzMax)/2; bzMin = m - pad; bzMax = m + pad; }
              const v = [
                [bxMin,byMin,bzMin],[bxMax,byMin,bzMin],
                [bxMax,byMin,bzMin],[bxMax,byMin,bzMax],
                [bxMax,byMin,bzMax],[bxMin,byMin,bzMax],
                [bxMin,byMin,bzMax],[bxMin,byMin,bzMin],
                [bxMin,byMax,bzMin],[bxMax,byMax,bzMin],
                [bxMax,byMax,bzMin],[bxMax,byMax,bzMax],
                [bxMax,byMax,bzMax],[bxMin,byMax,bzMax],
                [bxMin,byMax,bzMax],[bxMin,byMax,bzMin],
                [bxMin,byMin,bzMin],[bxMin,byMax,bzMin],
                [bxMax,byMin,bzMin],[bxMax,byMax,bzMin],
                [bxMax,byMin,bzMax],[bxMax,byMax,bzMax],
                [bxMin,byMin,bzMax],[bxMin,byMax,bzMax]
              ];
              const flat = new Float32Array(72);
              for (let i = 0; i < v.length; i++) {
                flat[i*3]   = v[i][0];
                flat[i*3+1] = v[i][1];
                flat[i*3+2] = v[i][2];
              }
              selectionBoxGeometry.setAttribute('position',
                new THREE.Float32BufferAttribute(flat, 3));
              selectionBoxGeometry.attributes.position.needsUpdate = true;
              selectionBoxGeometry.computeBoundingSphere();
              selectionBoxObject.visible = true;
            }
          }
          // If the initial render already has a selection (e.g. the
          // scene rebuilt while a selection from a prior session was
          // active), draw the box right away.
          if (selectedPositions.length > 0) {
            const initialIds = points
              .filter(p => p.selected)
              .map(p => p.point_id);
            rebuildSelectionMesh(initialIds);
          }

          const grid = new THREE.GridHelper(12, 12, 0x94a3b8, 0x334155);
          grid.position.y = -0.03;
          grid.name = 'grid';
          scene.add(grid);

          // All tree markers go into a single named Group so the
          // layer toggle can hide them with one .visible flag instead
          // of walking the scene each time.
          const treesGroup = new THREE.Group();
          treesGroup.name = 'trees';
          const treeMeshes = [];
          const markerMaterial = new THREE.MeshStandardMaterial({ color: 0xfacc15, roughness: 0.35, metalness: 0.05 });
          trees.forEach(t => {
            const radius = Math.max((t.crown_diameter_m || 1) / scale * 3, 0.055);
            const sphere = new THREE.Mesh(new THREE.SphereGeometry(radius, 16, 12), markerMaterial.clone());
            const p = pos(t);
            sphere.position.set(p[0], p[1] + radius * 1.2, p[2]);
            sphere.userData = t;
            treesGroup.add(sphere);
            treeMeshes.push(sphere);
          });
          scene.add(treesGroup);

          const hemi = new THREE.HemisphereLight(0xffffff, 0x1e293b, 1.6);
          scene.add(hemi);
          const dir = new THREE.DirectionalLight(0xffffff, 0.7);
          dir.position.set(3, 6, 4);
          scene.add(dir);

          const defaultTarget = new THREE.Vector3(0, 1.2, 0);
          const defaultPosition = new THREE.Vector3(6, 7, 9);
          const hasSavedCamera = savedCameraState &&
            Array.isArray(savedCameraState.position) &&
            Array.isArray(savedCameraState.target) &&
            savedCameraState.position.length === 3 &&
            savedCameraState.target.length === 3;
          if (hasSavedCamera) {
            camera.position.set(savedCameraState.position[0], savedCameraState.position[1], savedCameraState.position[2]);
          } else {
            camera.position.copy(defaultPosition);
          }
          camera.lookAt(hasSavedCamera ?
            new THREE.Vector3(savedCameraState.target[0], savedCameraState.target[1], savedCameraState.target[2]) :
            defaultTarget
          );
          const controls = THREE.OrbitControls ? new THREE.OrbitControls(camera, renderer.domElement) : null;
          if (controls) {
            controls.enableDamping = true;
            if (hasSavedCamera) {
              controls.target.set(savedCameraState.target[0], savedCameraState.target[1], savedCameraState.target[2]);
            } else {
              controls.target.copy(defaultTarget);
            }
            // Pix4D / Blender-style multi-button input: left button is
            // reserved for the active selection tool (or orbit when no
            // tool is active), middle pans, right ALWAYS orbits. The
            // user can therefore reframe at any moment without having
            // to switch the tool dropdown. controls.enabled stays TRUE;
            // the per-button mapping below gates left-click behaviour.
            const selectionToolNames = ['Box selection', 'Lasso selection',
              'Polygon selection', 'Manual crown edit', 'Measure distance'];
            const toolIsSelection = selectionToolNames.includes(mode);
            if (THREE.MOUSE) {
              controls.mouseButtons = {
                LEFT:   toolIsSelection ? null : THREE.MOUSE.ROTATE,
                MIDDLE: THREE.MOUSE.PAN,
                RIGHT:  THREE.MOUSE.ROTATE
              };
            }
            controls.enabled = true;
            // Suppress the browser context menu on the canvas so right
            // click drag is reserved for orbit.
            renderer.domElement.addEventListener('contextmenu', function(e) {
              e.preventDefault();
            });

            // Visible XYZ axes inside the scene. Length = ~1/4 of the scene
            // extent so it is always readable but never dominates the view.
            let axes = null;
            if (THREE.AxesHelper) {
              var axisLen = Math.max(1, Math.min(
                Math.abs(defaultPosition.x - defaultTarget.x),
                Math.abs(defaultPosition.y - defaultTarget.y),
                Math.abs(defaultPosition.z - defaultTarget.z)
              ) * 0.25);
              axes = new THREE.AxesHelper(axisLen);
              axes.position.copy(defaultTarget);
              axes.name = 'axes';
              scene.add(axes);
            }

            // Convenience bounds for camera presets - distance to the
            // scene origin / center used by Top/Front/Side/Iso.
            const sceneExtent = Math.max(
              Math.abs(maxX - minX), Math.abs(maxY - minY),
              Math.abs(maxZ - minZ), 1
            ) / scale * 10;
            const sceneRadius = Math.max(sceneExtent * 1.0, 6);

            // Expose handles for the sidebar/toolbar-driven custom
            // message handlers. Anything that wants to manipulate the
            // live scene (layer visibility, point size, camera presets,
            // background colour) does it through this object. Lookup
            // by .name ensures we hit the intended object even when the
            // scene has been re-created across renders.
            window.__dronebior_viewer = {
              camera: camera,
              controls: controls,
              renderer: renderer,
              scene: scene,
              defaultPosition: defaultPosition.clone(),
              defaultTarget: defaultTarget.clone(),
              pointMaterial: material,
              selectedPointMaterial: selectedPointMaterial,
              sceneRadius: sceneRadius,
              setLayerVisible: function(name, visible) {
                const obj = scene.getObjectByName(name);
                if (obj) obj.visible = !!visible;
              },
              setPointSize: function(pct) {
                const factor = (Math.max(10, Math.min(500, pct)) || 100) / 100;
                if (material) material.size = 0.035 * factor;
                if (selectedPointMaterial) selectedPointMaterial.size = 0.035 * 2.3 * factor;
              },
              setBackground: function(theme) {
                if (theme === 'light') {
                  scene.background = new THREE.Color(0xe9eef5);
                } else if (theme === 'white') {
                  scene.background = new THREE.Color(0xffffff);
                } else {
                  scene.background = new THREE.Color(0x0f172a);
                }
              },
              cameraPreset: function(name) {
                const r = sceneRadius;
                let pos, tgt;
                switch (name) {
                  case 'top':
                    pos = new THREE.Vector3(0, r * 1.5, 0.001);
                    tgt = new THREE.Vector3(0, 0, 0);
                    break;
                  case 'front':
                    pos = new THREE.Vector3(0, r * 0.4, r);
                    tgt = new THREE.Vector3(0, r * 0.2, 0);
                    break;
                  case 'side':
                    pos = new THREE.Vector3(r, r * 0.4, 0);
                    tgt = new THREE.Vector3(0, r * 0.2, 0);
                    break;
                  case 'iso':
                    pos = new THREE.Vector3(r * 0.75, r * 0.75, r * 0.75);
                    tgt = new THREE.Vector3(0, r * 0.15, 0);
                    break;
                  case 'frame':
                    pos = defaultPosition.clone();
                    tgt = defaultTarget.clone();
                    break;
                  default: return;
                }
                camera.position.copy(pos);
                controls.target.copy(tgt);
                controls.update();
              },
              cancelSelection: function() {
                // Called by Esc key handler / Cancel selection button.
                // Clears any in-progress overlay drawing so the user
                // can start a new polygon/lasso without re-loading.
                if (overlayCtx) overlayCtx.clearRect(0, 0, width, height);
              },
              setSelection: function(ids) {
                // Update the highlight mesh in place. The point cloud and
                // its full position buffer stay untouched, which is what
                // keeps clicking points snappy even on 35k+ samples - the
                // previous code rebuilt every BufferAttribute and re-ran
                // jsonlite::toJSON in R on every click.
                rebuildSelectionMesh(ids || []);
              }
            };

            // Bottom-right gizmo: a separate mini-scene with coloured XYZ
            // axes + sphere tips. The gizmo camera is reoriented every
            // frame to match the main camera's view direction (same
            // angle, fixed distance), so the gizmo always shows the
            // current orientation - the Blender / Pix4D convention.
            var gizmoContainer = document.getElementById('viewer_gizmo');
            if (gizmoContainer && THREE.AxesHelper) {
              while (gizmoContainer.firstChild) {
                gizmoContainer.removeChild(gizmoContainer.firstChild);
              }
              var gizmoScene = new THREE.Scene();
              var gizmoCamera = new THREE.PerspectiveCamera(50, 1, 0.1, 100);
              var gizmoAxes = new THREE.AxesHelper(1.5);
              gizmoScene.add(gizmoAxes);
              var sphereGeom = new THREE.SphereGeometry(0.16, 16, 16);
              var xTip = new THREE.Mesh(sphereGeom, new THREE.MeshBasicMaterial({ color: 0xef4444 }));
              var yTip = new THREE.Mesh(sphereGeom, new THREE.MeshBasicMaterial({ color: 0x22c55e }));
              var zTip = new THREE.Mesh(sphereGeom, new THREE.MeshBasicMaterial({ color: 0x3b82f6 }));
              xTip.position.set(1.5, 0, 0); yTip.position.set(0, 1.5, 0); zTip.position.set(0, 0, 1.5);
              gizmoScene.add(xTip); gizmoScene.add(yTip); gizmoScene.add(zTip);

              var gizmoSize = Math.max(80, Math.min(112, gizmoContainer.clientWidth));
              var gizmoRenderer = new THREE.WebGLRenderer({ alpha: true, antialias: true });
              gizmoRenderer.setPixelRatio(window.devicePixelRatio || 1);
              gizmoRenderer.setSize(gizmoSize, gizmoSize);
              gizmoRenderer.setClearColor(0x000000, 0);
              gizmoContainer.appendChild(gizmoRenderer.domElement);

              window.__dronebior_viewer.gizmoRenderer = gizmoRenderer;
              window.__dronebior_viewer.gizmoUpdate = function() {
                var dir = camera.position.clone().sub(controls.target);
                if (dir.lengthSq() < 1e-9) return;
                dir.normalize().multiplyScalar(4.5);
                gizmoCamera.position.copy(dir);
                gizmoCamera.up.copy(camera.up);
                gizmoCamera.lookAt(0, 0, 0);
                gizmoRenderer.render(gizmoScene, gizmoCamera);
              };
            }
            let lastCameraEmit = 0;
            controls.addEventListener('change', function() {
              const now = Date.now();
              if (now - lastCameraEmit < 180) return;
              lastCameraEmit = now;
              if (window.Shiny) {
                Shiny.setInputValue('point_cloud_camera_state', {
                  position: [camera.position.x, camera.position.y, camera.position.z],
                  target: [controls.target.x, controls.target.y, controls.target.z]
                }, { priority: 'event' });
              }
            });
          }

          const raycaster = new THREE.Raycaster();
          const mouse = new THREE.Vector2();
          const selectionModes = ['Box selection', 'Lasso selection', 'Polygon selection', 'Manual crown edit'];
          const isSelectionMode = selectionModes.includes(mode);
          let isDrawing = false;
          let startPoint = null;
          let path = [];
          let polygonPath = [];
          let measurementPoints = [];

          function drawOverlayRect(a, b) {
            overlayCtx.clearRect(0, 0, width, height);
            overlayCtx.strokeStyle = '#facc15';
            overlayCtx.fillStyle = 'rgba(250, 204, 21, 0.12)';
            overlayCtx.lineWidth = 2;
            const x = Math.min(a.x, b.x), y = Math.min(a.y, b.y);
            const w = Math.abs(a.x - b.x), h = Math.abs(a.y - b.y);
            overlayCtx.fillRect(x, y, w, h);
            overlayCtx.strokeRect(x, y, w, h);
          }
          function drawPath(pointsPath, closePath = false) {
            overlayCtx.clearRect(0, 0, width, height);
            if (pointsPath.length === 0) return;
            overlayCtx.strokeStyle = '#facc15';
            overlayCtx.fillStyle = 'rgba(250, 204, 21, 0.12)';
            overlayCtx.lineWidth = 2;
            overlayCtx.beginPath();
            overlayCtx.moveTo(pointsPath[0].x, pointsPath[0].y);
            pointsPath.slice(1).forEach(pt => overlayCtx.lineTo(pt.x, pt.y));
            if (closePath) {
              overlayCtx.closePath();
              overlayCtx.fill();
            }
            overlayCtx.stroke();
          }
          function pointInPolygon(pt, poly) {
            let inside = false;
            for (let i = 0, j = poly.length - 1; i < poly.length; j = i++) {
              const xi = poly[i].x, yi = poly[i].y;
              const xj = poly[j].x, yj = poly[j].y;
              const intersect = ((yi > pt.y) !== (yj > pt.y)) &&
                (pt.x < (xj - xi) * (pt.y - yi) / ((yj - yi) || 1e-9) + xi);
              if (intersect) inside = !inside;
            }
            return inside;
          }
          function projectedPoints() {
            return points.map((p, i) => {
              const coords = pos(p);
              const v = new THREE.Vector3(coords[0], coords[1], coords[2]).project(camera);
              return {
                idx: i,
                point_id: p.point_id,
                x: (v.x * 0.5 + 0.5) * width,
                y: (-v.y * 0.5 + 0.5) * height,
                z: v.z
              };
            }).filter(p => p.z >= -1 && p.z <= 1);
          }
          function emitSelection(ids, method) {
            if (window.Shiny) {
              Shiny.setInputValue('selected_point_ids', ids, { priority: 'event' });
              Shiny.setInputValue('selection_method', method, { priority: 'event' });
            }
          }
          function selectByRect(a, b) {
            const minSX = Math.min(a.x, b.x), maxSX = Math.max(a.x, b.x);
            const minSY = Math.min(a.y, b.y), maxSY = Math.max(a.y, b.y);
            const ids = projectedPoints()
              .filter(p => p.x >= minSX && p.x <= maxSX && p.y >= minSY && p.y <= maxSY)
              .map(p => p.point_id);
            emitSelection(ids, 'box');
          }
          function selectByPolygon(poly, method) {
            if (poly.length < 3) return;
            const ids = projectedPoints().filter(p => pointInPolygon(p, poly)).map(p => p.point_id);
            emitSelection(ids, method);
          }
          function nearestPoint(screenPt) {
            let best = null;
            let bestDist = Infinity;
            projectedPoints().forEach(pp => {
              const dx = pp.x - screenPt.x, dy = pp.y - screenPt.y;
              const dist = Math.sqrt(dx * dx + dy * dy);
              if (dist < bestDist) {
                bestDist = dist;
                best = pp;
              }
            });
            if (!best || bestDist > 18) return null;
            return points[best.idx];
          }
          function drawMeasurementLine() {
            overlayCtx.clearRect(0, 0, width, height);
            if (measurementPoints.length === 0) return;
            const projected = measurementPoints.map(p => {
              const coords = pos(p);
              const v = new THREE.Vector3(coords[0], coords[1], coords[2]).project(camera);
              return { x: (v.x * 0.5 + 0.5) * width, y: (-v.y * 0.5 + 0.5) * height };
            });
            overlayCtx.strokeStyle = '#38bdf8';
            overlayCtx.fillStyle = '#38bdf8';
            overlayCtx.lineWidth = 2;
            projected.forEach(pt => {
              overlayCtx.beginPath();
              overlayCtx.arc(pt.x, pt.y, 5, 0, Math.PI * 2);
              overlayCtx.fill();
            });
            if (projected.length === 2) {
              overlayCtx.beginPath();
              overlayCtx.moveTo(projected[0].x, projected[0].y);
              overlayCtx.lineTo(projected[1].x, projected[1].y);
              overlayCtx.stroke();
            }
          }

          renderer.domElement.addEventListener('click', function(event) {
            const rect = renderer.domElement.getBoundingClientRect();
            const screenPt = { x: event.clientX - rect.left, y: event.clientY - rect.top };
            if (mode === 'Polygon selection') {
              polygonPath.push(screenPt);
              drawPath(polygonPath, false);
              return;
            }
            if (mode === 'Measure distance') {
              const p = nearestPoint(screenPt);
              if (!p) return;
              measurementPoints.push(p);
              if (measurementPoints.length > 2) measurementPoints = [p];
              drawMeasurementLine();
              if (measurementPoints.length === 2 && window.Shiny) {
                Shiny.setInputValue('distance_measurement', {
                  point_id_1: measurementPoints[0].point_id,
                  point_id_2: measurementPoints[1].point_id,
                  distance_m: originalDistance(measurementPoints[0], measurementPoints[1])
                }, { priority: 'event' });
              }
              return;
            }
            if (mode === 'Inspect trees') {
              mouse.x = ((event.clientX - rect.left) / rect.width) * 2 - 1;
              mouse.y = -((event.clientY - rect.top) / rect.height) * 2 + 1;
              raycaster.setFromCamera(mouse, camera);
              const hits = raycaster.intersectObjects(treeMeshes);
              if (hits.length > 0) {
                treeMeshes.forEach(m => m.material.color.set(0xfacc15));
                hits[0].object.material.color.set(0xef4444);
                if (window.Shiny) {
                  Shiny.setInputValue('selected_tree_id', hits[0].object.userData.tree_id, { priority: 'event' });
                }
              }
            }
          });
          renderer.domElement.addEventListener('dblclick', function(event) {
            if (mode === 'Polygon selection' && polygonPath.length >= 3) {
              selectByPolygon(polygonPath, 'polygon');
              drawPath(polygonPath, true);
              polygonPath = [];
            }
          });
          renderer.domElement.addEventListener('pointerdown', function(event) {
            if (!isSelectionMode || mode === 'Polygon selection') return;
            const rect = renderer.domElement.getBoundingClientRect();
            startPoint = { x: event.clientX - rect.left, y: event.clientY - rect.top };
            path = [startPoint];
            isDrawing = true;
          });
          renderer.domElement.addEventListener('pointermove', function(event) {
            if (!isDrawing || !startPoint) return;
            const rect = renderer.domElement.getBoundingClientRect();
            const current = { x: event.clientX - rect.left, y: event.clientY - rect.top };
            if (mode === 'Box selection') {
              drawOverlayRect(startPoint, current);
            } else {
              path.push(current);
              drawPath(path, false);
            }
          });
          renderer.domElement.addEventListener('pointerup', function(event) {
            if (!isDrawing || !startPoint) return;
            const rect = renderer.domElement.getBoundingClientRect();
            const current = { x: event.clientX - rect.left, y: event.clientY - rect.top };
            if (mode === 'Box selection') {
              drawOverlayRect(startPoint, current);
              selectByRect(startPoint, current);
            } else {
              path.push(current);
              drawPath(path, true);
              selectByPolygon(path, mode === 'Manual crown edit' ? 'manual_crown_lasso' : 'lasso');
            }
            isDrawing = false;
            startPoint = null;
          });

          // Live scale bar: project two world points 1 metre apart at the
          // controls target depth, measure the pixel distance between
          // their screen projections, pick a nice round meter value
          // (1/2/5/10/20/50/100/...) for a ~110px bar, and update the
          // bottom-left overlay DOM. Runs in the animation loop so it
          // stays in sync with both camera moves and resize.
          function pickNiceMeters(targetPixels, pxPerMeter) {
            if (!isFinite(pxPerMeter) || pxPerMeter <= 0) return null;
            var meters = targetPixels / pxPerMeter;
            var pow = Math.pow(10, Math.floor(Math.log10(meters)));
            var rel = meters / pow;
            var nice;
            if      (rel < 1.5) nice = 1;
            else if (rel < 3.5) nice = 2;
            else if (rel < 7.5) nice = 5;
            else                nice = 10;
            return nice * pow;
          }
          var lastScaleEmit = 0;
          function updateScaleBar(now) {
            // Throttle DOM writes to ~10 Hz; the animation runs at 60 Hz
            // but the scale bar does not need that frequency.
            if (now - lastScaleEmit < 100) return;
            lastScaleEmit = now;
            var canvas = renderer.domElement;
            var rect = canvas.getBoundingClientRect();
            if (rect.width <= 0 || rect.height <= 0) return;
            var pA = controls ? controls.target.clone() : defaultTarget.clone();
            var pB = pA.clone().add(new THREE.Vector3(1, 0, 0));
            var nA = pA.clone().project(camera);
            var nB = pB.clone().project(camera);
            var dx = (nB.x - nA.x) * 0.5 * rect.width;
            var dy = (nB.y - nA.y) * 0.5 * rect.height;
            var pxPerMeter = Math.hypot(dx, dy);
            var nice = pickNiceMeters(110, pxPerMeter);
            if (nice === null) return;
            var barEl   = document.querySelector('.viewer-overlay.scale .scale-bar');
            var labelEl = document.getElementById('viewer_scale_label');
            if (barEl)   barEl.style.width = (nice * pxPerMeter).toFixed(0) + 'px';
            if (labelEl) labelEl.textContent =
              nice >= 1000 ? (nice / 1000) + ' km' : nice + ' m';
          }

          function animate() {
            requestAnimationFrame(animate);
            if (controls) controls.update();
            renderer.render(scene, camera);
            updateScaleBar(performance.now());
            if (window.__dronebior_viewer && window.__dronebior_viewer.gizmoUpdate) {
              window.__dronebior_viewer.gizmoUpdate();
            }
          }
          animate();
        })();
      "
        viewer_script <- gsub("__POINT_JSON__", point_json, viewer_script, fixed = TRUE)
        viewer_script <- gsub("__TREE_JSON__", tree_json, viewer_script, fixed = TRUE)
        viewer_script <- gsub("__MODE_JSON__", mode_json, viewer_script, fixed = TRUE)
        viewer_script <- gsub("__CAMERA_STATE_JSON__", camera_state_json, viewer_script, fixed = TRUE)
        viewer_script <- gsub("__BASEMAP_JSON__", basemap_json, viewer_script, fixed = TRUE)
        viewer_script <- gsub("__MESH_URL_JSON__", mesh_url_json, viewer_script, fixed = TRUE)
        viewer_script <- gsub("__DRAPED_DSM_JSON__", draped_dsm_json, viewer_script, fixed = TRUE)
        viewer_script
      }))
    )
  })
  # Keep the 3D viewer alive when the user navigates away to another main
  # tab. Without this, Shiny suspends the renderUI as soon as its DOM is
  # hidden; coming back to the 3D Modeling tab re-evaluates ALL its
  # reactive dependencies and rebuilds the entire three.js scene from
  # scratch - the symptom the user described as "every minute the view
  # redraws from a different plane." The viewer is cheap to keep mounted
  # because three.js's animate loop pauses naturally when the canvas is
  # not visible.
  outputOptions(output, "point_cloud_viewer", suspendWhenHidden = FALSE)

  output$point_cloud_status <- renderText({
    if (point_cloud_event() == 0) {
      return("No 3D scene loaded.")
    }
    paste0(
      format(nrow(display_points()), big.mark = ","),
      " visible of ",
      format(nrow(point_cloud()), big.mark = ","),
      " loaded points; ",
      format(length(selected_point_ids()), big.mark = ","),
      " selected."
    )
  })

  # ---- 3D Modeling: top metric strip -------------------------------------
  # Live numeric counts for the current scene. Each metric is a small UI
  # output so we can render bold numbers + a muted sub-line (e.g. "of N
  # sampled") without hand-coding strings on every reactive change.
  modeling_scene_descriptors <- reactive({
    if (point_cloud_event() == 0) {
      return(list(
        scene_loaded = FALSE,
        visible      = 0L,
        loaded       = 0L,
        selected     = 0L,
        trees        = 0L,
        source_path  = "",
        coord_source = "",
        height_src   = ""
      ))
    }
    pts <- tryCatch(point_cloud(), error = function(e) NULL)
    disp <- tryCatch(display_points(), error = function(e) NULL)
    trees <- tryCatch(tree_candidates(), error = function(e) data.frame())
    list(
      scene_loaded = TRUE,
      visible      = if (is.null(disp)) 0L else nrow(disp),
      loaded       = if (is.null(pts)) 0L else nrow(pts),
      selected     = length(selected_point_ids()),
      trees        = nrow(trees),
      source_path  = attr(pts, "point_cloud_source") %||% "",
      coord_source = attr(pts, "coordinate_source") %||% "",
      height_src   = attr(pts, "height_source") %||% ""
    )
  })

  output$modeling_metric_points <- renderUI({
    d <- modeling_scene_descriptors()
    if (!isTRUE(d$scene_loaded)) return(HTML("&mdash;"))
    HTML(paste0(format(d$visible, big.mark = ","),
                " / ",
                format(d$loaded, big.mark = ",")))
  })
  output$modeling_metric_points_sub <- renderUI({
    d <- modeling_scene_descriptors()
    if (!isTRUE(d$scene_loaded)) return("Load the 3D scene to see live counts")
    coord <- if (identical(d$coord_source, "full_georeferenced")) "georeferenced" else "PLY preview"
    HTML(paste0("visible / loaded &middot; ", coord))
  })

  output$modeling_metric_selected <- renderUI({
    d <- modeling_scene_descriptors()
    if (!isTRUE(d$scene_loaded)) return(HTML("&mdash;"))
    HTML(format(d$selected, big.mark = ","))
  })
  output$modeling_metric_selected_sub <- renderUI({
    d <- modeling_scene_descriptors()
    if (!isTRUE(d$scene_loaded)) return("0 points")
    method <- input$selection_method %||% ""
    method_label <- switch(method,
      box                 = "box selection",
      lasso               = "lasso selection",
      polygon             = "polygon selection",
      manual_crown_lasso  = "manual crown lasso",
      if (d$selected == 0) "no active selection" else "active selection"
    )
    HTML(paste0(method_label, " &middot; ", input$selection_tool %||% ""))
  })

  output$modeling_metric_trees <- renderUI({
    d <- modeling_scene_descriptors()
    if (!isTRUE(d$scene_loaded)) return(HTML("&mdash;"))
    HTML(format(d$trees, big.mark = ","))
  })
  output$modeling_metric_trees_sub <- renderUI({
    d <- modeling_scene_descriptors()
    if (!isTRUE(d$scene_loaded)) return("Tree candidates from canopy grid")
    HTML(paste0("min canopy ", formatC(input$min_tree_height %||% 0, format = "f", digits = 1),
                " m &middot; grid ", formatC(input$tree_grid %||% 0, format = "f", digits = 1), " m"))
  })

  output$modeling_metric_surface <- renderUI({
    layers <- character()
    if (isTRUE(input$show_draped_dsm))    layers <- c(layers, "Draped DSM")
    if (isTRUE(input$show_textured_mesh)) layers <- c(layers, "Textured mesh")
    if (length(layers) == 0) {
      return(HTML("Points only"))
    }
    paste(layers, collapse = " + ")
  })
  output$modeling_metric_surface_sub <- renderUI({
    products <- cached_products()
    ortho_ok <- file.exists(input$orthomosaic) || file.exists(products[["orthomosaic"]])
    dsm_ok   <- file.exists(products[["dsm"]])
    mesh_ok  <- file.exists(products[["textured_obj"]])
    bits <- c(
      if (ortho_ok) "ortho" else NULL,
      if (dsm_ok)   "DSM"   else NULL,
      if (mesh_ok)  "OBJ"   else NULL
    )
    if (length(bits) == 0) return("No surface products yet")
    HTML(paste0("available: ", paste(bits, collapse = ", ")))
  })

  # ---- 3D Modeling: Scene sources card -----------------------------------
  # One status pill per ODM product family, plus a one-line summary header.
  # Drives the cross-tab feedback: as soon as Processing Engine finishes,
  # or as soon as the user changes the orthomosaic path on the GIS
  # Workspace tab, the pills here flip from Missing to Available without
  # any extra action.
  scene_sources_state <- reactive({
    products <- cached_products()
    list(
      ortho = file.exists(input$orthomosaic) || file.exists(products[["orthomosaic"]]),
      dsm   = file.exists(products[["dsm"]]),
      dtm   = file.exists(products[["dtm"]]),
      cloud = any(file.exists(unname(products[c("point_cloud_las",
                                                "point_cloud_laz",
                                                "point_cloud_copc")]))),
      mesh  = file.exists(products[["textured_obj"]])
    )
  })

  output$scene_source_ortho <- renderUI(status_badge(scene_sources_state()$ortho, "Available", "Missing"))
  output$scene_source_dsm   <- renderUI(status_badge(scene_sources_state()$dsm,   "Available", "Missing"))
  output$scene_source_dtm   <- renderUI(status_badge(scene_sources_state()$dtm,   "Available", "Missing"))
  output$scene_source_cloud <- renderUI(status_badge(scene_sources_state()$cloud, "Available", "Missing"))
  output$scene_source_mesh  <- renderUI(status_badge(scene_sources_state()$mesh,  "Available", "Missing"))

  output$scene_sources_status <- renderText({
    s <- scene_sources_state()
    have <- sum(unlist(s))
    total <- length(s)
    if (have == 0) return("No sources yet - run Processing Engine or point the project to existing products.")
    if (have == total) return("All scene sources are ready.")
    paste0(have, " of ", total, " scene sources ready.")
  })

  # ---- 3D Modeling: viewer scene-info overlay ----------------------------
  output$viewer_info_source <- renderText({
    d <- modeling_scene_descriptors()
    if (!isTRUE(d$scene_loaded)) return("no scene loaded")
    if (nzchar(d$source_path)) basename(d$source_path) else "(unknown)"
  })
  output$viewer_info_heights <- renderText({
    d <- modeling_scene_descriptors()
    if (!isTRUE(d$scene_loaded)) return("-")
    if (nzchar(d$height_src)) d$height_src else "local low-Z proxy"
  })
  output$viewer_info_crs <- renderText({
    d <- modeling_scene_descriptors()
    if (!isTRUE(d$scene_loaded)) return("-")
    if (identical(d$coord_source, "full_georeferenced")) "projected metres" else "PLY local frame"
  })

  # ---- 3D Modeling: Spectral signature tab -------------------------------
  # Mirrors the per-tree / per-ROI spectral table produced in Spectral
  # Analytics. The same reactiveVal feeds both views so the 3D tab does
  # not need its own compute button - users run the computation where
  # the inputs live, then see the rows here in 3D context.
  output$modeling_tree_spectral <- renderTable({
    x <- tree_spectral_metrics_value()
    if (is.null(x) || nrow(x) == 0) {
      return(data.frame(
        info = paste0(
          "No tree/ROI spectral metrics yet. Open Spectral Analytics, ",
          "click 'Compute tree / ROI spectral metrics', then return here."
        ),
        stringsAsFactors = FALSE,
        check.names = FALSE
      ))
    }
    x
  }, digits = 2)

  # ---- 3D Modeling: pull a GIS Workspace ROI into the 3D selection -------
  # The GIS Workspace lets users draw polygons on the orthomosaic and
  # save them as named ROIs. Here we let the user pick any of those
  # named ROIs and convert it back into a 3D selection: the polygon is
  # projected into the same x/y frame the preview points use, then
  # point-in-polygon picks which preview points fall inside. Driving the
  # 3D selection by GIS Workspace polygons is the most common cross-tab
  # request - the user has already drawn the right region on the map.
  observe({
    rois <- roi_collection()
    # Keep the previously-selected ROI focused when the user adds another
    # one on the GIS Workspace tab; only pre-select the newest ROI when
    # the dropdown is currently empty. Drives the cross-tab ergonomics
    # the user expects: pick on the map, switch tab, the right ROI is
    # already chosen here.
    current <- isolate(input$gis_roi_to_3d) %||% ""
    keep <- nzchar(current) && current %in% names(rois)
    pick <- if (keep) current
            else if (length(rois) > 0) names(rois)[length(rois)]
            else NULL
    updateSelectInput(
      session,
      "gis_roi_to_3d",
      choices  = if (length(rois) == 0) character(0) else names(rois),
      selected = pick
    )
  })

  observeEvent(input$apply_gis_roi_to_3d, {
    name <- input$gis_roi_to_3d %||% ""
    rois <- roi_collection()
    if (!nzchar(name) || !name %in% names(rois)) {
      showNotification(
        if (length(rois) == 0)
          "No ROIs saved yet. Draw and save one on the GIS Workspace tab first."
        else
          "Pick a saved ROI in the dropdown first (Use GIS Workspace ROI section).",
        type = "warning", duration = 6
      )
      return()
    }
    if (point_cloud_event() == 0) {
      showNotification("Load the 3D scene first (click 'Load 3D scene'), then pull the ROI in.",
                       type = "warning", duration = 5)
      return()
    }
    pts <- tryCatch(point_cloud(), error = function(e) NULL)
    if (is.null(pts) || nrow(pts) == 0) {
      showNotification("Point cloud is empty - nothing to select.",
                       type = "warning", duration = 5)
      return()
    }

    # Reproject the ROI through the ORTHOMOSAIC's CRS. The ROI was drawn
    # on the GIS Workspace orthomosaic layer, so its WGS84 coordinates
    # are anchored to the orthomosaic's georeferencing. Previously this
    # used point_cloud_reference_raster() which prefers the DSM - on
    # projects where DSM and orthomosaic happen to be in different CRSs
    # the selected polygon landed in a slightly different location than
    # the user drew. We fall back to the DSM only when no orthomosaic
    # is available, since SOME reference frame is better than refusing.
    ref_raster <- NULL
    if (file.exists(input$orthomosaic %||% "")) {
      ref_raster <- tryCatch(terra::rast(input$orthomosaic)[[1]], error = function(e) NULL)
    }
    if (is.null(ref_raster)) {
      ref_raster <- point_cloud_reference_raster()
    }
    if (is.null(ref_raster)) {
      showNotification(
        "Project has no orthomosaic or DSM with a known CRS, so the ROI cannot be reprojected.",
        type = "warning", duration = 6)
      return()
    }

    roi <- rois[[name]]
    ll  <- data.frame(lng = roi$lng, lat = roi$lat)
    poly_xy <- tryCatch({
      ll_sf  <- sf::st_as_sf(ll, coords = c("lng", "lat"), crs = 4326)
      proj_s <- sf::st_transform(ll_sf, terra::crs(ref_raster))
      coords <- sf::st_coordinates(proj_s)
      if (points_are_georeferenced(pts)) {
        data.frame(x = coords[, 1], y = coords[, 2])
      } else {
        e  <- terra::ext(ref_raster)
        xr <- range(pts$x, na.rm = TRUE)
        yr <- range(pts$y, na.rm = TRUE)
        denom_x <- as.numeric(e[2] - e[1])
        denom_y <- as.numeric(e[4] - e[3])
        if (!is.finite(denom_x) || denom_x == 0 || !is.finite(denom_y) || denom_y == 0) return(NULL)
        x_local <- xr[1] + (coords[, 1] - as.numeric(e[1])) * diff(xr) / denom_x
        y_local <- yr[1] + (coords[, 2] - as.numeric(e[3])) * diff(yr) / denom_y
        data.frame(x = x_local, y = y_local)
      }
    }, error = function(err) NULL)

    if (is.null(poly_xy) || nrow(poly_xy) < 3) {
      showNotification("Could not project the ROI into the point-cloud frame.",
                       type = "warning", duration = 5)
      return()
    }

    poly_closed <- rbind(poly_xy, poly_xy[1, , drop = FALSE])
    hit_ids <- tryCatch({
      pts_sf  <- sf::st_as_sf(pts[, c("x", "y", "point_id")], coords = c("x", "y"))
      poly_sf <- sf::st_sfc(sf::st_polygon(list(as.matrix(poly_closed))))
      inside  <- sf::st_intersects(pts_sf, poly_sf, sparse = FALSE)[, 1]
      pts$point_id[inside]
    }, error = function(err) integer())

    if (length(hit_ids) == 0) {
      showNotification(paste0("ROI '", name, "' did not intersect any preview points."),
                       type = "warning", duration = 5)
      return()
    }

    selected_ids_value(unique(as.integer(hit_ids)))
    updateTextInput(session, "selection_label", value = name)
    showNotification(
      paste0("ROI '", name, "' applied to 3D selection: ",
             format(length(hit_ids), big.mark = ","), " preview points."),
      type = "message", duration = 5
    )
  })

  observeEvent(input$classify_selection, {
    ids <- selected_point_ids()
    if (length(ids) == 0) {
      showNotification("Select points before assigning a class.", type = "warning")
      return()
    }
    classes <- point_classes()
    classes$class[classes$point_id %in% ids] <- input$classification_label
    point_classes(classes)
    showNotification(
      paste(length(ids), "points classified as", input$classification_label),
      type = "message",
      duration = 4
    )
  })

  observeEvent(input$save_manual_crown, {
    pts <- analysis_points()
    if (nrow(pts) == 0) {
      showNotification("Select points before saving a crown ROI.", type = "warning")
      return()
    }
    label <- input$selection_label
    metrics <- selection_metrics()
    row <- data.frame(
      roi_label = label,
      n_points = metrics$n_points,
      footprint_area_m2 = metrics$footprint_area_m2,
      max_crown_diameter_m = metrics$max_crown_diameter_m,
      height_max_m = metrics$height_max_m,
      occupied_volume_m3 = metrics$occupied_volume_m3,
      chm_surface_volume_m3 = if ("chm_surface_volume_m3" %in% names(metrics)) metrics$chm_surface_volume_m3 else NA_real_,
      point_source = metrics$point_source,
      height_source = metrics$height_source,
      preview_point_ids = paste(selected_point_ids(), collapse = ";"),
      updated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      stringsAsFactors = FALSE
    )
    current <- manual_crowns()
    if (nrow(current) > 0) {
      current <- current[current$roi_label != label, , drop = FALSE]
    }
    manual_crowns(rbind(current, row))
    showNotification(paste("Saved ROI", label), type = "message", duration = 4)
  })

  observeEvent(input$delete_manual_crown, {
    label <- input$selection_label
    current <- manual_crowns()
    if (nrow(current) == 0 || !label %in% current$roi_label) {
      showNotification("No saved ROI with that label.", type = "warning")
      return()
    }
    manual_crowns(current[current$roi_label != label, , drop = FALSE])
    showNotification(paste("Deleted ROI", label), type = "message", duration = 4)
  })

  observeEvent(input$export_selection, {
    pts <- analysis_points()
    if (nrow(pts) == 0) {
      showNotification("Select points before exporting.", type = "warning")
      return()
    }
    out_dir <- file.path(input$output_dir, "selections")
    paths <- export_point_selection(
      points = pts,
      metrics = selection_metrics(),
      profile = vertical_profile(),
      output_dir = out_dir,
      label = input$selection_label,
      roi_polygon = selection_roi_analysis()
    )
    if (nrow(manual_crowns()) > 0) {
      manual_path <- file.path(out_dir, "manual_crown_rois.csv")
      utils::write.csv(manual_crowns(), manual_path, row.names = FALSE)
      paths <- c(paths, manual_crowns = manual_path)
    }
    selection_export_paths(paths)
    showNotification("Selection exported.", type = "message", duration = 4)
  })

  output$selection_metrics <- renderTable({
    format_selection_metrics(selection_metrics())
  }, digits = 2)

  # 3D viewer remote controls. Buttons in the top toolbar send custom
  # messages to the JS handlers registered in tags$head, which in turn
  # nudge the OrbitControls camera exposed via window.__dronebior_viewer.
  observeEvent(input$reset_3d_view, {
    session$sendCustomMessage("dronebior_3d_reset", list())
  })
  observeEvent(input$zoom_in_3d, {
    session$sendCustomMessage("dronebior_3d_zoom", list(factor = 0.8))
  })
  observeEvent(input$zoom_out_3d, {
    session$sendCustomMessage("dronebior_3d_zoom", list(factor = 1.25))
  })

  observeEvent(input$screenshot_3d, {
    session$sendCustomMessage("dronebior_3d_screenshot",
                              list(label = input$selection_label %||% "dronebior_3d_view"))
  })
  observeEvent(input$export_3d_gltf, {
    session$sendCustomMessage("dronebior_3d_export_gltf",
                              list(label = input$selection_label %||% "dronebior_3d_scene"))
  })

  # Camera presets - all go through the same JS handler, which calls
  # window.__dronebior_viewer.cameraPreset(name).
  observeEvent(input$cam_top,   { session$sendCustomMessage("dronebior_3d_camera_preset", list(name = "top")) })
  observeEvent(input$cam_front, { session$sendCustomMessage("dronebior_3d_camera_preset", list(name = "front")) })
  observeEvent(input$cam_side,  { session$sendCustomMessage("dronebior_3d_camera_preset", list(name = "side")) })
  observeEvent(input$cam_iso,   { session$sendCustomMessage("dronebior_3d_camera_preset", list(name = "iso")) })
  observeEvent(input$cam_frame, { session$sendCustomMessage("dronebior_3d_camera_preset", list(name = "frame")) })

  # Live layer visibility from the checkboxGroupInput. We send ONE
  # message per layer so the JS handler can flip the .visible flag
  # of the matching scene.getObjectByName() target. The mapping
  # UI-label -> three.js object name lives here so the JS does not
  # need to know about UI strings.
  observe({
    selected <- input$modeling_layers %||% character(0)
    layer_map <- list(
      "Points"          = c("points"),
      "Selected"        = c("selected_points", "selection_bbox"),
      "DSM drape"       = c("dsm_drape", "basemap_plane"),
      "Textured mesh"   = c("textured_mesh"),
      "Trees"           = c("trees"),
      "Grid"            = c("grid"),
      "Axes"            = c("axes")
    )
    for (label in names(layer_map)) {
      for (obj_name in layer_map[[label]]) {
        session$sendCustomMessage(
          "dronebior_3d_set_layer",
          list(name = obj_name, visible = label %in% selected)
        )
      }
    }
  })

  # Live point size + background theme. Both go through messages that
  # mutate the existing PointsMaterial / scene.background in place -
  # no point cloud rebuild, no flicker, instant response.
  observe({
    pct <- input$point_size_pct %||% 100
    session$sendCustomMessage("dronebior_3d_set_point_size", list(pct = pct))
  })
  observe({
    theme <- input$viewer_bg_theme %||% "dark"
    session$sendCustomMessage("dronebior_3d_set_bg", list(theme = theme))
  })

  # Cancel current selection: clears the overlay drawing in the
  # viewer (in-progress lasso / polygon) and clears the selected
  # point IDs on the R side. Triggered by toolbar button OR by the
  # Esc key (JS keyboard handler bumps input$dronebior_3d_esc).
  observeEvent(list(input$cancel_3d_selection, input$dronebior_3d_esc), {
    if ((input$cancel_3d_selection %||% 0L) == 0L &&
        is.null(input$dronebior_3d_esc)) return()
    session$sendCustomMessage("dronebior_3d_cancel", list())
    selected_ids_value(integer())
  }, ignoreInit = TRUE)

  # The top-left mode badge in the viewer-frame: name of the active
  # tool plus a small keyboard / mouse hint block. Re-renders on
  # input$selection_tool change; lightweight because it does not
  # touch three.js at all.
  output$viewer_mode_badge_content <- renderUI({
    tool <- input$selection_tool %||% "Box selection"
    hints <- switch(tool,
      "Inspect trees"     = "Click a tree marker for height/crown stats. Left=orbit, Middle=pan, Right=orbit.",
      "Box selection"     = "Left drag = box. Right drag orbits, Middle drag pans.",
      "Lasso selection"   = "Left drag = freehand. Right drag orbits, Middle drag pans.",
      "Polygon selection" = "Left click = add vertex. Double-click = close. Esc cancels. Right=orbit.",
      "Measure distance"  = "Click two points to measure their 3D distance in metres.",
      "Manual crown edit" = "Lasso a crown; Save/update crown ROI in the sidebar to persist.",
      "Left=orbit, Middle=pan, Right=orbit."
    )
    tagList(
      tags$div(class = "mode-name", tool),
      tags$div(class = "mode-hints",
               hints,
               tags$br(),
               tags$kbd("F"), " frame  ",
               tags$kbd("7"), " top  ",
               tags$kbd("1"), " front  ",
               tags$kbd("3"), " side  ",
               tags$kbd("5"), " iso  ",
               tags$kbd("Esc"), " cancel")
    )
  })

  # Dynamic legend driven by the currently selected point symbology.
  viewer_legend_info <- reactive({
    mode <- input$point_color_mode %||% "RGB"
    pts <- tryCatch(point_cloud(), error = function(e) NULL)
    if (identical(mode, "RGB") || is.null(pts) || nrow(pts) == 0) {
      return(list(
        heading = paste0("Point color: ", mode),
        min_str = "",
        max_str = ""
      ))
    }
    if (identical(mode, "Height")) {
      z <- if ("height_m" %in% names(pts)) pts$height_m else pts$z
      z <- z[is.finite(z)]
      if (length(z) == 0) {
        return(list(heading = "Height (no data)", min_str = "", max_str = ""))
      }
      return(list(
        heading = "Height above ground (m)",
        min_str = formatC(min(z), format = "f", digits = 2),
        max_str = formatC(max(z), format = "f", digits = 2)
      ))
    }
    if (identical(mode, "Classification")) {
      return(list(
        heading = "Point classification",
        min_str = "(see palette in 3D tools)",
        max_str = ""
      ))
    }
    list(heading = mode, min_str = "", max_str = "")
  })
  output$viewer_legend_heading <- renderText(viewer_legend_info()$heading)
  output$viewer_legend_min     <- renderText(viewer_legend_info()$min_str)
  output$viewer_legend_max     <- renderText(viewer_legend_info()$max_str)

  # Survey-grade volume metrics over the convex hull of the currently
  # selected points. Method, base plane and ground quantile come from
  # the sidebar; we just route them into compute_survey_volumes() and
  # format the resulting list as a small table.
  survey_volume_result <- reactive({
    if (point_cloud_event() == 0) return(NULL)
    pts <- selected_points()
    if (is.null(pts) || nrow(pts) < 3) return(NULL)

    method <- input$survey_volume_method %||% "dtm"

    # ROI = convex hull of selected points, as a closed polygon in the
    # CRS of the project rasters. Selection points are already in that
    # CRS (they come from the LAS / PLY readers that preserve coords).
    hull_idx <- grDevices::chull(pts$x, pts$y)
    roi <- data.frame(x = pts$x[hull_idx], y = pts$y[hull_idx])

    top <- dsm_raster()
    if (is.null(top)) return(NULL)

    with_error_toast("Compute survey volumes", {
      compute_survey_volumes(
        top              = top,
        roi              = roi,
        method           = method,
        dtm              = if (identical(method, "dtm")) dtm_raster() else NULL,
        base_z           = input$survey_user_plane_z,
        ground_quantile  = input$survey_ground_quantile
      )
    })
  })

  output$survey_volume_table <- renderTable({
    res <- survey_volume_result()
    if (is.null(res)) {
      return(data.frame(
        metric = "status",
        value  = "Select at least 3 points in the 3D viewer. The DSM (or DSM + DTM) must be available in the project.",
        units  = ""
      ))
    }
    fmt <- function(x, digits = 2) {
      if (!is.finite(x)) return("-") else formatC(x, format = "f", digits = digits)
    }
    data.frame(
      metric = c(
        "Base reference",
        "Cut volume",
        "Fill volume",
        "Net volume",
        "Planar area",
        "Draped (3D) area",
        "Perimeter",
        "Cells inside ROI",
        "Cell area",
        "Top z (mean / max)",
        "Base z (mean)",
        "Height above base (mean / max)"
      ),
      value = c(
        res$base_reference_text,
        fmt(res$cut_volume_m3),
        fmt(res$fill_volume_m3),
        fmt(res$net_volume_m3),
        fmt(res$surface_area_planar_m2, 2),
        fmt(res$surface_area_draped_m2, 2),
        fmt(res$perimeter_m, 2),
        as.character(res$cell_count),
        fmt(res$cell_area_m2, 4),
        paste0(fmt(res$top_z_summary[["mean"]]), " / ", fmt(res$top_z_summary[["max"]])),
        fmt(res$base_z_summary[["mean"]]),
        paste0(fmt(res$height_summary[["mean"]]), " / ", fmt(res$height_summary[["max"]]))
      ),
      units = c("", "m^3", "m^3", "m^3", "m^2", "m^2", "m", "count", "m^2", "m", "m", "m"),
      stringsAsFactors = FALSE
    )
  }, digits = 2)

  output$vertical_profile_plot <- renderPlot({
    profile <- vertical_profile()
    validate(need(nrow(profile) > 0, "Select points to build a vertical profile."))
    barplot(
      profile$point_count,
      names.arg = paste0(profile$bin_bottom_m, "-", profile$bin_top_m),
      las = 2,
      col = "#1f6f5b",
      border = NA,
      xlab = "Height bin (m)",
      ylab = "Point count",
      main = "Vertical point profile"
    )
  })

  output$classification_summary <- renderTable({
    req(classified_points())
    total <- as.data.frame(table(classified_points()$class), stringsAsFactors = FALSE)
    names(total) <- c("class", "total_points")
    selected <- selected_points()
    if (nrow(selected) > 0) {
      selected_counts <- as.data.frame(table(selected$class), stringsAsFactors = FALSE)
      names(selected_counts) <- c("class", "selected_points")
      total <- merge(total, selected_counts, by = "class", all.x = TRUE)
      total$selected_points[is.na(total$selected_points)] <- 0
    } else {
      total$selected_points <- 0
    }
    total$color <- unname(classification_palette[total$class])
    total
  }, digits = 2)

  output$distance_measurement <- renderTable({
    d <- input$distance_measurement
    if (is.null(d)) {
      return(data.frame(metric = "distance", value = "No measurement", unit = "m"))
    }
    data.frame(
      metric = c("point 1", "point 2", "3D distance"),
      value = c(d$point_id_1, d$point_id_2, formatC(d$distance_m, format = "f", digits = 2)),
      unit = c("point_id", "point_id", "m"),
      check.names = FALSE
    )
  }, digits = 2)

  output$full_roi_status <- renderText({
    full_roi_status_value()
  })

  output$manual_crowns_table <- renderTable({
    x <- manual_crowns()
    if (nrow(x) == 0) {
      return(data.frame(message = "No manual crown/ROI saved yet."))
    }
    data.frame(
      roi_label = x$roi_label,
      n_points = x$n_points,
      `footprint area (m2)` = formatC(x$footprint_area_m2, format = "f", digits = 2),
      `max crown diameter (m)` = formatC(x$max_crown_diameter_m, format = "f", digits = 2),
      `height max (m)` = formatC(x$height_max_m, format = "f", digits = 2),
      `occupied volume (m3)` = formatC(x$occupied_volume_m3, format = "f", digits = 2),
      `CHM surface volume (m3)` = formatC(x$chm_surface_volume_m3, format = "f", digits = 2),
      point_source = x$point_source,
      height_source = x$height_source,
      updated_at = x$updated_at,
      check.names = FALSE
    )
  }, digits = 2)

  output$selection_export_paths <- renderText({
    paths <- selection_export_paths()
    if (length(paths) == 0) return("")
    paste(paths, collapse = "\n")
  })

  output$point_cloud_context_map <- renderLeaflet({
    base <- leaflet(options = leafletOptions(
        worldCopyJump = FALSE,
        minZoom = 2
      )) |>
      addMapPane("localOrthomosaicFallback", zIndex = 150) |>
      addMapPane("treeCandidates", zIndex = 420) |>
      addMapPane("classifiedPoints", zIndex = 440) |>
      addMapPane("selectionRoi", zIndex = 460) |>
      add_esri_imagery_tiles(group = "Satellite") |>
      addProviderTiles(providers$CartoDB.Positron, group = "Light basemap",
                       options = providerTileOptions(
                         noWrap = TRUE,
                         bounds = list(c(-85, -180), c(85, 180))
                       ))

    if (!file.exists(input$orthomosaic)) {
      return(base |>
        addLayersControl(
          baseGroups = c("Satellite", "Light basemap"),
          options = layersControlOptions(collapsed = FALSE)
        ))
    }

    ortho_for_context <- orthomosaic_raster_cached()
    if (is.null(ortho_for_context)) {
      return(base |>
        addLayersControl(
          baseGroups = c("Satellite", "Light basemap"),
          options = layersControlOptions(collapsed = FALSE)
        ))
    }
    bounds <- raster_bounds_4326(ortho_for_context)
    local_ortho <- context_orthomosaic()
    if (!is.null(local_ortho)) {
      base <- base |>
        addRasterImage(
          local_ortho,
          opacity = 0.95,
          project = TRUE,
          group = "Local orthomosaic",
          maxBytes = 8 * 1024 * 1024,
          options = gridOptions(pane = "localOrthomosaicFallback", zIndex = 150)
        )
    }
    base <- base |>
      addRectangles(
        lng1 = bounds[["lng1"]],
        lat1 = bounds[["lat1"]],
        lng2 = bounds[["lng2"]],
        lat2 = bounds[["lat2"]],
        color = "#facc15",
        weight = 2,
        fill = FALSE,
        group = "Orthomosaic footprint",
        popup = "Orthomosaic footprint"
      ) |>
      fitBounds(bounds[["lng1"]], bounds[["lat1"]], bounds[["lng2"]], bounds[["lat2"]])

    base |>
      addLayersControl(
        baseGroups = c("Satellite", "Light basemap"),
        overlayGroups = c("Local orthomosaic", "Orthomosaic footprint", "Classified points", "Selected ROI", "Tree candidates"),
        options = layersControlOptions(collapsed = TRUE)
      )
  })
  outputOptions(output, "point_cloud_context_map", suspendWhenHidden = FALSE)

  observeEvent(input$recenter_context_map, {
    fit_leaflet_to_orthomosaic("point_cloud_context_map", input$orthomosaic)
  }, ignoreInit = TRUE)

  observe({
    leafletProxy("point_cloud_context_map") |> clearGroup("Selected ROI")
    if (point_cloud_event() == 0 || !file.exists(input$orthomosaic)) {
      return()
    }

    roi <- selection_roi_analysis()
    if (nrow(roi) < 3) {
      return()
    }

    ortho_for_context <- orthomosaic_raster_cached()
    if (is.null(ortho_for_context)) return()
    roi_ll <- transform_xy_to_wgs84(data.frame(x_map = roi$x, y_map = roi$y), ortho_for_context)
    roi_ll <- rbind(roi_ll, roi_ll[1, , drop = FALSE])
    leafletProxy("point_cloud_context_map") |>
      addPolygons(
        lng = roi_ll$lng,
        lat = roi_ll$lat,
        color = "#38bdf8",
        weight = 2,
        fillColor = "#38bdf8",
        fillOpacity = 0.14,
        group = "Selected ROI",
        options = pathOptions(pane = "selectionRoi"),
        popup = "Selected ROI used for full-resolution metrics"
      )
  })

  observe({
    proxy <- leafletProxy("point_cloud_context_map") |>
      clearGroup("Classified points") |>
      removeControl("classification_legend")
    if (point_cloud_event() == 0 || !file.exists(input$orthomosaic)) {
      return()
    }

    pts <- display_points()
    pts <- pts[!is.na(pts$class) & pts$class != "Unclassified", , drop = FALSE]
    if (nrow(pts) == 0) {
      return()
    }

    ortho_for_context <- orthomosaic_raster_cached()
    if (is.null(ortho_for_context)) return()
    mapped <- points_to_map_xy(pts$x, pts$y, point_cloud(), ortho_for_context)
    mapped <- transform_xy_to_wgs84(mapped, ortho_for_context)
    pts$lng <- mapped$lng
    pts$lat <- mapped$lat
    pts <- pts[is.finite(pts$lng) & is.finite(pts$lat), , drop = FALSE]
    if (nrow(pts) == 0) {
      return()
    }

    class_counts <- as.data.frame(table(pts$class), stringsAsFactors = FALSE)
    names(class_counts) <- c("class", "n")
    class_counts$color <- unname(classification_palette[class_counts$class])
    draw_pts <- sample_context_points(pts)

    proxy |>
      addCircleMarkers(
        data = draw_pts,
        lng = ~lng,
        lat = ~lat,
        radius = 3,
        color = ~class_color,
        weight = 1,
        opacity = 0.95,
        fillColor = ~class_color,
        fillOpacity = 0.78,
        group = "Classified points",
        options = pathOptions(pane = "classifiedPoints"),
        popup = ~paste0(
          "Point ", point_id,
          "<br>Class: ", class,
          "<br>Height: ", round(height_m, 2), " m"
        )
      ) |>
      addControl(
        html = HTML(classification_legend_html(class_counts, nrow(draw_pts), nrow(pts))),
        position = "bottomleft",
        layerId = "classification_legend",
        className = "db-class-legend-control"
      )
  })

  observe({
    leafletProxy("point_cloud_context_map") |> clearGroup("Tree candidates")
    if (point_cloud_event() == 0 || !file.exists(input$orthomosaic)) {
      return()
    }

    trees <- tree_candidates()
    if (is.null(trees) || nrow(trees) == 0) {
      return()
    }

    ortho_for_context <- orthomosaic_raster_cached()
    if (is.null(ortho_for_context)) return()
    mapped <- points_to_map_xy(trees$x, trees$y, point_cloud(), ortho_for_context)
    mapped <- transform_xy_to_wgs84(mapped, ortho_for_context)
    trees$lng <- mapped$lng
    trees$lat <- mapped$lat
    selected_id <- input$selected_tree_id %||% trees$tree_id[[1]]
    trees$is_selected <- trees$tree_id == selected_id

    leafletProxy("point_cloud_context_map") |>
      addCircleMarkers(
        data = trees,
        lng = ~lng,
        lat = ~lat,
        radius = ~pmax(3, pmin(8, sqrt(pmax(crown_diameter_m, 0.2)) * 2.2)),
        color = ~ifelse(is_selected, "#ef4444", "#facc15"),
        fillColor = ~ifelse(is_selected, "#ef4444", "#facc15"),
        fillOpacity = ~ifelse(is_selected, 0.82, 0.32),
        opacity = 0.92,
        weight = 2,
        options = pathOptions(pane = "treeCandidates"),
        popup = ~paste0(
          "Tree ", tree_id,
          "<br>Height: ", round(height_m, 2), " m",
          "<br>Crown diameter: ", round(crown_diameter_m, 2), " m",
          "<br>Crown volume: ", round(crown_volume_m3, 2), " m3"
        ),
        group = "Tree candidates"
      )
  })

  output$tree_metrics <- renderTable({
    req(tree_candidates())
    x <- tree_candidates()
    validate(need(nrow(x) > 0, "No tree candidates matched the current point-count and height thresholds."))
    format_tree_table(x)
  }, digits = 2)

  output$selected_tree <- renderTable({
    req(tree_candidates())
    x <- tree_candidates()
    validate(need(nrow(x) > 0, "No selected tree candidate."))
    id <- input$selected_tree_id %||% x$tree_id[[1]]
    format_tree_table(x[x$tree_id == id, , drop = FALSE])
  }, digits = 2)

  extracted_field <- eventReactive(input$extract_field, {
    with_error_toast("Extract field samples", {
      req(input$field_file, reflectance(), indices())
      field <- read_field_data(input$field_file$datapath)
      predictors <- c(reflectance(), indices())
      extract_field_spectral_data(field, predictors)
    })
  })

  output$field_preview <- renderTable({
    req(extracted_field())
    utils::head(extracted_field(), 20)
  }, digits = 2)

  model <- eventReactive(input$fit_model, {
    with_error_toast("Fit biomass model", {
      req(extracted_field())
      fit_biomass_lm(extracted_field())
    })
  })

  output$model_summary <- renderPrint({
    req(model())
    summary(model())
  })

  workflow <- eventReactive(input$run_workflow, {
    # Snapshot inputs as plain values: the future runs in a worker R session
    # that has no Shiny reactive context.
    project_dir <- input$project_dir
    images_dir  <- input$images_dir
    output_dir  <- input$output_dir
    ortho       <- input$orthomosaic
    use_alpha   <- input$use_alpha

    run_workflow_safely <- function() {
      DroneBioR::configure_proj_database(verbose = FALSE)
      proj <- DroneBioR::dronebio_project(project_dir)
      proj$images_dir <- images_dir
      proj$output_dir <- output_dir
      result <- DroneBioR::run_dronebio_workflow(
        project     = proj,
        orthomosaic = ortho,
        output_dir  = output_dir,
        use_alpha   = use_alpha
      )
      # Return only what the renders need; terra::SpatRaster cannot be
      # serialized cheaply across processes, and the renders do not look
      # at the rasters themselves, only at the written paths.
      list(
        status       = "Analysis completed.",
        output_paths = result$output_paths
      )
    }

    if (isTRUE(.dronebior_async_available)) {
      showNotification(
        "R analysis started in a background process. Switch panels - this notification will go away when it finishes.",
        type     = "message",
        duration = 6
      )
      # `future_promise` returns a promise that downstream renders chain on.
      promises::future_promise({
        suppressWarnings(run_workflow_safely())
      })
    } else {
      with_error_toast("Run DroneBioR workflow", {
        withProgress(message = "Running R analysis", value = 0.1, {
          result <- run_workflow_safely()
          incProgress(1)
          result
        })
      })
    }
  })

  # The renders accept either a resolved value (sync fallback) or a promise
  # (async path). `as_workflow_promise()` normalises them so the rest of
  # the render is identical regardless of mode.
  as_workflow_promise <- function(value) {
    if (isTRUE(.dronebior_async_available)) {
      if (inherits(value, "promise")) return(value)
      return(promises::promise_resolve(value))
    }
    # Without promises installed we just pass the value through as-is.
    value
  }

  output$workflow_status <- renderText({
    result <- workflow()
    if (.dronebior_async_available) {
      promises::then(
        as_workflow_promise(result),
        onFulfilled = function(r) r$status,
        onRejected  = function(e) paste("Workflow failed:", conditionMessage(e))
      )
    } else {
      req(result)
      result$status
    }
  })

  output$workflow_outputs <- renderText({
    result <- workflow()
    if (.dronebior_async_available) {
      promises::then(
        as_workflow_promise(result),
        onFulfilled = function(r) paste(r$output_paths, collapse = "\n"),
        onRejected  = function(e) ""
      )
    } else {
      req(result)
      paste(result$output_paths, collapse = "\n")
    }
  })

  # Report rendering. Uses the bundled biomass_report.Rmd template via
  # render_dronebio_report(). Caches the last produced path so the
  # "Report" card can show it after a successful render.
  report_output_path <- reactiveVal(NULL)
  observeEvent(input$render_report, {
    with_error_toast("Render report", {
      out <- file.path(input$project_dir, "DroneBioR_report.html")
      field_csv <- if (!is.null(input$report_field_csv)) input$report_field_csv$datapath else NULL
      render_dronebio_report(
        project     = project(),
        output_file = out,
        field_csv   = field_csv,
        use_alpha   = isTRUE(input$use_alpha)
      )
      report_output_path(out)
      showNotification(
        paste("Report saved to:", out),
        type     = "message",
        duration = 8
      )
    })
  })

  output$report_status <- renderText({
    path <- report_output_path()
    if (is.null(path)) {
      return("No report rendered yet. Click 'Render HTML report' to produce one.")
    }
    if (!file.exists(path)) {
      return("Report file not found at expected path.")
    }
    paste0(
      "Report:   ", path, "\n",
      "Bytes:    ", format(file.info(path)$size, big.mark = ","), "\n",
      "Modified: ", format(file.info(path)$mtime, "%Y-%m-%d %H:%M:%S")
    )
  })

  # Time-series panel: registry CRUD + metric plot.
  ts_refresh_trigger <- reactiveVal(0L)

  observeEvent(input$ts_register, {
    with_error_toast("Register flight", {
      validate(need(nzchar(input$ts_flight_project_dir),
                    "Enter the project directory for the flight."))
      validate(need(dir.exists(input$ts_flight_project_dir),
                    paste("Project directory not found:", input$ts_flight_project_dir)))
      register_flight(
        date          = input$ts_flight_date,
        project_dir   = input$ts_flight_project_dir,
        notes         = input$ts_flight_notes %||% "",
        registry_path = input$ts_registry_path
      )
      showNotification("Flight registered.", type = "message", duration = 3)
      ts_refresh_trigger(ts_refresh_trigger() + 1L)
    })
  })

  observeEvent(input$ts_clear_registry, {
    if (file.exists(input$ts_registry_path)) {
      unlink(input$ts_registry_path)
    }
    showNotification("Registry cleared.", type = "warning", duration = 3)
    ts_refresh_trigger(ts_refresh_trigger() + 1L)
  })

  observeEvent(input$ts_refresh, {
    ts_refresh_trigger(ts_refresh_trigger() + 1L)
  })

  output$ts_flights_table <- renderTable({
    ts_refresh_trigger()
    list_flights(input$ts_registry_path)
  }, digits = 2)

  output$ts_plot <- renderPlot({
    ts_refresh_trigger()
    fn <- switch(input$ts_metric,
                 ndvi    = flight_ndvi_mean,
                 biomass = flight_biomass_proxy_mean,
                 chm     = flight_chm_mean,
                 flight_ndvi_mean)
    label <- switch(input$ts_metric,
                    ndvi    = "NDVI mean",
                    biomass = "Biomass proxy mean",
                    chm     = "CHM mean (m)",
                    "NDVI mean")
    ts <- flight_time_series(fn, registry_path = input$ts_registry_path)
    if (nrow(ts) == 0) {
      plot.new(); title(main = paste(label, "- register at least one flight first."))
      return(invisible(NULL))
    }
    par(mar = c(4, 4, 2, 1))
    plot(ts$date, ts$value,
         type = "b", pch = 19, col = "#0f766e", lwd = 2, cex = 1.4,
         xlab = "Date", ylab = label,
         main = paste("Time series:", label))
    grid(col = "#e2e8f0")
    if (nrow(ts) > 0) {
      labs <- ts$flight_id
      if (any(nchar(labs) > 14)) labs <- substr(labs, 1, 14)
      text(ts$date, ts$value, labs, pos = 3, cex = 0.75, col = "#475569")
    }
  })
}

shinyApp(ui, server)
