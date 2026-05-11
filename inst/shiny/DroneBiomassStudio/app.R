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

# Cache of (raster identity hash -> tile-friendly local path). Re-rendering
# the map with the same raster reuses the prior temp file so we are not
# rewriting GeoTIFFs on every load.
.dronebior_tile_cache <- new.env(parent = emptyenv())

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
  diverging_idx <- c("NDVI", "NDRE", "NDWI", "GNDVI", "VARI", "Biomass_Index_Proxy")
  bands         <- c("Red", "Green", "Blue", "NIR", "RedEdge", "MSAVI2")
  if (layer_name %in% diverging_idx) return(c(-1, 1))
  if (layer_name %in% bands)         return(c(0, 1))
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

  rows <- vapply(items, function(item) {
    gradient <- paste(item$colors, collapse = ",")
    paste0(
      "<div class='db-legend-row'>",
      "<div class='db-legend-title'>", item$name, "</div>",
      "<div class='db-legend-bar' style='background: linear-gradient(to right,", gradient, ");'></div>",
      "<div class='db-legend-scale'><span>", format(round(item$min, 3), nsmall = 2),
      "</span><span>", format(round(item$max, 3), nsmall = 2), "</span></div>",
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
  NDVI = list(
    label = "NDVI",
    formula = "(NIR - Red) / (NIR + Red)",
    unit = "Unitless index, normally -1 to 1",
    use = "General vegetation vigor and canopy greenness."
  ),
  NDRE = list(
    label = "NDRE",
    formula = "(NIR - RedEdge) / (NIR + RedEdge)",
    unit = "Unitless index, normally -1 to 1",
    use = "Useful for chlorophyll and denser vegetation where NDVI may saturate."
  ),
  EVI = list(
    label = "EVI",
    formula = "2.5 * (NIR - Red) / (NIR + 6 * Red - 7.5 * Blue + 1)",
    unit = "Unitless index; values can exceed -1 to 1 in real imagery",
    use = "Vegetation vigor with atmospheric/background adjustment terms."
  ),
  SAVI = list(
    label = "SAVI",
    formula = "1.5 * (NIR - Red) / (NIR + Red + 0.5)",
    unit = "Unitless index, usually near -1 to 1",
    use = "Vegetation index with soil background adjustment."
  ),
  NDWI = list(
    label = "NDWI",
    formula = "(Green - NIR) / (Green + NIR)",
    unit = "Unitless index, normally -1 to 1",
    use = "Water/moisture contrast using green and NIR reflectance."
  ),
  Biomass_Index_Proxy = list(
    label = "Biomass proxy",
    formula = "mean(NDVI, SAVI, NDRE), clipped to -1 to 1",
    unit = "Unitless exploratory proxy",
    use = "Image-only exploratory biomass surface. It is not kg/ha without field calibration."
  ),
  NIR = list(
    label = "NIR",
    formula = "ODM MicaSense NIR band after alpha masking and reflectance scaling",
    unit = "Reflectance, displayed as 0 to 1",
    use = "Near-infrared canopy response, important for vegetation indices."
  ),
  RedEdge = list(
    label = "RedEdge",
    formula = "ODM MicaSense RedEdge band after alpha masking and reflectance scaling",
    unit = "Reflectance, displayed as 0 to 1",
    use = "Red-edge reflectance, useful for chlorophyll and dense canopies."
  ),
  Red = list(
    label = "Red",
    formula = "ODM MicaSense Red band after alpha masking and reflectance scaling",
    unit = "Reflectance, displayed as 0 to 1",
    use = "Visible red reflectance used in NDVI, EVI and SAVI."
  ),
  Green = list(
    label = "Green",
    formula = "ODM MicaSense Green band after alpha masking and reflectance scaling",
    unit = "Reflectance, displayed as 0 to 1",
    use = "Visible green reflectance used in NDWI and RGB visualization."
  ),
  Blue = list(
    label = "Blue",
    formula = "ODM MicaSense Blue band after alpha masking and reflectance scaling",
    unit = "Reflectance, displayed as 0 to 1",
    use = "Visible blue reflectance used in EVI and RGB visualization."
  ),
  Hillshade = list(
    label = "Hillshade",
    formula = "terra::shade(slope, aspect, angle = 45, direction = 315) on the DSM",
    unit = "Grayscale shading, 0 (dark) to 1 (bright)",
    use = "Adds relief shading under colored overlays so terrain structure is readable."
  )
)

layer_input_id <- function(layer_name) {
  paste0("layer_", gsub("[^A-Za-z0-9]", "_", layer_name))
}

help_input_id <- function(layer_name) {
  paste0("help_", gsub("[^A-Za-z0-9]", "_", layer_name))
}

format_summary_table <- function(x, unit, digits = 3) {
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
  NDWI = c(-1, 1),
  GNDVI = c(-1, 1),
  CIrededge = c(-1, 5),
  MSAVI2 = c(-1, 1),
  VARI = c(-1, 1),
  Biomass_Index_Proxy = c(-1, 1)
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

read_odm_exif_flight_plan <- function(images_dir, odm_project_dir) {
  exif_dir <- file.path(odm_project_dir, "opensfm", "exif")
  if (!dir.exists(exif_dir)) {
    return(data.frame())
  }
  files <- list.files(exif_dir, pattern = "\\.tif\\.exif$", full.names = TRUE, ignore.case = TRUE)
  if (length(files) == 0) {
    return(data.frame())
  }

  rows <- lapply(files, function(path) {
    x <- tryCatch(jsonlite::fromJSON(path), error = function(e) NULL)
    if (is.null(x) || is.null(x$gps$latitude) || is.null(x$gps$longitude)) {
      return(NULL)
    }
    filename <- sub("\\.exif$", "", basename(path), ignore.case = TRUE)
    parsed <- regexec("^(.+)_([0-9]+)\\.[A-Za-z0-9]+$", filename)
    match <- regmatches(filename, parsed)[[1]]
    if (length(match) != 3) {
      return(NULL)
    }
    data.frame(
      capture_id = match[[2]],
      band_id = as.integer(match[[3]]),
      filename = filename,
      longitude = as.numeric(x$gps$longitude),
      latitude = as.numeric(x$gps$latitude),
      altitude_m = as.numeric(x$gps$altitude %||% NA_real_),
      capture_time = as.numeric(x$capture_time %||% NA_real_),
      stringsAsFactors = FALSE
    )
  })
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
    tags$img(src = "logo.png", height = "80px",
             style = "vertical-align: middle;",
             alt = "DroneBioR logo"),
    tags$span(style = "font-size: 1.25rem; font-weight: 600;",
              "Drone Biomass Studio")
  ),
  theme = theme,
  header = tags$head(
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/three.js/r128/three.min.js"),
    tags$script(src = "https://cdn.jsdelivr.net/npm/three@0.128.0/examples/js/controls/OrbitControls.js"),
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
      .viewer-frame { background: #101828; }
      .viewer-frame { height: clamp(390px, 52vh, 560px); min-height: 390px; }
      .tree-workspace-grid {
        display: grid;
        grid-template-columns: minmax(0, 1.9fr) minmax(320px, 0.9fr);
        gap: 14px;
        align-items: start;
      }
      .tree-main-column,
      .tree-side-column {
        display: grid;
        gap: 12px;
        align-content: start;
      }
      .tree-workspace-grid .card {
        margin-bottom: 0;
      }
      @media (max-width: 1100px) {
        .tree-workspace-grid {
          grid-template-columns: 1fr;
        }
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
      pre { white-space: pre-wrap; }
      .btn { border-radius: 6px; }
      table { font-size: 0.88rem; }
    "))
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
          selected = "Fixed semantic"
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
        checkboxInput("annotation_mode", "Annotation mode (click map to pin)", value = FALSE),
        textInput("annotation_text", NULL, placeholder = "Annotation text..."),
        actionButton("save_annotations", "Save annotations (GeoJSON)", class = "btn-outline-secondary"),
        fileInput("load_annotations", "Load annotations (GeoJSON)", accept = c(".geojson", ".json")),
        actionButton("clear_annotations", "Clear annotations", class = "btn-outline-secondary"),
        div(class = "form-label", "ROI comparison"),
        div(class = "sidebar-note",
            "How to draw an ROI: (1) click 'Draw new ROI' to switch into polygon mode; (2) click vertices on the map to outline the region; (3) click 'Save ROI' to add it to the comparison table."),
        textInput("roi_name", NULL, placeholder = "ROI name (e.g. plot_3)", value = "roi_1"),
        actionButton("start_drawing_roi", "Draw new ROI", class = "btn-primary"),
        actionButton("save_roi", "Save ROI", class = "btn-outline-secondary"),
        actionButton("compute_roi_comparison", "Compute ROI comparison", class = "btn-outline-secondary"),
        actionButton("clear_rois", "Clear ROIs", class = "btn-outline-secondary"),
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
        div(class = "map-frame", leafletOutput("gis_map", height = "58vh")),
        card(card_header("Map measurement"), tableOutput("gis_measure_summary")),
        card(card_header("ROI comparison"), tableOutput("roi_comparison_table")),
        card(card_header("Available processing products"), tableOutput("product_table"))
      )
    )
  ),
  nav_panel(
    "Processing Engine",
    layout_sidebar(
      sidebar = sidebar(
        width = 380,
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
        div(class = "sidebar-note", "For full 3D textured products, turn off fast orthophoto. ODM uses fast orthophoto to prioritize rapid orthomosaic generation.")
      ),
      panel_intro_card(
        "Processing Engine",
        "Drives OpenDroneMap inside Docker. Pick a preset (the recommended one builds DSM + DTM + LAS point cloud), then 'Build command' to inspect the docker arguments before running. If you bring orthomosaics from WebODM, Pix4Dmapper or Metashape, skip this panel and point GIS Workspace at the existing GeoTIFF.",
        vignette = "external-engines"
      ),
      card(card_header("Processing workflow"), uiOutput("processing_workflow")),
      card(card_header("What this preset creates"), tableOutput("preset_outputs")),
      card(card_header("ODM Docker command"), verbatimTextOutput("odm_command")),
      card(card_header("Processing guidance"), textOutput("engine_note")),
      card(card_header("Product status"), tableOutput("processing_products"))
    )
  ),
  nav_panel(
    "3D & Tree Metrics",
    layout_sidebar(
      sidebar = sidebar(
        width = 340,
        textInput("full_cloud_path", "Full-resolution point cloud (LAS/LAZ/COPC)", value = default_full_cloud(default_products)),
        selectInput(
          "viewer_cloud_source",
          "3D viewer source",
          choices = c("Full georeferenced LAS/LAZ/COPC sample", "PLY preview fallback"),
          selected = if (file.exists(default_full_cloud(default_products))) "Full georeferenced LAS/LAZ/COPC sample" else "PLY preview fallback"
        ),
        checkboxInput("use_full_roi_metrics", "Recalculate selected ROI with full cloud + CHM", value = TRUE),
        textInput("ply_path", "Preview point cloud (PLY)", value = file.path(default_project$odm_project_dir, "odm_filterpoints", "point_cloud.ply")),
        numericInput("max_points", "Maximum preview points", value = 35000, min = 1000, max = 150000, step = 1000),
        numericInput("tree_grid", "Tree candidate grid size (m)", value = 4, min = 1, max = 15, step = 0.5),
        numericInput("min_tree_height", "Minimum canopy height (m)", value = 1.5, min = 0.1, max = 20, step = 0.1),
        numericInput("min_tree_points", "Minimum points per candidate", value = 5, min = 1, max = 100, step = 1),
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
          "point_color_mode",
          "Point symbology",
          choices = c("RGB", "Classification", "Height"),
          selected = "RGB"
        ),
        selectInput(
          "classification_label",
          "Class to assign",
          choices = c("Unclassified", "Canopy", "Ground", "Tree crown", "Stem/trunk", "Low vegetation", "Noise", "Exclude")
        ),
        actionButton("classify_selection", "Classify selected points", class = "btn-outline-secondary"),
        numericInput("min_point_height", "Point height filter min (m)", value = 0, min = 0, max = 100, step = 0.5),
        numericInput("max_point_height", "Point height filter max (m)", value = 100, min = 0, max = 150, step = 0.5),
        numericInput("voxel_size", "Volume voxel size (m)", value = 0.5, min = 0.05, max = 5, step = 0.05),
        numericInput("profile_bin_size", "Vertical profile bin size (m)", value = 1, min = 0.1, max = 10, step = 0.1),
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
          "3D & Tree Metrics",
          "Loads a dense point cloud (LAS/LAZ/COPC or a PLY preview) and lets you select ROIs by box, lasso or polygon. The R-side metrics include canopy height (from DSM-DTM), vertical profile, occupied voxel volume and approximate tree candidates. Switch the 3D viewer source to PLY for fast iteration; LAS gives full georeferenced metrics.",
          vignette = "point-clouds-and-chm"
        ),
        div(
          class = "viewer-toolbar",
          actionButton("open_file_browser_3d_main", "Browse PLY / project files", class = "btn-outline-secondary"),
          actionButton("load_3d_scene_main", "Load 3D scene now", class = "btn-primary"),
          span(class = "viewer-status", textOutput("point_cloud_status", inline = TRUE))
        ),
        div(
          class = "tree-workspace-grid",
          div(
            class = "tree-main-column",
            card(card_header("Interactive point cloud preview"), div(class = "viewer-frame", uiOutput("point_cloud_viewer"))),
            card(
              card_header("3D tool documentation"),
              tags$ul(
                tags$li(tags$strong("Box selection: "), "drag a rectangle over visible points in the 3D canvas."),
                tags$li(tags$strong("Lasso selection: "), "drag a freehand polygon over visible points."),
                tags$li(tags$strong("Polygon selection: "), "click vertices and double-click to close the polygon."),
                tags$li(tags$strong("Measure distance: "), "click two visible points; distance is reported in meters."),
                tags$li(tags$strong("Manual crown edit: "), "lasso a crown/ROI, then save it with the current Selection / ROI label."),
                tags$li(tags$strong("Volume: "), "reported as occupied voxel volume using the selected voxel size.")
              )
            ),
            card(card_header("Vertical profile"), plotOutput("vertical_profile_plot", height = "220px")),
            card(card_header("Point classification summary"), tableOutput("classification_summary")),
            card(card_header("Selected tree"), tableOutput("selected_tree")),
            card(card_header("Distance measurement"), tableOutput("distance_measurement")),
            card(card_header("Full-resolution ROI source"), verbatimTextOutput("full_roi_status")),
            card(card_header("Manual crown / ROI layer"), tableOutput("manual_crowns_table")),
            card(card_header("Approximate tree candidates"), tableOutput("tree_metrics")),
            card(card_header("Selection export"), verbatimTextOutput("selection_export_paths"))
          ),
          div(
            class = "tree-side-column",
            card(
              card_header(
                div(
                  class = "map-card-header",
                  span("2D context map"),
                  actionButton("recenter_context_map", "Center map", class = "btn-outline-secondary btn-sm map-center-btn")
                )
              ),
              leafletOutput("point_cloud_context_map", height = "280px")
            ),
            card(card_header("Selection measurements"), tableOutput("selection_metrics"))
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
  project <- reactive({
    p <- dronebio_project(project_dir = input$project_dir)
    p$images_dir <- input$images_dir
    p$output_dir <- input$output_dir
    p
  })

  overlay_choices <- c("NDVI", "NDRE", "EVI", "SAVI", "NDWI", "Biomass_Index_Proxy", "NIR", "RedEdge", "Red", "Green", "Blue", "Hillshade")
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
    bounds <- tryCatch(
      raster_bounds_4326(terra::rast(orthomosaic_path)[[1]]),
      error = function(e) NULL
    )
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

    bounds <- tryCatch(
      raster_bounds_4326(terra::rast(orthomosaic_path)[[1]]),
      error = function(e) NULL
    )
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
          keepBuffer = 8
        )
      )
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
    rows <- lapply(overlay_choices, function(layer_name) {
      meta <- product_metadata[[layer_name]]
      tags$div(
        class = "d-flex align-items-center justify-content-between gap-2 mb-1",
        checkboxInput(
          inputId = layer_input_id(layer_name),
          label = meta$label,
          value = identical(layer_name, "NDVI")
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
        showModal(modalDialog(
          title = meta$label,
          tags$p(tags$strong("Formula: "), meta$formula),
          tags$p(tags$strong("Display unit: "), meta$unit),
          tags$p(tags$strong("Interpretation: "), meta$use),
          easyClose = TRUE,
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
  })

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

  output$metric_images <- renderUI({
    status_badge(dir.exists(input$images_dir), "Ready", "Missing")
  })

  output$metric_ortho <- renderUI({
    status_badge(file.exists(input$orthomosaic), "Available", "Missing")
  })

  output$metric_cloud <- renderUI({
    products <- odm_product_paths(project())
    status_badge(any(file.exists(unname(products[c("point_cloud_las", "point_cloud_laz", "point_cloud_copc")]))), "Available", "Missing")
  })

  output$metric_dem <- renderUI({
    products <- odm_product_paths(project())
    ok <- file.exists(products[["dsm"]]) && file.exists(products[["dtm"]])
    status_badge(ok, "DSM/DTM OK", "Missing")
  })

  products <- reactive({
    summarize_odm_products(project())
  })

  output$product_table <- renderTable({
    products()
  })

  output$processing_products <- renderTable({
    products()
  })

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
  })

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
      ortho <- read_multispectral_orthomosaic(input$orthomosaic, use_alpha = input$use_alpha)
      refl <- if (isTRUE(input$scale_reflectance)) scale_to_reflectance(ortho$bands) else ortho$bands
      idx <- compute_spectral_indices(refl)
      proxy <- compute_biomass_proxy(idx)
      c(refl, idx, proxy)
    })
  }) |>
    bindCache(input$orthomosaic, input$use_alpha, input$scale_reflectance) |>
    bindEvent(input$load_gis)

  # Hillshade is derived from the DSM, independently of the gis_stack. We
  # compute it on demand and cache by project_dir so repeated map renders
  # do not redo the terrain math.
  hillshade_raster <- reactive({
    with_error_toast("Compute hillshade", {
      products <- odm_product_paths(project())
      validate(need(
        file.exists(products[["dsm"]]),
        "DSM not available - hillshade needs odm_dem/dsm.tif."
      ))
      dsm <- terra::rast(products[["dsm"]])[[1]]
      slope <- terra::terrain(dsm, "slope", unit = "radians", neighbors = 8)
      aspect <- terra::terrain(dsm, "aspect", unit = "radians", neighbors = 8)
      h <- terra::shade(slope, aspect, angle = 45, direction = 315)
      names(h) <- "Hillshade"
      h
    })
  }) |>
    bindCache(input$project_dir)

  output$gis_map <- renderLeaflet({
    m <- leaflet() |>
      add_esri_imagery_tiles(group = "Satellite") |>
      addProviderTiles(providers$CartoDB.Positron, group = "Light basemap") |>
      addScaleBar(position = "bottomleft")

    m <- add_project_footprint(m, input$orthomosaic)

    overlay_groups <- c(
      "Orthomosaic footprint",
      if (isTRUE(input$show_raw_flight)) flight_overlay_groups else character(),
      "Measurement"
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

  observeEvent(list(input$images_dir, input$project_dir), {
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

    # Hillshade is sourced from the DSM, not from gis_stack(), so peel it
    # off first and process the spectral layers afterwards.
    hillshade_selected <- "Hillshade" %in% all_selected
    spectral_selected <- setdiff(all_selected, "Hillshade")

    x <- if (length(spectral_selected) > 0) gis_stack() else NULL
    if (!is.null(x)) {
      spectral_selected <- intersect(spectral_selected, names(x))
    }
    validate(need(
      hillshade_selected || length(spectral_selected) > 0,
      "Selected products are not available in the current raster stack."
    ))

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
    selected_layers <- if (hillshade_selected) c(spectral_selected, "Hillshade") else spectral_selected

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
        overlayGroups = c(
          selected_layers,
          "Orthomosaic footprint",
          if (isTRUE(input$show_raw_flight)) flight_overlay_groups else character(),
          "Measurement"
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
      out_dir <- if (nzchar(input$project_dir %||% "")) input$project_dir else tempdir()
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      out_path <- file.path(out_dir, "annotations.geojson")
      sf_obj <- sf::st_as_sf(df, coords = c("lng", "lat"), crs = 4326)
      sf::st_write(sf_obj, out_path, delete_dsn = TRUE, quiet = TRUE)
      showNotification(
        paste("Annotations saved to", out_path),
        type     = "message",
        duration = 6
      )
    })
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
  }, digits = 3)

  observeEvent(input$gis_measure_tool, {
    gis_measure_points(data.frame(lng = numeric(), lat = numeric()))
    leafletProxy("gis_map") |> clearGroup("Measurement")
  }, ignoreInit = TRUE)

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
    proxy <- proxy |>
      addCircleMarkers(
        data = pts,
        lng = ~lng,
        lat = ~lat,
        radius = 5,
        color = "#0f172a",
        weight = 1.5,
        fillColor = "#38bdf8",
        fillOpacity = 0.9,
        group = "Measurement"
      )
    if (identical(input$gis_measure_tool, "Measure distance") && nrow(pts) >= 2) {
      proxy |>
        addPolylines(
          data = pts,
          lng = ~lng,
          lat = ~lat,
          color = "#38bdf8",
          weight = 3,
          opacity = 0.95,
          group = "Measurement"
        )
    } else if (identical(input$gis_measure_tool, "Measure area") && nrow(pts) >= 3) {
      closed <- rbind(pts, pts[1, , drop = FALSE])
      proxy |>
        addPolygons(
          data = closed,
          lng = ~lng,
          lat = ~lat,
          color = "#38bdf8",
          weight = 3,
          fillColor = "#38bdf8",
          fillOpacity = 0.18,
          group = "Measurement"
        )
    } else if (identical(input$gis_measure_tool, "Measure volume (CHM)") && nrow(pts) >= 3) {
      closed <- rbind(pts, pts[1, , drop = FALSE])
      proxy |>
        addPolygons(
          data = closed,
          lng = ~lng,
          lat = ~lat,
          # Distinct orange so users can tell volume polygons apart from
          # plain area polygons on the same canvas.
          color = "#f97316",
          weight = 3,
          fillColor = "#f97316",
          fillOpacity = 0.20,
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
          formatC(metrics$chm_height_mean_m, format = "f", digits = 3),
          formatC(metrics$chm_height_max_m, format = "f", digits = 3),
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
  })

  manifest <- reactive({
    validate(need(dir.exists(input$images_dir), paste("Image folder not found:", input$images_dir)))
    list_micasense_images(input$images_dir)
  })

  flight_plan <- reactive({
    if (!dir.exists(input$images_dir)) {
      return(data.frame())
    }
    read_odm_exif_flight_plan(input$images_dir, project()$odm_project_dir)
  })

  output$odm_command <- renderText({
    input$refresh_command
    isolate({
      args <- build_odm_args(
        dataset_dir = project()$odm_dataset_dir,
        project_name = project()$odm_project_name,
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

  output$engine_note <- renderText({
    if (isTRUE(input$fast_orthophoto) && (isTRUE(input$three_d_tiles) || isTRUE(input$gltf))) {
      "Fast orthophoto prioritizes rapid orthomosaic generation and ODM can skip full textured 3D outputs. Disable fast orthophoto for commercial-style 3D deliverables."
    } else if (isTRUE(input$build_dsm) && isTRUE(input$build_dtm)) {
      "DSM and DTM are enabled. This supports canopy height modeling and later tree segmentation."
    } else {
      "Enable DSM and DTM if you want scientifically defensible tree height, crown and volume estimates."
    }
  })

  observeEvent(input$run_odm, {
    showNotification("ODM processing started. This can take a long time and will block this prototype Shiny session.", type = "message", duration = 8)
    run_odm_project(
      project(),
      run = TRUE,
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
    showNotification("ODM processing finished.", type = "message", duration = 8)
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
  })

  output$radiometric_qa <- renderTable({
    req(mosaic(), base_reflectance())
    qa <- spectral_qa_summary(mosaic()$raw_bands, base_reflectance(), mosaic()$alpha, radiometric_scale_info())
    numeric_cols <- vapply(qa, is.numeric, logical(1))
    qa[numeric_cols] <- lapply(qa[numeric_cols], function(v) ifelse(abs(v) >= 1000, format(round(v), big.mark = ","), formatC(v, format = "f", digits = 3)))
    qa
  })

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
    x[numeric_cols] <- lapply(x[numeric_cols], function(v) formatC(v, format = "f", digits = 5))
    x
  })

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
      digits = 3
    )
  })

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
    out$area_ha <- formatC(out$area_ha, format = "f", digits = 4)
    out
  })

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
    x[numeric_cols] <- lapply(x[numeric_cols], function(v) formatC(v, format = "f", digits = 4))
    x
  })

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
    products <- odm_product_paths(project())
    if (!file.exists(products[["dsm"]]) || !file.exists(products[["dtm"]])) {
      return(NULL)
    }
    build_chm_from_dsm_dtm(products[["dsm"]], products[["dtm"]])
  })

  viewer_basemap <- reactive({
    build_orthomosaic_texture(
      input$orthomosaic,
      use_alpha = input$use_alpha,
      scale_reflectance = input$scale_reflectance
    )
  })

  context_orthomosaic <- reactive({
    build_context_orthomosaic_raster(
      input$orthomosaic,
      use_alpha = input$use_alpha,
      scale_reflectance = input$scale_reflectance
    )
  })

  point_cloud_reference_raster <- reactive({
    products <- odm_product_paths(project())
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

  point_cloud <- reactive({
    with_error_toast("Load point cloud", {
      use_full_sample <- identical(input$viewer_cloud_source, "Full georeferenced LAS/LAZ/COPC sample") &&
        file.exists(input$full_cloud_path)

      if (isTRUE(use_full_sample)) {
        pts <- read_full_point_cloud(input$full_cloud_path, max_points = input$max_points)
        chm <- chm_raster()
        if (!is.null(chm)) {
          pts <- add_chm_heights(pts, chm)
          pts <- add_cloud_runtime_attributes(pts, input$full_cloud_path, "full_georeferenced", "DSM-DTM CHM")
        } else {
          pts <- add_point_heights(pts)
          pts <- add_cloud_runtime_attributes(pts, input$full_cloud_path, "full_georeferenced", "local low-Z proxy")
        }
      } else {
        validate(need(file.exists(input$ply_path), paste("PLY file not found:", input$ply_path)))
        pts <- read_ply_point_cloud(input$ply_path, max_points = input$max_points)
        pts <- add_point_heights(pts)
        pts <- add_cloud_runtime_attributes(pts, input$ply_path, "local_preview", "local low-Z proxy")
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
      input$full_cloud_path,
      input$ply_path,
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
    if (!file.exists(input$full_cloud_path)) {
      full_roi_status_value("Full point cloud path is missing; using the viewer preview sample.")
      return(pts_preview[0, , drop = FALSE])
    }

    tryCatch({
      pts <- read_full_point_cloud(input$full_cloud_path, roi_polygon = roi, max_points = Inf)
      if (nrow(pts) == 0) {
        full_roi_status_value("The ROI polygon did not intersect any full-resolution points; using the viewer preview sample.")
        return(pts_preview[0, , drop = FALSE])
      }

      chm <- chm_raster()
      if (!is.null(chm)) {
        pts <- add_chm_heights(pts, chm)
        pts <- add_cloud_runtime_attributes(pts, input$full_cloud_path, "full_georeferenced", "DSM-DTM CHM")
      } else {
        pts <- add_point_heights(pts)
        pts <- add_cloud_runtime_attributes(pts, input$full_cloud_path, "full_georeferenced", "local low-Z proxy")
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
    points$selected <- points$point_id %in% selected_point_ids()

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
          const width = container.clientWidth;
          const height = container.clientHeight || 560;
          const scene = new THREE.Scene();
          scene.background = new THREE.Color(0x0f172a);
          const badge = document.createElement('div');
          badge.className = 'tool-badge';
          badge.textContent = points.length > 0
            ? mode + ' | visible points: ' + points.length.toLocaleString()
            : 'Orthomosaic basemap | load the 3D scene to add point cloud';
          container.appendChild(badge);

          const camera = new THREE.PerspectiveCamera(55, width / height, 0.01, 1000);
          const renderer = new THREE.WebGLRenderer({ antialias: true });
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

          if (hasBasemap && basemap.data_uri) {
            const textureLoader = new THREE.TextureLoader();
            textureLoader.load(basemap.data_uri, function(texture) {
              if ('colorSpace' in texture && THREE.SRGBColorSpace) {
                texture.colorSpace = THREE.SRGBColorSpace;
              }
              texture.anisotropy = renderer.capabilities.getMaxAnisotropy();
              const planeWidth = Math.max((basemap.xmax - basemap.xmin) / scale * 10, 0.1);
              const planeHeight = Math.max((basemap.ymax - basemap.ymin) / scale * 10, 0.1);
              const planeX = (((basemap.xmin + basemap.xmax) / 2) - cx) / scale * 10;
              const planeZ = -((((basemap.ymin + basemap.ymax) / 2) - cy) / scale * 10);
              const plane = new THREE.Mesh(
                new THREE.PlaneGeometry(planeWidth, planeHeight),
                new THREE.MeshBasicMaterial({ map: texture, transparent: true, opacity: 0.88, side: THREE.DoubleSide })
              );
              plane.rotation.x = -Math.PI / 2;
              plane.position.set(planeX, -0.08, planeZ);
              scene.add(plane);
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
          const material = new THREE.PointsMaterial({ size: 0.035, vertexColors: true });
          scene.add(new THREE.Points(geometry, material));
          if (selectedPositions.length > 0) {
            const selectedGeometry = new THREE.BufferGeometry();
            selectedGeometry.setAttribute('position', new THREE.Float32BufferAttribute(selectedPositions, 3));
            selectedGeometry.setAttribute('color', new THREE.Float32BufferAttribute(selectedColors, 3));
            scene.add(new THREE.Points(
              selectedGeometry,
              new THREE.PointsMaterial({ size: 0.08, vertexColors: true })
            ));
          }

          const grid = new THREE.GridHelper(12, 12, 0x94a3b8, 0x334155);
          grid.position.y = -0.03;
          scene.add(grid);

          const treeMeshes = [];
          const markerMaterial = new THREE.MeshStandardMaterial({ color: 0xfacc15, roughness: 0.35, metalness: 0.05 });
          trees.forEach(t => {
            const radius = Math.max((t.crown_diameter_m || 1) / scale * 3, 0.055);
            const sphere = new THREE.Mesh(new THREE.SphereGeometry(radius, 16, 12), markerMaterial.clone());
            const p = pos(t);
            sphere.position.set(p[0], p[1] + radius * 1.2, p[2]);
            sphere.userData = t;
            scene.add(sphere);
            treeMeshes.push(sphere);
          });

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
            controls.enabled = mode === 'Inspect trees';
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

          function animate() {
            requestAnimationFrame(animate);
            if (controls) controls.update();
            renderer.render(scene, camera);
          }
          animate();
        })();
      "
        viewer_script <- gsub("__POINT_JSON__", point_json, viewer_script, fixed = TRUE)
        viewer_script <- gsub("__TREE_JSON__", tree_json, viewer_script, fixed = TRUE)
        viewer_script <- gsub("__MODE_JSON__", mode_json, viewer_script, fixed = TRUE)
        viewer_script <- gsub("__CAMERA_STATE_JSON__", camera_state_json, viewer_script, fixed = TRUE)
        viewer_script <- gsub("__BASEMAP_JSON__", basemap_json, viewer_script, fixed = TRUE)
        viewer_script
      }))
    )
  })

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
  })

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
  })

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
  })

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
  })

  output$selection_export_paths <- renderText({
    paths <- selection_export_paths()
    if (length(paths) == 0) return("")
    paste(paths, collapse = "\n")
  })

  output$point_cloud_context_map <- renderLeaflet({
    base <- leaflet() |>
      addMapPane("localOrthomosaicFallback", zIndex = 150) |>
      addMapPane("treeCandidates", zIndex = 420) |>
      addMapPane("classifiedPoints", zIndex = 440) |>
      addMapPane("selectionRoi", zIndex = 460) |>
      add_esri_imagery_tiles(group = "Satellite") |>
      addProviderTiles(providers$CartoDB.Positron, group = "Light basemap")

    if (!file.exists(input$orthomosaic)) {
      return(base |>
        addLayersControl(
          baseGroups = c("Satellite", "Light basemap"),
          options = layersControlOptions(collapsed = FALSE)
        ))
    }

    ortho_for_context <- terra::rast(input$orthomosaic)[[1]]
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

    ortho_for_context <- terra::rast(input$orthomosaic)[[1]]
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

    ortho_for_context <- terra::rast(input$orthomosaic)[[1]]
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

    ortho_for_context <- terra::rast(input$orthomosaic)[[1]]
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
  })

  output$selected_tree <- renderTable({
    req(tree_candidates())
    x <- tree_candidates()
    validate(need(nrow(x) > 0, "No selected tree candidate."))
    id <- input$selected_tree_id %||% x$tree_id[[1]]
    format_tree_table(x[x$tree_id == id, , drop = FALSE])
  })

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
  }, digits = 4)

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
  })

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
