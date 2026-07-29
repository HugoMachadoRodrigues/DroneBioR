default_micasense_band_map <- function() {
  c(Red = 1, Green = 2, Blue = 3, NIR = 4, RedEdge = 5)
}

#' Default band map for a 3-band RGB orthomosaic (Sony / DJI / Phantom)
#'
#' @return Named integer vector with `Red`, `Green`, `Blue`.
#' @export
default_rgb_band_map <- function() {
  c(Red = 1, Green = 2, Blue = 3)
}

#' Default band map for the DJI Mavic 3M 7-band stacked orthomosaic
#'
#' [run_odm_dji_mavic_3m()] stacks five separate ODM outputs into a
#' single GeoTIFF with band order Red, Green, Blue (from the RGB run)
#' followed by MS_G, MS_R, MS_RE, MS_NIR (from the four per-band MS
#' runs). For spectral-index computation we want the *radiometrically
#' calibrated* MS bands and nothing else: this band map points
#' Green/Red/RedEdge/NIR at the MS layers (4-7) and **drops the Blue
#' channel entirely** because the Mavic 3M does not capture a
#' calibrated blue MS band — the only Blue available is the
#' uncalibrated RGB JPG channel, and mixing it with calibrated MS
#' bands inside EVI / VARI / ExG / GLI / TGI / RGBVI produces a
#' hybrid number that is not comparable to the values in the
#' literature. With Blue absent, [compute_spectral_indices()]
#' automatically skips the six Blue-dependent indices and returns
#' the 16 indices the MS bands can support honestly.
#'
#' Users who specifically want the visible-band indices on the RGB
#' JPG channels can override the band map manually with
#' `c(Blue = 3, Green = 2, Red = 1)` and pass it to
#' [read_multispectral_orthomosaic()].
#'
#' @return Named integer vector with `Green`, `Red`, `RedEdge`, `NIR`.
#' @export
default_dji_mavic_3m_band_map <- function() {
  # Layers 1-3 are the RGB camera; 4-7 the calibrated multispectral set.
  # Green and Red come from the MS bands, never the RGB camera, because only
  # those carry the sun-sensor radiometric calibration the indices assume.
  #
  # Blue is deliberately absent: the Mavic 3M has no blue MS band, and the RGB
  # camera's blue is not comparable to the calibrated bands. Including it would
  # make EVI / VARI / ExG / GLI / TGI / RGBVI compute a number that looks like
  # the index but is not, so those six stay unavailable and the honest 16
  # remain. Callers who accept the caveat can pass an explicit band_map.
  c(Green = 4, Red = 5, RedEdge = 6, NIR = 7)
}

#' Read a multispectral or RGB orthomosaic
#'
#' Reads an orthomosaic produced by OpenDroneMap / WebODM / Pix4Dmapper /
#' Agisoft Metashape. The function adapts to two common layouts:
#'
#' \itemize{
#'   \item **Multispectral** (MicaSense / Sequoia, 5 bands +/- alpha) -
#'     returned bands are Red, Green, Blue, RedEdge, NIR. Alpha is read
#'     from layer 6 when present.
#'   \item **RGB** (3 bands +/- alpha) - returned bands are Red, Green,
#'     Blue. Alpha is read from layer 4 when present.
#' }
#'
#' Layout is auto-detected from `terra::nlyr()` when `band_map` is left
#' as `NULL`; explicit band maps are honoured otherwise.
#'
#' @param orthomosaic Path to an orthomosaic GeoTIFF.
#' @param band_map Optional named integer vector. Default `NULL` =
#'   auto-detect: 3-4 layer inputs use [default_rgb_band_map()]; 5+
#'   layer inputs use the internal MicaSense default
#'   (`Red=1, Green=2, Blue=3, NIR=4, RedEdge=5`). Override with a
#'   custom named integer vector.
#' @param use_alpha Logical. Treat the layer immediately after the
#'   highest band-map index as an alpha mask if available.
#' @return A list containing `bands`, `alpha`, `source` and `n_layers`.
#' @examples
#' ortho_path <- system.file("extdata", "micasense_subset.tif", package = "DroneBioR")
#' ortho <- read_multispectral_orthomosaic(ortho_path)
#' names(ortho$bands)
#' ortho$n_layers
#' @export
read_multispectral_orthomosaic <- function(orthomosaic,
                                           band_map = NULL,
                                           use_alpha = TRUE) {
  if (!file.exists(orthomosaic)) {
    stop("Orthomosaic not found: ", orthomosaic, call. = FALSE)
  }

  raw <- terra::rast(orthomosaic)
  n_layers <- terra::nlyr(raw)

  if (is.null(band_map)) {
    band_map <- default_band_map_for_layers(n_layers)
  }

  required <- names(band_map)
  if (n_layers < max(band_map[required])) {
    stop(
      "The orthomosaic has ", n_layers, " layer(s), but the band map ",
      "requires layer ", max(band_map[required]), ".",
      call. = FALSE
    )
  }

  alpha <- NULL
  alpha_position <- max(band_map[required]) + 1L
  if (isTRUE(use_alpha) && n_layers >= alpha_position) {
    alpha <- raw[[alpha_position]]
    names(alpha) <- "valid_data_mask"
  }

  # Output order: Blue, Green, Red, then RedEdge / NIR when available.
  full_order <- c("Blue", "Green", "Red", "RedEdge", "NIR")
  order_out  <- intersect(full_order, required)
  bands <- raw[[as.integer(band_map[order_out])]]
  names(bands) <- order_out

  if (!is.null(alpha)) {
    bands <- terra::mask(bands, alpha, maskvalues = 0, updatevalue = NA)
  }

  list(
    bands    = bands,
    alpha    = alpha,
    source   = normalizePath(orthomosaic, mustWork = FALSE),
    n_layers = n_layers
  )
}

#' Scale raster values to reflectance-like 0-1 values
#'
#' @param x A `terra::SpatRaster`.
#' @param scale_factor Optional numeric scale factor.
#' @return A `terra::SpatRaster`.
#' @examples
#' ortho_path <- system.file("extdata", "micasense_subset.tif", package = "DroneBioR")
#' ortho <- read_multispectral_orthomosaic(ortho_path)
#' refl <- scale_to_reflectance(ortho$bands)
#' terra::minmax(refl)
#' @export
scale_to_reflectance <- function(x, scale_factor = NULL) {
  # Detect the scale factor cheaply. The previous spatSample path
  # measured at ~91 s on the user's real 5-band 437 Mcell ortho even
  # at size = 5000: with na.rm = TRUE and a heavy alpha mask, terra
  # keeps reading more cells until it finds 5000 valid samples,
  # which on COGs with sparse coverage degenerates into a near-full
  # scan. We use a three-step cascade instead:
  #   1. terra::minmax(x, compute = FALSE) - reads the cached
  #      metadata statistics if the file has them (GDAL writes
  #      STATISTICS_MAXIMUM for most COGs). Zero pixel reads.
  #   2. terra::values(x, row = middle, nrows = 50) - a single
  #      contiguous block of ~50 rows from the middle of the
  #      raster. ~11 MB for a 22k-wide ortho on disk, ~100 ms.
  #      We pick the middle so we are inside the valid-data area
  #      on heavily-masked orthos.
  #   3. terra::global("max", na.rm = TRUE) - full scan, only as a
  #      last resort. This is the original (slow) path; preserved
  #      so degenerate inputs still return a correct answer.
  # We only need to discriminate among {<=1.5, 10000, 65535}, so any
  # of the three suffice in practice.
  if (is.null(scale_factor)) {
    max_value <- NA_real_

    # 1) Cached metadata.
    mm <- tryCatch(terra::minmax(x, compute = FALSE),
                   error = function(e) NULL)
    if (!is.null(mm) && is.matrix(mm) && "max" %in% rownames(mm)) {
      candidate <- suppressWarnings(max(mm["max", ], na.rm = TRUE))
      if (is.finite(candidate)) max_value <- candidate
    }

    # 2) Mid-raster contiguous block.
    if (!is.finite(max_value)) {
      n_rows <- tryCatch(terra::nrow(x), error = function(e) NA_integer_)
      if (is.finite(n_rows) && n_rows > 0L) {
        mid    <- max(1L, as.integer(n_rows) %/% 2L - 25L)
        nr     <- min(50L, as.integer(n_rows) - mid + 1L)
        block  <- tryCatch(
          terra::values(x, mat = TRUE, row = mid, nrows = nr),
          error = function(e) NULL
        )
        if (!is.null(block)) {
          v <- as.numeric(block)
          v <- v[is.finite(v)]
          if (length(v) > 0L) max_value <- max(v)
        }
      }
    }

    # 3) Last-resort full scan.
    if (!is.finite(max_value)) {
      max_value <- max(terra::global(x, "max", na.rm = TRUE)$max, na.rm = TRUE)
    }

    if (!is.finite(max_value)) {
      stop("Could not compute raster maximum for radiometric scaling.", call. = FALSE)
    }
    if (max_value <= 1.5) {
      return(x)
    }
    scale_factor <- if (max_value > 10000) 65535 else 10000
  }
  terra::clamp(x / scale_factor, lower = 0, upper = 1, values = TRUE)
}

#' Summarize a SpatRaster by layer
#'
#' @param x A `terra::SpatRaster`.
#' @param fun Summary functions supported by `terra::global()`.
#' @return A data frame.
#' @examples
#' ortho_path <- system.file("extdata", "micasense_subset.tif", package = "DroneBioR")
#' ortho <- read_multispectral_orthomosaic(ortho_path)
#' summarize_spatraster(ortho$bands)
#' @export
summarize_spatraster <- function(x, fun = c("min", "mean", "max", "sd")) {
  summary <- terra::global(x, fun, na.rm = TRUE)
  data.frame(layer = rownames(summary), summary, row.names = NULL, check.names = FALSE)
}

#' Write DroneBioR raster products
#'
#' @param output_dir Output folder.
#' @param reflectance Reflectance band stack.
#' @param indices Spectral index stack.
#' @param biomass_proxy Biomass proxy raster.
#' @param valid_mask Optional alpha/valid-data mask.
#' @return Named character vector of output paths.
#' @examples
#' ortho_path <- system.file("extdata", "micasense_subset.tif", package = "DroneBioR")
#' ortho <- read_multispectral_orthomosaic(ortho_path)
#' refl <- scale_to_reflectance(ortho$bands)
#' ix <- compute_spectral_indices(refl)
#' proxy <- compute_biomass_proxy(ix)
#' out <- tempfile("dronebior-rasters-")
#' write_dronebio_rasters(out, refl, ix, proxy)
#' @export
write_dronebio_rasters <- function(output_dir,
                                   reflectance,
                                   indices,
                                   biomass_proxy,
                                   valid_mask = NULL) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  paths <- c(
    reflectance = file.path(output_dir, "reflectance_bands.tif"),
    indices = file.path(output_dir, "spectral_indices.tif"),
    biomass_proxy = file.path(output_dir, "biomass_index_proxy.tif")
  )

  terra::writeRaster(reflectance, paths[["reflectance"]], overwrite = TRUE, datatype = "FLT4S")
  terra::writeRaster(indices, paths[["indices"]], overwrite = TRUE, datatype = "FLT4S")
  terra::writeRaster(biomass_proxy, paths[["biomass_proxy"]], overwrite = TRUE, datatype = "FLT4S")

  if (!is.null(valid_mask)) {
    paths <- c(paths, valid_data_mask = file.path(output_dir, "valid_data_mask.tif"))
    terra::writeRaster(valid_mask, paths[["valid_data_mask"]], overwrite = TRUE, datatype = "INT1U")
  }

  paths
}

#' Which spectral bands an orthomosaic actually carries
#'
#' Decides whether NIR and RedEdge are present from the layer *names* a
#' raster declares, falling back to the layer count only when the file was
#' written without informative names.
#'
#' Counting layers alone is not enough, and getting this wrong is expensive:
#' it silently hides NDVI, NDRE, EVI and every other multispectral index from
#' a flight that has the bands. A MicaSense orthomosaic labels its bands
#' `Red, Green, Blue, NIR, Rededge` (plus alpha), and a DJI Mavic 3M stack
#' likewise, so the names are the reliable signal; a 4-band RGB + alpha file
#' and a 4-band multispectral subset are indistinguishable by count.
#'
#' @param x A `SpatRaster`, or a character vector of layer names.
#' @param nlyr Layer count, used only as a fallback when `x` carries no
#'   recognisable band names. Taken from `x` when it is a `SpatRaster`.
#' @return A list with `has_nir`, `has_rededge` and `by`, the last being
#'   `"name"` or `"count"` depending on which signal was used.
#' @examples
#' orthomosaic_band_presence(c("Red", "Green", "Blue", "NIR", "Rededge"))
#' orthomosaic_band_presence(c("red", "green", "blue"), nlyr = 3)
#' @export
orthomosaic_band_presence <- function(x, nlyr = NULL) {
  if (inherits(x, "SpatRaster")) {
    if (is.null(nlyr)) nlyr <- terra::nlyr(x)
    x <- names(x)
  }
  canon <- canonical_band_names(x)
  named <- canon[!is.na(canon)]
  if (length(named)) {
    present <- unique(named)
    # Availability must mirror what read_multispectral_orthomosaic() will
    # actually map, or the UI offers an index that is then missing from the
    # result. That function picks its band map from the layer count, so a
    # 7-layer DJI stack gets the calibrated-only map with no Blue even though
    # a blue layer is physically there. Intersecting with that map keeps the
    # two in step by construction instead of by a heuristic.
    n <- as.integer(nlyr %||% length(canon))
    if (is.finite(n)) {
      mapped <- names(default_band_map_for_layers(n))
      present <- intersect(present, mapped)
    }
    return(list(
      bands       = present,
      has_blue    = "Blue"    %in% present,
      has_green   = "Green"   %in% present,
      has_red     = "Red"     %in% present,
      has_nir     = "NIR"     %in% present,
      has_rededge = "RedEdge" %in% present,
      by          = "name"
    ))
  }
  # Nothing recognisable to go on: >4 layers is the weak legacy proxy for a
  # multispectral file, and it cannot say which bands those layers are.
  proxy <- isTRUE(as.integer(nlyr %||% NA_integer_) > 4L)
  list(
    bands       = character(),
    has_blue    = proxy, has_green = proxy, has_red = proxy,
    has_nir     = proxy, has_rededge = proxy,
    by          = "count"
  )
}

#' The band map [read_multispectral_orthomosaic()] picks for a layer count
#'
#' Kept as one function so the reader and anything that has to predict what the
#' reader will do -- band availability in the app, for one -- cannot drift
#' apart. A 7-layer DJI stack maps only the calibrated bands, which is why Blue
#' is absent there even though the stack carries an RGB triplet.
#'
#' @param n_layers Number of layers in the orthomosaic.
#' @return A named integer vector of layer positions.
#' @examples
#' names(default_band_map_for_layers(7))
#' names(default_band_map_for_layers(5))
#' @export
default_band_map_for_layers <- function(n_layers) {
  n_layers <- as.integer(n_layers)
  if (!is.finite(n_layers)) return(default_rgb_band_map())
  if (n_layers >= 7L) return(default_dji_mavic_3m_band_map())
  if (n_layers >= 5L) return(default_micasense_band_map())
  default_rgb_band_map()
}

#' Canonical band names from whatever a raster calls its layers
#'
#' Maps layer names to the canonical `Blue` / `Green` / `Red` / `RedEdge` /
#' `NIR` used throughout the package, so band detection does not depend on one
#' vendor's spelling. Recognises the common forms seen in the wild:
#' MicaSense (`Red`, `Rededge`, `NIR`), the DJI Mavic 3M stack (`MS_R`,
#' `MS_RE`, `MS_NIR`), Parrot Sequoia (`red`, `red_edge`, `nir`), plain
#' one-letter names, and `band_*` / `b*` prefixes.
#'
#' Order matters and the rules are deliberately specific-first: `RE` and
#' `rededge` must win over `red`, and `NIR` over `N`, or a red-edge layer gets
#' silently filed as red and every index built on it is quietly wrong.
#'
#' Anything unrecognised returns `NA`, which callers should treat as "this band
#' is absent" rather than guessing.
#'
#' @param x Character vector of layer names, or a `SpatRaster`.
#' @return A character vector the same length as `x`, holding canonical names
#'   or `NA`.
#' @examples
#' canonical_band_names(c("Red", "Green", "Blue", "MS_RE", "MS_NIR"))
#' canonical_band_names(c("b1", "b2", "b3"))
#' @export
canonical_band_names <- function(x) {
  if (inherits(x, "SpatRaster")) x <- names(x)
  raw <- as.character(x %||% character())
  # Normalise separators and case, then drop a leading vendor prefix so
  # "MS_NIR", "band_nir" and "nir" collapse to the same token.
  key <- tolower(gsub("[^A-Za-z0-9]", "", raw))
  key <- sub("^(ms|band|bnd)", "", key)

  rules <- list(
    RedEdge = "^(rededge|redege|re|edge|rededge1|reg)$",
    NIR     = "^(nir|nearinfrared|nearir|infrared|n)$",
    Blue    = "^(blue|blu|b)$",
    Green   = "^(green|grn|g)$",
    Red     = "^(red|r)$"
  )
  out <- rep(NA_character_, length(key))
  for (nm in names(rules)) {
    hit <- is.na(out) & grepl(rules[[nm]], key)
    out[hit] <- nm
  }
  out
}
