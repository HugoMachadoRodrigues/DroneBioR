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
    band_map <- if (n_layers >= 5L) default_micasense_band_map() else default_rgb_band_map()
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
    reflectance = file.path(output_dir, "micasense_reflectance_bands.tif"),
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
