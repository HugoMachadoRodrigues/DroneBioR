default_micasense_band_map <- function() {
  c(Red = 1, Green = 2, Blue = 3, NIR = 4, RedEdge = 5)
}

#' Read a multispectral orthomosaic
#'
#' @param orthomosaic Path to a multispectral GeoTIFF.
#' @param band_map Named integer vector with Red, Green, Blue, NIR and RedEdge.
#' @param use_alpha Logical. Use layer 6 as an alpha mask when available.
#' @return A list containing `bands`, `alpha`, `source` and `n_layers`.
#' @export
read_multispectral_orthomosaic <- function(orthomosaic,
                                           band_map = default_micasense_band_map(),
                                           use_alpha = TRUE) {
  if (!file.exists(orthomosaic)) {
    stop("Orthomosaic not found: ", orthomosaic, call. = FALSE)
  }

  required <- c("Red", "Green", "Blue", "NIR", "RedEdge")
  missing_names <- setdiff(required, names(band_map))
  if (length(missing_names) > 0) {
    stop("band_map is missing: ", paste(missing_names, collapse = ", "), call. = FALSE)
  }

  raw <- terra::rast(orthomosaic)
  if (terra::nlyr(raw) < max(band_map[required])) {
    stop(
      "The orthomosaic has ", terra::nlyr(raw), " layer(s), but the band map ",
      "requires layer ", max(band_map[required]), ".",
      call. = FALSE
    )
  }

  alpha <- NULL
  if (isTRUE(use_alpha) && terra::nlyr(raw) >= 6) {
    alpha <- raw[[6]]
    names(alpha) <- "valid_data_mask"
  }

  order_out <- c("Blue", "Green", "Red", "RedEdge", "NIR")
  bands <- raw[[as.integer(band_map[order_out])]]
  names(bands) <- order_out

  if (!is.null(alpha)) {
    bands <- terra::mask(bands, alpha, maskvalues = 0, updatevalue = NA)
  }

  list(
    bands = bands,
    alpha = alpha,
    source = normalizePath(orthomosaic, mustWork = FALSE),
    n_layers = terra::nlyr(raw)
  )
}

#' Scale raster values to reflectance-like 0-1 values
#'
#' @param x A `terra::SpatRaster`.
#' @param scale_factor Optional numeric scale factor.
#' @return A `terra::SpatRaster`.
#' @export
scale_to_reflectance <- function(x, scale_factor = NULL) {
  max_value <- max(terra::global(x, "max", na.rm = TRUE)$max, na.rm = TRUE)
  if (!is.finite(max_value)) {
    stop("Could not compute raster maximum for radiometric scaling.", call. = FALSE)
  }
  if (is.null(scale_factor)) {
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
