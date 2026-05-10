safe_ratio <- function(numerator, denominator, eps = 1e-6) {
  terra::ifel(abs(denominator) <= eps, NA, numerator / denominator)
}

#' Compute spectral vegetation indices
#'
#' @param reflectance A reflectance-scale `terra::SpatRaster` with Blue, Green,
#'   Red, RedEdge and NIR layers.
#' @param eps Small denominator threshold.
#' @return A `terra::SpatRaster` with NDVI, NDRE, EVI, SAVI, NDWI, GNDVI,
#'   CIrededge, MSAVI2 and VARI.
#' @export
compute_spectral_indices <- function(reflectance, eps = 1e-6) {
  required <- c("Blue", "Green", "Red", "RedEdge", "NIR")
  missing <- setdiff(required, names(reflectance))
  if (length(missing) > 0) {
    stop("Reflectance raster is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  blue <- reflectance[["Blue"]]
  green <- reflectance[["Green"]]
  red <- reflectance[["Red"]]
  rededge <- reflectance[["RedEdge"]]
  nir <- reflectance[["NIR"]]

  ndvi <- safe_ratio(nir - red, nir + red, eps)
  names(ndvi) <- "NDVI"
  ndre <- safe_ratio(nir - rededge, nir + rededge, eps)
  names(ndre) <- "NDRE"
  evi <- 2.5 * safe_ratio(nir - red, nir + 6 * red - 7.5 * blue + 1, eps)
  names(evi) <- "EVI"
  savi <- 1.5 * safe_ratio(nir - red, nir + red + 0.5, eps)
  names(savi) <- "SAVI"
  ndwi <- safe_ratio(green - nir, green + nir, eps)
  names(ndwi) <- "NDWI"

  gndvi <- safe_ratio(nir - green, nir + green, eps)
  names(gndvi) <- "GNDVI"
  cirededge <- safe_ratio(nir, rededge, eps) - 1
  names(cirededge) <- "CIrededge"
  msavi_term <- (2 * nir + 1)^2 - 8 * (nir - red)
  msavi2 <- (2 * nir + 1 - terra::ifel(msavi_term < 0, NA, sqrt(msavi_term))) / 2
  names(msavi2) <- "MSAVI2"
  vari <- safe_ratio(green - red, green + red - blue, eps)
  names(vari) <- "VARI"

  c(ndvi, ndre, evi, savi, ndwi, gndvi, cirededge, msavi2, vari)
}

#' Compute an image-only biomass proxy
#'
#' @param indices Spectral index stack from `compute_spectral_indices()`.
#' @return A `terra::SpatRaster` named `Biomass_Index_Proxy`.
#' @export
compute_biomass_proxy <- function(indices) {
  required <- c("NDVI", "SAVI", "NDRE")
  missing <- setdiff(required, names(indices))
  if (length(missing) > 0) {
    stop("Index stack is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  proxy <- terra::clamp(
    (indices[["NDVI"]] + indices[["SAVI"]] + indices[["NDRE"]]) / 3,
    lower = -1,
    upper = 1,
    values = TRUE
  )
  names(proxy) <- "Biomass_Index_Proxy"
  proxy
}
