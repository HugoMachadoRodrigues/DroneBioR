safe_ratio <- function(numerator, denominator, eps = 1e-6) {
  terra::ifel(abs(denominator) <= eps, NA, numerator / denominator)
}

#' Compute spectral vegetation indices
#'
#' Computes the subset of indices the input bands actually support.
#' Multispectral input (Blue + Green + Red + RedEdge + NIR) yields all
#' nine: NDVI, NDRE, EVI, SAVI, NDWI, GNDVI, CIrededge, MSAVI2 and VARI.
#' RGB-only input (Blue + Green + Red) yields just VARI - the other
#' indices need NIR / RedEdge.
#'
#' Pass `strict = TRUE` to keep the legacy behaviour where missing
#' RedEdge / NIR raises an error; the default is to silently compute
#' whatever is possible, which is what Drone Biomass Studio needs to
#' degrade gracefully for RGB datasets.
#'
#' @param reflectance A reflectance-scale `terra::SpatRaster`. Must
#'   contain at least Blue, Green, Red; RedEdge and NIR enable the
#'   remaining indices.
#' @param eps Small denominator threshold.
#' @param strict Logical. When `TRUE`, error if RedEdge / NIR are
#'   missing instead of returning the partial index stack.
#' @return A `terra::SpatRaster` with whatever indices the bands
#'   support.
#' @examples
#' ortho_path <- system.file("extdata", "micasense_subset.tif", package = "DroneBioR")
#' refl <- scale_to_reflectance(read_multispectral_orthomosaic(ortho_path)$bands)
#' ix <- compute_spectral_indices(refl)
#' names(ix)
#' @export
compute_spectral_indices <- function(reflectance, eps = 1e-6, strict = FALSE) {
  rgb_required <- c("Blue", "Green", "Red")
  rgb_missing <- setdiff(rgb_required, names(reflectance))
  if (length(rgb_missing) > 0) {
    stop("Reflectance raster is missing: ", paste(rgb_missing, collapse = ", "),
         call. = FALSE)
  }
  has_nir     <- "NIR" %in% names(reflectance)
  has_rededge <- "RedEdge" %in% names(reflectance)

  if (isTRUE(strict)) {
    needed_for_full <- c("RedEdge", "NIR")
    full_missing <- setdiff(needed_for_full, names(reflectance))
    if (length(full_missing) > 0) {
      stop("Reflectance raster is missing: ", paste(full_missing, collapse = ", "),
           call. = FALSE)
    }
  }

  blue  <- reflectance[["Blue"]]
  green <- reflectance[["Green"]]
  red   <- reflectance[["Red"]]
  rededge <- if (has_rededge) reflectance[["RedEdge"]] else NULL
  nir     <- if (has_nir)     reflectance[["NIR"]]     else NULL

  indices <- list()
  if (has_nir) {
    ndvi <- safe_ratio(nir - red, nir + red, eps); names(ndvi) <- "NDVI"
    evi  <- 2.5 * safe_ratio(nir - red, nir + 6 * red - 7.5 * blue + 1, eps)
    names(evi) <- "EVI"
    savi <- 1.5 * safe_ratio(nir - red, nir + red + 0.5, eps); names(savi) <- "SAVI"
    ndwi <- safe_ratio(green - nir, green + nir, eps); names(ndwi) <- "NDWI"
    gndvi <- safe_ratio(nir - green, nir + green, eps); names(gndvi) <- "GNDVI"
    msavi_term <- (2 * nir + 1)^2 - 8 * (nir - red)
    msavi2 <- (2 * nir + 1 - terra::ifel(msavi_term < 0, NA, sqrt(msavi_term))) / 2
    names(msavi2) <- "MSAVI2"
    indices$NDVI  <- ndvi
    indices$EVI   <- evi
    indices$SAVI  <- savi
    indices$NDWI  <- ndwi
    indices$GNDVI <- gndvi
    indices$MSAVI2 <- msavi2
  }
  if (has_nir && has_rededge) {
    ndre <- safe_ratio(nir - rededge, nir + rededge, eps); names(ndre) <- "NDRE"
    cirededge <- safe_ratio(nir, rededge, eps) - 1;       names(cirededge) <- "CIrededge"
    indices$NDRE     <- ndre
    indices$CIrededge <- cirededge
  }
  # VARI is RGB-only and always available when Blue/Green/Red are present.
  vari <- safe_ratio(green - red, green + red - blue, eps); names(vari) <- "VARI"
  indices$VARI <- vari

  # Preserve the canonical output order so downstream code that expects
  # a fixed index ordering keeps working when the bands are full.
  # NB: `do.call(c, list_of_spatRasters)` falls through to `base::c` and
  # returns a list, not a SpatRaster. Build the stack with explicit
  # binary `c()` calls so the terra S4 method dispatches correctly.
  canonical_order <- c("NDVI", "NDRE", "EVI", "SAVI", "NDWI",
                       "GNDVI", "CIrededge", "MSAVI2", "VARI")
  out_order <- intersect(canonical_order, names(indices))
  result <- indices[[out_order[1L]]]
  for (nm in out_order[-1L]) {
    result <- c(result, indices[[nm]])
  }
  result
}

#' Compute an image-only biomass proxy
#'
#' @param indices Spectral index stack from `compute_spectral_indices()`.
#' @return A `terra::SpatRaster` named `Biomass_Index_Proxy`.
#' @examples
#' ortho_path <- system.file("extdata", "micasense_subset.tif", package = "DroneBioR")
#' refl <- scale_to_reflectance(read_multispectral_orthomosaic(ortho_path)$bands)
#' proxy <- compute_biomass_proxy(compute_spectral_indices(refl))
#' names(proxy)
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
