safe_ratio <- function(numerator, denominator, eps = 1e-6) {
  terra::ifel(abs(denominator) <= eps, NA, numerator / denominator)
}

#' Compute spectral vegetation indices
#'
#' Computes the subset of indices the input bands actually support. The
#' strict minimum is **Green + Red**; Blue, RedEdge and NIR each unlock
#' additional indices when present. A 5-band MicaSense reflectance
#' (Blue + Green + Red + RedEdge + NIR) yields the full set; a 4-band
#' DJI Mavic 3M reflectance (Green + Red + RedEdge + NIR, no calibrated
#' Blue) yields every index except the five Blue-dependent ones (EVI,
#' VARI, ExG, GLI, TGI, RGBVI); an RGB-only image still gets the
#' visible-band indices (VARI, ExG, GLI, TGI, MGRVI, RGBVI). All formulas
#' are referenced below; see the `?<index>` modal in the Shiny UI for
#' the citation next to each one.
#'
#' Indices computed when **NIR** is present:
#'   NDVI    Rouse et al. 1974   (NIR - Red) / (NIR + Red)
#'   EVI     Huete et al. 2002   2.5 * (NIR - Red) / (NIR + 6*Red - 7.5*Blue + 1)
#'   SAVI    Huete 1988          1.5 * (NIR - Red) / (NIR + Red + 0.5)
#'   OSAVI   Rondeaux 1996       (NIR - Red) / (NIR + Red + 0.16)
#'   MSAVI2  Qi 1994             (2*NIR + 1 - sqrt((2*NIR+1)^2 - 8*(NIR-Red))) / 2
#'   NDWI    McFeeters 1996      (Green - NIR) / (Green + NIR)
#'   GNDVI   Gitelson 1996       (NIR - Green) / (NIR + Green)
#'   GCI     Gitelson 2003       NIR / Green - 1
#'   RVI     Jordan 1969         NIR / Red
#'   DVI     Tucker 1979         NIR - Red
#'   WDRVI   Gitelson 2004       (0.2*NIR - Red) / (0.2*NIR + Red)
#'   TVI     Broge 2001          0.5 * (120*(NIR-Green) - 200*(Red-Green))
#'
#' Indices computed when **both NIR and RedEdge** are present:
#'   NDRE        Gitelson 1994   (NIR - RedEdge) / (NIR + RedEdge)
#'   CIrededge   Gitelson 2003   NIR / RedEdge - 1
#'   MCARI       Daughtry 2000   ((RedEdge - Red) - 0.2*(RedEdge - Green)) * (RedEdge / Red)
#'   PSRI        Merzlyak 1999   (Red - Green) / RedEdge
#'
#' Indices that work on **RGB-only** orthomosaics (always computed):
#'   VARI    Gitelson 2002       (Green - Red) / (Green + Red - Blue)
#'   ExG     Woebbecke 1995      2*Green - Red - Blue
#'   GLI     Louhaichi 2001      (2*Green - Red - Blue) / (2*Green + Red + Blue)
#'   TGI     Hunt 2013           -0.5 * (190*(Red - Green) - 120*(Red - Blue))
#'   MGRVI   Bendig 2015         (Green^2 - Red^2) / (Green^2 + Red^2)
#'   RGBVI   Bendig 2015         (Green^2 - Red*Blue) / (Green^2 + Red*Blue)
#'
#' Pass `strict = TRUE` to keep the legacy behaviour where missing
#' Blue / RedEdge / NIR raises an error; the default is to silently
#' compute whatever is possible, which is what Drone Biomass Studio
#' needs to degrade gracefully for RGB-only and MS-only datasets.
#'
#' @param reflectance A reflectance-scale `terra::SpatRaster`. Must
#'   contain at least Green and Red; Blue, RedEdge and NIR each unlock
#'   additional indices when present.
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
  # Green + Red are the strict minimum (every supported index uses at
  # least one of them, and the visible-band indices need both). Blue is
  # optional: the DJI Mavic 3M and similar 4-band MS rigs do not capture
  # a calibrated blue. When Blue is absent we skip the Blue-dependent
  # indices (EVI, VARI, ExG, GLI, TGI, RGBVI) rather than erroring, so
  # MS-only datasets still produce a full NDVI / NDRE / SAVI / ... stack.
  minimum_required <- c("Green", "Red")
  min_missing <- setdiff(minimum_required, names(reflectance))
  if (length(min_missing) > 0) {
    stop("Reflectance raster is missing: ", paste(min_missing, collapse = ", "),
         call. = FALSE)
  }
  has_blue    <- "Blue" %in% names(reflectance)
  has_nir     <- "NIR" %in% names(reflectance)
  has_rededge <- "RedEdge" %in% names(reflectance)

  if (isTRUE(strict)) {
    needed_for_full <- c("Blue", "RedEdge", "NIR")
    full_missing <- setdiff(needed_for_full, names(reflectance))
    if (length(full_missing) > 0) {
      stop("Reflectance raster is missing: ", paste(full_missing, collapse = ", "),
           call. = FALSE)
    }
  }

  blue    <- if (has_blue)    reflectance[["Blue"]]    else NULL
  green   <- reflectance[["Green"]]
  red     <- reflectance[["Red"]]
  rededge <- if (has_rededge) reflectance[["RedEdge"]] else NULL
  nir     <- if (has_nir)     reflectance[["NIR"]]     else NULL

  indices <- list()
  if (has_nir) {
    ndvi <- safe_ratio(nir - red, nir + red, eps); names(ndvi) <- "NDVI"
    savi <- 1.5 * safe_ratio(nir - red, nir + red + 0.5, eps); names(savi) <- "SAVI"
    osavi <- safe_ratio(nir - red, nir + red + 0.16, eps); names(osavi) <- "OSAVI"
    ndwi <- safe_ratio(green - nir, green + nir, eps); names(ndwi) <- "NDWI"
    gndvi <- safe_ratio(nir - green, nir + green, eps); names(gndvi) <- "GNDVI"
    msavi_term <- (2 * nir + 1)^2 - 8 * (nir - red)
    msavi2 <- (2 * nir + 1 - terra::ifel(msavi_term < 0, NA, sqrt(msavi_term))) / 2
    names(msavi2) <- "MSAVI2"
    gci <- safe_ratio(nir, green, eps) - 1; names(gci) <- "GCI"
    rvi <- safe_ratio(nir, red, eps); names(rvi) <- "RVI"
    dvi <- nir - red; names(dvi) <- "DVI"
    wdrvi <- safe_ratio(0.2 * nir - red, 0.2 * nir + red, eps); names(wdrvi) <- "WDRVI"
    tvi   <- 0.5 * (120 * (nir - green) - 200 * (red - green)); names(tvi) <- "TVI"
    indices$NDVI   <- ndvi
    indices$SAVI   <- savi
    indices$OSAVI  <- osavi
    indices$NDWI   <- ndwi
    indices$GNDVI  <- gndvi
    indices$MSAVI2 <- msavi2
    indices$GCI    <- gci
    indices$RVI    <- rvi
    indices$DVI    <- dvi
    indices$WDRVI  <- wdrvi
    indices$TVI    <- tvi
    # EVI uses Blue; skip when no Blue band is available.
    if (has_blue) {
      evi <- 2.5 * safe_ratio(nir - red, nir + 6 * red - 7.5 * blue + 1, eps)
      names(evi) <- "EVI"
      indices$EVI <- evi
    }
  }
  if (has_nir && has_rededge) {
    ndre <- safe_ratio(nir - rededge, nir + rededge, eps); names(ndre) <- "NDRE"
    cirededge <- safe_ratio(nir, rededge, eps) - 1;       names(cirededge) <- "CIrededge"
    mcari <- ((rededge - red) - 0.2 * (rededge - green)) * safe_ratio(rededge, red, eps)
    names(mcari) <- "MCARI"
    psri <- safe_ratio(red - green, rededge, eps); names(psri) <- "PSRI"
    indices$NDRE      <- ndre
    indices$CIrededge <- cirededge
    indices$MCARI     <- mcari
    indices$PSRI      <- psri
  }
  # Visible-band indices: MGRVI works on Green + Red alone; the rest
  # need Blue and are skipped when it is missing.
  mgrvi <- safe_ratio(green^2 - red^2, green^2 + red^2, eps); names(mgrvi) <- "MGRVI"
  indices$MGRVI <- mgrvi
  if (has_blue) {
    vari <- safe_ratio(green - red, green + red - blue, eps); names(vari) <- "VARI"
    exg  <- 2 * green - red - blue; names(exg) <- "ExG"
    gli  <- safe_ratio(2 * green - red - blue, 2 * green + red + blue, eps); names(gli) <- "GLI"
    tgi  <- -0.5 * (190 * (red - green) - 120 * (red - blue)); names(tgi) <- "TGI"
    rgbvi <- safe_ratio(green^2 - red * blue, green^2 + red * blue, eps); names(rgbvi) <- "RGBVI"
    indices$VARI  <- vari
    indices$ExG   <- exg
    indices$GLI   <- gli
    indices$TGI   <- tgi
    indices$RGBVI <- rgbvi
  }

  # Preserve the canonical output order so downstream code that expects
  # a fixed index ordering keeps working when the bands are full.
  # NB: `do.call(c, list_of_spatRasters)` falls through to `base::c` and
  # returns a list, not a SpatRaster. Build the stack with explicit
  # binary `c()` calls so the terra S4 method dispatches correctly.
  canonical_order <- c(
    "NDVI", "NDRE", "EVI", "SAVI", "OSAVI", "MSAVI2", "NDWI",
    "GNDVI", "CIrededge", "GCI", "RVI", "DVI", "WDRVI", "TVI",
    "MCARI", "PSRI",
    "VARI", "ExG", "GLI", "TGI", "MGRVI", "RGBVI"
  )
  out_order <- intersect(canonical_order, names(indices))
  result <- indices[[out_order[1L]]]
  for (nm in out_order[-1L]) {
    result <- c(result, indices[[nm]])
  }
  result
}

#' Compute an image-only biomass proxy
#'
#' Combines NDVI, SAVI and NDRE into a single -1..1 surface. Use as a
#' qualitative biomass surrogate when no canopy-height information is
#' available. For a more defensible biomass estimate, use
#' [compute_biomass_proxies()] which also produces height-weighted
#' variants (greenness x CHM).
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

#' Compute multiple biomass-proxy rasters
#'
#' Returns a stack of biomass-related surfaces derived from a spectral
#' index stack and (optionally) a Canopy Height Model. The greenness x
#' height products (`NDVI_CHM`, `NDRE_CHM`, ...) are the most defensible
#' image-only proxies for above-ground biomass on drone surveys: a
#' multiplicative greenness x canopy-height surface tracks the volume
#' of photosynthetically active material per pixel, which scales with
#' fresh biomass for most herbaceous and shrub canopies (Bendig et al.
#' 2015; Lussem et al. 2019).
#'
#' Without a CHM the function still returns the pure-spectral proxies
#' so RGB-only or DSM-only datasets get something useful. None of these
#' are biomass in kg/ha without field calibration - they are surfaces
#' that correlate with biomass and should be regressed against ground
#' truth.
#'
#' Layers always returned when the input indices allow:
#'   Biomass_Spectral   mean(NDVI, SAVI, NDRE) clipped to `[-1, 1]`
#'                      (the legacy `compute_biomass_proxy()` output)
#'   Biomass_NDVI_x_CHM NDVI * CHM (m greenness)
#'   Biomass_NDRE_x_CHM NDRE * CHM (m greenness, RedEdge)
#'   Biomass_SAVI_x_CHM SAVI * CHM
#'   Biomass_GNDVI_x_CHM GNDVI * CHM
#'   Biomass_VARI_x_CHM VARI * CHM (RGB-only)
#'   Biomass_EXG_x_CHM  ExG * CHM (RGB-only; common in turf / crop UAS)
#'   Biomass_MGRVI_x_CHM MGRVI * CHM (RGB-only; Bendig 2015)
#'   Biomass_RGBVI_x_CHM RGBVI * CHM (RGB-only; Bendig 2015)
#'
#' @param indices Spectral index stack from `compute_spectral_indices()`.
#' @param chm Optional `terra::SpatRaster` with the Canopy Height Model
#'   (m above ground). When `NULL` only the spectral biomass surface is
#'   returned. The CHM is resampled onto the index grid when geometry
#'   differs.
#' @return A `terra::SpatRaster` with one or more biomass-proxy layers.
#' @examples
#' \dontrun{
#'   refl <- scale_to_reflectance(read_multispectral_orthomosaic(path)$bands)
#'   ix   <- compute_spectral_indices(refl)
#'   chm  <- terra::rast("chm.tif")
#'   compute_biomass_proxies(ix, chm)
#' }
#' @export
compute_biomass_proxies <- function(indices, chm = NULL) {
  out <- list()

  if (all(c("NDVI", "SAVI", "NDRE") %in% names(indices))) {
    bs <- terra::clamp(
      (indices[["NDVI"]] + indices[["SAVI"]] + indices[["NDRE"]]) / 3,
      lower = -1, upper = 1, values = TRUE
    )
    names(bs) <- "Biomass_Spectral"
    out$Biomass_Spectral <- bs
  } else if ("VARI" %in% names(indices)) {
    bs <- terra::clamp(indices[["VARI"]], lower = -1, upper = 1, values = TRUE)
    names(bs) <- "Biomass_Spectral"
    out$Biomass_Spectral <- bs
  }

  if (!is.null(chm)) {
    chm_b <- chm[[1]]
    # Align CHM onto the index grid when needed - some Pipelines crop the
    # ortho a few pixels tighter than the DSM, so geometries seldom match.
    ref <- indices[[1]]
    if (!terra::compareGeom(ref, chm_b, stopOnError = FALSE,
                            lyrs = FALSE, messages = FALSE)) {
      chm_b <- terra::resample(chm_b, ref, method = "bilinear")
    }
    # Negative noise from DSM-DTM near edges of the canopy is meaningless
    # for biomass; clamp to >= 0 before multiplying so the proxy stays
    # non-negative.
    chm_b <- terra::clamp(chm_b, lower = 0, upper = Inf, values = TRUE)

    multiply <- function(ix_name, out_name) {
      if (!ix_name %in% names(indices)) return(NULL)
      r <- indices[[ix_name]] * chm_b
      names(r) <- out_name
      r
    }
    candidates <- list(
      Biomass_NDVI_x_CHM  = "NDVI",
      Biomass_NDRE_x_CHM  = "NDRE",
      Biomass_SAVI_x_CHM  = "SAVI",
      Biomass_GNDVI_x_CHM = "GNDVI",
      Biomass_VARI_x_CHM  = "VARI",
      Biomass_EXG_x_CHM   = "ExG",
      Biomass_MGRVI_x_CHM = "MGRVI",
      Biomass_RGBVI_x_CHM = "RGBVI"
    )
    for (out_name in names(candidates)) {
      r <- multiply(candidates[[out_name]], out_name)
      if (!is.null(r)) out[[out_name]] <- r
    }
  }

  if (length(out) == 0) {
    stop("compute_biomass_proxies(): the index stack has none of the supported bands.",
         call. = FALSE)
  }
  result <- out[[1]]
  for (nm in names(out)[-1L]) {
    result <- c(result, out[[nm]])
  }
  result
}

#' Compute and export every covariate to a folder
#'
#' Builds the full covariate set the Field Models tab can use -- the reflectance
#' bands, every spectral index [compute_spectral_indices()] can derive, the
#' biomass proxies [compute_biomass_proxies()] (the canopy-height ones when a
#' CHM is supplied), and the terrain layers -- and writes each as its own
#' GeoTIFF into `out_dir` (a folder named `covariates/` by convention), at the
#' orthomosaic's native resolution, ready to load into any GIS afterwards.
#'
#' Layers that cannot be produced (e.g. the biomass proxies when the cloud has
#' none of the bands they need) are skipped, not fatal, so the export always
#' returns whatever could be built.
#'
#' @param reflectance A `terra::SpatRaster` of the scaled reflectance bands
#'   (named, e.g. `Red`, `Green`, `NIR`, `RedEdge`).
#' @param out_dir Destination folder. Created if missing.
#' @param chm,dsm,dtm Optional terrain `SpatRaster`s; each exported when given,
#'   and the CHM also feeds the `*_x_CHM` biomass proxies.
#' @param custom_index Optional single-layer `SpatRaster` of a user index.
#' @param overwrite Overwrite existing files (default `TRUE`); when `FALSE`,
#'   existing outputs are kept and still reported as written.
#' @return Invisibly, a character vector of the GeoTIFFs written.
#' @examples
#' \dontrun{
#' refl <- read_multispectral_orthomosaic("odm_orthophoto.tif")
#' chm  <- build_chm_from_dsm_dtm("odm_dem/dsm.tif", "odm_dem/dtm.tif")
#' export_all_covariates(refl, file.path(project$project_dir, "covariates"),
#'                       chm = chm)
#' }
#' @export
export_all_covariates <- function(reflectance, out_dir,
                                  chm = NULL, dsm = NULL, dtm = NULL,
                                  custom_index = NULL, overwrite = TRUE) {
  if (!inherits(reflectance, "SpatRaster")) {
    stop("`reflectance` must be a terra SpatRaster.", call. = FALSE)
  }
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  indices <- compute_spectral_indices(reflectance)
  proxies <- tryCatch(compute_biomass_proxies(indices, chm = chm),
                      error = function(e) NULL)

  # One named single-layer raster per covariate, in a stable order.
  layers <- list()
  add <- function(nm, r) if (!is.null(r)) layers[[nm]] <<- r
  for (b in names(reflectance)) add(b, reflectance[[b]])
  for (i in names(indices))     add(i, indices[[i]])
  if (!is.null(proxies)) for (nm in names(proxies)) add(nm, proxies[[nm]])
  if (inherits(chm, "SpatRaster")) add("CHM", chm[[1L]])
  if (inherits(dsm, "SpatRaster")) add("DSM", dsm[[1L]])
  if (inherits(dtm, "SpatRaster")) add("DTM", dtm[[1L]])
  if (inherits(custom_index, "SpatRaster")) {
    add(names(custom_index)[[1L]] %||% "Custom_Index", custom_index[[1L]])
  }

  safe <- function(nm) gsub("[^A-Za-z0-9._-]+", "_", nm)
  write_one <- function(r, dst) {
    tryCatch({
      terra::writeRaster(r, dst, overwrite = TRUE, filetype = "COG",
                         gdal = c("COMPRESS=DEFLATE", "BIGTIFF=IF_SAFER"))
      TRUE
    }, error = function(e) {
      # Older GDAL builds reject the COG driver; plain GeoTIFF is universal.
      tryCatch({
        terra::writeRaster(r, dst, overwrite = TRUE, filetype = "GTiff",
                           gdal = "COMPRESS=DEFLATE")
        TRUE
      }, error = function(e2) FALSE)
    })
  }

  written <- character()
  for (nm in names(layers)) {
    dst <- file.path(out_dir, paste0(safe(nm), ".tif"))
    if (!isTRUE(overwrite) && file.exists(dst)) {
      written <- c(written, dst)
      next
    }
    if (isTRUE(write_one(layers[[nm]], dst))) written <- c(written, dst)
  }
  invisible(written)
}
