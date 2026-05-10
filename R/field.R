#' Read field biomass data
#'
#' @param path CSV path.
#' @return A data frame.
#' @examples
#' field_path <- system.file("extdata", "field_samples.csv", package = "DroneBioR")
#' field <- read_field_data(field_path)
#' head(field)
#' @export
read_field_data <- function(path) {
  if (!file.exists(path)) {
    stop("Field data file not found: ", path, call. = FALSE)
  }
  x <- utils::read.csv(path, stringsAsFactors = FALSE)
  required <- c("sample_id", "biomass_kgha")
  missing <- setdiff(required, names(x))
  if (length(missing) > 0) {
    stop("Field data is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  has_xy <- all(c("x", "y") %in% names(x))
  has_lonlat <- all(c("longitude", "latitude") %in% names(x))
  if (!has_xy && !has_lonlat) {
    stop("Field data needs x/y or longitude/latitude columns.", call. = FALSE)
  }
  x
}

#' Extract raster values at field sample points
#'
#' @param field_data Field data frame.
#' @param predictors Raster stack with bands and indices.
#' @param predictor_crs CRS of x/y coordinates when x/y are used.
#' @return A data frame with field columns and extracted raster values.
#' @examples
#' ortho_path <- system.file("extdata", "micasense_subset.tif", package = "DroneBioR")
#' field_path <- system.file("extdata", "field_samples.csv", package = "DroneBioR")
#' refl <- scale_to_reflectance(read_multispectral_orthomosaic(ortho_path)$bands)
#' ix <- compute_spectral_indices(refl)
#' field <- read_field_data(field_path)
#' head(extract_field_spectral_data(field, ix))
#' @export
extract_field_spectral_data <- function(field_data, predictors, predictor_crs = terra::crs(predictors)) {
  if (all(c("x", "y") %in% names(field_data))) {
    points <- sf::st_as_sf(field_data, coords = c("x", "y"), crs = predictor_crs, remove = FALSE)
  } else if (all(c("longitude", "latitude") %in% names(field_data))) {
    points <- sf::st_as_sf(field_data, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
    if (nzchar(terra::crs(predictors))) {
      points <- sf::st_transform(points, terra::crs(predictors))
    }
  } else {
    stop("Field data needs x/y or longitude/latitude columns.", call. = FALSE)
  }

  vect_points <- terra::vect(points)
  values <- terra::extract(predictors, vect_points, ID = FALSE)
  data.frame(sf::st_drop_geometry(points), values, check.names = FALSE)
}

#' Fit a baseline biomass linear model
#'
#' @param data Data frame containing biomass and predictor columns.
#' @param response Response column.
#' @param predictors Optional predictor columns.
#' @return An `lm` object.
#' @examples
#' set.seed(1)
#' ndvi <- runif(20, 0.3, 0.9)
#' field <- data.frame(
#'   sample_id = sprintf("S%02d", 1:20),
#'   biomass_kgha = 1000 + 3000 * ndvi + rnorm(20, sd = 200),
#'   NDVI = ndvi
#' )
#' model <- fit_biomass_lm(field, predictors = "NDVI")
#' coef(model)
#' @export
fit_biomass_lm <- function(data,
                           response = "biomass_kgha",
                           predictors = NULL) {
  if (is.null(predictors)) {
    predictors <- intersect(c("NDVI", "NDRE", "EVI", "SAVI", "NDWI", "NIR", "RedEdge"), names(data))
  }
  if (length(predictors) == 0) {
    stop("No predictor columns were found for biomass modeling.", call. = FALSE)
  }

  model_data <- data[, c(response, predictors), drop = FALSE]
  model_data <- model_data[stats::complete.cases(model_data), , drop = FALSE]
  if (nrow(model_data) < length(predictors) + 2) {
    stop("Not enough complete samples for the selected baseline model.", call. = FALSE)
  }

  stats::lm(stats::as.formula(paste(response, "~", paste(predictors, collapse = " + "))), data = model_data)
}
