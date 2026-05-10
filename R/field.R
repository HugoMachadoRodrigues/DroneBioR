#' Read field biomass data
#'
#' @param path CSV path.
#' @return A data frame.
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
