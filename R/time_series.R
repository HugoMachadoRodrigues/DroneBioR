#' Default location of the DroneBioR flight registry
#'
#' The registry is a CSV that stores one row per flight: a date, the
#' project directory for that flight, and an optional notes string.
#' The default location lives under the user's home directory so the
#' same registry can be reused across separate R sessions.
#'
#' @return Absolute path to the default registry CSV.
#' @examples
#' default_flight_registry()
#' @export
default_flight_registry <- function() {
  dir <- file.path(Sys.getenv("HOME", unset = tempdir()), ".dronebior")
  file.path(dir, "flights.csv")
}

ensure_flight_registry <- function(registry_path) {
  if (file.exists(registry_path)) return(invisible(registry_path))
  dir.create(dirname(registry_path), recursive = TRUE, showWarnings = FALSE)
  empty <- data.frame(
    flight_id   = character(),
    date        = character(),
    project_dir = character(),
    notes       = character(),
    stringsAsFactors = FALSE
  )
  utils::write.csv(empty, registry_path, row.names = FALSE)
  invisible(registry_path)
}

#' Register a flight in the time-series registry
#'
#' Appends one row to the registry CSV. The `flight_id` is auto-generated
#' from the date plus a short hash of the project directory, so repeated
#' calls with the same date and project_dir are idempotent.
#'
#' @param date Date or character that `as.Date()` can parse.
#' @param project_dir Project directory for the flight. Will be
#'   `normalizePath`-ed.
#' @param notes Optional free-text notes.
#' @param registry_path Path to the registry CSV. Defaults to
#'   [default_flight_registry()].
#' @return Invisibly returns the updated registry data frame.
#' @examples
#' reg <- tempfile(fileext = ".csv")
#' project <- dronebio_sample_project(target_dir = tempfile("flight-1-"))
#' register_flight(date = Sys.Date(), project_dir = project$project_dir,
#'                 registry_path = reg)
#' list_flights(reg)
#' @export
register_flight <- function(date,
                            project_dir,
                            notes = "",
                            registry_path = default_flight_registry()) {
  ensure_flight_registry(registry_path)
  # base::as.Date.character() throws on unparseable input rather than returning
  # NA; we wrap it so callers see a uniform "Could not parse date" message.
  date_parsed <- tryCatch(as.Date(date), error = function(e) NA)
  if (is.na(date_parsed)) {
    stop("Could not parse date: ", date, call. = FALSE)
  }
  project_dir <- normalizePath(project_dir, mustWork = FALSE)

  flight_id <- paste0(
    format(date_parsed, "%Y%m%d"),
    "-",
    substr(rlang_compatible_hash(project_dir), 1L, 8L)
  )
  current <- list_flights(registry_path)
  if (flight_id %in% current$flight_id) {
    # Idempotent: do not re-append the same flight.
    return(invisible(current))
  }
  new_row <- data.frame(
    flight_id   = flight_id,
    date        = format(date_parsed, "%Y-%m-%d"),
    project_dir = project_dir,
    notes       = as.character(notes %||% ""),
    stringsAsFactors = FALSE
  )
  updated <- rbind(current, new_row)
  utils::write.csv(updated, registry_path, row.names = FALSE)
  invisible(updated)
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# Tiny self-contained hash to avoid depending on digest / rlang here.
rlang_compatible_hash <- function(x) {
  raw <- charToRaw(paste(x, collapse = "|"))
  h <- 0L
  for (b in as.integer(raw)) {
    h <- bitwXor(bitwShiftL(h, 5L) - h, b)
  }
  format(as.hexmode(abs(h)), width = 8)
}

#' List flights registered in the time-series registry
#'
#' @param registry_path Path to the registry CSV. Defaults to
#'   [default_flight_registry()].
#' @return A data frame with columns `flight_id`, `date`,
#'   `project_dir`, `notes`.
#' @examples
#' reg <- tempfile(fileext = ".csv")
#' list_flights(reg)  # empty registry
#' @export
list_flights <- function(registry_path = default_flight_registry()) {
  if (!file.exists(registry_path)) {
    return(data.frame(
      flight_id   = character(),
      date        = character(),
      project_dir = character(),
      notes       = character(),
      stringsAsFactors = FALSE
    ))
  }
  utils::read.csv(registry_path, stringsAsFactors = FALSE)
}

#' Compute a time series of a custom flight summary
#'
#' For each registered flight, opens the project at `flights$project_dir`
#' and applies `summary_fn(project)` to obtain a single numeric value.
#' Returns the values keyed by date so the result can be plotted directly
#' as a time series.
#'
#' Errors thrown by `summary_fn` for individual flights surface as `NA`
#' values, so a partial registry (e.g. a few flights with missing
#' orthomosaics) still produces a usable plot.
#'
#' @param summary_fn A function that takes a `dronebio_project` and
#'   returns a single numeric value.
#' @param registry_path Path to the registry CSV.
#' @return A data frame with columns `date`, `value`, `flight_id`,
#'   `project_dir`.
#' @examples
#' reg <- tempfile(fileext = ".csv")
#' project <- dronebio_sample_project(target_dir = tempfile("ts-flight-"))
#' register_flight(Sys.Date(), project$project_dir, registry_path = reg)
#' ts <- flight_time_series(flight_ndvi_mean, registry_path = reg)
#' ts
#' @export
flight_time_series <- function(summary_fn,
                               registry_path = default_flight_registry()) {
  if (!is.function(summary_fn)) {
    stop("`summary_fn` must be a function taking a dronebio_project.",
         call. = FALSE)
  }
  flights <- list_flights(registry_path)
  if (nrow(flights) == 0) {
    return(data.frame(
      date        = as.Date(character()),
      value       = numeric(),
      flight_id   = character(),
      project_dir = character(),
      stringsAsFactors = FALSE
    ))
  }
  values <- vapply(seq_len(nrow(flights)), function(i) {
    proj <- dronebio_project(flights$project_dir[i])
    tryCatch(as.numeric(summary_fn(proj))[[1]],
             error = function(e) NA_real_)
  }, numeric(1))
  result <- data.frame(
    date        = as.Date(flights$date),
    value       = values,
    flight_id   = flights$flight_id,
    project_dir = flights$project_dir,
    stringsAsFactors = FALSE
  )
  result[order(result$date), , drop = FALSE]
}

#' Stock summary helpers for [flight_time_series()]
#'
#' Each helper accepts a `dronebio_project` and returns a single numeric
#' value, suitable for use as the `summary_fn` argument of
#' [flight_time_series()].
#'
#' @param project A `dronebio_project` object.
#' @return A single numeric value, or `NA` when the underlying product
#'   is missing.
#' @name flight_summary_helpers
NULL

# Per-flight metric cache. Each entry is keyed by the metric name plus
# the source file path and mtime; this way reopening the same flight
# returns instantly while a freshly written ODM output transparently
# invalidates the cached value. The cache lives under
# `~/.dronebior/flight_metrics_cache.rds` so it survives R session
# restarts and is shared between the Time Series tab and the
# command-line callers of `flight_time_series()`.
flight_metric_cache_path <- function() {
  dir <- file.path(Sys.getenv("HOME", unset = tempdir()), ".dronebior")
  file.path(dir, "flight_metrics_cache.rds")
}

read_flight_metric_cache <- function() {
  path <- flight_metric_cache_path()
  if (!file.exists(path)) return(list())
  tryCatch(readRDS(path), error = function(e) list())
}

write_flight_metric_cache <- function(cache) {
  path <- flight_metric_cache_path()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tryCatch(saveRDS(cache, path), error = function(e) NULL)
  invisible(path)
}

flight_metric_key <- function(metric, source_path) {
  if (!file.exists(source_path)) return(NA_character_)
  info <- file.info(source_path)
  paste0(metric, "|", normalizePath(source_path, mustWork = FALSE),
         "|", info$size, "|", info$mtime)
}

cached_flight_metric <- function(metric, source_path, compute) {
  key <- flight_metric_key(metric, source_path)
  if (is.na(key)) return(NA_real_)
  cache <- read_flight_metric_cache()
  if (!is.null(cache[[key]]) && is.finite(cache[[key]])) {
    return(cache[[key]])
  }
  value <- tryCatch(as.numeric(compute()), error = function(e) NA_real_)
  cache[[key]] <- value
  write_flight_metric_cache(cache)
  value
}

#' @rdname flight_summary_helpers
#' @examples
#' project <- dronebio_sample_project(target_dir = tempfile("ts-ndvi-"))
#' flight_ndvi_mean(project)
#' @export
flight_ndvi_mean <- function(project) {
  ortho_path <- project$odm_orthomosaic
  if (!file.exists(ortho_path)) return(NA_real_)
  cached_flight_metric("ndvi_mean", ortho_path, function() {
    # NDVI-only path: read just the Red + NIR bands, scale them, and
    # compute the single index. The previous implementation called
    # compute_spectral_indices() which materialises ~22 SpatRaster
    # operations -- on a 22k x 20k MicaSense ortho that is minutes of
    # work even though we only need NDVI.
    ortho <- read_multispectral_orthomosaic(ortho_path)
    bands <- ortho$bands
    needed <- c("Red", "NIR")
    missing_bands <- setdiff(needed, names(bands))
    if (length(missing_bands) > 0L) return(NA_real_)
    bands_subset <- bands[[needed]]
    refl <- scale_to_reflectance(bands_subset)
    nir <- refl[["NIR"]]
    red <- refl[["Red"]]
    ndvi <- safe_ratio(nir - red, nir + red, eps = 1e-6)
    as.numeric(terra::global(ndvi, "mean", na.rm = TRUE)[, 1])
  })
}

#' @rdname flight_summary_helpers
#' @examples
#' flight_biomass_proxy_mean(project)
#' @export
flight_biomass_proxy_mean <- function(project) {
  ortho_path <- project$odm_orthomosaic
  if (!file.exists(ortho_path)) return(NA_real_)
  cached_flight_metric("biomass_proxy_mean", ortho_path, function() {
    ortho <- read_multispectral_orthomosaic(ortho_path)
    refl  <- scale_to_reflectance(ortho$bands)
    ix    <- compute_spectral_indices(refl)
    proxy <- compute_biomass_proxy(ix)
    as.numeric(terra::global(proxy, "mean", na.rm = TRUE)[, 1])
  })
}

#' @rdname flight_summary_helpers
#' @examples
#' flight_chm_mean(project)
#' @export
flight_chm_mean <- function(project) {
  dsm <- file.path(project$odm_project_dir, "odm_dem", "dsm.tif")
  dtm <- file.path(project$odm_project_dir, "odm_dem", "dtm.tif")
  if (!file.exists(dsm) || !file.exists(dtm)) return(NA_real_)
  # Prefer the persisted chm.tif when present and fresher than its
  # DSM/DTM parents: tens of milliseconds vs a full subtract +
  # clamp + write pass on every call. Falls back to in-memory build
  # for projects that have not been through build_chm_raster() yet.
  chm_path <- file.path(dirname(dsm), "chm.tif")
  if (file.exists(chm_path) &&
      isTRUE(file.info(chm_path)$mtime >=
             max(file.info(dsm)$mtime, file.info(dtm)$mtime))) {
    return(cached_flight_metric("chm_mean", chm_path, function() {
      chm <- terra::rast(chm_path)
      as.numeric(terra::global(chm, "mean", na.rm = TRUE)[, 1])
    }))
  }
  cached_flight_metric("chm_mean_inmem", dsm, function() {
    chm <- build_chm_from_dsm_dtm(dsm, dtm)
    as.numeric(terra::global(chm, "mean", na.rm = TRUE)[, 1])
  })
}
