#' Default location of the DroneBioR flight registry
#'
#' The registry is a CSV that stores one row per flight: a date, the
#' project directory for that flight, and an optional notes string.
#' The default location is the package's own directory under
#' `tools::R_user_dir("DroneBioR", "data")`, so the same registry can be
#' reused across separate R sessions. It is created on first write, not by
#' asking where it is.
#'
#' @return Absolute path to the default registry CSV.
#' @examples
#' default_flight_registry()
#' @export
default_flight_registry <- function() {
  dronebior_user_file("flights.csv", "data")
}

ensure_flight_registry <- function(registry_path) {
  if (file.exists(registry_path)) return(invisible(registry_path))
  dir.create(dirname(registry_path), recursive = TRUE, showWarnings = FALSE)
  empty <- data.frame(
    flight_id          = character(),
    date               = character(),
    project_dir        = character(),
    notes              = character(),
    odm_dataset_subdir = character(),
    odm_project_name   = character(),
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
#' # Registering a flight only records a path, so a throwaway directory is
#' # enough to demonstrate it; in practice project_dir is the ODM project
#' # directory produced by the flight.
#' reg <- tempfile(fileext = ".csv")
#' register_flight(date = Sys.Date(), project_dir = tempdir(),
#'                 registry_path = reg)
#' list_flights(reg)
#' @export
register_flight <- function(date,
                            project_dir,
                            notes = "",
                            registry_path = default_flight_registry(),
                            odm_dataset_subdir = NA_character_,
                            odm_project_name = NA_character_) {
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
  # Idempotent on (date, project_dir), the two fields flight_id is built from.
  # Matching those directly rather than the id keeps registries written before
  # the hash fix -- where every id degenerated to "<date>-NA" -- from gaining a
  # duplicate row the first time each flight is re-registered.
  if (any(current$date == format(date_parsed, "%Y-%m-%d") &
          current$project_dir == project_dir)) {
    return(invisible(current))
  }
  new_row <- data.frame(
    flight_id          = flight_id,
    date               = format(date_parsed, "%Y-%m-%d"),
    project_dir        = project_dir,
    notes              = as.character(notes %||% ""),
    odm_dataset_subdir = as.character(odm_dataset_subdir %||% NA_character_),
    odm_project_name   = as.character(odm_project_name %||% NA_character_),
    stringsAsFactors = FALSE
  )
  # `current` comes from list_flights(), which backfills the two ODM-layout
  # columns, so it and new_row share a schema and rbind lines up by name.
  updated <- rbind(current[names(new_row)], new_row)
  utils::write.csv(updated, registry_path, row.names = FALSE)
  invisible(updated)
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# Tiny self-contained hash to avoid depending on digest / rlang here.
# The running value is a double reduced modulo 2^31 - 1 on every step so it
# always fits a 32-bit signed integer: accumulating in an R integer overflowed
# to NA after ~9 characters, which collapsed every realistic project path to
# the same digest.
rlang_compatible_hash <- function(x) {
  raw <- charToRaw(paste(x, collapse = "|"))
  modulus <- 2147483647
  h <- 0
  for (b in as.integer(raw)) {
    h <- bitwXor(as.integer((h * 31) %% modulus), b)
  }
  format(as.hexmode(h), width = 8)
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
      flight_id          = character(),
      date               = character(),
      project_dir        = character(),
      notes              = character(),
      odm_dataset_subdir = character(),
      odm_project_name   = character(),
      stringsAsFactors = FALSE
    ))
  }
  df <- utils::read.csv(registry_path, stringsAsFactors = FALSE)
  # Backfill the ODM-layout columns on registries written before they existed,
  # so callers see a stable schema and flight_time_series can rebuild each
  # flight's project with the right sub-project (not just the micasense default).
  if (is.null(df$odm_dataset_subdir)) df$odm_dataset_subdir <- NA_character_
  if (is.null(df$odm_project_name))   df$odm_project_name   <- NA_character_
  df
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
#' \dontrun{
#' # One row per flight, so register the projects first.
#' reg <- tempfile(fileext = ".csv")
#' register_flight("2026-04-01", "~/flights/2026-04-01", registry_path = reg)
#' register_flight("2026-05-01", "~/flights/2026-05-01", registry_path = reg)
#' flight_time_series(flight_ndvi_mean, registry_path = reg)
#' }
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
    # Rebuild each flight with its stored ODM sub-project when the registry
    # recorded one (DJI / Sony / a picker-selected project), else the
    # dronebio_project() defaults. Without this every non-"micasense" flight
    # resolved to a product-less path and silently plotted NA.
    subdir <- flights$odm_dataset_subdir[i]
    name   <- flights$odm_project_name[i]
    proj <- if (!is.null(subdir) && !is.na(subdir) && nzchar(subdir) &&
                !is.null(name) && !is.na(name) && nzchar(name)) {
      dronebio_project(flights$project_dir[i],
                       odm_dataset_subdir = subdir,
                       odm_project_name   = name)
    } else {
      dronebio_project(flights$project_dir[i])
    }
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
  dronebior_user_file("flight_metrics_cache.rds", "cache")
}

read_flight_metric_cache <- function() {
  path <- flight_metric_cache_path()
  if (!file.exists(path)) return(list())
  tryCatch(readRDS(path), error = function(e) list())
}

write_flight_metric_cache <- function(cache) {
  path <- dronebior_user_file("flight_metrics_cache.rds", "cache", create = TRUE)
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
#' \dontrun{
#' # Each helper reads the products of one flight, so point dronebio_project()
#' # at a directory that already holds ODM output.
#' project <- dronebio_project("~/flights/2026-05-01")
#' flight_ndvi_mean(project)
#' flight_biomass_proxy_mean(project)
#' flight_chm_mean(project)
#' }
#' @export
flight_ndvi_mean <- function(project) {
  # Resolve via odm_product_paths so a DJI Mavic 3M flight uses its 7-band
  # odm_orthophoto_dji.tif (project$odm_orthomosaic points at the RGB-only
  # odm_orthophoto.tif, which has no NIR and would return NA).
  ortho_path <- unname(odm_product_paths(project)[["orthomosaic"]])
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
#' @export
flight_biomass_proxy_mean <- function(project) {
  # See flight_ndvi_mean: resolve via odm_product_paths for DJI 7-band orthos.
  ortho_path <- unname(odm_product_paths(project)[["orthomosaic"]])
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
