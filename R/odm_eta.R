# Helpers for ETA estimation of ODM runs from a stage-completion history.
# All internal: the Shiny app (`inst/shiny/DroneBiomassStudio/app.R`) consumes
# them via DroneBioR:::. No `@export` tags so the namespace stays tidy.

#' Baseline per-stage durations (seconds) for an ODM run.
#'
#' Hand-tuned for a 100-image / 5 cm / 10-core RGB run on a modern Apple
#' Silicon laptop. Only used when no history rows exist for the stage.
#' Override by populating ~/.dronebior/odm_stage_history.csv.
#' @noRd
odm_stage_baseline_seconds <- function() {
  c(
    dataset            = 5,
    split              = 1,
    merge              = 1,
    opensfm            = 45 * 60,
    openmvs            = 30 * 60,
    odm_filterpoints   = 3 * 60,
    odm_meshing        = 8 * 60,
    mvs_texturing      = 15 * 60,
    odm_georeferencing = 2 * 60,
    odm_dem            = 8 * 60,
    odm_orthophoto     = 8 * 60,
    odm_report         = 60,
    odm_postprocess    = 30
  )
}

#' Canonical pipeline stage order for ODM 3.6.
#' @noRd
odm_stage_order <- function() {
  c("dataset", "split", "merge", "opensfm", "openmvs",
    "odm_filterpoints", "odm_meshing", "mvs_texturing",
    "odm_georeferencing", "odm_dem", "odm_orthophoto",
    "odm_report", "odm_postprocess")
}

#' Path to the persistent ODM stage-history CSV under ~/.dronebior.
#' @noRd
odm_history_path <- function() {
  dir <- file.path(Sys.getenv("HOME"), ".dronebior")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  file.path(dir, "odm_stage_history.csv")
}

#' Read the history. Returns an empty data frame when the file is missing.
#' @noRd
read_odm_stage_history <- function() {
  path <- odm_history_path()
  if (!file.exists(path)) {
    return(data.frame(
      run_started_at   = character(),
      image_count      = integer(),
      stage            = character(),
      duration_seconds = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE),
    error = function(e) {
      data.frame(
        run_started_at   = character(),
        image_count      = integer(),
        stage            = character(),
        duration_seconds = numeric(),
        stringsAsFactors = FALSE
      )
    }
  )
}

#' Append one stage-completion row, replacing any duplicate (run, stage).
#' @noRd
record_odm_stage_completion <- function(run_started_at, image_count, stage, duration_seconds) {
  if (!is.finite(duration_seconds) || duration_seconds < 0) {
    return(invisible(FALSE))
  }
  hist <- read_odm_stage_history()
  new_row <- data.frame(
    run_started_at   = as.character(run_started_at),
    image_count      = as.integer(image_count),
    stage            = as.character(stage),
    duration_seconds = as.numeric(duration_seconds),
    stringsAsFactors = FALSE
  )
  dup <- hist$run_started_at == new_row$run_started_at[1L] &
         hist$stage          == new_row$stage[1L]
  if (any(dup)) hist <- hist[!dup, , drop = FALSE]
  out <- rbind(hist, new_row)
  utils::write.csv(out, odm_history_path(), row.names = FALSE)
  invisible(TRUE)
}

#' Estimate the duration of `stage` for an upcoming run of `image_count`.
#'
#' Strategy: take the median of historical durations for that stage,
#' linearly scaled by the ratio of `image_count` to the median historical
#' count. Falls back to the hardcoded baseline when no history exists.
#' @noRd
estimate_odm_stage_seconds <- function(stage, image_count = NA_integer_) {
  hist <- read_odm_stage_history()
  rows <- hist[hist$stage == stage & is.finite(hist$duration_seconds), , drop = FALSE]
  if (!nrow(rows)) {
    baseline <- odm_stage_baseline_seconds()
    return(unname(baseline[stage] %||% 60))
  }
  if (is.finite(image_count) && image_count > 0 && nrow(rows)) {
    counts <- rows$image_count[is.finite(rows$image_count) & rows$image_count > 0]
    median_count <- if (length(counts)) stats::median(counts) else NA_real_
    scale <- if (is.finite(median_count) && median_count > 0) {
      image_count / median_count
    } else 1
    return(stats::median(rows$duration_seconds, na.rm = TRUE) * scale)
  }
  stats::median(rows$duration_seconds, na.rm = TRUE)
}

#' Sum the remaining seconds (active + pending) for a partially-finished run.
#'
#' `active_elapsed_seconds` is subtracted from the active stage's estimate so
#' the ETA shrinks as the active stage progresses. Pending stages contribute
#' their full estimate.
#' @noRd
estimate_remaining_seconds <- function(active_stage,
                                       pending_stages,
                                       active_elapsed_seconds = 0,
                                       image_count = NA_integer_) {
  active_est <- if (!is.null(active_stage) && !is.na(active_stage)) {
    max(0, estimate_odm_stage_seconds(active_stage, image_count) - active_elapsed_seconds)
  } else 0
  pending_est <- if (length(pending_stages)) {
    sum(vapply(pending_stages, estimate_odm_stage_seconds, numeric(1),
               image_count = image_count))
  } else 0
  active_est + pending_est
}

#' Best-effort camera-type detection from a folder of source images.
#'
#' Counts file extensions and returns one of `"multispectral"`, `"rgb"`
#' or `NA_character_` when uncertain. Multispectral is inferred from
#' MicaSense / Sequoia .tif filenames; RGB from .jpg/.jpeg.
#' @noRd
detect_camera_from_folder <- function(images_dir) {
  if (!is.character(images_dir) || !length(images_dir) || !dir.exists(images_dir)) {
    return(NA_character_)
  }
  files <- list.files(images_dir, pattern = "\\.(jpe?g|tif?f)$",
                      ignore.case = TRUE, recursive = FALSE)
  if (!length(files)) return(NA_character_)
  ext <- tolower(tools::file_ext(files))
  n_jpg <- sum(ext %in% c("jpg", "jpeg"))
  n_tif <- sum(ext %in% c("tif", "tiff"))
  if (n_tif > 0 && n_tif >= n_jpg) return("multispectral")
  if (n_jpg > 0 && n_jpg > n_tif) return("rgb")
  NA_character_
}
