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

#' Path to the persistent ODM stage-history CSV.
#'
#' @param create Create the parent directory. Only a caller that is about to
#'   write passes `TRUE`; asking where the file lives must not leave a
#'   directory behind.
#' @noRd
odm_history_path <- function(create = FALSE) {
  dronebior_user_file("odm_stage_history.csv", "data", create = create)
}

#' Collapse a camera or band label to the buckets the history distinguishes.
#'
#' Accepts what the various callers already carry: `camera_type` values
#' (`"multispectral"` / `"rgb"`) and per-band labels from the DJI Mavic 3M
#' runner (`"RGB"`, `"MS"`, `"MS_NIR"`, and suffixed forms like
#' `"MS/oom-retry"`). Anything unrecognised becomes `NA`, which means
#' "do not filter on it".
#' @noRd
normalize_camera_type <- function(camera) {
  if (is.null(camera) || !length(camera)) return(NA_character_)
  cam <- tolower(trimws(as.character(camera)[1L]))
  if (is.na(cam) || !nzchar(cam)) return(NA_character_)
  cam <- gsub("[^a-z0-9]+", "_", cam)

  # Sensor models first. A MicaSense set and a DJI Mavic 3M flight are both
  # "multispectral" and their per-image cost is not remotely comparable: on
  # this project a 210-image MicaSense opensfm took 70 seconds while a
  # 39-image DJI one took 39 minutes. Pooling them made the ETA overestimate
  # a MicaSense run by a factor of about 200.
  if (grepl("mavic|djim3m|dji_m3m|m3m", cam)) return("dji_mavic_3m")
  if (grepl("micasense|rededge|altum", cam))  return("micasense")
  if (grepl("sequoia|parrot", cam))           return("sequoia")
  if (grepl("^(multispectral|multi|ms)($|_)", cam)) return("multispectral")
  if (grepl("^rgb($|_)", cam)) return("rgb")
  NA_character_
}

#' The coarse class a camera label belongs to
#'
#' Used as the fallback tier when no history exists for the exact sensor: a
#' MicaSense estimate is better served by another multispectral run than by an
#' RGB one, even though neither is the same camera.
#' @noRd
camera_class <- function(camera) {
  cam <- normalize_camera_type(camera)
  if (is.na(cam)) return(NA_character_)
  if (cam %in% c("dji_mavic_3m", "micasense", "sequoia", "multispectral")) {
    return("multispectral")
  }
  "rgb"
}

#' Empty history frame, used for a missing or unreadable file.
#' @noRd
empty_odm_stage_history <- function() {
  data.frame(
    run_started_at   = character(),
    image_count      = integer(),
    stage            = character(),
    duration_seconds = numeric(),
    camera           = character(),
    stringsAsFactors = FALSE
  )
}

#' Read the history. Returns an empty data frame when the file is missing.
#'
#' Histories written before per-camera tracking have no `camera` column; it is
#' added as `NA` so those rows stay usable as an unlabelled pool.
#' @noRd
read_odm_stage_history <- function() {
  path <- odm_history_path()
  if (!file.exists(path)) return(empty_odm_stage_history())
  hist <- tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE),
    error = function(e) empty_odm_stage_history()
  )
  if (is.null(hist$camera)) hist$camera <- NA_character_
  hist
}

#' Append one stage-completion row, replacing any duplicate (run, stage).
#'
#' `camera` records which kind of sensor produced the run so later estimates
#' can avoid mixing them; see [estimate_odm_stage_seconds()].
#' @noRd
record_odm_stage_completion <- function(run_started_at, image_count, stage,
                                        duration_seconds, camera = NA_character_) {
  if (!is.finite(duration_seconds) || duration_seconds < 0) {
    return(invisible(FALSE))
  }
  hist <- read_odm_stage_history()
  new_row <- data.frame(
    run_started_at   = as.character(run_started_at),
    image_count      = as.integer(image_count),
    stage            = as.character(stage),
    duration_seconds = as.numeric(duration_seconds),
    camera           = normalize_camera_type(camera),
    stringsAsFactors = FALSE
  )
  dup <- hist$run_started_at == new_row$run_started_at[1L] &
         hist$stage          == new_row$stage[1L]
  if (any(dup)) hist <- hist[!dup, , drop = FALSE]
  out <- rbind(hist, new_row)
  utils::write.csv(out, odm_history_path(create = TRUE), row.names = FALSE)
  invisible(TRUE)
}

#' Estimate the duration of `stage` for an upcoming run of `image_count`.
#'
#' Strategy: take the median of historical durations for that stage,
#' linearly scaled by the ratio of `image_count` to the median historical
#' count. Falls back to the hardcoded baseline when no history exists.
#'
#' A multispectral run costs far more per image than an RGB one -- 12-bit
#' per-band TIFFs, and feature matching across NIR / red-edge bands over
#' low-texture canopy -- so pooling the two produced estimates wrong by an
#' order of magnitude in whichever direction the history leaned. When `camera`
#' is given the rows are narrowed in three tiers:
#'
#' 1. rows recorded for that same sensor;
#' 2. failing that, rows whose sensor is unknown (`camera = NA`) -- every run
#'    recorded before per-camera tracking, which are kept usable rather than
#'    discarded;
#' 3. failing that, the hardcoded baseline.
#'
#' Rows from a sensor known to be *different* are never used: they are the
#' thing this split exists to keep out, in either direction.
#' @noRd
estimate_odm_stage_seconds <- function(stage, image_count = NA_integer_,
                                       camera = NA_character_) {
  hist <- read_odm_stage_history()
  rows <- hist[hist$stage == stage & is.finite(hist$duration_seconds), , drop = FALSE]

  cam <- normalize_camera_type(camera)
  if (!is.na(cam) && nrow(rows)) {
    # Exact sensor, or nothing. Borrowing across sensors is what produced the
    # 200x error: on this project a 210-image MicaSense opensfm took 70 s and
    # a 39-image DJI Mavic 3M one took 39 min, so each would mis-estimate the
    # other by more than the estimate is worth -- in opposite directions.
    # Rows labelled only "multispectral" name a class, not a camera, so they
    # carry no more information here than an unlabelled row.
    specific <- c("dji_mavic_3m", "micasense", "sequoia")
    exact <- rows[!is.na(rows$camera) & rows$camera == cam, , drop = FALSE]
    rows <- if (nrow(exact)) {
      exact
    } else {
      # Ambiguous rows only: no camera at all, or the coarse label of this same
      # class. A row from a different class, or from a different named sensor,
      # is excluded -- borrowing either way is what the split exists to stop.
      klass <- camera_class(cam)
      ambiguous <- is.na(rows$camera) |
        (!rows$camera %in% specific &
           vapply(rows$camera, function(x) identical(camera_class(x), klass),
                  logical(1)))
      rows[ambiguous, , drop = FALSE]
    }
  }

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
#' While the active stage is inside its estimate, `active_elapsed_seconds` is
#' subtracted from it and pending stages contribute their full estimate, so the
#' ETA shrinks as the run progresses.
#'
#' Once the active stage runs *past* its estimate, that estimate is disproved
#' and so is the history behind it. The active stage is then the only live
#' measurement of how badly the history predicts this run, so its overrun ratio
#' is carried over to the stages that have not started yet. Without this, an
#' overrunning stage contributed `max(0, est - elapsed)` = 0 and every pending
#' stage kept a figure the run had just disproved, freezing the ETA at "sum of
#' the pending estimates" however far the run overran — a 39-image
#' multispectral run predicted from 300-image RGB history sat at ~3 minutes
#' remaining while a stage estimated at 48s passed 10 minutes.
#'
#' @param overrun_progress Assumed completion fraction of a stage that has
#'   passed its estimate. No sub-stage progress is available from the ODM log,
#'   so 0.5 is the memoryless guess: expect roughly as much again as it has
#'   already spent.
#' @param min_calibration_seconds Ignore the overrun ratio when the active
#'   stage's own estimate is below this. Stages the history puts at a fraction
#'   of a second (`odm_report` medians ~0.02s) would otherwise turn a few
#'   seconds of runtime into a 100x multiplier.
#' @param max_slowdown Upper bound on the carried-over ratio, so one
#'   pathological stage cannot inflate the whole tail without limit.
#' @noRd
estimate_remaining_seconds <- function(active_stage,
                                       pending_stages,
                                       active_elapsed_seconds = 0,
                                       image_count = NA_integer_,
                                       camera = NA_character_,
                                       overrun_progress = 0.5,
                                       min_calibration_seconds = 30,
                                       max_slowdown = 20) {
  active_base <- if (!is.null(active_stage) && !is.na(active_stage)) {
    estimate_odm_stage_seconds(active_stage, image_count, camera = camera)
  } else NA_real_

  overrun <- is.finite(active_base) &&
             active_base > 0 &&
             active_elapsed_seconds > active_base

  slowdown <- if (overrun && active_base >= min_calibration_seconds) {
    min(active_elapsed_seconds / active_base, max_slowdown)
  } else 1

  active_est <- if (!is.finite(active_base)) {
    0
  } else if (overrun) {
    active_elapsed_seconds * (1 - overrun_progress) / overrun_progress
  } else {
    max(0, active_base - active_elapsed_seconds)
  }

  pending_est <- if (length(pending_stages)) {
    sum(vapply(pending_stages, estimate_odm_stage_seconds, numeric(1),
               image_count = image_count, camera = camera)) * slowdown
  } else 0

  active_est + pending_est
}

#' Path to the persistent active-run record.
#'
#' @param create Create the parent directory; see [odm_history_path()].
#' @noRd
active_run_record_path <- function(create = FALSE) {
  dronebior_user_file("active_runs.json", "data", create = create)
}

#' Persist a run record so the Shiny session can recover after a refresh.
#'
#' Writes a tiny JSON document with `run_id`, `log_path`, `project_dir`,
#' `image_count` and `started_at`. The Shiny app reads this on startup
#' to repoint the Progress card at an ongoing run. Overwrites any
#' previous record; we only track the latest run.
#' @noRd
write_active_run_record <- function(run_id, log_path, project_dir,
                                    image_count = NA_integer_,
                                    started_at = Sys.time()) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) return(invisible(FALSE))
  rec <- list(
    run_id      = as.character(run_id),
    log_path    = as.character(log_path),
    project_dir = as.character(project_dir),
    image_count = as.integer(image_count),
    started_at  = format(as.POSIXct(started_at), "%Y-%m-%dT%H:%M:%S%z")
  )
  tryCatch(
    {
      writeLines(jsonlite::toJSON(rec, auto_unbox = TRUE, null = "null"),
                 active_run_record_path(create = TRUE))
      invisible(TRUE)
    },
    error = function(e) invisible(FALSE)
  )
}

#' Read the most recent active-run record. Returns NULL when missing or stale.
#'
#' A record older than `max_age_hours` is considered stale (probably ended);
#' callers can choose to ignore it. The `log_path` is also checked for
#' existence — a missing log usually means the run was reset.
#' @noRd
read_active_run_record <- function(max_age_hours = 48) {
  path <- active_run_record_path()
  if (!file.exists(path)) return(NULL)
  if (!requireNamespace("jsonlite", quietly = TRUE)) return(NULL)
  rec <- tryCatch(jsonlite::fromJSON(path, simplifyVector = TRUE),
                  error = function(e) NULL)
  if (is.null(rec) || !length(rec)) return(NULL)
  # Age gating
  if (!is.null(rec$started_at)) {
    started <- tryCatch(as.POSIXct(rec$started_at, format = "%Y-%m-%dT%H:%M:%S%z"),
                        error = function(e) NA)
    if (!is.na(started) && difftime(Sys.time(), started, units = "hours") > max_age_hours) {
      return(NULL)
    }
  }
  if (!is.null(rec$log_path) && !file.exists(rec$log_path)) return(NULL)
  rec
}

#' Clear the active-run record (after pipeline completion).
#' @noRd
clear_active_run_record <- function() {
  path <- active_run_record_path()
  if (file.exists(path)) unlink(path)
  invisible(TRUE)
}

#' Best-effort camera-type detection from a folder of source images.
#'
#' Returns one of `"multispectral"`, `"rgb"` or `NA_character_` when
#' uncertain. A DJI Mavic 3M is recognised by its filenames; otherwise
#' multispectral is inferred from MicaSense / Sequoia .tif filenames and RGB
#' from .jpg/.jpeg.
#' @noRd
detect_camera_from_folder <- function(images_dir) {
  if (!is.character(images_dir) || !length(images_dir) || !dir.exists(images_dir)) {
    return(NA_character_)
  }
  # Name the rig before counting extensions. A Mavic 3M carries two cameras -
  # a 20 MP RGB and a four-band multispectral (green, red, red edge, NIR; no
  # blue) - and writes both on every shot. Deciding by extension count made it
  # "multispectral" only because there happen to be four TIFFs per JPG; a
  # flight where the RGB outnumbered the MS frames would have been called RGB
  # and reconstructed from the colour camera alone.
  #
  # has_djim3m_images() is not enough on its own: it also matches a folder
  # holding only the `_D.JPG` frames, and the package's own staged ODM images
  # folder is exactly that. Calling such a folder multispectral is worse than
  # the bug being fixed, so require a band file to be present.
  if (length(djim3m_bands_present(images_dir))) return("multispectral")

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

#' Which DJI Mavic 3M multispectral bands are present in a folder.
#'
#' Returns the band suffixes actually on disk, in canonical order. Empty when
#' the folder holds no `_MS_` frames - including the common case of a folder
#' holding only the rig's `_D.JPG` colour frames, which
#' [has_djim3m_images()] also matches.
#' @noRd
djim3m_bands_present <- function(images_dir) {
  if (!is.character(images_dir) || !length(images_dir) || is.na(images_dir[[1]]) ||
      !nzchar(images_dir[[1]]) || !dir.exists(images_dir[[1]])) {
    return(character(0))
  }
  f <- list.files(images_dir[[1]],
                  pattern = "^DJI_[0-9]+_[0-9]+_MS_(G|R|RE|NIR)\\.[A-Za-z]+$",
                  ignore.case = TRUE)
  if (!length(f)) return(character(0))
  seen <- toupper(sub("^.*_MS_([A-Za-z]+)\\.[A-Za-z]+$", "\\1", f))
  intersect(c("G", "R", "RE", "NIR"), unique(seen))
}

#' Human-readable name of the rig in a folder of source images.
#'
#' Used by the Studio to tell the user which camera it recognised, rather than
#' asking them to classify it themselves. Returns `NA_character_` when the
#' folder holds no images.
#' @noRd
detect_sensor_label <- function(images_dir) {
  cam <- detect_camera_from_folder(images_dir)
  if (is.na(cam)) return(NA_character_)
  # Name the bands that are there, not the bands the model is capable of. A
  # folder holding only the colour frames, or a partial band set, must not be
  # announced as carrying all four: the label is the thing the user checks the
  # workflow against, so it has to describe this folder.
  bands <- djim3m_bands_present(images_dir)
  if (length(bands)) {
    has_rgb <- length(list.files(
      images_dir[[1]], pattern = "^DJI_[0-9]+_[0-9]+_D\\.[A-Za-z]+$",
      ignore.case = TRUE)) > 0
    return(sprintf("DJI Mavic 3M - multispectral (%s)%s",
                   paste(bands, collapse = ", "),
                   if (has_rgb) " + RGB camera" else ""))
  }
  switch(cam,
         multispectral = "Multispectral (MicaSense / Sequoia-style band files)",
         rgb           = "RGB only (Sony / Phantom / generic colour camera)",
         NA_character_)
}
