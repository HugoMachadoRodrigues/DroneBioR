# Real-time progress tracking for blocking ODM Docker runs.
#
# system2("docker", args) blocks for the full ODM run (often 30-90 min
# per band, several hours per Mavic 3M flight) and prints ODM's raw
# stage-by-stage log. Without a high-level percent-complete / ETA the
# user cannot tell at a glance whether the run is making progress or
# wedged. `run_docker_with_progress()` solves that by running docker
# under processx (non-blocking) and polling the ODM project directory
# every `poll_interval_secs` for stage-subfolder creation. It prints a
# single-line status (active stage, elapsed in stage, % stages done,
# ETA) on each poll and forwards docker's own stdout in between so the
# user still sees ODM's familiar output.
#
# The poller leans on the existing odm_eta.R machinery
# (estimate_remaining_seconds(), odm_stage_order()) so historical
# stage durations from ~/.dronebior/odm_stage_history.csv feed the
# ETA. When processx is not installed the helper transparently falls
# back to system2(), still emitting a single pre-run banner.

#' Format a number of seconds as a compact human string
#'
#' @noRd
format_seconds_human <- function(seconds) {
  if (!is.finite(seconds) || seconds < 0) return("?")
  s <- as.integer(round(seconds))
  if (s < 60) return(sprintf("%ds", s))
  m <- s %/% 60
  if (m < 60) return(sprintf("%dm %02ds", m, s %% 60))
  h <- m %/% 60
  sprintf("%dh %02dm", h, m %% 60)
}

#' Build a stateful stage poller closure
#'
#' Given an ODM project directory and the expected stage order, returns
#' a function that, called repeatedly, prints a one-line progress
#' status reflecting the project's current on-disk state. The closure
#' remembers which stages it has already seen so it only announces new
#' transitions once.
#'
#' @noRd
make_odm_stage_poller <- function(project_dir,
                                  image_count = NA_integer_,
                                  expected_stages = odm_stage_order(),
                                  band_label = NULL) {
  seen_stages   <- character()
  prefix        <- if (!is.null(band_label)) sprintf("[%s] ", band_label) else ""
  started_at    <- Sys.time()
  total_stages  <- length(expected_stages)

  function() {
    now <- Sys.time()
    existing <- list.dirs(project_dir, recursive = FALSE, full.names = FALSE)
    # Stages already started, in canonical order.
    started_stages <- intersect(expected_stages, existing)
    # Active stage = the latest canonical stage that exists on disk.
    active <- if (length(started_stages)) {
      ord_idx <- max(match(started_stages, expected_stages))
      expected_stages[ord_idx]
    } else NA_character_
    pending <- if (!is.na(active)) {
      idx <- match(active, expected_stages)
      if (idx < total_stages) expected_stages[(idx + 1L):total_stages] else character()
    } else expected_stages

    # Announce newly-seen stages once.
    new_stages <- setdiff(started_stages, seen_stages)
    for (s in new_stages) {
      message(sprintf("%s[%s] -> stage `%s` started",
                      prefix, format(now, "%H:%M:%S"), s))
    }
    seen_stages <<- started_stages

    # Elapsed in active stage = now - mtime of that stage's directory.
    active_elapsed <- if (!is.na(active)) {
      mt <- file.info(file.path(project_dir, active))$mtime
      if (length(mt) && !is.na(mt)) {
        as.numeric(difftime(now, mt, units = "secs"))
      } else 0
    } else 0

    remaining <- estimate_remaining_seconds(
      active_stage           = active,
      pending_stages         = pending,
      active_elapsed_seconds = active_elapsed,
      image_count            = image_count
    )
    elapsed_total <- as.numeric(difftime(now, started_at, units = "secs"))
    total_estimate <- elapsed_total + remaining
    pct_complete <- if (total_estimate > 0) {
      min(99, 100 * elapsed_total / total_estimate)
    } else 0

    # Stage count is a coarser (but easier to grok) percent.
    stages_done_pct <- if (total_stages > 0) {
      100 * length(started_stages) / total_stages
    } else 0

    message(sprintf(
      "%s[%s] %s | elapsed %s | ~%s remaining | %d/%d stages (%d%%)",
      prefix,
      format(now, "%H:%M:%S"),
      if (!is.na(active)) paste0("stage `", active, "`") else "starting",
      format_seconds_human(elapsed_total),
      format_seconds_human(remaining),
      length(started_stages), total_stages,
      as.integer(round(stages_done_pct))
    ))

    invisible(list(
      active_stage   = active,
      elapsed_secs   = elapsed_total,
      remaining_secs = remaining,
      stages_done    = length(started_stages),
      total_stages   = total_stages
    ))
  }
}

#' Run `docker` and poll for ODM stage progress until completion
#'
#' Drop-in replacement for `system2("docker", args = args)` that
#' streams a one-line progress status every `poll_interval_secs`
#' alongside docker's own stdout. Falls back to blocking `system2()`
#' when the `processx` package is not installed.
#'
#' @param args Docker arguments (as for [system2()]).
#' @param project_dir ODM project directory to poll for stage
#'   subfolders. Created on the fly by ODM as each stage starts.
#' @param image_count Image count passed through to the ETA
#'   estimator so per-stage durations scale roughly with workload.
#' @param band_label Optional short label (e.g., `"RGB"`, `"MS_G"`)
#'   prefixed onto every status line so output is easy to filter in
#'   multi-band runs.
#' @param poll_interval_secs How often to emit a status line. 15 s is
#'   slow enough to avoid spamming the console and fast enough that a
#'   stalled run shows up.
#' @return Integer exit status from docker.
#' @noRd
run_docker_with_progress <- function(args,
                                     project_dir,
                                     image_count = NA_integer_,
                                     band_label = NULL,
                                     poll_interval_secs = 15) {
  if (!requireNamespace("processx", quietly = TRUE)) {
    # Without processx we cannot read docker's stdout while it runs,
    # so emit a single banner and fall back to the blocking call.
    message(sprintf(
      "%s[%s] Running ODM (install `processx` to get live progress)...",
      if (!is.null(band_label)) sprintf("[%s] ", band_label) else "",
      format(Sys.time(), "%H:%M:%S")
    ))
    return(system2("docker", args = args))
  }

  poller <- make_odm_stage_poller(
    project_dir = project_dir,
    image_count = image_count,
    band_label  = band_label
  )

  message(sprintf(
    "%s[%s] Starting ODM run (polling every %ds for stage progress)...",
    if (!is.null(band_label)) sprintf("[%s] ", band_label) else "",
    format(Sys.time(), "%H:%M:%S"),
    poll_interval_secs
  ))

  proc <- processx::process$new(
    "docker", args,
    stdout = "|", stderr = "2>&1",
    cleanup = TRUE
  )

  last_poll_at <- Sys.time()
  # Drain output and poll on a wall-clock cadence.
  while (proc$is_alive()) {
    out <- proc$read_output(timeout = 1000L)  # up to 1 s
    if (nzchar(out)) cat(out)
    now <- Sys.time()
    if (as.numeric(difftime(now, last_poll_at, units = "secs")) >= poll_interval_secs) {
      tryCatch(poller(), error = function(e) NULL)
      last_poll_at <- now
    }
  }
  # Final drain after exit.
  remaining <- proc$read_all_output()
  if (nzchar(remaining)) cat(remaining)
  # One last status line so the user sees the final state.
  tryCatch(poller(), error = function(e) NULL)

  status <- proc$get_exit_status()
  if (is.null(status)) status <- 1L
  as.integer(status)
}
