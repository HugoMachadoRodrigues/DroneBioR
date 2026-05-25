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
#' a function that, called repeatedly, reports the project's current
#' on-disk state. The closure remembers which stages it has already
#' seen so each new transition is reported only once.
#'
#' Unlike the previous iteration of this helper, the poller does not
#' print anything itself. It returns a list with:
#'
#'   - `announcements`: a character vector of new-stage transition
#'     lines (one per stage that became visible since the last poll);
#'     callers should print these as ordinary "above the status bar"
#'     lines.
#'   - `status`: a single-line string with the current sticky status
#'     (active stage, elapsed, ETA, percent); callers should render
#'     this in-place using `\r` so the line stays put.
#'
#' Separating "render" from "report" lets [run_docker_with_progress()]
#' use one carriage-return-updated status line while interleaving
#' stage-transition lines above it without scrolling.
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
    # Stages whose directory is present on disk AND was touched during
    # the current run. The mtime filter is critical: stale stage
    # directories from a previous failed/cancelled run still exist on
    # disk; without the filter the canonical-order tie-breaker below
    # would pick a downstream stale dir (e.g. odm_georeferencing) as
    # "active" even when the current run is actually still grinding
    # through opensfm, and the ETA would be wildly wrong.
    candidate_stages <- intersect(expected_stages, existing)
    started_stages <- character()
    if (length(candidate_stages)) {
      mtimes <- file.info(file.path(project_dir, candidate_stages))$mtime
      fresh <- !is.na(mtimes) & mtimes >= started_at
      started_stages <- candidate_stages[fresh]
    }
    # Active stage = the latest canonical stage that is both on disk
    # and was modified during this run.
    active <- if (length(started_stages)) {
      ord_idx <- max(match(started_stages, expected_stages))
      expected_stages[ord_idx]
    } else NA_character_
    pending <- if (!is.na(active)) {
      idx <- match(active, expected_stages)
      if (idx < total_stages) expected_stages[(idx + 1L):total_stages] else character()
    } else expected_stages

    # Announce newly-seen stages once (return as strings; caller prints).
    new_stages <- setdiff(started_stages, seen_stages)
    announcements <- if (length(new_stages)) {
      vapply(new_stages, function(s) {
        sprintf("%s[%s] -> stage `%s` started",
                prefix, format(now, "%H:%M:%S"), s)
      }, character(1), USE.NAMES = FALSE)
    } else character()
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

    # Stage count is a coarse (but easy-to-grok) percent.
    stages_done_pct <- if (total_stages > 0) {
      100 * length(started_stages) / total_stages
    } else 0

    status <- sprintf(
      "%s[%s] %s | elapsed %s | ~%s remaining | %d/%d stages (%d%%)",
      prefix,
      format(now, "%H:%M:%S"),
      if (!is.na(active)) paste0("stage `", active, "`") else "starting",
      format_seconds_human(elapsed_total),
      format_seconds_human(remaining),
      length(started_stages), total_stages,
      as.integer(round(stages_done_pct))
    )

    invisible(list(
      announcements  = announcements,
      status         = status,
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
#' @param command Subprocess command to invoke. Defaults to
#'   `"docker"`; exposed so tests can swap in a benign command (e.g.,
#'   `"sleep"`) without needing the docker daemon.
#' @return Integer exit status from the subprocess.
#' @noRd
run_docker_with_progress <- function(args,
                                     project_dir,
                                     image_count = NA_integer_,
                                     band_label = NULL,
                                     poll_interval_secs = 15,
                                     command = "docker") {
  if (!requireNamespace("processx", quietly = TRUE)) {
    # Without processx we cannot poll while the subprocess runs, so
    # emit a single banner and fall back to the blocking call.
    message(sprintf(
      "%s[%s] Running ODM (install `processx` to get live progress)...",
      if (!is.null(band_label)) sprintf("[%s] ", band_label) else "",
      format(Sys.time(), "%H:%M:%S")
    ))
    return(system2(command, args = args))
  }

  poller <- make_odm_stage_poller(
    project_dir = project_dir,
    image_count = image_count,
    band_label  = band_label
  )

  # Redirect docker's verbose stdout/stderr to a per-run log file so
  # our progress line can stay put. Users who want the raw ODM output
  # can `tail -f` the log in another terminal.
  dir.create(project_dir, recursive = TRUE, showWarnings = FALSE)
  log_path <- file.path(project_dir, "dronebior_odm.log")

  message(sprintf(
    "%s[%s] Starting ODM run; raw docker log -> %s (tail -f to follow). Status updates in place every %ss.",
    if (!is.null(band_label)) sprintf("[%s] ", band_label) else "",
    format(Sys.time(), "%H:%M:%S"),
    log_path,
    format(poll_interval_secs)
  ))

  proc <- processx::process$new(
    command, args,
    stdout  = log_path,
    stderr  = "2>&1",
    cleanup = TRUE
  )

  # Sticky-line renderer.
  # `\r` returns the cursor to the start of the current terminal line,
  # so the next write overwrites it. We pre-pad with spaces sized to
  # the previous status so partial overwrites do not leave trailing
  # characters from a longer previous line.
  status_width <- 0L
  render <- function(poll_result) {
    if (is.null(poll_result)) return(invisible(NULL))
    # New-stage transitions get a full line of their own ABOVE the
    # sticky status: clear the current sticky line first, print each
    # transition with \n, then re-print the sticky status.
    if (length(poll_result$announcements)) {
      cat("\r", strrep(" ", status_width), "\r", sep = "")
      for (a in poll_result$announcements) cat(a, "\n", sep = "")
    }
    # Overwrite the previous sticky status. Pad the new status out so
    # short status lines fully erase any leftover characters from a
    # longer previous one.
    new_status <- poll_result$status
    pad <- max(0L, status_width - nchar(new_status))
    cat("\r", new_status, strrep(" ", pad), sep = "")
    flush.console()
    status_width <<- nchar(new_status)
  }

  while (proc$is_alive()) {
    proc$wait(timeout = as.integer(poll_interval_secs * 1000L))
    render(tryCatch(poller(), error = function(e) NULL))
  }
  # Final render so the user sees the terminal on-disk state.
  render(tryCatch(poller(), error = function(e) NULL))
  cat("\n")  # commit the sticky line with a final newline

  status <- proc$get_exit_status()
  if (is.null(status)) status <- 1L
  as.integer(status)
}
