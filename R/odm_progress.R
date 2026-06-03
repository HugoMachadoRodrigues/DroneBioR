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

#' Authoritative active ODM stage from the docker log
#'
#' ODM writes `Running <stage> stage` and `Finished <stage> stage`
#' lines to its stdout. The active stage is the last one that started
#' but has not yet finished. Returns `NA_character_` when the log is
#' missing/empty or has no stage markers yet (the caller then falls
#' back to the directory heuristic). Only stages in `expected_stages`
#' are considered, so stray log text cannot inject a bogus stage.
#'
#' @noRd
log_based_active_stage <- function(log_path, expected_stages) {
  if (!is.character(log_path) || !length(log_path) ||
      !file.exists(log_path)) {
    return(NA_character_)
  }
  lines <- tryCatch(readLines(log_path, warn = FALSE),
                    error = function(e) character())
  if (!length(lines)) return(NA_character_)
  running  <- sub(".*Running ([a-z_]+) stage.*",  "\\1",
                  grep("Running [a-z_]+ stage",  lines, value = TRUE))
  finished <- sub(".*Finished ([a-z_]+) stage.*", "\\1",
                  grep("Finished [a-z_]+ stage", lines, value = TRUE))
  running  <- running[running   %in% expected_stages]
  finished <- finished[finished %in% expected_stages]
  if (!length(running)) return(NA_character_)
  # Walk the running stages newest-first; the active one is the most
  # recent that has not been finished.
  for (s in rev(running)) {
    if (!(s %in% finished)) return(s)
  }
  NA_character_
}

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

    # --- Authoritative stage from the ODM log -----------------------------
    # ODM prints `Running <stage> stage` / `Finished <stage> stage` to its
    # stdout, which run_docker_with_progress() captures in
    # <project_dir>/dronebior_odm.log. Those markers are the ground truth
    # for which stage is *actually* executing. We prefer them over the
    # directory heuristic because ODM creates some stage directories
    # early — notably, when `--geo` is passed it materialises
    # `odm_georeferencing/` to stage the coordinates while opensfm is
    # still grinding, which fooled the directory-based heuristic into
    # reporting odm_georeferencing as active for the whole opensfm pass.
    log_active <- log_based_active_stage(file.path(project_dir,
                                                   "dronebior_odm.log"),
                                         expected_stages)

    existing <- list.dirs(project_dir, recursive = FALSE, full.names = FALSE)
    # Stages whose directory is present on disk AND was touched during
    # the current run (mtime filter rejects stale dirs from prior runs).
    candidate_stages <- intersect(expected_stages, existing)
    fresh_dir_stages <- character()
    if (length(candidate_stages)) {
      mtimes <- file.info(file.path(project_dir, candidate_stages))$mtime
      fresh <- !is.na(mtimes) & mtimes >= started_at
      fresh_dir_stages <- candidate_stages[fresh]
    }

    # Active stage: log marker wins; otherwise fall back to the latest
    # canonical stage with a fresh directory.
    active_from_log <- !is.na(log_active)
    active <- if (active_from_log) {
      log_active
    } else if (length(fresh_dir_stages)) {
      expected_stages[max(match(fresh_dir_stages, expected_stages))]
    } else {
      NA_character_
    }

    # "Started" stages for the progress count. When the active stage
    # comes from the log we can safely infer that every canonical
    # predecessor has run (ODM is strictly sequential), so we count
    # everything up to and including the active stage. In the
    # directory-only fallback we do NOT infer — ODM materialises some
    # stage dirs out of order (e.g. odm_georeferencing under --geo), so
    # we count only the directories that are actually present and fresh.
    started_stages <- if (active_from_log) {
      expected_stages[seq_len(match(active, expected_stages))]
    } else {
      fresh_dir_stages
    }

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
    "%s[%s] Starting ODM run; raw docker log -> %s (tail -f to follow). Polling every %ss.",
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

  # Plain message()-based renderer. The previous implementation tried
  # to use \r to overwrite a single sticky line in place; it worked in
  # a real terminal but rendered as a frozen-looking single line in
  # RStudio Console on some R versions, hiding the progress entirely.
  # Since docker's verbose output is already redirected to `log_path`,
  # the console has nothing else to print besides our poller, so a
  # plain "one line per poll" stream is uncluttered AND it is
  # rendered reliably everywhere R runs.
  render <- function(poll_result) {
    if (is.null(poll_result)) return(invisible(NULL))
    for (a in poll_result$announcements) message(a)
    message(poll_result$status)
  }

  while (proc$is_alive()) {
    proc$wait(timeout = as.integer(poll_interval_secs * 1000L))
    render(tryCatch(poller(), error = function(e) NULL))
  }
  # Final poll so the user sees the terminal on-disk state alongside
  # the docker exit status. We also count the final products that
  # made it to disk so the user can sanity-check whether the run
  # actually delivered DSM / DTM / orthomosaic.
  render(tryCatch(poller(), error = function(e) NULL))

  status <- proc$get_exit_status()
  if (is.null(status)) status <- 1L
  status <- as.integer(status)

  prod_summary <- summarise_odm_products_on_disk(project_dir)
  message(sprintf(
    "%s[%s] Docker exited with status %d. On disk now: %s.",
    if (!is.null(band_label)) sprintf("[%s] ", band_label) else "",
    format(Sys.time(), "%H:%M:%S"),
    status,
    prod_summary
  ))
  status
}

#' Concise report on which ODM final products are on disk
#'
#' Looks for the four artefacts DroneBioR cares about — orthomosaic,
#' DSM, DTM, georeferenced LAS — and returns a single-line summary
#' like `"ortho YES, dsm YES, dtm NO, las NO"`. Surfaced by
#' [run_docker_with_progress()] after the subprocess exits so the
#' user can immediately tell whether the run actually delivered the
#' products even if the poll status looked confusing.
#'
#' @noRd
summarise_odm_products_on_disk <- function(project_dir) {
  checks <- list(
    ortho = file.path(project_dir, "odm_orthophoto", "odm_orthophoto.tif"),
    dsm   = file.path(project_dir, "odm_dem",       "dsm.tif"),
    dtm   = file.path(project_dir, "odm_dem",       "dtm.tif"),
    las   = file.path(project_dir, "odm_georeferencing",
                      "odm_georeferenced_model.las")
  )
  paste(
    vapply(names(checks), function(k) {
      sprintf("%s %s", k, if (file.exists(checks[[k]])) "YES" else "NO")
    }, character(1)),
    collapse = ", "
  )
}
