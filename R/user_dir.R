# Where the package is allowed to keep state between sessions.
#
# Four things outlive a session: the flight registry, the flight-metric cache,
# the ODM stage-history used to estimate run times, and the active-run record
# the Shiny app reads to recover after a browser refresh. All four used to live
# in `~/.dronebior`, which CRAN does not permit: a package may not write to the
# user's home filespace, and `R CMD check` runs the tests and examples on a
# machine where doing so is a policy violation rather than a convenience.
#
# `tools::R_user_dir()` is the sanctioned location. It is per-package, it obeys
# the platform's conventions, and - the reason it matters for testing - it
# honours `R_USER_DATA_DIR` and `R_USER_CACHE_DIR`, so a test can point the
# whole thing at `tempdir()` and be sure nothing escapes.

#' Directory for state that outlives the session.
#'
#' @param which `"data"` for things the user would miss if deleted (the flight
#'   registry, the run record), `"cache"` for things that are only an
#'   optimisation and can be recomputed (the metric cache).
#' @param create Create the directory. Defaults to `FALSE`, so that *computing*
#'   a path has no side effect: the previous helpers created a directory merely
#'   by being asked where a file would go, which meant a read-only call left a
#'   directory behind.
#' @return The directory path, invisibly created when `create` is `TRUE`.
#' @noRd
dronebior_user_dir <- function(which = c("data", "cache"), create = FALSE) {
  which <- match.arg(which)
  dir <- tools::R_user_dir("DroneBioR", which = which)
  if (create && !dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  dir
}

#' Path to a file under the package's own state directory.
#'
#' @param file File name.
#' @param which Passed to [dronebior_user_dir()].
#' @param create Create the parent directory. Callers that are about to write
#'   pass `TRUE`; callers that only want to know where to read pass `FALSE`.
#' @return The full path.
#' @noRd
dronebior_user_file <- function(file, which = c("data", "cache"), create = FALSE) {
  file.path(dronebior_user_dir(which, create = create), file)
}
