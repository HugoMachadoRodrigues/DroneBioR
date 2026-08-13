#' Configure PROJ paths for terra and sf
#'
#' Some macOS R installations can load sf or terra without a valid pointer to
#' `proj.db`. This helper searches common locations and sets `PROJ_DATA` and
#' `PROJ_LIB` before spatial packages need CRS transformations.
#'
#' @param verbose Logical. Print the selected PROJ path when found.
#' @param force Logical. When `TRUE` (the default for a direct call), search
#'   and set the variables whatever they currently hold. When `FALSE`, an
#'   existing `PROJ_DATA` or `PROJ_LIB` that already points at a directory
#'   containing `proj.db` is left untouched, and nothing is warned about if no
#'   database is found. The package's load hook uses `FALSE`, so that attaching
#'   DroneBioR does not replace a configuration the user or another geospatial
#'   package chose.
#' @return Invisibly returns `TRUE` if `proj.db` was found, otherwise `FALSE`.
#' @examples
#' configure_proj_database(verbose = FALSE)
#' @export
configure_proj_database <- function(verbose = FALSE, force = TRUE) {
  current_paths <- c(Sys.getenv("PROJ_DATA"), Sys.getenv("PROJ_LIB"))

  # Attaching a package must not rewrite the user's environment. If PROJ_DATA
  # or PROJ_LIB already resolves to a directory holding proj.db, that is the
  # user's own configuration - or another geospatial package's - and it is not
  # ours to replace. Only step in when nothing usable is set, which is the case
  # this function exists for: a system where sf and terra cannot find proj.db
  # and fail with a coordinate-reference error that names nothing useful.
  if (!isTRUE(force)) {
    set_paths <- current_paths[nzchar(current_paths)]
    if (length(set_paths) &&
        any(file.exists(file.path(set_paths, "proj.db")))) {
      return(invisible(TRUE))
    }
  }

  candidate_paths <- unique(c(
    current_paths,
    system.file("proj", package = "sf"),
    file.path(R.home("library"), "sf", "proj"),
    # macOS (Homebrew)
    "/opt/homebrew/opt/proj/share/proj",
    "/opt/homebrew/share/proj",
    "/usr/local/opt/proj/share/proj",
    "/usr/local/share/proj",
    # Linux (Debian/Ubuntu libproj-dev)
    "/usr/share/proj",
    "/usr/lib/x86_64-linux-gnu/proj"
  ))
  candidate_paths <- candidate_paths[nzchar(candidate_paths)]
  valid_paths <- candidate_paths[file.exists(file.path(candidate_paths, "proj.db"))]

  if (length(valid_paths) == 0) {
    if (isTRUE(force)) {
      warning(
        "Could not find proj.db. Install PROJ or set PROJ_DATA/PROJ_LIB to ",
        "the directory containing proj.db.",
        call. = FALSE
      )
    }
    return(invisible(FALSE))
  }

  # Remember what was there so .onUnload can put it back.
  if (is.null(.dronebior_proj$saved)) {
    .dronebior_proj$saved <- list(
      PROJ_DATA = Sys.getenv("PROJ_DATA", unset = NA_character_),
      PROJ_LIB  = Sys.getenv("PROJ_LIB",  unset = NA_character_)
    )
  }
  Sys.setenv(PROJ_DATA = valid_paths[[1]], PROJ_LIB = valid_paths[[1]])
  if (isTRUE(verbose)) {
    message("Using PROJ database: ", valid_paths[[1]])
  }
  invisible(TRUE)
}

# What PROJ_DATA / PROJ_LIB held before we touched them, so unloading the
# package leaves the session as it found it.
.dronebior_proj <- new.env(parent = emptyenv())

#' Put PROJ_DATA and PROJ_LIB back the way they were.
#' @noRd
restore_proj_database <- function() {
  saved <- .dronebior_proj$saved
  if (is.null(saved)) return(invisible(FALSE))
  for (nm in names(saved)) {
    if (is.na(saved[[nm]])) Sys.unsetenv(nm) else do.call(Sys.setenv, saved[nm])
  }
  .dronebior_proj$saved <- NULL
  invisible(TRUE)
}
