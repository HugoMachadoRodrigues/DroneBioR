#' Configure PROJ paths for terra and sf
#'
#' Some macOS R installations can load sf or terra without a valid pointer to
#' `proj.db`. This helper searches common locations and sets `PROJ_DATA` and
#' `PROJ_LIB` before spatial packages need CRS transformations.
#'
#' @param verbose Logical. Print the selected PROJ path when found.
#' @return Invisibly returns `TRUE` if `proj.db` was found, otherwise `FALSE`.
#' @examples
#' configure_proj_database(verbose = FALSE)
#' @export
configure_proj_database <- function(verbose = FALSE) {
  current_paths <- c(Sys.getenv("PROJ_DATA"), Sys.getenv("PROJ_LIB"))
  candidate_paths <- unique(c(
    current_paths,
    system.file("proj", package = "sf"),
    file.path(R.home("library"), "sf", "proj"),
    "/opt/homebrew/opt/proj/share/proj",
    "/opt/homebrew/share/proj",
    "/usr/local/opt/proj/share/proj",
    "/usr/local/share/proj"
  ))
  candidate_paths <- candidate_paths[nzchar(candidate_paths)]
  valid_paths <- candidate_paths[file.exists(file.path(candidate_paths, "proj.db"))]

  if (length(valid_paths) == 0) {
    warning(
      "Could not find proj.db. Install PROJ or set PROJ_DATA/PROJ_LIB to ",
      "the directory containing proj.db.",
      call. = FALSE
    )
    return(invisible(FALSE))
  }

  Sys.setenv(PROJ_DATA = valid_paths[[1]], PROJ_LIB = valid_paths[[1]])
  if (isTRUE(verbose)) {
    message("Using PROJ database: ", valid_paths[[1]])
  }
  invisible(TRUE)
}
