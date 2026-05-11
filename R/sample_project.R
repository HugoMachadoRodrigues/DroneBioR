#' Seed a clickable sample DroneBioR project from bundled fixtures
#'
#' Builds a working `dronebio_project` in `target_dir` and seeds it with the
#' synthetic fixtures shipped under `inst/extdata/`. The seeded tree mirrors
#' the layout an OpenDroneMap run would produce, so downstream functions
#' (`read_multispectral_orthomosaic`, `build_chm_from_dsm_dtm`,
#' `summarize_odm_products`, `run_dronebio_workflow`, the Shiny app) work
#' against it with no extra configuration.
#'
#' Useful for clicking through the package and the Shiny app before you have
#' real flight data of your own. The fixtures are intentionally tiny
#' (32x32 pixel multispectral subset, ~17 KB total) and **must not be used
#' for science**.
#'
#' Files are copied with `overwrite = FALSE`, so re-running the function is
#' safe: it tops up missing files but does not clobber edits you have made
#' to the seeded folder.
#'
#' @param target_dir Target project directory. Defaults to a stable folder
#'   under `tempdir()` so multiple Shiny launches reuse the same seed.
#' @return A `dronebio_project` pointing at `target_dir`.
#' @examples
#' project <- dronebio_sample_project(target_dir = tempfile("dronebior-sample-"))
#' file.exists(project$odm_orthomosaic)
#' summarize_odm_products(project)
#' @export
dronebio_sample_project <- function(target_dir = file.path(tempdir(), "DroneBioR-sample")) {
  project <- dronebio_project(project_dir = target_dir)

  dirs_to_create <- c(
    project$project_dir,
    project$output_dir,
    project$odm_dataset_dir,
    project$odm_project_dir,
    project$odm_images_dir,
    file.path(project$odm_project_dir, "odm_orthophoto"),
    file.path(project$odm_project_dir, "odm_dem")
  )
  for (d in dirs_to_create) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }

  fixtures <- list(
    list(
      src = system.file("extdata", "micasense_subset.tif", package = "DroneBioR"),
      dst = project$odm_orthomosaic
    ),
    list(
      src = system.file("extdata", "dsm_subset.tif", package = "DroneBioR"),
      dst = file.path(project$odm_project_dir, "odm_dem", "dsm.tif")
    ),
    list(
      src = system.file("extdata", "dtm_subset.tif", package = "DroneBioR"),
      dst = file.path(project$odm_project_dir, "odm_dem", "dtm.tif")
    ),
    list(
      src = system.file("extdata", "field_samples.csv", package = "DroneBioR"),
      dst = file.path(project$project_dir, "field_samples.csv")
    )
  )

  for (fx in fixtures) {
    if (nzchar(fx$src) && file.exists(fx$src) && !file.exists(fx$dst)) {
      file.copy(fx$src, fx$dst, overwrite = FALSE)
    }
  }

  project
}
