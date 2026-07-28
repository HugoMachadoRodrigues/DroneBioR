#' Seed a fixture-backed DroneBioR project (internal test helper)
#'
#' Internal helper used only by the package's `testthat` suite. Copies
#' the tiny GeoTIFFs and CSV fixtures shipped under `inst/extdata/` into
#' a writable project directory laid out like a real ODM run. This lets
#' the test suite exercise the full pipeline (read mosaic -> reflectance
#' -> indices -> CHM -> report) without depending on real flight data.
#'
#' Documented examples must not call this helper: it is not exported, so
#' anything it appears in fails for users and breaks `R CMD check`. Use
#' the fixtures under `system.file("extdata", ...)` for runnable examples,
#' or `\dontrun{}` with a real project directory.
#'
#' Not exported: the package has no user-facing "demo project" path.
#' To use the app, point [run_drone_biomass_studio()] at a real
#' project directory.
#'
#' @param target_dir Target project directory. Defaults to a stable
#'   folder under `tempdir()` so repeated test runs reuse the same
#'   seed.
#' @return A `dronebio_project` pointing at `target_dir`.
#' @keywords internal
#' @noRd
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
