#' Start Drone Biomass Studio
#'
#' @param project_dir Default project directory. Ignored when `sample = TRUE`.
#' @param port Optional local port.
#' @param launch.browser Logical. Open a browser window.
#' @param sample Logical. When `TRUE`, seed and open the bundled sample
#'   project via [dronebio_sample_project()] so the app is immediately
#'   clickable without real flight data. Useful for demos and first-time
#'   users.
#' @param ... Additional arguments passed to `shiny::runApp()`.
#' @return The result of `shiny::runApp()`.
#' @examples
#' \dontrun{
#' # First-time / no data of your own:
#' run_drone_biomass_studio(sample = TRUE)
#'
#' # Real project:
#' run_drone_biomass_studio(project_dir = "/path/to/Drone_Biomass")
#' }
#' @export
run_drone_biomass_studio <- function(project_dir = getwd(),
                                     port = NULL,
                                     launch.browser = TRUE,
                                     sample = FALSE,
                                     ...) {
  if (isTRUE(sample)) {
    project <- dronebio_sample_project()
    project_dir <- project$project_dir
    message(
      "Opening the bundled sample project at: ", project_dir, "\n",
      "Fixtures are synthetic (32x32 pixel demo data) - do not use for science."
    )
  }
  options(dronebior.project_dir = normalizePath(project_dir, mustWork = FALSE))
  app_dir <- system.file("shiny", "DroneBiomassStudio", package = "DroneBioR")
  if (!nzchar(app_dir)) {
    stop("Could not find the Drone Biomass Studio app directory.", call. = FALSE)
  }
  shiny::runApp(appDir = app_dir, port = port, launch.browser = launch.browser, ...)
}
