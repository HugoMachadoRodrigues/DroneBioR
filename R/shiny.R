#' Start Drone Biomass Studio
#'
#' @param project_dir Default project directory. Should point at the root
#'   of a DroneBioR project (i.e. the folder that contains `outputs/odm_*`
#'   from a previous engine run, or the parent of a folder where you will
#'   place real flight images and run the engine from inside the app).
#' @param port Optional local port.
#' @param launch.browser Logical. Open a browser window.
#' @param ... Additional arguments passed to `shiny::runApp()`.
#' @return The result of `shiny::runApp()`.
#' @examples
#' \dontrun{
#' run_drone_biomass_studio(project_dir = "/path/to/Drone_Biomass")
#' }
#' @export
run_drone_biomass_studio <- function(project_dir = getwd(),
                                     port = NULL,
                                     launch.browser = TRUE,
                                     ...) {
  options(dronebior.project_dir = normalizePath(project_dir, mustWork = FALSE))
  app_dir <- system.file("shiny", "DroneBiomassStudio", package = "DroneBioR")
  if (!nzchar(app_dir)) {
    stop("Could not find the Drone Biomass Studio app directory.", call. = FALSE)
  }
  shiny::runApp(appDir = app_dir, port = port, launch.browser = launch.browser, ...)
}
