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

  # Cap terra's in-memory footprint so the app runs on any laptop. By
  # default terra holds intermediate rasters in RAM up to ~60% of system
  # memory, which on a 5 cm/px multispectral ortho (22k x 20k x 5 bands
  # ~ 9 GB per snapshot) easily climbs past 20-30 GB once mask / scale /
  # index operations chain. Capping memfrac to 0.25 and memmax to 4 GB
  # forces terra to stream big rasters via /tmp instead of inflating
  # RAM. Users with more headroom can still raise these from the R
  # console before calling this function:
  #   terra::terraOptions(memfrac = 0.5, memmax = 12)
  if (requireNamespace("terra", quietly = TRUE)) {
    current <- terra::terraOptions(print = FALSE)
    if (is.null(getOption("dronebior.skip_terra_memcap"))) {
      terra::terraOptions(
        memfrac = min(current$memfrac %||% 0.6, 0.25),
        memmax  = min(current$memmax  %||% Inf, 4)
      )
    }
  }

  app_dir <- system.file("shiny", "DroneBiomassStudio", package = "DroneBioR")
  if (!nzchar(app_dir)) {
    stop("Could not find the Drone Biomass Studio app directory.", call. = FALSE)
  }
  shiny::runApp(appDir = app_dir, port = port, launch.browser = launch.browser, ...)
}
