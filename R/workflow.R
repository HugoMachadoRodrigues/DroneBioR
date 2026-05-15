#' Run the DroneBioR orthomosaic analysis workflow
#'
#' @param project A `dronebio_project` object or project directory path.
#' @param orthomosaic Optional orthomosaic path. Defaults to the ODM output path.
#' @param output_dir Optional output folder.
#' @param band_map Named band map.
#' @param use_alpha Logical. Use layer 6 as alpha mask when available.
#' @return A list with rasters, summaries and output paths.
#' @examples
#' \donttest{
#' project <- dronebio_project(project_dir = tempdir())
#' ortho <- system.file("extdata", "micasense_subset.tif", package = "DroneBioR")
#' result <- run_dronebio_workflow(
#'   project = project,
#'   orthomosaic = ortho,
#'   output_dir = tempfile("dronebior-out-")
#' )
#' names(result)
#' }
#' @export
run_dronebio_workflow <- function(project = dronebio_project(),
                                  orthomosaic = NULL,
                                  output_dir = NULL,
                                  band_map = default_micasense_band_map(),
                                  use_alpha = TRUE) {
  # configure_proj_database() is a macOS-focused helper. Inside the workflow
  # it is purely opportunistic - if it cannot locate proj.db, terra and sf
  # still work on any properly installed system, so we silence the warning
  # to avoid polluting downstream output (notably testthat warnings on CI).
  suppressWarnings(configure_proj_database(verbose = FALSE))

  if (is.character(project)) {
    project <- dronebio_project(project)
  }
  if (is.null(orthomosaic)) {
    orthomosaic <- project$odm_orthomosaic
  }
  if (is.null(output_dir)) {
    output_dir <- project$output_dir
  }

  t0 <- Sys.time()
  ortho <- read_multispectral_orthomosaic(
    orthomosaic = orthomosaic,
    band_map = band_map,
    use_alpha = use_alpha
  )
  reflectance <- scale_to_reflectance(ortho$bands)
  indices <- compute_spectral_indices(reflectance)
  biomass_proxy <- compute_biomass_proxy(indices)

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  reflectance_summary <- summarize_spatraster(reflectance, c("min", "mean", "max"))
  index_summary <- summarize_spatraster(indices, c("min", "mean", "max", "sd"))

  utils::write.csv(reflectance_summary, file.path(output_dir, "reflectance_summary.csv"), row.names = FALSE)
  utils::write.csv(index_summary, file.path(output_dir, "spectral_index_summary.csv"), row.names = FALSE)
  paths <- write_dronebio_rasters(output_dir, reflectance, indices, biomass_proxy, ortho$alpha)

  # Provenance: append a row to <project_dir>/dronebio_runs.csv so the
  # user (and the Project Control Center in the Shiny app) can trace
  # which products on disk came from which workflow invocation, with
  # what parameters, on what date. record_dronebio_run() is no-op
  # when project has no project_dir, so this is safe under
  # dronebio_project() with no arguments.
  bands_str <- tryCatch(paste(names(ortho$bands), collapse = "+"),
                        error = function(e) NA_character_)
  crs_str <- tryCatch(terra::crs(ortho$bands, describe = TRUE)$name,
                      error = function(e) NA_character_)
  tryCatch(
    record_dronebio_run(project, list(
      engine          = "run_dronebio_workflow",
      orthomosaic     = orthomosaic,
      bands           = bands_str,
      crs             = if (length(crs_str)) crs_str else NA_character_,
      runtime_seconds = as.numeric(difftime(Sys.time(), t0, units = "secs")),
      output_dir      = output_dir,
      use_alpha       = isTRUE(use_alpha)
    )),
    error = function(e) NULL
  )

  list(
    project = project,
    orthomosaic = orthomosaic,
    bands = ortho$bands,
    reflectance = reflectance,
    indices = indices,
    biomass_proxy = biomass_proxy,
    alpha = ortho$alpha,
    reflectance_summary = reflectance_summary,
    index_summary = index_summary,
    output_paths = paths
  )
}
