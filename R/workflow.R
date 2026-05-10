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
  configure_proj_database(verbose = FALSE)

  if (is.character(project)) {
    project <- dronebio_project(project)
  }
  if (is.null(orthomosaic)) {
    orthomosaic <- project$odm_orthomosaic
  }
  if (is.null(output_dir)) {
    output_dir <- project$output_dir
  }

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
