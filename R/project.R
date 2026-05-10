#' Create a DroneBioR project description
#'
#' @param project_dir Root folder for the drone biomass project.
#' @param images_subdir Relative folder containing raw MicaSense images.
#' @param output_subdir Relative folder for DroneBioR outputs.
#' @param odm_dataset_subdir Relative folder mounted into ODM Docker.
#' @param odm_project_name ODM project name inside the dataset folder.
#' @return A list with normalized project paths.
#' @export
dronebio_project <- function(project_dir = getwd(),
                             images_subdir = file.path("imagens", "micasense"),
                             output_subdir = file.path("outputs", "dronebior_analysis"),
                             odm_dataset_subdir = file.path("outputs", "odm_micasense_dataset"),
                             odm_project_name = "micasense") {
  project_dir <- normalizePath(project_dir, mustWork = FALSE)
  odm_dataset_dir <- file.path(project_dir, odm_dataset_subdir)
  odm_project_dir <- file.path(odm_dataset_dir, odm_project_name)

  x <- list(
    project_dir = project_dir,
    images_dir = file.path(project_dir, images_subdir),
    output_dir = file.path(project_dir, output_subdir),
    odm_dataset_dir = odm_dataset_dir,
    odm_project_name = odm_project_name,
    odm_project_dir = odm_project_dir,
    odm_images_dir = file.path(odm_project_dir, "images"),
    odm_orthomosaic = file.path(odm_project_dir, "odm_orthophoto", "odm_orthophoto.tif")
  )
  class(x) <- "dronebio_project"
  x
}
