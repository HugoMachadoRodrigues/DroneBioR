#' Return expected ODM product paths
#'
#' @param project A `dronebio_project` object.
#' @return Named character vector of expected product paths.
#' @examples
#' project <- dronebio_project(project_dir = tempdir())
#' odm_product_paths(project)
#' @export
odm_product_paths <- function(project) {
  c(
    orthomosaic = project$odm_orthomosaic,
    dsm = file.path(project$odm_project_dir, "odm_dem", "dsm.tif"),
    dtm = file.path(project$odm_project_dir, "odm_dem", "dtm.tif"),
    point_cloud_las = file.path(project$odm_project_dir, "odm_georeferencing", "odm_georeferenced_model.las"),
    point_cloud_laz = file.path(project$odm_project_dir, "odm_georeferencing", "odm_georeferenced_model.laz"),
    point_cloud_copc = file.path(project$odm_project_dir, "odm_georeferencing", "odm_georeferenced_model.copc.laz"),
    point_cloud_ply = file.path(project$odm_project_dir, "odm_filterpoints", "point_cloud.ply"),
    mesh_ply = file.path(project$odm_project_dir, "odm_meshing", "odm_25dmesh.ply"),
    textured_obj = file.path(project$odm_project_dir, "odm_texturing_25d", "odm_textured_model_geo.obj")
  )
}

#' Summarize available ODM products
#'
#' @param project A `dronebio_project` object.
#' @return A data frame with product, path, availability and file size.
#' @examples
#' project <- dronebio_project(project_dir = tempdir())
#' summarize_odm_products(project)
#' @export
summarize_odm_products <- function(project) {
  paths <- odm_product_paths(project)
  exists <- file.exists(paths)
  size_mb <- rep(NA_real_, length(paths))
  size_mb[exists] <- round(file.info(paths[exists])$size / 1024^2, 2)

  data.frame(
    product = names(paths),
    available = exists,
    size_mb = size_mb,
    path = unname(paths),
    stringsAsFactors = FALSE
  )
}
