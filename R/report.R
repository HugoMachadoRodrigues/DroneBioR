#' Render a DroneBioR biomass report
#'
#' Renders the bundled RMarkdown template `inst/report/biomass_report.Rmd`
#' against a DroneBioR project. The report contains the ODM product
#' inventory, per-band reflectance and per-index summaries, index
#' histograms, the canopy height model and (when a field CSV is supplied)
#' the baseline biomass model.
#'
#' Requires the `rmarkdown` package (a Suggests dependency).
#'
#' @param project A `dronebio_project` object or a project directory path.
#' @param output_file Output HTML file path. Defaults to
#'   `DroneBioR_report.html` in the project directory.
#' @param field_csv Optional path to a field biomass CSV. When supplied,
#'   the report includes the baseline biomass model section.
#' @param use_alpha Logical. Use the orthomosaic alpha band as a valid-data
#'   mask.
#' @param roi_geojson Optional path to a GeoJSON file containing one or
#'   more ROI polygons (the format `studio_assets/rois.geojson` produces).
#'   When supplied, the report adds a "Survey-grade volumes" section
#'   that runs `compute_survey_volumes()` with four base-reference
#'   methods (DTM, min Z, mean Z, perimeter TIN) for each ROI. Defaults
#'   to `<project>/studio_assets/rois.geojson` when that file exists.
#' @param snapshot_path Optional PNG of the 3D viewer (e.g. from the
#'   "Screenshot" toolbar button in Drone Biomass Studio). When supplied,
#'   the report embeds it in the "3D scene documentation" section.
#'   Otherwise the section falls back to a server-side `persp()`
#'   rendering of the DSM.
#' @param rerun_workflow Logical. When `TRUE`, the report calls
#'   `run_dronebio_workflow()` from scratch and uses its in-memory
#'   outputs. When `FALSE` (the default), the report consumes the
#'   workflow artefacts already on disk under `<output_dir>/` or the
#'   project's canonical `outputs/` folder, falling back to a fresh
#'   workflow run only when no existing outputs can be found. Keeps
#'   the report cheap to re-render after the user has already run the
#'   workflow once - the previous behaviour silently re-did all the
#'   raster math on every render.
#' @return Invisibly returns the absolute path to the rendered file.
#' @examples
#' \dontrun{
#' project <- dronebio_project("~/flights/2026-05-01")
#' out <- render_dronebio_report(
#'   project     = project,
#'   output_file = file.path(tempdir(), "flight_report.html"),
#'   field_csv   = file.path(project$project_dir, "field_samples.csv")
#' )
#' file.exists(out)
#' }
#' @export
render_dronebio_report <- function(project,
                                   output_file    = NULL,
                                   field_csv      = NULL,
                                   use_alpha      = TRUE,
                                   roi_geojson    = NULL,
                                   snapshot_path  = NULL,
                                   rerun_workflow = FALSE) {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop(
      "The 'rmarkdown' package is required to render reports. ",
      "Install it with install.packages('rmarkdown').",
      call. = FALSE
    )
  }

  if (is.character(project)) {
    project <- dronebio_project(project)
  }
  if (!inherits(project, "dronebio_project")) {
    stop("'project' must be a dronebio_project object or a directory path.",
         call. = FALSE)
  }

  template <- system.file("report", "biomass_report.Rmd", package = "DroneBioR")
  if (!nzchar(template)) {
    stop("Report template not found inside the DroneBioR package.", call. = FALSE)
  }

  if (is.null(output_file)) {
    output_file <- file.path(project$project_dir, "DroneBioR_report.html")
  }
  # Auto-pick up ROIs persisted by Drone Biomass Studio when the caller
  # does not pass an explicit path.
  if (is.null(roi_geojson)) {
    auto <- file.path(project$project_dir, "studio_assets", "rois.geojson")
    if (file.exists(auto)) roi_geojson <- auto
  }
  output_dir  <- dirname(output_file)
  output_name <- basename(output_file)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # rmarkdown::render() modifies the working directory of the template
  # which can confuse relative paths in user code. We render against a
  # copy of the template in a temp dir to keep the original untouched.
  tmp_template <- tempfile(fileext = ".Rmd")
  file.copy(template, tmp_template, overwrite = TRUE)
  on.exit(unlink(tmp_template), add = TRUE)

  intermediate_dir <- tempfile("dronebior-render-")
  dir.create(intermediate_dir, recursive = TRUE, showWarnings = FALSE)

  rmarkdown::render(
    input             = tmp_template,
    output_file       = output_name,
    output_dir        = output_dir,
    intermediates_dir = intermediate_dir,
    knit_root_dir     = intermediate_dir,
    params = list(
      project_dir    = project$project_dir,
      field_csv      = field_csv,
      use_alpha      = isTRUE(use_alpha),
      output_dir     = file.path(intermediate_dir, "workflow_outputs"),
      roi_geojson    = roi_geojson,
      snapshot_path  = snapshot_path,
      rerun_workflow = isTRUE(rerun_workflow),
      # Pass the project's existing output folder so the Rmd can read
      # back reflectance_summary.csv / spectral_index_summary.csv /
      # GeoTIFFs without rerunning the workflow.
      existing_output_dir = project$output_dir
    ),
    quiet = TRUE,
    envir = new.env(parent = globalenv())
  )

  invisible(normalizePath(file.path(output_dir, output_name), mustWork = FALSE))
}
