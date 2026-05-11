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
#' @return Invisibly returns the absolute path to the rendered file.
#' @examples
#' \donttest{
#' project <- dronebio_sample_project(target_dir = tempfile("dronebior-sample-"))
#' out <- render_dronebio_report(
#'   project     = project,
#'   output_file = file.path(tempdir(), "demo_report.html"),
#'   field_csv   = file.path(project$project_dir, "field_samples.csv")
#' )
#' file.exists(out)
#' }
#' @export
render_dronebio_report <- function(project,
                                   output_file = NULL,
                                   field_csv   = NULL,
                                   use_alpha   = TRUE) {
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
      project_dir = project$project_dir,
      field_csv   = field_csv,
      use_alpha   = isTRUE(use_alpha),
      output_dir  = file.path(intermediate_dir, "workflow_outputs")
    ),
    quiet = TRUE,
    envir = new.env(parent = globalenv())
  )

  invisible(normalizePath(file.path(output_dir, output_name), mustWork = FALSE))
}
