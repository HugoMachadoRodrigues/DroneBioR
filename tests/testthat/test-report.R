test_that("render_dronebio_report errors when rmarkdown is missing", {
  skip_if(requireNamespace("rmarkdown", quietly = TRUE))
  expect_error(
    render_dronebio_report(tempdir()),
    "rmarkdown"
  )
})

test_that("render_dronebio_report renders against the sample project", {
  skip_if_not_installed("rmarkdown")
  skip_on_cran()

  project <- dronebio_sample_project(target_dir = tempfile("dronebior-report-"))
  out <- file.path(tempdir(), "DroneBioR_report_test.html")
  if (file.exists(out)) unlink(out)

  result <- render_dronebio_report(
    project     = project,
    output_file = out,
    field_csv   = file.path(project$project_dir, "field_samples.csv")
  )

  expect_true(file.exists(result))
  expect_match(result, "\\.html$")
  expect_gt(file.info(result)$size, 1000)
})

test_that("render_dronebio_report errors on bad project input", {
  skip_if_not_installed("rmarkdown")
  expect_error(
    render_dronebio_report(project = 42),
    "dronebio_project"
  )
})
