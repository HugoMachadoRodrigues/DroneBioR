test_that("run_dronebio_workflow runs end-to-end against the bundled fixtures", {
  project <- dronebio_project(project_dir = tempdir())
  ortho <- system.file("extdata", "micasense_subset.tif", package = "DroneBioR")
  out <- tempfile("dronebior-out-")
  result <- run_dronebio_workflow(
    project     = project,
    orthomosaic = ortho,
    output_dir  = out
  )

  expected <- c("project", "orthomosaic", "bands", "reflectance", "indices",
                "biomass_proxy", "alpha", "reflectance_summary",
                "index_summary", "output_paths")
  expect_true(all(expected %in% names(result)))
  expect_equal(terra::nlyr(result$indices), 9)
  expect_equal(terra::nlyr(result$reflectance), 5)
  expect_true(file.exists(result$output_paths[["indices"]]))
  expect_true(file.exists(result$output_paths[["reflectance"]]))
  expect_true(file.exists(result$output_paths[["biomass_proxy"]]))
})

test_that("run_dronebio_workflow accepts a project_dir character path", {
  out <- tempfile("dronebior-out2-")
  ortho <- system.file("extdata", "micasense_subset.tif", package = "DroneBioR")
  result <- run_dronebio_workflow(
    project     = tempdir(),
    orthomosaic = ortho,
    output_dir  = out
  )
  expect_s3_class(result$project, "dronebio_project")
})
