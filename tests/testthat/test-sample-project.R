test_that("dronebio_sample_project seeds an ODM-shaped tree with fixtures", {
  target <- tempfile("dronebior-sample-test-")
  project <- dronebio_sample_project(target_dir = target)

  expect_s3_class(project, "dronebio_project")
  expect_equal(basename(project$project_dir), basename(target))

  # Files that downstream functions look for:
  expect_true(file.exists(project$odm_orthomosaic))
  expect_true(file.exists(file.path(project$odm_project_dir, "odm_dem", "dsm.tif")))
  expect_true(file.exists(file.path(project$odm_project_dir, "odm_dem", "dtm.tif")))
  expect_true(file.exists(file.path(project$project_dir, "field_samples.csv")))
})

test_that("dronebio_sample_project is idempotent and does not clobber edits", {
  target <- tempfile("dronebior-sample-idempotent-")
  project <- dronebio_sample_project(target_dir = target)

  # Touch the orthomosaic to a known size; a second call must NOT overwrite.
  ortho_path <- project$odm_orthomosaic
  writeLines("SENTINEL", ortho_path)
  size_after_edit <- file.info(ortho_path)$size

  dronebio_sample_project(target_dir = target)
  expect_equal(file.info(ortho_path)$size, size_after_edit)
})

test_that("The seeded sample project is consumable by the scientific pipeline", {
  project <- dronebio_sample_project(target_dir = tempfile("dronebior-sample-pipeline-"))

  s <- summarize_odm_products(project)
  expect_true(s[s$product == "orthomosaic", "available"])
  expect_true(s[s$product == "dsm", "available"])
  expect_true(s[s$product == "dtm", "available"])

  result <- run_dronebio_workflow(
    project    = project,
    output_dir = tempfile("dronebior-sample-out-")
  )
  expect_equal(terra::nlyr(result$indices), 22)
})
