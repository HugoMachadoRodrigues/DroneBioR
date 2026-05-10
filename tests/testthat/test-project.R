test_that("dronebio_project builds normalized paths and has the right class", {
  p <- dronebio_project(project_dir = tempdir())
  expect_s3_class(p, "dronebio_project")
  expect_true(startsWith(p$odm_orthomosaic, p$project_dir))
  expect_equal(basename(p$odm_orthomosaic), "odm_orthophoto.tif")
  expect_equal(p$odm_project_name, "micasense")
})

test_that("dronebio_project honors custom subdirs", {
  p <- dronebio_project(
    project_dir       = tempdir(),
    images_subdir     = "raw_imgs",
    output_subdir     = "my_outputs",
    odm_project_name  = "flightA"
  )
  expect_equal(basename(p$images_dir), "raw_imgs")
  expect_equal(basename(p$output_dir), "my_outputs")
  expect_equal(p$odm_project_name, "flightA")
})

test_that("odm_product_paths returns all expected products", {
  p <- dronebio_project(project_dir = tempdir())
  paths <- odm_product_paths(p)
  expected <- c("orthomosaic", "dsm", "dtm", "point_cloud_las",
                "point_cloud_laz", "point_cloud_copc",
                "point_cloud_ply", "mesh_ply", "textured_obj")
  expect_true(all(expected %in% names(paths)))
})

test_that("summarize_odm_products reports nothing available for an empty project", {
  p <- dronebio_project(project_dir = tempdir())
  s <- summarize_odm_products(p)
  expect_true(all(c("product", "available", "size_mb", "path") %in% names(s)))
  expect_false(any(s$available))
})

test_that("configure_proj_database returns a logical without raising", {
  # Returns TRUE invisibly when proj.db is reachable, FALSE with a warning
  # otherwise. Both outcomes are valid for this smoke test.
  result <- suppressWarnings(configure_proj_database(verbose = FALSE))
  expect_type(result, "logical")
})
