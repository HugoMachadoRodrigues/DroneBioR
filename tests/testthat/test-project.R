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
                "point_cloud_ply", "mesh_ply",
                "textured_obj", "textured_obj_25d",
                "textured_glb", "textured_glb_25d",
                "tiles_3d", "map_tiles_dir", "report")
  expect_true(all(expected %in% names(paths)))
})

test_that("pick_best_textured_obj prefers 3D over 2.5D when present", {
  tmp <- tempfile("pick_obj_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE))
  p <- dronebio_project(project_dir = tmp,
                        odm_dataset_subdir = "ds", odm_project_name = "proj")

  # Neither file exists -> returns the 3D path as the sensible default
  expect_equal(pick_best_textured_obj(p),
               file.path(p$odm_project_dir, "odm_texturing", "odm_textured_model_geo.obj"))

  # Only 2.5D exists -> returns it
  d25 <- file.path(p$odm_project_dir, "odm_texturing_25d")
  dir.create(d25, recursive = TRUE)
  file.create(file.path(d25, "odm_textured_model_geo.obj"))
  expect_equal(pick_best_textured_obj(p),
               file.path(d25, "odm_textured_model_geo.obj"))

  # Both exist -> 3D wins
  d3 <- file.path(p$odm_project_dir, "odm_texturing")
  dir.create(d3, recursive = TRUE)
  file.create(file.path(d3, "odm_textured_model_geo.obj"))
  expect_equal(pick_best_textured_obj(p),
               file.path(d3, "odm_textured_model_geo.obj"))
})

test_that("pick_best_point_cloud picks COPC > LAZ > LAS > PLY", {
  tmp <- tempfile("pick_cloud_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE))
  p <- dronebio_project(project_dir = tmp,
                        odm_dataset_subdir = "ds", odm_project_name = "proj")
  paths <- odm_product_paths(p)
  for (k in c("point_cloud_las", "point_cloud_laz",
              "point_cloud_copc", "point_cloud_ply")) {
    dir.create(dirname(paths[[k]]), recursive = TRUE, showWarnings = FALSE)
  }

  # Nothing on disk -> default to COPC path
  expect_equal(pick_best_point_cloud(p), unname(paths[["point_cloud_copc"]]))

  # Only LAS exists
  file.create(paths[["point_cloud_las"]])
  expect_equal(pick_best_point_cloud(p), unname(paths[["point_cloud_las"]]))

  # Add LAZ -> wins
  file.create(paths[["point_cloud_laz"]])
  expect_equal(pick_best_point_cloud(p), unname(paths[["point_cloud_laz"]]))

  # Add COPC -> wins
  file.create(paths[["point_cloud_copc"]])
  expect_equal(pick_best_point_cloud(p), unname(paths[["point_cloud_copc"]]))
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
