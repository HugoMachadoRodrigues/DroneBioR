# The georeferenced cloud is a final product, not an intermediate: the 3-D
# editor, the CSF terrain refinement and every point-cloud metric read it, and
# it cannot be recovered without repeating the whole reconstruction.

make_finished_project <- function() {
  d <- tempfile("odm_done_"); dir.create(d)
  for (sub in c("odm_dem", "odm_orthophoto", "odm_georeferencing",
                "opensfm", "images", "odm_filterpoints", "odm_meshing")) {
    dir.create(file.path(d, sub))
  }
  file.create(file.path(d, "odm_dem", c("dsm.tif", "dtm.tif")))
  file.create(file.path(d, "odm_orthophoto",
                        c("odm_orthophoto.tif", "odm_orthophoto_dji.tif")))
  file.create(file.path(d, c("log.json", "dronebior_odm.log",
                             "benchmark.txt", "images.json")))
  d
}

test_that("cleanup keeps the georeferenced cloud", {
  d <- make_finished_project()
  laz <- file.path(d, "odm_georeferencing", "odm_georeferenced_model.laz")
  file.create(laz)

  DroneBioR:::keep_only_final_odm_products(d)

  expect_true(file.exists(laz))
  expect_true(dir.exists(file.path(d, "odm_dem")))
  expect_true(dir.exists(file.path(d, "odm_orthophoto")))
  # intermediates still go
  expect_false(dir.exists(file.path(d, "opensfm")))
  expect_false(dir.exists(file.path(d, "odm_filterpoints")))
  expect_false(file.exists(file.path(d, "benchmark.txt")))
  # the logs are small and worth keeping for forensics
  expect_true(file.exists(file.path(d, "log.json")))
})

test_that("the redundant uncompressed .las is dropped when a .laz exists", {
  d <- make_finished_project()
  geo <- file.path(d, "odm_georeferencing")
  laz <- file.path(geo, "odm_georeferenced_model.laz")
  las <- file.path(geo, "odm_georeferenced_model.las")
  file.create(c(laz, las))

  DroneBioR:::keep_only_final_odm_products(d)

  expect_true(file.exists(laz))
  expect_false(file.exists(las))
})

test_that("a .las with no compressed twin is kept, not silently deleted", {
  d <- make_finished_project()
  las <- file.path(d, "odm_georeferencing", "lonely_model.las")
  file.create(las)

  DroneBioR:::keep_only_final_odm_products(d)

  expect_true(file.exists(las))
})

test_that("odm_product_paths prefers the 7-band stack, and the bands are recognised", {
  root <- tempfile("dji_ortho_"); dir.create(root)
  p <- dronebio_project(project_dir = root)
  od <- file.path(p$odm_project_dir, "odm_orthophoto")
  dir.create(od, recursive = TRUE)
  file.create(file.path(od, "odm_orthophoto.tif"))

  # with only the RGB file, that is what a caller gets
  expect_match(unname(odm_product_paths(p)[["orthomosaic"]]),
               "odm_orthophoto\\.tif$")

  # once the stack exists it wins - this is the path the Studio must show,
  # because taking p$odm_orthomosaic instead reports a DJI flight as RGB-only
  file.create(file.path(od, "odm_orthophoto_dji.tif"))
  expect_match(unname(odm_product_paths(p)[["orthomosaic"]]),
               "odm_orthophoto_dji\\.tif$")
  expect_match(p$odm_orthomosaic, "odm_orthophoto\\.tif$")  # the raw field lags
})

test_that("the DJI stack's band names resolve to NIR and RedEdge", {
  # The stack is written as Red, Green, Blue, MS_G, MS_R, MS_RE, MS_NIR; the
  # multispectral indices depend on those MS_ names mapping to canonical ones.
  nms <- c("Red", "Green", "Blue", "MS_G", "MS_R", "MS_RE", "MS_NIR")
  b <- orthomosaic_band_presence(nms, nlyr = length(nms))
  expect_true(b$has_nir)
  expect_true(b$has_rededge)
  expect_true(b$has_red)
  expect_true(b$has_green)
})
