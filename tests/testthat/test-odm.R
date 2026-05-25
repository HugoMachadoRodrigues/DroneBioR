test_that("ODM command includes project and core options", {
  args <- build_odm_args(
    dataset_dir = tempfile(),
    project_name = "micasense",
    orthophoto_resolution_cm = 5,
    fast_orthophoto = TRUE
  )

  expect_true("opendronemap/odm" %in% args)
  expect_true("--radiometric-calibration" %in% args)
  expect_true("--fast-orthophoto" %in% args)
  expect_equal(tail(args, 1), "micasense")
})

# --- clean_incomplete_odm_state -------------------------------------------

make_partial_opensfm_project <- function() {
  proj <- tempfile("odm-partial-")
  dir.create(file.path(proj, "images"), recursive = TRUE)
  dir.create(file.path(proj, "opensfm", "features"), recursive = TRUE)
  dir.create(file.path(proj, "opensfm", "exif"), recursive = TRUE)
  file.create(file.path(proj, "opensfm", "image_list.txt"))
  # NB: reconstruction.json is deliberately absent — that is what
  # marks the OpenSfM stage as incomplete.
  dir.create(file.path(proj, "odm_dem"), recursive = TRUE)
  dir.create(file.path(proj, "odm_orthophoto"), recursive = TRUE)
  file.create(file.path(proj, "images", "DJI_0001_D.JPG"))
  proj
}

test_that("clean_incomplete_odm_state wipes partial OpenSfM and downstream", {
  proj <- make_partial_opensfm_project()
  did <- DroneBioR:::clean_incomplete_odm_state(proj)
  expect_true(isTRUE(did))
  expect_false(dir.exists(file.path(proj, "opensfm")))
  expect_false(dir.exists(file.path(proj, "odm_dem")))
  expect_false(dir.exists(file.path(proj, "odm_orthophoto")))
  # images/ must be preserved — the hardlinks / copies are expensive
  # to recreate and they were not part of the broken state.
  expect_true(dir.exists(file.path(proj, "images")))
  expect_true(file.exists(file.path(proj, "images", "DJI_0001_D.JPG")))
})

test_that("clean_incomplete_odm_state leaves completed OpenSfM alone", {
  proj <- make_partial_opensfm_project()
  # Mark OpenSfM as completed by writing the reconstruction marker.
  file.create(file.path(proj, "opensfm", "reconstruction.json"))
  did <- DroneBioR:::clean_incomplete_odm_state(proj)
  expect_false(isTRUE(did))
  expect_true(dir.exists(file.path(proj, "opensfm")))
  expect_true(dir.exists(file.path(proj, "odm_dem")))
})

test_that("clean_incomplete_odm_state is a no-op on fresh project", {
  proj <- tempfile("odm-fresh-")
  dir.create(file.path(proj, "images"), recursive = TRUE)
  did <- DroneBioR:::clean_incomplete_odm_state(proj)
  expect_false(isTRUE(did))
})

test_that("clean_incomplete_odm_state handles non-existent project dirs", {
  expect_false(isTRUE(DroneBioR:::clean_incomplete_odm_state(tempfile("never-"))))
})
