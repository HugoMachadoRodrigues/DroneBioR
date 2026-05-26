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

test_that("build_odm_args wires --skip-3dmodel and --skip-report flags", {
  args_off <- build_odm_args(
    dataset_dir  = tempfile(),
    project_name = "p"
  )
  expect_false("--skip-3dmodel" %in% args_off)
  expect_false("--skip-report" %in% args_off)

  args_on <- build_odm_args(
    dataset_dir  = tempfile(),
    project_name = "p",
    skip_3dmodel = TRUE,
    skip_report  = TRUE
  )
  expect_true("--skip-3dmodel" %in% args_on)
  expect_true("--skip-report" %in% args_on)
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

# --- keep_only_final_odm_products -----------------------------------------

make_complete_odm_project <- function() {
  proj <- tempfile("odm-complete-")
  # Final products we want to KEEP.
  dir.create(file.path(proj, "odm_dem"), recursive = TRUE)
  file.create(file.path(proj, "odm_dem", "dsm.tif"))
  file.create(file.path(proj, "odm_dem", "dtm.tif"))
  dir.create(file.path(proj, "odm_orthophoto"))
  file.create(file.path(proj, "odm_orthophoto", "odm_orthophoto.tif"))
  file.create(file.path(proj, "log.json"))
  file.create(file.path(proj, "dronebior_odm.log"))
  # Intermediates we want to STRIP.
  dir.create(file.path(proj, "images"))
  file.create(file.path(proj, "images", "DJI_0001.JPG"))
  dir.create(file.path(proj, "opensfm"))
  file.create(file.path(proj, "opensfm", "reconstruction.json"))
  dir.create(file.path(proj, "openmvs"))
  dir.create(file.path(proj, "odm_filterpoints"))
  dir.create(file.path(proj, "odm_georeferencing"))
  dir.create(file.path(proj, "odm_postprocess"))
  file.create(file.path(proj, "cameras.json"))
  file.create(file.path(proj, "images.json"))
  file.create(file.path(proj, "img_list.txt"))
  file.create(file.path(proj, "benchmark.txt"))
  proj
}

test_that("keep_only_final_odm_products preserves DEM + ortho + logs", {
  proj <- make_complete_odm_project()
  removed <- DroneBioR:::keep_only_final_odm_products(proj)
  # Kept:
  expect_true(file.exists(file.path(proj, "odm_dem", "dsm.tif")))
  expect_true(file.exists(file.path(proj, "odm_dem", "dtm.tif")))
  expect_true(file.exists(file.path(proj, "odm_orthophoto",
                                    "odm_orthophoto.tif")))
  expect_true(file.exists(file.path(proj, "log.json")))
  expect_true(file.exists(file.path(proj, "dronebior_odm.log")))
  # Removed:
  for (gone in c("images", "opensfm", "openmvs", "odm_filterpoints",
                 "odm_georeferencing", "odm_postprocess",
                 "cameras.json", "images.json", "img_list.txt",
                 "benchmark.txt")) {
    expect_false(file.exists(file.path(proj, gone)),
                 info = gone)
    expect_false(dir.exists(file.path(proj, gone)),
                 info = gone)
    expect_true(gone %in% removed, info = gone)
  }
})

test_that("keep_only_final_odm_products is a no-op on non-existent dir", {
  out <- DroneBioR:::keep_only_final_odm_products(tempfile("never-"))
  expect_length(out, 0L)
})

test_that("run_one_dji_band has the OOM exit-137 retry path", {
  # Structural regression guard: a real OOM cannot be simulated
  # without docker, but the retry branch should always be visible in
  # the function body. If a future refactor drops it, this test
  # surfaces the change.
  body_str <- paste(
    deparse(body(DroneBioR:::run_one_dji_band)),
    collapse = "\n"
  )
  expect_match(body_str, "137")
  expect_match(body_str, "max_concurrency *= *1")
  expect_match(body_str, "feature-quality")
  expect_match(body_str, "oom-retry")
})

test_that("run_odm_project has the OOM exit-137 retry path", {
  body_str <- paste(
    deparse(body(DroneBioR::run_odm_project)),
    collapse = "\n"
  )
  expect_match(body_str, "137")
  expect_match(body_str, "max_concurrency *= *1")
  expect_match(body_str, "feature-quality")
})

test_that("keep_only_final_odm_products honours keep_extra allowlist", {
  proj <- make_complete_odm_project()
  removed <- DroneBioR:::keep_only_final_odm_products(
    proj,
    keep_extra = c("opensfm", "cameras.json")
  )
  # The two extras are now preserved.
  expect_true(dir.exists(file.path(proj, "opensfm")))
  expect_true(file.exists(file.path(proj, "cameras.json")))
  expect_false("opensfm" %in% removed)
  expect_false("cameras.json" %in% removed)
  # The standard intermediates are still stripped.
  expect_false(dir.exists(file.path(proj, "openmvs")))
})
