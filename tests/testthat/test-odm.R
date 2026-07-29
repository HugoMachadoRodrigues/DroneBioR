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
  # Diagnostic message must name BOTH failure modes (SfM memory cap
  # AND divergent reconstruction -> oversized orthophoto), not just
  # the Docker memory cap.
  expect_match(body_str, "diverged")
  expect_match(body_str, "Model bounds")
  expect_match(body_str, "gps-accuracy")
})

test_that("run_odm_project has the OOM exit-137 retry path", {
  body_str <- paste(
    deparse(body(DroneBioR::run_odm_project)),
    collapse = "\n"
  )
  expect_match(body_str, "137")
  expect_match(body_str, "max_concurrency *= *1")
  expect_match(body_str, "feature-quality")
  expect_match(body_str, "diverged")
  expect_match(body_str, "gps-accuracy")
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

test_that("build_odm_args emits point-cloud cleanup flags", {
  a <- build_odm_args(tempdir(), "demo")
  # Tighter than ODM's own default of 5: that leaves the floating specks and
  # needles this stage exists to remove.
  expect_true("--pc-filter" %in% a)
  expect_equal(a[which(a == "--pc-filter") + 1L], "2.5")
  expect_false("--pc-sample" %in% a)
  expect_false("--pc-rectify" %in% a)

  b <- build_odm_args(tempdir(), "demo", pc_filter = 1.5,
                      pc_sample = 0.04, pc_rectify = TRUE)
  expect_equal(b[which(b == "--pc-filter") + 1L], "1.5")
  expect_equal(b[which(b == "--pc-sample") + 1L], "0.04")
  expect_true("--pc-rectify" %in% b)
})

test_that("pc_filter = 0 is passed through as ODM's disable value", {
  a <- build_odm_args(tempdir(), "demo", pc_filter = 0)
  expect_equal(a[which(a == "--pc-filter") + 1L], "0")
})

test_that("build_odm_args omits --pc-filter when pc_filter is NULL", {
  a <- build_odm_args(tempdir(), "demo", pc_filter = NULL)
  expect_false("--pc-filter" %in% a)
})

test_that("build_odm_args rejects nonsensical point-cloud settings", {
  expect_error(build_odm_args(tempdir(), "demo", pc_filter = -1),
               "non-negative")
  expect_error(build_odm_args(tempdir(), "demo", pc_sample = 0),
               "positive radius")
})

test_that("the numbers are written plainly, never in scientific notation", {
  # ODM parses these as floats; "1e-04" would be passed through verbatim.
  a <- build_odm_args(tempdir(), "demo", pc_sample = 0.0001)
  expect_equal(a[which(a == "--pc-sample") + 1L], "0.0001")
})

test_that("refilter_odm_point_cloud reruns from odm_filterpoints only", {
  # The point of the helper: opensfm/openmvs are reused, so the retune costs
  # only the stages after the filter.
  dir <- tempfile("refilter-")
  p <- dronebio_project(dir)
  expect_error(refilter_odm_point_cloud(p), "No reconstruction to reuse")

  dir.create(file.path(p$odm_project_dir, "opensfm"), recursive = TRUE)
  called <- NULL
  testthat::local_mocked_bindings(
    run_odm_project = function(project, ...) {
      called <<- list(...)
      invisible(TRUE)
    }
  )
  refilter_odm_point_cloud(p, pc_filter = 1.5, pc_rectify = TRUE)
  expect_equal(called$rerun_from, "odm_filterpoints")
  expect_equal(called$pc_filter, 1.5)
  expect_true(called$pc_rectify)
})

test_that("rebuild_from_edited_cloud reruns from odm_meshing and refuses earlier stages", {
  # Rerunning from odm_filterpoints (or anything before it) would regenerate
  # the cloud and silently discard the hand edit -- the whole point of this
  # entry point is that it cannot happen by accident.
  dir <- tempfile("edited-")
  p <- dronebio_project(dir)
  expect_error(rebuild_from_edited_cloud(p), "No filtered point cloud")

  fp <- file.path(p$odm_project_dir, "odm_filterpoints")
  dir.create(fp, recursive = TRUE)
  file.create(file.path(fp, "point_cloud.ply"))

  called <- NULL
  testthat::local_mocked_bindings(
    run_odm_project = function(project, ...) { called <<- list(...); invisible(TRUE) }
  )
  rebuild_from_edited_cloud(p, build_dsm = TRUE)
  expect_equal(called$rerun_from, "odm_meshing")
  expect_true(called$build_dsm)

  expect_error(rebuild_from_edited_cloud(p, rerun_from = "odm_filterpoints"),
               "would rebuild odm_filterpoints")
  # Asking for the stage it already uses is harmless.
  expect_silent(rebuild_from_edited_cloud(p, rerun_from = "odm_meshing"))
})

test_that("build_odm_args emits and validates --pc-quality", {
  a <- build_odm_args(tempdir(), "demo", pc_quality = "high")
  expect_equal(a[which(a == "--pc-quality") + 1L], "high")
  # NULL leaves ODM's own default alone rather than pinning it.
  expect_false("--pc-quality" %in% build_odm_args(tempdir(), "demo"))
  expect_error(build_odm_args(tempdir(), "demo", pc_quality = "turbo"))
})

test_that("build_point_cloud_only stops at odm_filterpoints and forbids fast orthophoto", {
  # fast_orthophoto skips densification, so there would be no dense cloud to
  # inspect -- the whole point of this stage.
  dir <- tempfile("stage0-")
  p <- dronebio_project(dir)
  called <- NULL
  testthat::local_mocked_bindings(
    run_odm_project = function(project, ...) { called <<- list(...); invisible(TRUE) }
  )
  build_point_cloud_only(p, pc_quality = "low", pc_filter = 1.5)
  expect_equal(called$end_with, "odm_filterpoints")
  expect_equal(called$pc_quality, "low")
  expect_equal(called$pc_filter, 1.5)
  expect_false(called$fast_orthophoto)

  expect_error(build_point_cloud_only(p, fast_orthophoto = TRUE),
               "skips the dense reconstruction")
})

test_that("the DJI runner threads the point-cloud settings to the RGB run", {
  # The MS runs contribute radiance only; the RGB run is the one whose
  # geometry every DEM and the stacked ortho inherit.
  a <- names(formals(run_odm_dji_mavic_3m))
  expect_true(all(c("pc_filter", "pc_sample", "pc_rectify") %in% a))
  b <- names(formals(DroneBioR:::run_one_dji_band))
  expect_true(all(c("pc_filter", "pc_sample", "pc_rectify") %in% b))
})

test_that("build_point_cloud_only routes DJI Mavic 3M folders to the RGB sub-run", {
  # run_odm_project() would read the folder with list_micasense_images(), which
  # rejects DJI_..._MS_NIR.TIF for not matching capture_band.tif. The geometry
  # of a Mavic 3M flight comes from its RGB sub-run, so that is what stage 0
  # takes to odm_filterpoints.
  dir <- tempfile("djistage0-")
  p <- dronebio_project(dir, images_subdir = "imgs")
  dir.create(p$images_dir, recursive = TRUE)
  for (f in c("DJI_20260501132033_0001_D.JPG",
              "DJI_20260501132034_0002_MS_G.TIF",
              "DJI_20260501132034_0002_MS_R.TIF",
              "DJI_20260501132034_0002_MS_RE.TIF",
              "DJI_20260501132034_0002_MS_NIR.TIF")) {
    file.create(file.path(p$images_dir, f))
  }
  expect_true(has_djim3m_images(p$images_dir))

  seen <- NULL
  testthat::local_mocked_bindings(
    run_one_dji_band = function(...) { seen <<- list(...); invisible(TRUE) },
    run_odm_project = function(...) stop("must not take the MicaSense path")
  )
  res <- build_point_cloud_only(p, pc_quality = "low", pc_filter = 1.5)
  expect_equal(seen$band, "RGB")
  expect_equal(seen$end_with, "odm_filterpoints")
  expect_false(seen$fast_orthophoto)
  expect_equal(seen$pc_filter, 1.5)
  expect_equal(res$camera, "dji_mavic_3m")
})
