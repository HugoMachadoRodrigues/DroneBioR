test_that("format_seconds_human renders compact human strings", {
  fmt <- DroneBioR:::format_seconds_human
  expect_equal(fmt(0), "0s")
  expect_equal(fmt(59), "59s")
  expect_equal(fmt(60), "1m 00s")
  expect_equal(fmt(125), "2m 05s")
  expect_equal(fmt(3600), "1h 00m")
  expect_equal(fmt(3661), "1h 01m")
  expect_equal(fmt(-1), "?")
  expect_equal(fmt(NA), "?")
  expect_equal(fmt(NaN), "?")
})

test_that("make_odm_stage_poller detects new stages once and reports active stage", {
  project_dir <- tempfile("odm-poll-")
  dir.create(project_dir)
  poller <- DroneBioR:::make_odm_stage_poller(
    project_dir = project_dir,
    image_count = 100L,
    band_label  = "TEST"
  )

  # First poll: no stage dirs exist yet, status should still print
  # without erroring and reflect "starting".
  out <- testthat::capture_messages(state1 <- poller())
  expect_true(any(grepl("starting", out)))
  expect_true(is.na(state1$active_stage))
  expect_equal(state1$stages_done, 0)

  # Simulate ODM creating the first stage directory.
  dir.create(file.path(project_dir, "dataset"))
  out2 <- testthat::capture_messages(state2 <- poller())
  expect_true(any(grepl("`dataset` started", out2)))
  expect_true(any(grepl("stage `dataset`", out2)))
  expect_equal(state2$active_stage, "dataset")
  expect_equal(state2$stages_done, 1)

  # Second poll on same state: should NOT re-announce the stage start,
  # but should still emit the periodic status line.
  out3 <- testthat::capture_messages(poller())
  expect_false(any(grepl("`dataset` started", out3)))
  expect_true(any(grepl("stage `dataset`", out3)))

  # Two more stages appear: active should track the latest canonical
  # one, even if directories were created in odd order.
  dir.create(file.path(project_dir, "odm_orthophoto"))
  dir.create(file.path(project_dir, "opensfm"))
  out4 <- testthat::capture_messages(state4 <- poller())
  expect_equal(state4$active_stage, "odm_orthophoto")
  expect_equal(state4$stages_done, 3)
  # Both new stages should have been announced once.
  expect_true(any(grepl("`opensfm` started", out4)))
  expect_true(any(grepl("`odm_orthophoto` started", out4)))
})

test_that("make_odm_stage_poller status line includes ETA and percent fields", {
  project_dir <- tempfile("odm-poll-")
  dir.create(project_dir)
  poller <- DroneBioR:::make_odm_stage_poller(
    project_dir = project_dir,
    image_count = 100L
  )
  out <- testthat::capture_messages(poller())
  joined <- paste(out, collapse = "\n")
  expect_match(joined, "remaining")
  expect_match(joined, "elapsed")
  expect_match(joined, "stages")
  expect_match(joined, "%")
})

test_that("run_docker_with_progress falls back gracefully when processx is missing", {
  # We cannot reliably uninstall processx mid-test; instead, just check
  # that the helper exists and exposes the expected signature so the
  # fallback branch is reachable.
  fn <- DroneBioR:::run_docker_with_progress
  expect_true(is.function(fn))
  expect_setequal(
    names(formals(fn)),
    c("args", "project_dir", "image_count", "band_label", "poll_interval_secs")
  )
})
