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

test_that("run_docker_with_progress exposes the expected signature", {
  fn <- DroneBioR:::run_docker_with_progress
  expect_true(is.function(fn))
  expect_setequal(
    names(formals(fn)),
    c("args", "project_dir", "image_count", "band_label",
      "poll_interval_secs", "command")
  )
})

test_that("run_docker_with_progress drives a real subprocess to clean exit", {
  # Integration test that actually exercises the processx machinery
  # end-to-end. The previous suite only checked the signature, which
  # let an API-shape bug (passing `timeout` to `read_output`) reach
  # main. Swap docker for the POSIX `sleep` binary so we don't need
  # the docker daemon, then verify (a) the helper returns 0, (b) at
  # least one progress poll fires, and (c) the wall time matches the
  # sleep duration (subprocess actually ran, was not skipped).
  skip_if_not_installed("processx")
  if (!nzchar(Sys.which("sleep"))) skip("`sleep` binary not on PATH")

  project_dir <- tempfile("progress-int-")
  dir.create(project_dir)

  t0 <- Sys.time()
  out <- testthat::capture_messages(
    status <- DroneBioR:::run_docker_with_progress(
      args               = c("1"),
      project_dir        = project_dir,
      image_count        = 10L,
      band_label         = "TEST",
      poll_interval_secs = 0.3,   # 300 ms -> ~3 polls during the 1 s sleep
      command            = "sleep"
    )
  )
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  expect_equal(status, 0L)
  expect_gte(elapsed, 0.9)        # the subprocess really ran ~1 s
  expect_true(any(grepl("Starting ODM run", out)))
  # The poller emits a status line on each tick; with 300 ms cadence
  # over ~1 s we should see at least 2 lines (start + ~3 polls).
  poll_lines <- grep("stages", out, value = TRUE)
  expect_gte(length(poll_lines), 2L)
})

test_that("run_docker_with_progress fallback path is structurally reachable", {
  # We cannot uninstall processx mid-test, so we cannot exercise the
  # fallback branch end-to-end. Instead grep the function body to
  # confirm it still calls system2() in the !requireNamespace branch
  # — a regression guard against silently dropping the fallback the
  # next time the helper is refactored.
  body_str <- paste(deparse(body(DroneBioR:::run_docker_with_progress)),
                    collapse = "\n")
  expect_match(body_str, "requireNamespace\\(\"processx\"")
  expect_match(body_str, "system2\\(command, args = args\\)")
})
