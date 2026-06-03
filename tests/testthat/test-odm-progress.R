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

  # First poll: no stage dirs exist yet, status should reflect "starting".
  state1 <- poller()
  expect_length(state1$announcements, 0L)
  expect_match(state1$status, "starting")
  expect_true(is.na(state1$active_stage))
  expect_equal(state1$stages_done, 0)

  # Simulate ODM creating the first stage directory.
  dir.create(file.path(project_dir, "dataset"))
  state2 <- poller()
  expect_length(state2$announcements, 1L)
  expect_match(state2$announcements[1L], "`dataset` started")
  expect_match(state2$status, "stage `dataset`")
  expect_equal(state2$active_stage, "dataset")
  expect_equal(state2$stages_done, 1)

  # Second poll on same state: NO new announcements, status still
  # reports the active stage.
  state3 <- poller()
  expect_length(state3$announcements, 0L)
  expect_match(state3$status, "stage `dataset`")

  # Two more stages appear: active tracks the latest canonical one
  # regardless of directory creation order.
  dir.create(file.path(project_dir, "odm_orthophoto"))
  dir.create(file.path(project_dir, "opensfm"))
  state4 <- poller()
  expect_length(state4$announcements, 2L)
  expect_equal(state4$active_stage, "odm_orthophoto")
  expect_equal(state4$stages_done, 3)
  joined_announcements <- paste(state4$announcements, collapse = "\n")
  expect_match(joined_announcements, "`opensfm` started")
  expect_match(joined_announcements, "`odm_orthophoto` started")
})

test_that("log_based_active_stage reads the authoritative running stage", {
  fn <- DroneBioR:::log_based_active_stage
  stages <- DroneBioR:::odm_stage_order()

  log <- tempfile(fileext = ".log")
  writeLines(c(
    "[INFO]    Running dataset stage",
    "[INFO]    Finished dataset stage",
    "[INFO]    Running opensfm stage",
    "2026-06-03 10:44:17 DEBUG: Undistorting image DJI_0295_D.JPG"
  ), log)
  # opensfm started, never finished -> it is the active stage, even
  # though an odm_georeferencing DIRECTORY might exist on disk.
  expect_equal(fn(log, stages), "opensfm")

  # Once opensfm finishes and odm_dem starts, that becomes active.
  writeLines(c(
    "[INFO]    Running opensfm stage",
    "[INFO]    Finished opensfm stage",
    "[INFO]    Running odm_dem stage"
  ), log)
  expect_equal(fn(log, stages), "odm_dem")

  # No markers yet / missing file -> NA (caller falls back to dirs).
  expect_true(is.na(fn(tempfile(fileext = ".log"), stages)))
  empty <- tempfile(fileext = ".log"); file.create(empty)
  expect_true(is.na(fn(empty, stages)))
})

test_that("poller prefers the log stage over an early odm_georeferencing dir", {
  # Reproduces the --geo bug: ODM creates odm_georeferencing/ early
  # while opensfm is still running. The poller must report opensfm
  # (from the log), not odm_georeferencing (from the directory).
  project_dir <- tempfile("geo-early-")
  dir.create(project_dir)
  # Both dirs exist and are "fresh".
  dir.create(file.path(project_dir, "opensfm"))
  dir.create(file.path(project_dir, "odm_georeferencing"))
  # The log says opensfm is the running stage.
  writeLines(c(
    "[INFO]    Running dataset stage",
    "[INFO]    Finished dataset stage",
    "[INFO]    Running opensfm stage"
  ), file.path(project_dir, "dronebior_odm.log"))

  Sys.sleep(0.05)  # ensure started_at < dir mtimes is not the gate here
  poller <- DroneBioR:::make_odm_stage_poller(
    project_dir = project_dir, image_count = 100L
  )
  state <- poller()
  expect_equal(state$active_stage, "opensfm")
  expect_match(state$status, "opensfm")
})

test_that("run_docker_with_progress records per-stage durations on clean exit", {
  skip_if_not_installed("processx")
  if (!nzchar(Sys.which("sh"))) skip("`sh` not on PATH")

  # Point the history file at a temp location so the test does not
  # touch the user's real ~/.dronebior/odm_stage_history.csv.
  tmp_home <- tempfile("dronebior-home-")
  dir.create(file.path(tmp_home, ".dronebior"), recursive = TRUE)
  withr::with_envvar(c(HOME = tmp_home), {
    project_dir <- tempfile("rec-")
    dir.create(project_dir)

    # The subprocess must WRITE the stage marker to its stdout (which
    # run_docker_with_progress redirects to dronebior_odm.log), because
    # the redirect truncates any pre-seeded file. `sh -c` echoes the
    # marker then sleeps so the poller has time to read it.
    DroneBioR:::run_docker_with_progress(
      args               = c("-c", "echo 'Running opensfm stage'; sleep 1"),
      project_dir        = project_dir,
      image_count        = 308L,
      poll_interval_secs = 0.3,
      command            = "sh"
    )

    hist <- DroneBioR:::read_odm_stage_history()
    # opensfm was the active stage throughout -> it must have been
    # recorded against image_count 308.
    expect_true(any(hist$stage == "opensfm" & hist$image_count == 308L))
  })
})

test_that("default_odm_concurrency returns a sane positive integer", {
  n <- DroneBioR:::default_odm_concurrency()
  expect_true(is.integer(n))
  expect_gte(n, 1L)
  expect_lte(n, 16L)
})

test_that("run_odm_dji_mavic_3m defaults max_concurrency to NULL (auto)", {
  expect_null(eval(formals(DroneBioR::run_odm_dji_mavic_3m)$max_concurrency))
})

test_that("make_odm_stage_poller ignores stale stage dirs from previous runs", {
  # Regression for the case where a previous failed ODM run left a
  # downstream stage directory (e.g. odm_georeferencing) on disk.
  # Without the mtime filter the poller would canonically pick that
  # stale dir as the active stage, producing wildly-wrong ETAs (e.g.
  # ~10 min remaining while opensfm has another ~30 min to go).
  project_dir <- tempfile("odm-stale-")
  dir.create(project_dir)

  # Stale downstream dir from a previous run, mtime in the past.
  dir.create(file.path(project_dir, "odm_georeferencing"))
  Sys.setFileTime(file.path(project_dir, "odm_georeferencing"),
                  Sys.time() - 3600 * 24)  # 1 day old

  # Wait a beat so the poller's `started_at` is strictly later than
  # the stale dir's mtime even on filesystems with second-level mtime
  # resolution.
  Sys.sleep(1)

  poller <- DroneBioR:::make_odm_stage_poller(
    project_dir = project_dir,
    image_count = 100L,
    band_label  = "TEST"
  )

  # First poll — only the stale dir exists. Active must NOT be the
  # stale dir; it should be NA (the run hasn't created anything yet).
  state1 <- poller()
  expect_true(is.na(state1$active_stage))
  expect_match(state1$status, "starting")

  # Now ODM creates a fresh upstream stage (`opensfm`). The poller
  # must report that as active, not the older stale downstream dir.
  dir.create(file.path(project_dir, "opensfm"))
  state2 <- poller()
  expect_equal(state2$active_stage, "opensfm")
  expect_equal(state2$stages_done, 1)
  expect_length(state2$announcements, 1L)
  expect_match(state2$announcements[1L], "`opensfm` started")
})

test_that("make_odm_stage_poller status line includes ETA and percent fields", {
  project_dir <- tempfile("odm-poll-")
  dir.create(project_dir)
  poller <- DroneBioR:::make_odm_stage_poller(
    project_dir = project_dir,
    image_count = 100L
  )
  state <- poller()
  expect_match(state$status, "remaining")
  expect_match(state$status, "elapsed")
  expect_match(state$status, "stages")
  expect_match(state$status, "%")
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
  # Both the start banner, the per-poll status line and the final
  # exit-status report go through message() (stderr) — capture them
  # in one stream.
  captured_msg <- testthat::capture_messages(
    status <- DroneBioR:::run_docker_with_progress(
      args               = c("1"),
      project_dir        = project_dir,
      image_count        = 10L,
      band_label         = "TEST",
      poll_interval_secs = 0.3,
      command            = "sleep"
    )
  )
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  joined <- paste(captured_msg, collapse = "\n")

  expect_equal(status, 0L)
  expect_gte(elapsed, 0.9)        # the subprocess really ran ~1 s
  expect_match(joined, "Starting ODM run")
  # At least two status lines (one mid-run, one final) should mention
  # the stage breakdown.
  poll_lines <- grep("stages", captured_msg, value = TRUE)
  expect_gte(length(poll_lines), 2L)
  # The post-run summary names the docker exit status and lists
  # which final products are on disk (none here, since `sleep` is
  # not docker).
  expect_match(joined, "Docker exited with status 0")
  expect_match(joined, "ortho")
  expect_match(joined, "dsm")
  # Docker output would land in a log file inside project_dir; for a
  # `sleep` subprocess there is none to capture but the file is still
  # created.
  expect_true(file.exists(file.path(project_dir, "dronebior_odm.log")))
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
