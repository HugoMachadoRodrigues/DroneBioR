# Internal helpers — tested via ::: so they don't need to be exported.
# Each test owns a private HOME so we never touch the user's real
# ~/.dronebior/ directory.

with_fake_home <- function(code) {
  tmp <- tempfile("dronebior_home_")
  dir.create(tmp, recursive = TRUE)
  old <- Sys.getenv("HOME")
  Sys.setenv(HOME = tmp)
  on.exit({
    Sys.setenv(HOME = old)
    unlink(tmp, recursive = TRUE)
  })
  force(code)
}

test_that("odm_stage_baseline_seconds covers the canonical ODM 3.6 pipeline", {
  baseline <- DroneBioR:::odm_stage_baseline_seconds()
  expected <- c("dataset", "split", "merge", "opensfm", "openmvs",
                "odm_filterpoints", "odm_meshing", "mvs_texturing",
                "odm_georeferencing", "odm_dem", "odm_orthophoto",
                "odm_report", "odm_postprocess")
  expect_equal(sort(names(baseline)), sort(expected))
  expect_true(all(baseline > 0))
  # opensfm and openmvs dominate; sanity-check the magnitudes
  expect_true(baseline[["opensfm"]] > baseline[["dataset"]])
  expect_true(baseline[["openmvs"]] > baseline[["dataset"]])
})

test_that("odm_stage_order matches the baseline keys", {
  expect_equal(sort(DroneBioR:::odm_stage_order()),
               sort(names(DroneBioR:::odm_stage_baseline_seconds())))
})

test_that("read_odm_stage_history returns empty frame when CSV is missing", {
  with_fake_home({
    hist <- DroneBioR:::read_odm_stage_history()
    expect_s3_class(hist, "data.frame")
    expect_equal(nrow(hist), 0L)
    expect_setequal(names(hist),
                    c("run_started_at", "image_count", "stage", "duration_seconds",
                      "camera"))
  })
})

test_that("record_odm_stage_completion appends and dedupes on (run, stage)", {
  with_fake_home({
    DroneBioR:::record_odm_stage_completion("run-A", 100L, "opensfm", 1800)
    DroneBioR:::record_odm_stage_completion("run-A", 100L, "openmvs", 1200)
    DroneBioR:::record_odm_stage_completion("run-B", 200L, "opensfm", 3600)
    h <- DroneBioR:::read_odm_stage_history()
    expect_equal(nrow(h), 3L)

    # Re-record same (run, stage) -> replaces, doesn't append
    DroneBioR:::record_odm_stage_completion("run-A", 100L, "opensfm", 1900)
    h2 <- DroneBioR:::read_odm_stage_history()
    expect_equal(nrow(h2), 3L)
    rowA <- h2[h2$run_started_at == "run-A" & h2$stage == "opensfm", ]
    expect_equal(rowA$duration_seconds, 1900)
  })
})

test_that("record_odm_stage_completion rejects non-finite or negative durations", {
  with_fake_home({
    expect_false(DroneBioR:::record_odm_stage_completion("run", 100L, "opensfm", NA_real_))
    expect_false(DroneBioR:::record_odm_stage_completion("run", 100L, "opensfm", -5))
    expect_equal(nrow(DroneBioR:::read_odm_stage_history()), 0L)
  })
})

test_that("estimate_odm_stage_seconds falls back to baseline with no history", {
  with_fake_home({
    baseline_opensfm <- DroneBioR:::odm_stage_baseline_seconds()[["opensfm"]]
    expect_equal(DroneBioR:::estimate_odm_stage_seconds("opensfm"), baseline_opensfm)
    expect_equal(DroneBioR:::estimate_odm_stage_seconds("opensfm", image_count = 200L),
                 baseline_opensfm)
  })
})

test_that("estimate_odm_stage_seconds uses median of history when available", {
  with_fake_home({
    DroneBioR:::record_odm_stage_completion("run-1", 100L, "openmvs", 1000)
    DroneBioR:::record_odm_stage_completion("run-2", 100L, "openmvs", 2000)
    DroneBioR:::record_odm_stage_completion("run-3", 100L, "openmvs", 1500)
    # Median is 1500
    expect_equal(DroneBioR:::estimate_odm_stage_seconds("openmvs", image_count = 100L), 1500)
  })
})

test_that("estimate_odm_stage_seconds scales by image count ratio", {
  with_fake_home({
    DroneBioR:::record_odm_stage_completion("run-1", 100L, "openmvs", 600)
    # Median historical count = 100; request 200 -> scale 2.0 -> 1200
    expect_equal(DroneBioR:::estimate_odm_stage_seconds("openmvs", image_count = 200L), 1200)
    # Half the images -> scale 0.5 -> 300
    expect_equal(DroneBioR:::estimate_odm_stage_seconds("openmvs", image_count = 50L), 300)
  })
})

test_that("estimate_remaining_seconds subtracts elapsed from the active stage", {
  with_fake_home({
    DroneBioR:::record_odm_stage_completion("run-1", 100L, "opensfm", 1000)
    DroneBioR:::record_odm_stage_completion("run-1", 100L, "openmvs", 500)
    # Active opensfm with 400s elapsed -> 600 remaining; plus 500 openmvs = 1100
    rem <- DroneBioR:::estimate_remaining_seconds(
      active_stage           = "opensfm",
      pending_stages         = "openmvs",
      active_elapsed_seconds = 400,
      image_count            = 100L
    )
    expect_equal(rem, 1100)
  })
})

test_that("an overrunning active stage still reports time remaining", {
  with_fake_home({
    DroneBioR:::record_odm_stage_completion("run-1", 100L, "opensfm", 1000)
    rem <- DroneBioR:::estimate_remaining_seconds(
      active_stage           = "opensfm",
      pending_stages         = character(),
      active_elapsed_seconds = 5000,  # already past the 1000s estimate
      image_count            = 100L
    )
    # Assumed half-done, so expect as much again rather than the old 0.
    expect_equal(rem, 5000)
  })
})

test_that("an overrun carries its slowdown over to the pending stages", {
  with_fake_home({
    DroneBioR:::record_odm_stage_completion("run-1", 100L, "opensfm", 1000)
    DroneBioR:::record_odm_stage_completion("run-1", 100L, "openmvs", 500)
    # opensfm at 4000s against a 1000s estimate: this run is 4x slower than
    # history predicts, so the 500s openmvs estimate should be read as 2000s.
    rem <- DroneBioR:::estimate_remaining_seconds(
      active_stage           = "opensfm",
      pending_stages         = "openmvs",
      active_elapsed_seconds = 4000,
      image_count            = 100L
    )
    expect_equal(rem, 4000 + 500 * 4)
  })
})

test_that("the ETA grows while a stage keeps overrunning", {
  with_fake_home({
    DroneBioR:::record_odm_stage_completion("run-1", 100L, "opensfm", 1000)
    DroneBioR:::record_odm_stage_completion("run-1", 100L, "openmvs", 500)
    at <- function(elapsed) {
      DroneBioR:::estimate_remaining_seconds(
        active_stage = "opensfm", pending_stages = "openmvs",
        active_elapsed_seconds = elapsed, image_count = 100L
      )
    }
    # Inside the estimate it still counts down; past it, it must not flatline.
    expect_lt(at(900), at(100))
    expect_gt(at(4000), at(2000))
  })
})

test_that("a sub-second baseline stage cannot inflate the tail", {
  with_fake_home({
    # odm_report medians are a fraction of a second in real histories; a few
    # seconds of runtime must not become a 100x multiplier on what follows.
    DroneBioR:::record_odm_stage_completion("run-1", 100L, "odm_report", 0.02)
    DroneBioR:::record_odm_stage_completion("run-1", 100L, "odm_postprocess", 30)
    rem <- DroneBioR:::estimate_remaining_seconds(
      active_stage           = "odm_report",
      pending_stages         = "odm_postprocess",
      active_elapsed_seconds = 5,
      image_count            = 100L
    )
    # Pending stays unscaled (30); only the active stage's own tail is added.
    expect_equal(rem, 5 + 30)
  })
})

test_that("the carried-over slowdown is capped", {
  with_fake_home({
    DroneBioR:::record_odm_stage_completion("run-1", 100L, "opensfm", 100)
    DroneBioR:::record_odm_stage_completion("run-1", 100L, "openmvs", 10)
    # 100x over the estimate, but the multiplier on pending is capped at 20.
    rem <- DroneBioR:::estimate_remaining_seconds(
      active_stage           = "opensfm",
      pending_stages         = "openmvs",
      active_elapsed_seconds = 10000,
      image_count            = 100L
    )
    expect_equal(rem, 10000 + 10 * 20)
  })
})

test_that("normalize_camera_type collapses camera and band labels", {
  n <- DroneBioR:::normalize_camera_type
  expect_equal(n("multispectral"), "multispectral")
  expect_equal(n("MS"), "multispectral")
  expect_equal(n("MS_NIR"), "multispectral")
  expect_equal(n("MS/oom-retry"), "multispectral")
  expect_equal(n("rgb"), "rgb")
  expect_equal(n("RGB"), "rgb")
  expect_equal(n("RGB/retry"), "rgb")
  # Anything that does not name a sensor means "do not filter".
  expect_true(is.na(n(NULL)))
  expect_true(is.na(n("")))
  expect_true(is.na(n(NA)))
  expect_true(is.na(n("oom-retry")))
  expect_true(is.na(n("retry")))
})

test_that("estimates prefer history recorded for the same camera", {
  with_fake_home({
    # An RGB-heavy history plus one multispectral run, same image count.
    DroneBioR:::record_odm_stage_completion("rgb-1", 100L, "opensfm", 100, camera = "rgb")
    DroneBioR:::record_odm_stage_completion("rgb-2", 100L, "opensfm", 120, camera = "rgb")
    DroneBioR:::record_odm_stage_completion("ms-1", 100L, "opensfm", 3000, camera = "multispectral")

    expect_equal(
      DroneBioR:::estimate_odm_stage_seconds("opensfm", 100L, camera = "multispectral"),
      3000
    )
    expect_equal(
      DroneBioR:::estimate_odm_stage_seconds("opensfm", 100L, camera = "rgb"),
      110
    )
    # Without a camera the pooled median is used, as before.
    expect_equal(DroneBioR:::estimate_odm_stage_seconds("opensfm", 100L), 120)
  })
})

test_that("unlabelled rows are the fallback, never a different sensor's rows", {
  with_fake_home({
    DroneBioR:::record_odm_stage_completion("rgb-1", 100L, "opensfm", 200, camera = "rgb")
    # Only RGB rows exist. A multispectral query must not borrow them -- that
    # is the mixing this split exists to prevent -- so it drops to the baseline.
    expect_equal(
      DroneBioR:::estimate_odm_stage_seconds("opensfm", 100L, camera = "multispectral"),
      unname(DroneBioR:::odm_stage_baseline_seconds()["opensfm"])
    )

    # Add an unlabelled row: now that is the fallback, in preference to RGB.
    DroneBioR:::record_odm_stage_completion("old-1", 100L, "opensfm", 3000)
    expect_equal(
      DroneBioR:::estimate_odm_stage_seconds("opensfm", 100L, camera = "multispectral"),
      3000
    )
    # And the RGB estimate stays on its own rows, unpolluted by that addition.
    expect_equal(
      DroneBioR:::estimate_odm_stage_seconds("opensfm", 100L, camera = "rgb"),
      200
    )
  })
})

test_that("history written before camera tracking stays usable", {
  with_fake_home({
    # Simulate a legacy CSV: the columns the old code wrote, no `camera`.
    legacy <- data.frame(
      run_started_at   = "old-run",
      image_count      = 100L,
      stage            = "opensfm",
      duration_seconds = 900,
      stringsAsFactors = FALSE
    )
    utils::write.csv(legacy, DroneBioR:::odm_history_path(), row.names = FALSE)

    hist <- DroneBioR:::read_odm_stage_history()
    expect_true("camera" %in% names(hist))
    expect_true(is.na(hist$camera[1]))
    expect_equal(DroneBioR:::estimate_odm_stage_seconds("opensfm", 100L), 900)
    expect_equal(
      DroneBioR:::estimate_odm_stage_seconds("opensfm", 100L, camera = "multispectral"),
      900
    )
    # A new row appends cleanly alongside the legacy ones.
    expect_true(DroneBioR:::record_odm_stage_completion("new-run", 100L, "opensfm",
                                                       120, camera = "rgb"))
    expect_equal(nrow(DroneBioR:::read_odm_stage_history()), 2)
  })
})

test_that("estimate_remaining_seconds threads the camera through", {
  with_fake_home({
    DroneBioR:::record_odm_stage_completion("rgb-1", 100L, "opensfm", 100, camera = "rgb")
    DroneBioR:::record_odm_stage_completion("rgb-1", 100L, "openmvs", 50, camera = "rgb")
    DroneBioR:::record_odm_stage_completion("ms-1", 100L, "opensfm", 1000, camera = "multispectral")
    DroneBioR:::record_odm_stage_completion("ms-1", 100L, "openmvs", 500, camera = "multispectral")

    rem_ms <- DroneBioR:::estimate_remaining_seconds(
      active_stage = "opensfm", pending_stages = "openmvs",
      active_elapsed_seconds = 0, image_count = 100L, camera = "multispectral"
    )
    rem_rgb <- DroneBioR:::estimate_remaining_seconds(
      active_stage = "opensfm", pending_stages = "openmvs",
      active_elapsed_seconds = 0, image_count = 100L, camera = "rgb"
    )
    expect_equal(rem_ms, 1500)
    expect_equal(rem_rgb, 150)
  })
})

test_that("detect_camera_from_folder reads file extensions", {
  tmp <- tempfile("cam_detect_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  # Empty folder
  expect_true(is.na(DroneBioR:::detect_camera_from_folder(tmp)))

  # JPGs only -> rgb
  file.create(file.path(tmp, c("a.JPG", "b.jpg", "c.jpeg")))
  expect_equal(DroneBioR:::detect_camera_from_folder(tmp), "rgb")

  # Add TIFs (more than JPGs) -> multispectral
  file.create(file.path(tmp, c("d.tif", "e.tif", "f.tif", "g.tif")))
  expect_equal(DroneBioR:::detect_camera_from_folder(tmp), "multispectral")

  # Non-existent dir
  expect_true(is.na(DroneBioR:::detect_camera_from_folder(file.path(tmp, "nope"))))
})

test_that("active-run record round-trips via JSON", {
  skip_if_not_installed("jsonlite")
  with_fake_home({
    expect_null(DroneBioR:::read_active_run_record())

    DroneBioR:::write_active_run_record(
      run_id      = "run-xyz",
      log_path    = tempfile(fileext = ".log"),
      project_dir = tempdir(),
      image_count = 250L
    )
    # File doesn't exist yet, so read returns NULL (log_path check)
    expect_null(DroneBioR:::read_active_run_record())

    # Write a real log file and try again
    lp <- file.path(Sys.getenv("HOME"), "fake_run.log")
    writeLines("hello", lp)
    DroneBioR:::write_active_run_record(
      run_id      = "run-real",
      log_path    = lp,
      project_dir = tempdir(),
      image_count = 444L
    )
    rec <- DroneBioR:::read_active_run_record()
    expect_equal(rec$run_id, "run-real")
    expect_equal(rec$log_path, lp)
    expect_equal(rec$image_count, 444L)

    DroneBioR:::clear_active_run_record()
    expect_null(DroneBioR:::read_active_run_record())
  })
})

test_that("active-run record honours max_age_hours gating", {
  skip_if_not_installed("jsonlite")
  with_fake_home({
    lp <- file.path(Sys.getenv("HOME"), "log.log")
    writeLines("x", lp)
    # Write a record dated 100h ago
    old_time <- Sys.time() - as.difftime(100, units = "hours")
    DroneBioR:::write_active_run_record(
      run_id = "old", log_path = lp, project_dir = tempdir(),
      image_count = 10L, started_at = old_time
    )
    expect_null(DroneBioR:::read_active_run_record(max_age_hours = 48))
    expect_false(is.null(DroneBioR:::read_active_run_record(max_age_hours = 200)))
  })
})

test_that("normalize_camera_type identifies the sensor, not just the class", {
  n <- DroneBioR:::normalize_camera_type
  expect_equal(n("dji_mavic_3m"), "dji_mavic_3m")
  expect_equal(n("DJI Mavic 3M"), "dji_mavic_3m")
  expect_equal(n("micasense"), "micasense")
  expect_equal(n("RedEdge-MX"), "micasense")
  expect_equal(n("Parrot Sequoia"), "sequoia")
  # The coarse labels still parse, for callers that genuinely do not know.
  expect_equal(n("multispectral"), "multispectral")
  expect_equal(n("MS_NIR"), "multispectral")
  expect_equal(n("rgb"), "rgb")
  expect_true(is.na(n("oom-retry")))
})

test_that("estimates never borrow across sensor models", {
  # Measured on this project: a 210-image MicaSense opensfm took ~70 s while a
  # 39-image DJI Mavic 3M one took 39 min. Sharing history between them made a
  # MicaSense run estimate 3.5 hours -- out by about 200x.
  with_fake_home({
    DroneBioR:::record_odm_stage_completion("dji-1", 39L, "opensfm", 2338,
                                            camera = "dji_mavic_3m")
    # No MicaSense rows yet: must not fall back to the DJI ones.
    est_mica <- DroneBioR:::estimate_odm_stage_seconds("opensfm", 210L,
                                                       camera = "micasense")
    baseline <- unname(DroneBioR:::odm_stage_baseline_seconds()["opensfm"])
    expect_equal(est_mica, baseline)

    DroneBioR:::record_odm_stage_completion("mica-1", 210L, "opensfm", 91,
                                            camera = "micasense")
    expect_equal(DroneBioR:::estimate_odm_stage_seconds("opensfm", 210L,
                                                        camera = "micasense"), 91)
    # And the DJI estimate is untouched by the MicaSense row.
    expect_equal(DroneBioR:::estimate_odm_stage_seconds("opensfm", 39L,
                                                        camera = "dji_mavic_3m"), 2338)
  })
})

test_that("a coarse multispectral row is treated as unknown, not as a sensor", {
  # It names a class, not a camera: a DJI run hiding under that label is
  # exactly what produced the 200x overestimate.
  with_fake_home({
    DroneBioR:::record_odm_stage_completion("old-1", 39L, "opensfm", 2338,
                                            camera = "multispectral")
    DroneBioR:::record_odm_stage_completion("old-2", 39L, "opensfm", 2338)
    # Both rows are ambiguous, so a micasense query sees them as the unlabelled
    # pool rather than as same-sensor evidence.
    expect_equal(
      DroneBioR:::estimate_odm_stage_seconds("opensfm", 39L, camera = "micasense"),
      2338
    )
    # Once a real micasense row exists it wins outright.
    DroneBioR:::record_odm_stage_completion("mica-1", 39L, "opensfm", 70,
                                            camera = "micasense")
    expect_equal(
      DroneBioR:::estimate_odm_stage_seconds("opensfm", 39L, camera = "micasense"),
      70
    )
  })
})
