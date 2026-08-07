# A reconstruction that dies leaves nothing behind, and the user finds out an
# hour later. What they need at that moment is the cause and the setting that
# fixes it, not a pointer to a log.

make_project <- function() {
  root <- tempfile("diag_"); dir.create(root)
  p <- dronebio_project(project_dir = root)
  dir.create(p$odm_project_dir, recursive = TRUE)
  p
}

test_that("exit 137 is named as an out-of-memory kill, with the remedy", {
  p <- make_project()
  writeLines('{"error": {"code": 137, "message": "Child returned 137"}}',
             file.path(p$odm_project_dir, "log.json"))
  msg <- DroneBioR:::diagnose_odm_failure(p)
  expect_match(msg, "out of memory")
  expect_match(msg, "137")
  expect_match(msg, "Detail level")          # the setting that fixes it
  expect_match(msg, "Nothing was lost")      # and that re-running is safe
})

test_that("OpenMVS narrating its own OOM is enough, without log.json", {
  p <- make_project()
  writeLines(c("Estimated depth-maps 1 (0.33%)...Killed",
               "[WARNING] OpenMVS ran out of memory, we're going to turn on tiling"),
             file.path(p$odm_project_dir, "dronebior_odm.log"))
  expect_match(DroneBioR:::diagnose_odm_failure(p), "out of memory")
})

test_that("the unreadable-TIFF failure is distinguished from OOM", {
  p <- make_project()
  writeLines(c("error: unsupported TIFF image",
               "error: failed loading image header"),
             file.path(p$odm_project_dir, "dronebior_odm.log"))
  msg <- DroneBioR:::diagnose_odm_failure(p)
  expect_match(msg, "could not read the undistorted images")
  expect_false(grepl("out of memory", msg))
})

test_that("a non-zero exit with no known signature still names the code", {
  p <- make_project()
  writeLines('{"error": {"code": 42, "message": "boom"}}',
             file.path(p$odm_project_dir, "log.json"))
  expect_match(DroneBioR:::diagnose_odm_failure(p), "code 42")
})

test_that("a clean project diagnoses nothing rather than inventing a cause", {
  p <- make_project()
  expect_null(DroneBioR:::diagnose_odm_failure(p))
  writeLines('{"stages": []}', file.path(p$odm_project_dir, "log.json"))
  expect_null(DroneBioR:::diagnose_odm_failure(p))
  # and a missing project directory must not error
  q <- dronebio_project(project_dir = tempfile("absent_"))
  expect_null(DroneBioR:::diagnose_odm_failure(q))
})
