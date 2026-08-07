# A future's worker is a fresh R process: every variable the body reads must be
# named in `globals` AND exist in the launching scope. Missing the second half
# fails only when the button is pressed - "object 'x' not found", after the
# reconstruction banner has already gone up.

test_that("every future_promise global is defined before it is passed", {
  app <- system.file("shiny", "DroneBiomassStudio", "app.R",
                     package = "DroneBioR")
  skip_if(!nzchar(app) || !file.exists(app), "app.R not installed")
  src <- readLines(app, warn = FALSE)

  defs <- grep("dronebior_pkg_path\\s*<-", src)
  uses <- grep("dronebior_pkg_path\\s*=\\s*dronebior_pkg_path", src)
  expect_gt(length(uses), 0)

  for (u in uses) {
    prior <- defs[defs < u]
    expect_true(length(prior) > 0,
                info = sprintf("line %d passes dronebior_pkg_path with no prior assignment", u))
    # the assignment must be in the same handler, not several observers back
    expect_lt(u - max(prior), 200L)
  }
})

test_that("step 2 runs its reconstruction off the main thread", {
  # Held on the Shiny thread, the session cannot redraw, so the stop button the
  # run enables never appears until the run it would stop has finished.
  app <- system.file("shiny", "DroneBiomassStudio", "app.R",
                     package = "DroneBioR")
  skip_if(!nzchar(app) || !file.exists(app), "app.R not installed")
  src <- paste(readLines(app, warn = FALSE), collapse = "\n")
  expect_true(grepl(
    "(?s)observeEvent\\(input\\$run_stage0,.{0,6000}?future_promise",
    src, perl = TRUE))
})
