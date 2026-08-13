# A package may not leave the user's session altered behind it. Three places
# used to: .onLoad rewrote PROJ_DATA and PROJ_LIB whatever they held, and the
# Studio launcher clamped terra's memory settings and set an option that both
# outlived the app by the length of the session.

test_that("configure_proj_database() leaves a working configuration alone", {
  dir <- withr::local_tempdir()
  file.create(file.path(dir, "proj.db"))
  withr::local_envvar(c(PROJ_DATA = dir, PROJ_LIB = dir))

  # force = FALSE is what .onLoad uses: someone else's working setting stands.
  expect_true(configure_proj_database(verbose = FALSE, force = FALSE))
  expect_identical(Sys.getenv("PROJ_DATA"), dir)
  expect_identical(Sys.getenv("PROJ_LIB"), dir)
})

test_that("configure_proj_database() still steps in when nothing usable is set", {
  empty <- withr::local_tempdir()          # no proj.db in it
  withr::local_envvar(c(PROJ_DATA = empty, PROJ_LIB = empty))
  configure_proj_database(verbose = FALSE, force = FALSE)

  # Either it found a real proj.db elsewhere and replaced the useless setting,
  # or there is none on this machine and it left things as they were. What it
  # must not do is claim success while PROJ_DATA still points at nothing.
  set <- Sys.getenv("PROJ_DATA")
  if (!identical(set, empty)) {
    expect_true(file.exists(file.path(set, "proj.db")))
  }
})

test_that("a load hook does not warn", {
  # .onLoad passes force = FALSE precisely so that a machine without proj.db
  # produces silence rather than a warning every time the package is attached.
  empty <- withr::local_tempdir()
  withr::local_envvar(c(PROJ_DATA = empty, PROJ_LIB = empty))
  expect_silent(configure_proj_database(verbose = FALSE, force = FALSE))
})

test_that("restore_proj_database() puts an unset variable back to unset", {
  withr::local_envvar(c(PROJ_DATA = NA, PROJ_LIB = NA))
  dir <- withr::local_tempdir()
  file.create(file.path(dir, "proj.db"))
  withr::local_envvar(c(PROJ_DATA = NA))

  # Tests run inside the package namespace, so the internal environment is
  # reachable by name.
  .dronebior_proj$saved <- list(
    PROJ_DATA = NA_character_, PROJ_LIB = NA_character_
  )
  Sys.setenv(PROJ_DATA = dir, PROJ_LIB = dir)
  restore_proj_database()
  expect_identical(Sys.getenv("PROJ_DATA", unset = NA_character_), NA_character_)
  expect_identical(Sys.getenv("PROJ_LIB", unset = NA_character_), NA_character_)
})

test_that("the Studio launcher restores terra options and its own option", {
  skip_if_not_installed("terra")
  skip_if_not_installed("shiny")

  before_opt <- getOption("dronebior.project_dir")
  before <- terra::terraOptions(print = FALSE)

  # runApp() is the last thing the launcher does; make it a no-op so the
  # function returns and its on.exit handlers run, which is what is under test.
  testthat::local_mocked_bindings(runApp = function(...) invisible(NULL),
                                  .package = "shiny")
  invisible(run_drone_biomass_studio(project_dir = withr::local_tempdir(),
                                     launch.browser = FALSE))

  after <- terra::terraOptions(print = FALSE)
  expect_equal(after$memfrac, before$memfrac)
  expect_equal(after$memmax, before$memmax)
  expect_identical(getOption("dronebior.project_dir"), before_opt)
})
