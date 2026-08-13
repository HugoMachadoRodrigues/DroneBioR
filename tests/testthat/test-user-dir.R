# CRAN forbids a package from writing into the user's home filespace. Everything
# the package keeps between sessions - the flight registry, the metric cache,
# the ODM stage history, the active-run record - used to live in `~/.dronebior`,
# and a plain `R CMD check` really did create it: the check that produced
# version 0.6.0 left a flight_metrics_cache.rds in the maintainer's home.
#
# These tests pin the two properties that stop it happening again: state goes
# under tools::R_user_dir(), and merely asking where a file lives creates
# nothing.

test_that("persistent state lives under R_user_dir, not the home directory", {
  paths <- list(
    registry = default_flight_registry(),
    cache    = flight_metric_cache_path(),
    history  = odm_history_path(),
    runs     = active_run_record_path()
  )
  data_dir  <- tools::R_user_dir("DroneBioR", "data")
  cache_dir <- tools::R_user_dir("DroneBioR", "cache")

  for (nm in names(paths)) {
    expect_true(
      startsWith(paths[[nm]], data_dir) || startsWith(paths[[nm]], cache_dir),
      info = paste0(nm, " is at ", paths[[nm]])
    )
    # The old location, and the shape of any other home-directory dotfile.
    expect_false(grepl("[.]dronebior", paths[[nm]]), info = nm)
  }
})

test_that("asking where a file lives does not create a directory", {
  withr::local_envvar(c(
    R_USER_DATA_DIR  = file.path(tempdir(), "dronebior-data-probe"),
    R_USER_CACHE_DIR = file.path(tempdir(), "dronebior-cache-probe")
  ))
  data_dir  <- tools::R_user_dir("DroneBioR", "data")
  cache_dir <- tools::R_user_dir("DroneBioR", "cache")
  unlink(c(data_dir, cache_dir), recursive = TRUE)

  invisible(default_flight_registry())
  invisible(flight_metric_cache_path())
  invisible(odm_history_path())
  invisible(active_run_record_path())
  invisible(read_odm_stage_history())
  invisible(read_active_run_record())

  expect_false(dir.exists(data_dir))
  expect_false(dir.exists(cache_dir))
})

test_that("a flight metric computed end to end writes nothing outside R_user_dir", {
  skip_if_not_installed("terra")
  withr::local_envvar(c(
    R_USER_DATA_DIR  = file.path(tempdir(), "dronebior-data-write"),
    R_USER_CACHE_DIR = file.path(tempdir(), "dronebior-cache-write")
  ))
  home <- Sys.getenv("HOME", unset = NA_character_)
  skip_if(is.na(home))
  before <- list.files(home, all.files = TRUE)

  ras <- terra::rast(nrows = 4, ncols = 4, nlyrs = 1, vals = runif(16, 0.1, 0.9))
  names(ras) <- "NDVI"
  dir <- withr::local_tempdir()
  terra::writeRaster(ras, file.path(dir, "NDVI.tif"))

  # Whatever this returns, it must not have touched the home directory.
  invisible(tryCatch(flight_ndvi_mean(dronebio_project(dir)),
                     error = function(e) NA_real_))

  expect_identical(sort(list.files(home, all.files = TRUE)), sort(before))
})
