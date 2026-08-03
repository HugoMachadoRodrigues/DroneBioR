test_that("default_flight_registry returns an absolute path with the expected name", {
  p <- default_flight_registry()
  expect_match(basename(p), "^flights\\.csv$")
})

test_that("list_flights returns an empty data frame for a missing registry", {
  flights <- list_flights(tempfile(fileext = ".csv"))
  expect_equal(nrow(flights), 0)
  expect_true(all(c("flight_id", "date", "project_dir", "notes") %in% names(flights)))
})

test_that("register_flight writes a row and is idempotent on repeated calls", {
  reg <- tempfile(fileext = ".csv")
  project <- dronebio_sample_project(target_dir = tempfile("flight-A-"))

  register_flight(date = "2026-05-01",
                  project_dir = project$project_dir,
                  notes = "Spring flight",
                  registry_path = reg)
  expect_equal(nrow(list_flights(reg)), 1)

  # Second call with the same date+project must NOT duplicate.
  register_flight(date = "2026-05-01",
                  project_dir = project$project_dir,
                  registry_path = reg)
  expect_equal(nrow(list_flights(reg)), 1)
})

test_that("register_flight records the ODM sub-project and list_flights backfills it", {
  reg <- tempfile(fileext = ".csv")
  project <- dronebio_sample_project(target_dir = tempfile("flight-sub-"))

  register_flight(date = "2026-05-01",
                  project_dir = project$project_dir,
                  registry_path = reg,
                  odm_dataset_subdir = "outputs/odm_dji_dataset",
                  odm_project_name = "dji")
  f <- list_flights(reg)
  expect_true(all(c("odm_dataset_subdir", "odm_project_name") %in% names(f)))
  expect_equal(f$odm_dataset_subdir[1], "outputs/odm_dji_dataset")
  expect_equal(f$odm_project_name[1], "dji")

  # A registry written WITHOUT the columns (pre-migration) is backfilled to NA
  # so downstream code sees a stable schema and register_flight can rbind onto it.
  legacy <- tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(flight_id = "x", date = "2026-04-01",
               project_dir = project$project_dir, notes = "",
               stringsAsFactors = FALSE),
    legacy, row.names = FALSE)
  lf <- list_flights(legacy)
  expect_true(all(c("odm_dataset_subdir", "odm_project_name") %in% names(lf)))
  expect_true(is.na(lf$odm_dataset_subdir[1]))
  expect_silent(register_flight(date = "2026-06-01", project_dir = tempdir(),
                                registry_path = legacy))
  expect_equal(nrow(list_flights(legacy)), 2)
})

test_that("register_flight refuses unparseable dates", {
  reg <- tempfile(fileext = ".csv")
  expect_error(
    register_flight("not-a-date", tempdir(), registry_path = reg),
    "parse date"
  )
})

test_that("register_flight accepts multiple flights at distinct dates", {
  reg <- tempfile(fileext = ".csv")
  p1 <- dronebio_sample_project(target_dir = tempfile("flight-B1-"))
  p2 <- dronebio_sample_project(target_dir = tempfile("flight-B2-"))

  register_flight("2026-04-01", p1$project_dir, registry_path = reg)
  register_flight("2026-05-01", p2$project_dir, registry_path = reg)
  flights <- list_flights(reg)
  expect_equal(nrow(flights), 2)
})

test_that("register_flight keeps distinct projects flown on the same date", {
  reg <- tempfile(fileext = ".csv")
  p1 <- dronebio_sample_project(target_dir = tempfile("flight-same-day-1-"))
  p2 <- dronebio_sample_project(target_dir = tempfile("flight-same-day-2-"))

  register_flight("2026-05-01", p1$project_dir, notes = "plot A", registry_path = reg)
  register_flight("2026-05-01", p2$project_dir, notes = "plot B", registry_path = reg)

  flights <- list_flights(reg)
  expect_equal(nrow(flights), 2)
  expect_equal(length(unique(flights$flight_id)), 2)
})

test_that("flight ids survive long, deeply nested project paths", {
  # The id embeds a hash of project_dir. Accumulating that hash in a 32-bit
  # integer overflowed to NA after ~9 characters, so every real-world path
  # produced the same "<date>-NA" id and same-date flights silently collided.
  reg <- tempfile(fileext = ".csv")
  base <- file.path(tempdir(), "OneDrive-UniversityofFlorida", "REC_Ona",
                    "Drone_Biomass", "season2026")
  a <- file.path(base, "plot_A")
  b <- file.path(base, "plot_B")
  dir.create(a, recursive = TRUE, showWarnings = FALSE)
  dir.create(b, recursive = TRUE, showWarnings = FALSE)

  expect_silent(register_flight("2026-05-01", a, registry_path = reg))
  register_flight("2026-05-01", b, registry_path = reg)

  flights <- list_flights(reg)
  expect_equal(nrow(flights), 2)
  expect_false(any(grepl("NA", flights$flight_id, fixed = TRUE)))
})

test_that("flight_time_series returns one numeric value per registered flight", {
  reg <- tempfile(fileext = ".csv")
  p1 <- dronebio_sample_project(target_dir = tempfile("flight-ts-1-"))
  p2 <- dronebio_sample_project(target_dir = tempfile("flight-ts-2-"))

  register_flight("2026-04-01", p1$project_dir, registry_path = reg)
  register_flight("2026-05-01", p2$project_dir, registry_path = reg)

  ts <- flight_time_series(flight_ndvi_mean, registry_path = reg)
  expect_equal(nrow(ts), 2)
  expect_true(all(c("date", "value", "flight_id") %in% names(ts)))
  expect_s3_class(ts$date, "Date")
  expect_true(all(is.finite(ts$value)))
})

test_that("flight_time_series surfaces summary_fn errors as NA, not crashes", {
  reg <- tempfile(fileext = ".csv")
  p1 <- dronebio_sample_project(target_dir = tempfile("flight-err-"))
  register_flight("2026-04-01", p1$project_dir, registry_path = reg)

  bad <- function(proj) stop("boom")
  ts <- flight_time_series(bad, registry_path = reg)
  expect_equal(nrow(ts), 1)
  expect_true(is.na(ts$value))
})

test_that("flight_chm_mean returns a positive value for the sample project", {
  project <- dronebio_sample_project(target_dir = tempfile("flight-chm-"))
  expect_gt(flight_chm_mean(project), 0)
})

test_that("flight_biomass_proxy_mean returns a finite value", {
  project <- dronebio_sample_project(target_dir = tempfile("flight-bp-"))
  expect_true(is.finite(flight_biomass_proxy_mean(project)))
})

test_that("flight_time_series returns an empty frame on an empty registry", {
  ts <- flight_time_series(flight_ndvi_mean, registry_path = tempfile(fileext = ".csv"))
  expect_equal(nrow(ts), 0)
})
