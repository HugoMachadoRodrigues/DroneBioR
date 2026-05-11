test_that("as_webodm_options builds the right key/value list for multispectral", {
  opts <- as_webodm_options(camera_type = "multispectral", build_dsm = TRUE)
  expect_named(opts, c("orthophoto-resolution", "fast-orthophoto", "dsm",
                       "dtm", "pc-las", "pc-copc", "pc-csv", "tiles",
                       "3d-tiles", "gltf", "radiometric-calibration"),
               ignore.order = TRUE)
  expect_true(opts$dsm)
  expect_equal(opts[["radiometric-calibration"]], "camera+sun")
})

test_that("as_webodm_options omits radiometric-calibration for RGB", {
  opts <- as_webodm_options(camera_type = "rgb")
  expect_false("radiometric-calibration" %in% names(opts))
})

test_that("as_webodm_options merges extra options", {
  opts <- as_webodm_options(camera_type = "rgb",
                            extra = list(`auto-boundary` = TRUE,
                                         `feature-quality` = "high"))
  expect_true(opts$`auto-boundary`)
  expect_equal(opts$`feature-quality`, "high")
})

test_that("webodm helpers normalise the base URL (strip trailing slashes)", {
  expect_equal(DroneBioR:::.webodm_normalize_url("http://x:8000///"),
               "http://x:8000")
  expect_equal(DroneBioR:::.webodm_normalize_url("https://example.org"),
               "https://example.org")
  expect_error(DroneBioR:::.webodm_normalize_url(""), "non-empty character")
  expect_error(DroneBioR:::.webodm_normalize_url(NULL), "non-empty character")
})

test_that("webodm functions error informatively when httr is missing", {
  skip_if(requireNamespace("httr", quietly = TRUE))
  expect_error(webodm_authenticate("http://x", "u", "p"), "httr")
  expect_error(webodm_create_project("http://x", "t", "n"), "httr")
})

test_that("webodm_submit_task refuses missing inputs", {
  skip_if_not_installed("httr")
  expect_error(
    webodm_submit_task("http://x", "tok", 1L, image_paths = character()),
    "No image files"
  )
  fake <- tempfile()
  expect_error(
    webodm_submit_task("http://x", "tok", 1L, image_paths = c(fake, fake)),
    "not found"
  )
})

test_that("run_webodm_project rejects bad project input", {
  expect_error(
    run_webodm_project(project = 42, base_url = "http://x",
                       username = "u", password = "p"),
    "dronebio_project"
  )
})
