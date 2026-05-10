test_that("ODM command includes project and core options", {
  args <- build_odm_args(
    dataset_dir = tempfile(),
    project_name = "micasense",
    orthophoto_resolution_cm = 5,
    fast_orthophoto = TRUE
  )

  expect_true("opendronemap/odm" %in% args)
  expect_true("--radiometric-calibration" %in% args)
  expect_true("--fast-orthophoto" %in% args)
  expect_equal(tail(args, 1), "micasense")
})
