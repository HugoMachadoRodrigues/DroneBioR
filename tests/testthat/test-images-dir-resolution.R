test_that("resolve_images_dir finds photos one level down, unambiguously", {
  root <- tempfile("resolve_"); dir.create(root)
  sub  <- file.path(root, "flight1"); dir.create(sub)
  file.create(file.path(sub, sprintf("IMG_%04d_1.tif", 1:4)))

  r <- DroneBioR:::resolve_images_dir(sub)
  expect_identical(normalizePath(r$dir), normalizePath(sub))
  expect_false(r$moved)
  expect_equal(r$n, 4L)

  r <- DroneBioR:::resolve_images_dir(root)
  expect_identical(normalizePath(r$dir), normalizePath(sub))
  expect_true(r$moved)
  expect_equal(r$n, 4L)
})

test_that("resolve_images_dir refuses rather than guessing between two folders", {
  root <- tempfile("resolve_"); dir.create(root)
  for (s in c("a", "b")) {
    d <- file.path(root, s); dir.create(d)
    file.create(file.path(d, "IMG_0001_1.tif"))
  }
  r <- DroneBioR:::resolve_images_dir(root)
  expect_true(is.na(r$dir))
  expect_setequal(r$candidates, c("a", "b"))
})

test_that("resolve_images_dir never descends into an ODM output tree", {
  root <- tempfile("resolve_"); dir.create(root)
  out  <- file.path(root, "outputs"); dir.create(out)
  file.create(file.path(out, sprintf("IMG_%04d_1.tif", 1:9)))
  r <- DroneBioR:::resolve_images_dir(root)
  expect_true(is.na(r$dir))
  expect_length(r$candidates, 0L)
})

test_that("resolve_images_dir returns a clean sentinel for unusable input", {
  for (bad in list(NULL, NA_character_, "", character(0),
                   file.path(tempdir(), "definitely-not-here"))) {
    r <- DroneBioR:::resolve_images_dir(bad)
    expect_true(is.na(r$dir))
    expect_equal(r$n, 0L)
    expect_false(r$moved)
  }
})

test_that("a folder of DJI colour frames alone is not called multispectral", {
  # has_djim3m_images() matches the _D.JPG frames too, and the Studio stages
  # exactly such a folder for ODM. Announcing it as carrying G/R/RE/NIR would
  # be a worse error than the one the DJI detection was added to fix.
  d <- tempfile("djionly_"); dir.create(d)
  file.create(file.path(d, sprintf("DJI_2026010112000%d_000%d_D.JPG", 1:3, 1:3)))

  expect_true(DroneBioR::has_djim3m_images(d))
  expect_length(DroneBioR:::djim3m_bands_present(d), 0L)
  expect_identical(DroneBioR:::detect_camera_from_folder(d), "rgb")
  expect_false(grepl("multispectral", DroneBioR:::detect_sensor_label(d)))
})

test_that("the sensor label names the bands on disk, not the model's spec", {
  d <- tempfile("djipart_"); dir.create(d)
  file.create(file.path(d, c("DJI_20260101120000_0001_MS_G.TIF",
                             "DJI_20260101120000_0001_MS_R.TIF")))
  expect_identical(DroneBioR:::djim3m_bands_present(d), c("G", "R"))
  lab <- DroneBioR:::detect_sensor_label(d)
  expect_match(lab, "\\(G, R\\)")
  expect_false(grepl("NIR", lab))
  expect_false(grepl("RGB camera", lab))
})
