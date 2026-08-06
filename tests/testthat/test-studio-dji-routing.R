# The Studio's two live Process buttons must send a DJI Mavic 3M flight to the
# multispectral pipeline. Getting this wrong is expensive and silent: the run
# completes, takes hours, and yields a three-band orthomosaic from a camera
# that carries four spectral bands.

test_that("resolve_images_dir finds photos one level down, and refuses when it cannot", {
  root <- tempfile("resolve_"); dir.create(root)

  # nothing anywhere
  expect_true(is.na(DroneBioR:::resolve_images_dir(root)$dir))

  # exactly one image-bearing subfolder: unambiguous, descend
  flight <- file.path(root, "flight1"); dir.create(flight)
  file.create(file.path(flight, sprintf("DJI_2026_%04d_D.JPG", 1:3)))
  r <- DroneBioR:::resolve_images_dir(root)
  expect_equal(normalizePath(r$dir), normalizePath(flight))
  expect_true(r$moved)
  expect_equal(r$n, 3L)

  # images directly in the folder win over any subfolder
  file.create(file.path(root, "DJI_2026_0009_D.JPG"))
  r <- DroneBioR:::resolve_images_dir(root)
  expect_equal(normalizePath(r$dir), normalizePath(root))
  expect_false(r$moved)

  # two candidates is genuinely ambiguous: report, do not guess
  root2 <- tempfile("resolve2_"); dir.create(root2)
  for (f in c("a", "b")) {
    d <- file.path(root2, f); dir.create(d)
    file.create(file.path(d, "DJI_2026_0001_D.JPG"))
  }
  r <- DroneBioR:::resolve_images_dir(root2)
  expect_true(is.na(r$dir))
  expect_setequal(r$candidates, c("a", "b"))

  # an ODM output tree is not a source folder
  root3 <- tempfile("resolve3_"); dir.create(root3)
  d <- file.path(root3, "outputs"); dir.create(d)
  file.create(file.path(d, "DJI_2026_0001_D.JPG"))
  expect_true(is.na(DroneBioR:::resolve_images_dir(root3)$dir))

  # degenerate inputs return the sentinel rather than erroring
  for (bad in list(NA_character_, "", character(0), NULL, 42,
                   file.path(root, "does-not-exist"))) {
    expect_true(is.na(DroneBioR:::resolve_images_dir(bad)$dir))
  }
})

test_that("a Mavic 3M folder is detected as multispectral, not RGB", {
  d <- tempfile("m3m_"); dir.create(d)
  # one shot: four MS band files plus the RGB sibling
  file.create(file.path(d, c("DJI_20260501112424_0001_MS_G.TIF",
                             "DJI_20260501112424_0001_MS_R.TIF",
                             "DJI_20260501112424_0001_MS_RE.TIF",
                             "DJI_20260501112424_0001_MS_NIR.TIF",
                             "DJI_20260501112424_0001_D.JPG")))
  expect_true(has_djim3m_images(d))
  expect_equal(DroneBioR:::detect_camera_from_folder(d), "multispectral")
  expect_match(DroneBioR:::detect_sensor_label(d), "Mavic 3M")

  # and the RGB frames outnumbering the MS ones must not flip the answer:
  # deciding by extension count is what made this fragile.
  file.create(file.path(d, sprintf("DJI_20260501112%03d_%04d_D.JPG", 500:520, 2:22)))
  expect_true(has_djim3m_images(d))
  expect_equal(DroneBioR:::detect_camera_from_folder(d), "multispectral")
})

test_that("odm_product_paths prefers the stacked 7-band ortho over the RGB one", {
  root <- tempfile("proj_"); dir.create(root)
  p <- dronebio_project(project_dir = root)
  od <- file.path(p$odm_project_dir, "odm_orthophoto")
  dir.create(od, recursive = TRUE)

  rgb <- file.path(od, "odm_orthophoto.tif")
  file.create(rgb)
  expect_equal(normalizePath(unname(odm_product_paths(p)[["orthomosaic"]])),
               normalizePath(rgb))

  dji <- file.path(od, "odm_orthophoto_dji.tif")
  file.create(dji)
  expect_equal(normalizePath(unname(odm_product_paths(p)[["orthomosaic"]])),
               normalizePath(dji))
})
