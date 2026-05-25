make_mock_micasense_dir <- function(n_captures = 3) {
  tmp <- tempfile("micasense-")
  dir.create(tmp)
  for (cap in sprintf("IMG_%04d", seq_len(n_captures)))
    for (band in 1:5)
      file.create(file.path(tmp, paste0(cap, "_", band, ".tif")))
  tmp
}

test_that("list_micasense_images parses capture and band ids", {
  tmp <- make_mock_micasense_dir(n_captures = 3)
  manifest <- list_micasense_images(tmp)
  expect_equal(nrow(manifest), 15)
  expect_true(all(manifest$band_id %in% 1:5))
  expect_equal(length(unique(manifest$capture_id)), 3)
  expect_true(all(c("file", "filename", "capture_id", "band_id", "file_size_mb")
                  %in% names(manifest)))
})

test_that("list_micasense_images errors on non-MicaSense filenames", {
  tmp <- tempfile("bad-"); dir.create(tmp)
  file.create(file.path(tmp, "weird.tif"))
  expect_error(list_micasense_images(tmp), "MicaSense pattern")
})

test_that("list_micasense_images errors when the directory is missing", {
  expect_error(list_micasense_images(tempfile("nonexistent-")), "directory not found")
})

test_that("list_micasense_images errors when the directory has no images", {
  tmp <- tempfile("empty-"); dir.create(tmp)
  expect_error(list_micasense_images(tmp), "No image files")
})

test_that("copy_images_for_odm copies every file in the manifest", {
  src <- make_mock_micasense_dir(n_captures = 2)
  manifest <- list_micasense_images(src)
  dest <- tempfile("odm-images-")
  copy_images_for_odm(manifest, dest)
  expect_equal(length(list.files(dest)), 10)
})

test_that("copy_images_for_odm is idempotent on a second invocation", {
  src <- make_mock_micasense_dir(n_captures = 2)
  manifest <- list_micasense_images(src)
  dest <- tempfile("odm-images-")
  copy_images_for_odm(manifest, dest)
  mtimes_first <- file.info(list.files(dest, full.names = TRUE))$mtime
  Sys.sleep(0.05)
  copy_images_for_odm(manifest, dest)
  mtimes_second <- file.info(list.files(dest, full.names = TRUE))$mtime
  # Same-size files should not have been re-copied
  expect_equal(mtimes_first, mtimes_second)
})

# --- DJI Mavic 3M ----------------------------------------------------------

make_mock_dji_mavic_3m_dir <- function(n_captures = 3, with_ms = TRUE) {
  tmp <- tempfile("djim3m-"); dir.create(tmp)
  for (i in seq_len(n_captures)) {
    stem <- sprintf("DJI_20260501132033_%04d", i)
    file.create(file.path(tmp, paste0(stem, "_D.JPG")))
    if (isTRUE(with_ms)) {
      for (band in c("G", "R", "RE", "NIR"))
        file.create(file.path(tmp, paste0(stem, "_MS_", band, ".TIF")))
    }
  }
  tmp
}

test_that("list_aerial_images drops DJI Mavic 3M MS TIFs when both are present", {
  tmp <- make_mock_dji_mavic_3m_dir(n_captures = 3, with_ms = TRUE)
  manifest <- list_aerial_images(tmp)
  expect_equal(nrow(manifest), 3)  # only the 3 D.JPGs survive
  expect_true(all(grepl("_D\\.JPG$", manifest$filename, ignore.case = TRUE)))
  expect_true(isTRUE(attr(manifest, "dji_visible_multispectral")))
})

test_that("list_aerial_images keeps MS TIFs when no DJI visible JPGs accompany them", {
  tmp <- tempfile("ms-only-"); dir.create(tmp)
  for (i in 1:2) {
    stem <- sprintf("DJI_20260501132033_%04d", i)
    for (band in c("G", "R", "RE", "NIR"))
      file.create(file.path(tmp, paste0(stem, "_MS_", band, ".TIF")))
  }
  manifest <- list_aerial_images(tmp)
  expect_equal(nrow(manifest), 8)  # 4 MS bands x 2 captures, kept
  expect_false(isTRUE(attr(manifest, "dji_visible_multispectral")))
})

test_that("list_dji_mavic_3m_images returns one manifest per camera band", {
  tmp <- make_mock_dji_mavic_3m_dir(n_captures = 3, with_ms = TRUE)
  out <- list_dji_mavic_3m_images(tmp)
  expect_setequal(names(out), c("D", "MS_G", "MS_R", "MS_RE", "MS_NIR"))
  for (band in names(out)) {
    expect_equal(nrow(out[[band]]), 3)
    expect_true(all(c("file", "filename", "capture_id", "band_id",
                      "file_size_mb") %in% names(out[[band]])))
  }
})

test_that("list_dji_mavic_3m_images omits bands that have no matching files", {
  tmp <- tempfile("djim3m-partial-"); dir.create(tmp)
  for (i in 1:2) {
    stem <- sprintf("DJI_20260501132033_%04d", i)
    file.create(file.path(tmp, paste0(stem, "_D.JPG")))
    file.create(file.path(tmp, paste0(stem, "_MS_G.TIF")))
    # MS_R, MS_RE, MS_NIR are missing on purpose
  }
  out <- list_dji_mavic_3m_images(tmp)
  expect_setequal(names(out), c("D", "MS_G"))
})

test_that("list_dji_mavic_3m_images errors when no DJI files match", {
  tmp <- tempfile("not-dji-"); dir.create(tmp)
  file.create(file.path(tmp, "IMG_0001.JPG"))
  expect_error(list_dji_mavic_3m_images(tmp),
               "No DJI Mavic 3M images found")
})

test_that("list_dji_mavic_3m_images errors when the directory is missing", {
  expect_error(list_dji_mavic_3m_images(tempfile("nope-")),
               "directory not found")
})
