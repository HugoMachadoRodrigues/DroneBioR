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
