# A picture is not a measurement. The band map omits Blue for a DJI Mavic 3M
# by design - the only blue comes from the colour camera, and mixing it with
# calibrated bands would make an index wrong - so any display code that asks
# that map for c("Blue","Green","Red") dies with "[subset] invalid name(s)".

mk <- function(nms) {
  r <- terra::rast(nrows = 4, ncols = 4, nlyrs = length(nms))
  names(r) <- nms
  terra::values(r) <- seq_len(16 * length(nms))
  r
}

test_that("canonical names are used as-is, in blue-green-red order", {
  out <- DroneBioR:::display_rgb_bands(mk(c("Blue", "Green", "Red", "NIR", "RedEdge")))
  expect_equal(names(out), c("Blue", "Green", "Red"))
  expect_false(isTRUE(attr(out, "false_colour")))
})

test_that("a raw DJI stack yields true colour from the camera's own layers", {
  out <- DroneBioR:::display_rgb_bands(
    mk(c("Red", "Green", "Blue", "MS_G", "MS_R", "MS_RE", "MS_NIR")))
  expect_equal(names(out), c("Blue", "Green", "Red"))
  expect_false(isTRUE(attr(out, "false_colour")))
})

test_that("mapped DJI bands still yield a drawable image, flagged false colour", {
  # This is what base_reflectance() holds on a DJI flight: no blue at all.
  out <- DroneBioR:::display_rgb_bands(mk(c("Green", "Red", "RedEdge", "NIR")))
  expect_equal(terra::nlyr(out), 3L)
  expect_true(isTRUE(attr(out, "false_colour")))
})

test_that("case does not decide whether a preview renders", {
  out <- DroneBioR:::display_rgb_bands(mk(c("blue", "green", "red")))
  expect_equal(terra::nlyr(out), 3L)
  expect_false(isTRUE(attr(out, "false_colour")))
})

test_that("too few layers returns NULL rather than erroring", {
  expect_null(DroneBioR:::display_rgb_bands(mk(c("a", "b"))))
  expect_null(DroneBioR:::display_rgb_bands(NULL))
  expect_null(DroneBioR:::display_rgb_bands("not a raster"))
})
