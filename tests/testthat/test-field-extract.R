# Tests for the windowed extraction core: window_cells ->
# extract_window_values / synthesize_pixel_raster ->
# covariate_frame_from_pixels -> extract_field_covariates.
#
# The load-bearing claims are the two identities: indices and biomass
# proxies computed through the one-row synthetic raster must equal the
# values obtained by computing them over the whole raster and extracting.

ortho_fixture <- function() {
  system.file("extdata", "micasense_subset.tif", package = "DroneBioR")
}

test_reflectance <- function() {
  scale_to_reflectance(read_multispectral_orthomosaic(ortho_fixture())$bands)
}

# Two interior points plus the top-left corner cell centre.
test_points <- function(r) {
  res <- terra::res(r)
  e <- terra::ext(r)
  data.frame(
    x = c(392004, 392012, e[1] + res[1] / 2),
    y = c(3033007, 3033012, e[4] - res[2] / 2)
  )
}

test_that("window_cells returns exactly window^2 cells for every allowed size", {
  r <- terra::rast(ortho_fixture())
  pts <- test_points(r)
  for (w in seq(1L, 21L, by = 2L)) {
    wc <- window_cells(r, pts, window = w)
    expect_equal(ncol(wc$cells), w^2,
                 info = sprintf("window = %d", w))
    expect_equal(nrow(wc$cells), nrow(pts))
  }
})

test_that("window_cells rejects even and out-of-range windows by name", {
  r <- terra::rast(ortho_fixture())
  pts <- test_points(r)
  expect_error(window_cells(r, pts, window = 2), "1, 3, 5")
  expect_error(window_cells(r, pts, window = 23), "1, 3, 5")
  expect_error(window_cells(r, pts, window = 4.5), "1, 3, 5")
})

test_that("out-of-extent points are dropped before adjacent() and never absorb cell 1", {
  # Raw bands, not reflectance: the masked reflectance stack has NA at cell 1,
  # which would make the "not cell 1" assertion vacuously true.
  bands <- terra::rast(ortho_fixture())[[1:3]]
  # terra::adjacent() on an NA cell number returns a row of NaN whose LAST
  # element is 1; an unguarded implementation would silently give this point
  # the value of the raster's first cell.
  pts <- data.frame(x = c(392004, 1e6), y = c(3033007, 1e6))
  wc <- window_cells(bands, pts, window = 3)
  expect_equal(wc$in_extent, c(TRUE, FALSE))
  expect_equal(nrow(wc$cells), 1L)
  expect_false(any(wc$cells == 1, na.rm = TRUE))

  sel <- names(bands)
  out <- extract_field_covariates(pts, bands, sel, window = 3)
  expect_equal(nrow(out), 2L)
  expect_false(out$.in_extent[2])
  expect_true(all(is.na(out[2, sel])))

  cell1 <- terra::extract(bands, 1L)
  expect_true(all(is.finite(as.numeric(cell1))))
  for (nm in sel) {
    expect_false(isTRUE(all.equal(out[[nm]][2], cell1[[nm]])))
  }
})

test_that("a 5x5 mean equals the brute-force mean over the same cells", {
  r <- terra::rast(ortho_fixture())[[1]]
  pt <- data.frame(x = 392008, y = 3033008)
  wc <- window_cells(r, pt, window = 5)
  got <- extract_window_values(r, wc$cells, fun = "mean")

  centre <- terra::cellFromXY(r, cbind(pt$x, pt$y))
  rc <- terra::rowColFromCell(r, centre)
  brute_cells <- terra::cellFromRowColCombine(r, rc[1] + (-2:2), rc[2] + (-2:2))
  brute <- mean(terra::extract(r, brute_cells)[[1]], na.rm = TRUE)

  expect_equal(got[[names(r)]][1], brute, tolerance = 1e-12)
  expect_equal(got$.n_valid_px[1], 25L)
})

test_that("a corner point aggregates over its 9 in-raster pixels only", {
  bands <- terra::rast(ortho_fixture())[[1:3]]
  e <- terra::ext(bands)
  res <- terra::res(bands)
  corner <- data.frame(x = e[1] + res[1] / 2, y = e[4] - res[2] / 2)

  out <- extract_field_covariates(corner, bands, names(bands)[1], window = 5)
  expect_equal(out$.window_px, 25L)
  expect_equal(out$.n_valid_px, 9L)
  expect_equal(out$.window_valid_frac, 9 / 25)

  wc <- window_cells(bands, corner, window = 5)
  in_cells <- wc$cells[1, ]
  in_cells <- in_cells[is.finite(in_cells)]
  expect_length(in_cells, 9L)
  # The mean must come from those 9 cells, not from a 25-cell window padded
  # with whatever adjacent() returned for the off-raster neighbours.
  expect_equal(out[[names(bands)[1]]],
               mean(terra::extract(bands[[1]], in_cells)[[1]], na.rm = TRUE),
               tolerance = 1e-12)
})

test_that("max >= mean >= min elementwise and one column per layer", {
  r <- terra::rast(ortho_fixture())[[1:3]]
  pts <- test_points(r)
  wc <- window_cells(r, pts, window = 3)
  mx <- extract_window_values(r, wc$cells, fun = "max")
  mn <- extract_window_values(r, wc$cells, fun = "mean")
  lo <- extract_window_values(r, wc$cells, fun = "min")

  expect_equal(setdiff(names(mx), ".n_valid_px"), names(r))
  for (nm in names(r)) {
    expect_true(all(mx[[nm]] >= mn[[nm]] - 1e-9, na.rm = TRUE))
    expect_true(all(mn[[nm]] >= lo[[nm]] - 1e-9, na.rm = TRUE))
  }
})

test_that("synthesize_pixel_raster preserves row order as cell order", {
  vals <- data.frame(A = c(3, 1, 2, 9), B = c(30, 10, 20, 90))
  r <- synthesize_pixel_raster(vals)
  expect_equal(dim(r), c(1L, 4L, 2L))
  expect_equal(names(r), c("A", "B"))
  expect_equal(unname(terra::values(r)), unname(as.matrix(vals)))
  expect_equal(terra::extract(r, seq_len(4L))$A, vals$A)
})

test_that("the synthetic route reproduces every globally computed index exactly", {
  refl <- test_reflectance()
  cells <- c(1L, 50L, 300L, 585L, 1024L)
  band_values <- terra::extract(refl, cells)

  synthetic <- compute_spectral_indices(
    synthesize_pixel_raster(band_values, terra::crs(refl))
  )
  global <- compute_spectral_indices(refl)

  expect_equal(names(synthetic), names(global))
  a <- as.matrix(as.data.frame(terra::values(synthetic)))
  b <- as.matrix(terra::extract(global, cells)[, names(global), drop = FALSE])
  expect_equal(max(abs(a - b), na.rm = TRUE), 0, tolerance = 1e-12)
  expect_identical(is.na(a), is.na(b))
})

test_that("biomass proxies take the no-resample branch on the synthetic grid", {
  refl <- test_reflectance()
  # CHM on the reflectance grid so compute_biomass_proxies() has something
  # real to multiply; the synthetic route must reproduce it to 1e-12.
  chm <- refl[[1]]
  terra::values(chm) <- seq_len(terra::ncell(chm)) / terra::ncell(chm) * 1.5
  names(chm) <- "CHM_m"

  global <- compute_biomass_proxies(compute_spectral_indices(refl), chm)
  cells <- c(1L, 77L, 400L, 900L)
  band_values <- terra::extract(refl, cells)
  chm_values <- terra::extract(chm, cells)[[1]]

  got <- covariate_frame_from_pixels(band_values, names(global),
                                     chm_values = chm_values,
                                     crs = terra::crs(refl))
  want <- terra::extract(global, cells)[, names(global), drop = FALSE]
  expect_equal(max(abs(as.matrix(got) - as.matrix(want)), na.rm = TRUE), 0,
               tolerance = 1e-12)
  expect_true("Biomass_NDVI_x_CHM" %in% names(got))
})

test_that("covariate_frame_from_pixels names an id it cannot supply", {
  bands <- data.frame(Green = c(0.2, 0.3), Red = c(0.1, 0.15),
                      RedEdge = c(0.3, 0.35), NIR = c(0.6, 0.7))
  expect_error(
    covariate_frame_from_pixels(bands, c("NDVI", "CHM_m")),
    "CHM_m"
  )
  ok <- covariate_frame_from_pixels(bands, c("NDRE", "NDVI", "Red"))
  expect_named(ok, c("NDRE", "NDVI", "Red"))
})

test_that("extraction preserves row count, order and sample ids", {
  refl <- test_reflectance()
  pts <- data.frame(
    sample_id = c("a", "OUT", "c"),
    biomass_kgha = c(1000, 2000, 3000),
    x = c(392004, 1e6, 392012),
    y = c(3033007, 1e6, 3033012),
    stringsAsFactors = FALSE
  )
  sel <- c("NDRE", "NDVI", "Green")
  out <- extract_field_covariates(pts, refl, sel, window = 3)

  expect_equal(nrow(out), nrow(pts))
  expect_identical(out$sample_id, pts$sample_id)
  expect_identical(out$biomass_kgha, pts$biomass_kgha)
  # Covariates come back in the requested order, right after the attributes.
  cov_pos <- match(sel, names(out))
  expect_equal(cov_pos, sort(cov_pos))
  expect_identical(names(out)[cov_pos], sel)
  expect_true(all(c(".n_valid_px", ".window_px", ".window_valid_frac",
                    ".in_extent") %in% names(out)))
  expect_equal(attr(out, "window_px"), 3L)
  expect_equal(attr(out, "window_fun"), "mean")
})

test_that("window = 1 extraction matches a plain single-cell extract", {
  refl <- test_reflectance()
  pts <- data.frame(x = c(392004, 392012), y = c(3033007, 3033012))
  out <- extract_field_covariates(pts, refl, c("NDVI", "Red"), window = 1)
  cells <- terra::cellFromXY(refl[[1]], as.matrix(pts))
  want <- covariate_frame_from_pixels(terra::extract(refl, cells),
                                      c("NDVI", "Red"), crs = terra::crs(refl))
  expect_equal(out$NDVI, want$NDVI, tolerance = 1e-12)
  expect_equal(out$Red, want$Red, tolerance = 1e-12)
})

test_that("a field-file column named like a covariate never shadows the extracted one", {
  # Two columns of the same name leave every downstream selector reading the
  # first -- the stale value from the file -- so the model would train on the
  # wrong numbers with no error. The extracted value must win.
  skip_if_not_installed("sf")
  r <- terra::rast(nrows = 20, ncols = 20, xmin = 0, xmax = 20,
                   ymin = 0, ymax = 20, crs = "EPSG:32636", nlyrs = 1)
  names(r) <- "Red"
  terra::values(r) <- 0.5
  pts <- sf::st_as_sf(
    data.frame(sample_id = c("a", "b"), biomass_kgha = c(1, 2),
               Red = c(-99, -98), x = c(5, 10), y = c(5, 10)),
    coords = c("x", "y"), crs = 32636, remove = FALSE
  )
  expect_warning(
    tab <- extract_field_covariates(pts, reflectance = r, selected = "Red", window = 3L),
    "share a name with an extracted covariate"
  )
  expect_equal(sum(names(tab) == "Red"), 1L)
  expect_equal(unname(tab$Red), c(0.5, 0.5))
})

# ---- metric windows --------------------------------------------------------

test_that("window_from_metres converts to the nearest odd pixel count", {
  r <- terra::rast(nrows = 100, ncols = 100, xmin = 0, xmax = 5.7587,
                   ymin = 0, ymax = 5.7587)
  expect_equal(window_from_metres(r, 0.52)$window, 9L)
  expect_equal(window_from_metres(r, 0.17)$window, 3L)
  expect_equal(window_from_metres(r, 0.06)$window, 1L)
  w <- window_from_metres(r, 0.52)
  expect_equal(w$window_m_actual, 9 * min(terra::res(r)))
  expect_equal(w$resolution, min(terra::res(r)))
})

test_that("window_from_metres refuses what it cannot deliver", {
  r <- terra::rast(nrows = 100, ncols = 100, xmin = 0, xmax = 5.7587,
                   ymin = 0, ymax = 5.7587)
  expect_error(window_from_metres(r, 5), "Supported windows")
  expect_error(window_from_metres(r, 0), "positive number")
  expect_error(window_from_metres(r, -1), "positive number")
  expect_error(window_from_metres(data.frame(a = 1), 0.5), "SpatRaster")
})

test_that("window_m and the equivalent pixel window give the same extraction", {
  set.seed(3)
  r <- terra::rast(nrows = 200, ncols = 200, xmin = 0, xmax = 11.5174,
                   ymin = 0, ymax = 11.5174, nlyr = 5)
  terra::values(r) <- runif(200 * 200 * 5)
  names(r) <- c("Blue", "Green", "Red", "RedEdge", "NIR")
  pts <- data.frame(x = runif(10, 1, 10), y = runif(10, 1, 10))
  a <- extract_field_covariates(pts, r, "NDVI", window = 9L)
  b <- extract_field_covariates(pts, r, "NDVI", window_m = 0.52)
  expect_equal(a$NDVI, b$NDVI)
  expect_equal(attr(a, "window_px"), attr(b, "window_px"))
  expect_equal(attr(b, "window_px"), 9L)
  expect_equal(attr(b, "window_m"), 9 * min(terra::res(r)))
})

test_that("an out-of-range window_m is reported, not silently truncated", {
  set.seed(4)
  r <- terra::rast(nrows = 50, ncols = 50, xmin = 0, xmax = 2.88,
                   ymin = 0, ymax = 2.88, nlyr = 5)
  terra::values(r) <- runif(50 * 50 * 5)
  names(r) <- c("Blue", "Green", "Red", "RedEdge", "NIR")
  pts <- data.frame(x = runif(5, 0.5, 2), y = runif(5, 0.5, 2))
  expect_error(extract_field_covariates(pts, r, "NDVI", window_m = 99),
               "Supported windows")
})
