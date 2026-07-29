test_that("points_in_roi flags points inside and outside a square", {
  roi <- data.frame(x = c(0, 5, 5, 0), y = c(0, 0, 5, 5))
  expect_equal(
    points_in_roi(c(1, 3, 6, -1), c(1, 4, 6, 0), roi),
    c(TRUE, TRUE, FALSE, FALSE)
  )
})

test_that("points_in_roi returns all FALSE for a degenerate polygon", {
  expect_equal(
    points_in_roi(c(1, 2), c(1, 2), data.frame(x = c(0, 1), y = c(0, 1))),
    c(FALSE, FALSE)
  )
})

test_that("build_roi_polygon returns hull and bbox variants", {
  set.seed(1)
  pts <- data.frame(x = runif(50, 0, 10), y = runif(50, 0, 10))
  bbox <- build_roi_polygon(pts, method = "bbox")
  hull <- build_roi_polygon(pts, method = "hull")
  expect_equal(nrow(bbox), 4)
  expect_gt(nrow(hull), 2)
})

test_that("build_chm_from_dsm_dtm produces non-negative CHM values", {
  dsm <- system.file("extdata", "dsm_subset.tif", package = "DroneBioR")
  dtm <- system.file("extdata", "dtm_subset.tif", package = "DroneBioR")
  chm <- build_chm_from_dsm_dtm(dsm, dtm)
  expect_equal(names(chm), "CHM_m")
  mm <- terra::minmax(chm)
  expect_true(all(mm[1, ] >= 0))
})

test_that("derive_tree_candidates clusters high points into ranked candidates", {
  set.seed(1)
  n <- 300
  pts <- data.frame(
    x = c(rnorm(n / 3, 5, 0.5), rnorm(n / 3, 15, 0.5), rnorm(n / 3, 25, 0.5)),
    y = c(rnorm(n / 3, 5, 0.5), rnorm(n / 3, 5, 0.5),  rnorm(n / 3, 15, 0.5)),
    z = c(rnorm(n / 3, 55, 0.2), rnorm(n / 3, 57, 0.2), rnorm(n / 3, 54, 0.2))
  )
  trees <- derive_tree_candidates(pts)
  expect_gte(nrow(trees), 1)
  expect_true(all(trees$height_m >= 1.5))
  expect_true(all(diff(trees$height_m) <= 0))
})

test_that("derive_tree_candidates returns an empty frame when no points are high", {
  pts <- data.frame(x = runif(50), y = runif(50), z = rep(0, 50))
  expect_equal(nrow(derive_tree_candidates(pts, min_height = 5)), 0)
})

test_that("export_point_selection writes CSV outputs to the chosen folder", {
  set.seed(1)
  pts <- data.frame(x = runif(50, 0, 10), y = runif(50, 0, 10), z = runif(50, 50, 55))
  pts <- add_point_heights(pts)
  m <- compute_selection_metrics(pts)
  p <- compute_vertical_profile(pts)
  out <- tempfile("sel-")
  paths <- export_point_selection(pts, m, p, output_dir = out, label = "plot 1")
  expect_true(all(file.exists(paths)))
  expect_true(all(c("points", "metrics", "vertical_profile") %in% names(paths)))
  # Label is sanitized
  expect_true(grepl("plot_1", paths[["points"]]))
})

# Build a binary little-endian PLY with an arbitrary property layout, so the
# tests exercise the parser rather than one hard-coded shape.
.mk_ply <- function(path, n = 20L, with_normals = TRUE, colour_order = c("red", "blue", "green")) {
  props <- c("property float x", "property float y", "property float z")
  if (with_normals) props <- c(props, "property float nx", "property float ny",
                               "property float nz")
  props <- c(props, paste0("property uchar ", colour_order), "property uchar views")
  hdr <- paste0("ply\nformat binary_little_endian 1.0\nelement vertex ", n, "\n",
                paste(props, collapse = "\n"), "\nend_header\n")
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw(hdr), con)
  set.seed(7)
  vals <- list(x = seq_len(n) * 1.5, y = seq_len(n) * 2.5, z = seq_len(n) * 0.5,
               nx = rep(0, n), ny = rep(0, n), nz = rep(1, n))
  cols <- list(red = seq_len(n) %% 250L, blue = (seq_len(n) * 3L) %% 250L,
               green = (seq_len(n) * 7L) %% 250L, views = rep(4L, n))
  for (i in seq_len(n)) {
    writeBin(c(vals$x[i], vals$y[i], vals$z[i]), con, size = 4L, endian = "little")
    if (with_normals) writeBin(c(0, 0, 1), con, size = 4L, endian = "little")
    for (nm in c(colour_order, "views")) {
      writeBin(as.integer(cols[[nm]][i]), con, size = 1L, endian = "little")
    }
  }
  invisible(list(x = vals$x, y = vals$y, z = vals$z, cols = cols))
}

test_that("parse_ply_header computes the stride from the declared properties", {
  p <- tempfile(fileext = ".ply"); .mk_ply(p, n = 5L)
  h <- parse_ply_header(p)
  expect_equal(h$n_vertices, 5L)
  # 3 floats xyz + 3 floats normals + 4 uchars = 28, not the 16 a fixed-stride
  # reader assumes. Getting this wrong silently misaligns every vertex.
  expect_equal(h$stride, 28L)
  expect_equal(h$props$name, c("x","y","z","nx","ny","nz","red","blue","green","views"))
  expect_equal(h$props$offset[h$props$name == "red"], 24L)

  p2 <- tempfile(fileext = ".ply"); .mk_ply(p2, n = 5L, with_normals = FALSE)
  expect_equal(parse_ply_header(p2)$stride, 16L)
})

test_that("read_ply_point_cloud decodes a 28-byte record correctly", {
  p <- tempfile(fileext = ".ply"); ref <- .mk_ply(p, n = 12L)
  pc <- read_ply_point_cloud(p, max_points = 100L)
  expect_equal(nrow(pc), 12L)
  expect_equal(pc$x, ref$x)
  expect_equal(pc$z, ref$z)
  expect_equal(pc$point_id, 1:12)
})

test_that("colours are matched by name, not by position", {
  # ODM declares red, blue, green in that order; reading the three uchars
  # positionally swaps blue and green.
  p <- tempfile(fileext = ".ply"); ref <- .mk_ply(p, n = 8L)
  pc <- read_ply_point_cloud(p, max_points = 100L)
  expect_equal(pc$red,   as.integer(ref$cols$red))
  expect_equal(pc$green, as.integer(ref$cols$green))
  expect_equal(pc$blue,  as.integer(ref$cols$blue))
})

test_that("write_ply_subset preserves layout and the kept vertices exactly", {
  p <- tempfile(fileext = ".ply"); .mk_ply(p, n = 30L)
  out <- tempfile(fileext = ".ply")
  keep <- c(2L, 5L, 9L, 30L)
  n <- write_ply_subset(p, out, keep = keep, backup = FALSE)
  expect_equal(n, 4L)

  h_in <- parse_ply_header(p); h_out <- parse_ply_header(out)
  expect_equal(h_out$n_vertices, 4L)
  expect_equal(h_out$stride, h_in$stride)
  expect_equal(h_out$props, h_in$props)   # normals survive for meshing

  full <- read_ply_point_cloud(p, max_points = 100L)
  sub  <- read_ply_point_cloud(out, max_points = 100L)
  expect_equal(sub$x, full$x[keep])
  expect_equal(sub$red, full$red[keep])
})

test_that("write_ply_subset accepts a logical mask and rejects bad input", {
  p <- tempfile(fileext = ".ply"); .mk_ply(p, n = 10L)
  out <- tempfile(fileext = ".ply")
  mask <- rep(c(TRUE, FALSE), 5)
  expect_equal(write_ply_subset(p, out, keep = mask, backup = FALSE), 5L)

  expect_error(write_ply_subset(p, out, keep = rep(TRUE, 3), backup = FALSE),
               "length 3")
  expect_error(write_ply_subset(p, out, keep = c(1L, 999L), backup = FALSE),
               "outside 1")
  expect_error(write_ply_subset(p, out, keep = integer(0), backup = FALSE),
               "selected no vertices")
})

test_that("editing in place keeps one recoverable original", {
  p <- tempfile(fileext = ".ply"); .mk_ply(p, n = 10L)
  before <- file.size(p)
  write_ply_subset(p, p, keep = 1:6)
  expect_true(file.exists(paste0(p, ".orig")))
  expect_equal(file.size(paste0(p, ".orig")), before)
  expect_equal(parse_ply_header(p)$n_vertices, 6L)

  # A second edit must not overwrite the pristine copy with the edited one.
  write_ply_subset(p, p, keep = 1:3)
  expect_equal(parse_ply_header(paste0(p, ".orig"))$n_vertices, 10L)
  expect_equal(parse_ply_header(p)$n_vertices, 3L)
})

test_that("a uint32 property is read without a readBin warning", {
  # readBin only allows signed = FALSE for 1- and 2-byte integers; passing it
  # for a uint32 warns on every single call.
  p <- tempfile(fileext = ".ply")
  hdr <- paste0("ply\nformat binary_little_endian 1.0\nelement vertex 4\n",
                "property float x\nproperty float y\nproperty float z\n",
                "property uint views\nend_header\n")
  con <- file(p, open = "wb"); writeBin(charToRaw(hdr), con)
  for (i in 1:4) {
    writeBin(c(i * 1, i * 2, i * 3), con, size = 4L, endian = "little")
    writeBin(as.integer(i), con, size = 4L, endian = "little")
  }
  close(con)
  expect_equal(parse_ply_header(p)$stride, 16L)
  expect_silent(pc <- read_ply_point_cloud(p, max_points = 10L))
  expect_equal(pc$x, c(1, 2, 3, 4))
  expect_equal(pc$views, 1:4)
})

test_that("byte offsets stay exact past the 32-bit integer limit", {
  # starts <- body_at + (point_index - 1) * stride overflows to NA once
  # (n - 1) * stride passes 2^31, i.e. ~76.7 M vertices for a 28-byte record.
  n <- 100e6; stride <- 28L
  expect_true(is.na(suppressWarnings((as.integer(n) - 1L) * stride)))
  expect_equal((as.numeric(n) - 1) * stride, 2799999972)
})

test_that("parse_ply_header names the file when the header is not found", {
  p <- tempfile(fileext = ".ply")
  writeBin(charToRaw(strrep("not a ply ", 100)), p)
  expect_error(parse_ply_header(p), "is this a PLY file")
})
