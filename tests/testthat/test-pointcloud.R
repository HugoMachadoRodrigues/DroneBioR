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

test_that("backup_path keeps a named, visible snapshot of the original", {
  p <- tempfile(fileext = ".ply"); .mk_ply(p, n = 10L)
  before <- file.size(p)
  snap <- sub("\\.ply$", ".original.ply", p)

  write_ply_subset(p, p, keep = 1:6, backup = TRUE, backup_path = snap)
  # The snapshot is the named file, not the .orig dotfile.
  expect_true(file.exists(snap))
  expect_false(file.exists(paste0(p, ".orig")))
  expect_equal(file.size(snap), before)
  expect_equal(parse_ply_header(snap)$n_vertices, 10L)
  expect_equal(parse_ply_header(p)$n_vertices, 6L)

  # A second edit must not refresh the snapshot: it captures the cloud as it
  # stood before the FIRST edit, so it stays restorable to the pristine state.
  write_ply_subset(p, p, keep = 1:3, backup = TRUE, backup_path = snap)
  expect_equal(parse_ply_header(snap)$n_vertices, 10L)
  expect_equal(parse_ply_header(p)$n_vertices, 3L)

  # And it must be byte-identical to the untouched original.
  orig2 <- tempfile(fileext = ".ply"); .mk_ply(orig2, n = 10L)
  expect_equal(readBin(snap, "raw", n = file.size(snap) + 1L),
               readBin(orig2, "raw", n = file.size(orig2) + 1L))
})

test_that("backup_path refuses to be the file being written", {
  p <- tempfile(fileext = ".ply"); .mk_ply(p, n = 8L)
  expect_error(
    write_ply_subset(p, p, keep = 1:4, backup = TRUE, backup_path = p),
    "cannot be the file"
  )
})

test_that("a cloud-sync destination is written correctly with no leftover .part", {
  # A path under Library/CloudStorage/<Provider> triggers the local-staging
  # branch: the streamed chunks go to the OS tempdir, then one copy lands the
  # result. Regression guard that this path produces the right cloud, keeps the
  # original, and does not litter the synced folder with a .part staging file.
  root <- tempfile("cloudroot_")
  dest_dir <- file.path(root, "Library", "CloudStorage",
                        "OneDrive-Test", "proj", "odm_filterpoints")
  dir.create(dest_dir, recursive = TRUE)
  skip_if(is.na(DroneBioR::is_cloud_sync_path(dest_dir)),
          "path did not register as cloud-sync on this platform")

  p <- file.path(dest_dir, "point_cloud.ply")
  ref <- .mk_ply(p, n = 40L)
  orig <- sub("\\.ply$", ".original.ply", p)
  keep <- c(3L, 8L, 15L, 40L)

  n <- write_ply_subset(p, p, keep = keep, backup = TRUE, backup_path = orig)
  expect_equal(n, 4L)
  expect_equal(parse_ply_header(p)$n_vertices, 4L)
  expect_true(file.exists(orig))
  expect_equal(parse_ply_header(orig)$n_vertices, 40L)

  # The kept vertices are the right ones, and no staging file was left behind.
  sub <- read_ply_point_cloud(p, max_points = 100L)
  expect_equal(sub$x, ref$x[keep])
  expect_length(list.files(dest_dir, pattern = "\\.part$"), 0L)
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

test_that("write_ply_subset refuses a truncated source instead of inventing points", {
  # A file shorter than its header promises: the strided gather would pad with
  # zero bytes and produce a cloud that reads fine and is mostly fabricated.
  p <- tempfile(fileext = ".ply")
  hdr <- paste0("ply\nformat binary_little_endian 1.0\nelement vertex 100\n",
                "property float x\nproperty float y\nproperty float z\nend_header\n")
  con <- file(p, open = "wb")
  writeBin(charToRaw(hdr), con)
  for (i in 1:10) writeBin(c(i, i, i), con, size = 4L, endian = "little")
  close(con)
  expect_error(write_ply_subset(p, tempfile(fileext = ".ply"), keep = 1:100,
                                backup = FALSE),
               "Truncated PLY")
})

# --- despiking -------------------------------------------------------------

# A rough flat-ish surface plus a handful of tall vertical needles, as a
# data frame. The needles are the reconstruction spikes we want gone.
.mk_spiky_cloud <- function(n_ground = 4000L, n_spikes = 60L, seed = 11L) {
  set.seed(seed)
  g <- data.frame(x = runif(n_ground, 0, 40), y = runif(n_ground, 0, 40))
  g$z <- 100 + 0.05 * g$x + rnorm(n_ground, 0, 0.15)   # gentle slope + roughness
  s <- data.frame(x = runif(n_spikes, 2, 38), y = runif(n_spikes, 2, 38))
  s$z <- 100 + runif(n_spikes, 5, 30)                  # 5-30 m needles
  list(cloud = rbind(g, s), n_ground = n_ground, n_spikes = n_spikes)
}

test_that("despike_point_cloud (surface) removes tall needles, keeps the surface", {
  skip_if_not_installed("terra")
  d <- .mk_spiky_cloud()
  keep <- despike_point_cloud(d$cloud, methods = "surface", height_cap = 2)
  is_spike <- seq_len(nrow(d$cloud)) > d$n_ground
  # Every planted needle is dropped ...
  expect_true(all(!keep[is_spike]))
  # ... and the ground surface is essentially all kept (a rough surface loses
  # only a sliver to noise).
  expect_gt(mean(keep[!is_spike]), 0.97)
})

test_that("despike_point_cloud (sor) removes sparse floaters", {
  skip_if(is.null(DroneBioR:::.knn_backend()), "no kNN backend (RANN/FNN)")
  set.seed(3)
  ground <- data.frame(x = runif(3000, 0, 20), y = runif(3000, 0, 20))
  ground$z <- rnorm(3000, 0, 0.05)
  floaters <- data.frame(x = runif(15, 0, 20), y = runif(15, 0, 20),
                         z = runif(15, 3, 8))          # isolated points aloft
  pc <- rbind(ground, floaters)
  keep <- despike_point_cloud(pc, methods = "sor", sor_mult = 2)
  is_float <- seq_len(nrow(pc)) > 3000
  expect_gt(mean(!keep[is_float]), 0.8)   # most floaters flagged
  expect_gt(mean(keep[!is_float]), 0.90)  # ground largely kept (SOR trims a
                                          # few % of a tight gaussian at mult 2)
})

test_that("despike_point_cloud keeps everything when the cloud has no spikes", {
  skip_if_not_installed("terra")
  set.seed(5)
  flat <- data.frame(x = runif(2000, 0, 20), y = runif(2000, 0, 20),
                     z = rnorm(2000, 50, 0.05))
  keep <- despike_point_cloud(flat, methods = "surface", height_cap = 2)
  expect_gt(mean(keep), 0.98)
})

test_that("despike_ply rewrites a cleaned cloud and preserves layout + original", {
  skip_if_not_installed("terra")
  d <- .mk_spiky_cloud(n_ground = 3000L, n_spikes = 40L)
  cl <- d$cloud
  # Write a binary PLY (x,y,z float + one uchar) carrying the spiky cloud.
  p <- tempfile(fileext = ".ply")
  hdr <- paste0("ply\nformat binary_little_endian 1.0\nelement vertex ",
                nrow(cl), "\n",
                "property float x\nproperty float y\nproperty float z\n",
                "property uchar intensity\nend_header\n")
  con <- file(p, "wb"); writeBin(charToRaw(hdr), con)
  fb <- writeBin(as.numeric(t(as.matrix(cl[, c("x", "y", "z")]))), raw(),
                 size = 4L, endian = "little")
  fmat <- matrix(fb, nrow = 12)                        # 12 bytes x n (3 floats)
  ub <- as.raw(rep(7L, nrow(cl)))
  writeBin(as.vector(rbind(fmat, matrix(ub, nrow = 1))), con)  # + 1 uchar = 13 B
  close(con)
  expect_equal(parse_ply_header(p)$stride, 13L)

  orig <- sub("[.]ply$", ".original.ply", p)
  res <- despike_ply(p, backup_path = orig, methods = "surface", height_cap = 2)
  expect_equal(res$n_before, nrow(cl))
  expect_equal(res$n_removed, d$n_spikes)              # exactly the needles
  expect_true(file.exists(orig))
  expect_equal(parse_ply_header(orig)$n_vertices, nrow(cl))
  # Result is a valid PLY with the same layout, re-readable.
  h <- parse_ply_header(p)
  expect_equal(h$n_vertices, nrow(cl) - d$n_spikes)
  expect_equal(h$stride, 13L)
})

test_that("despike_point_cloud accepts a matrix as well as a data frame", {
  # `[[` on a matrix returns one ELEMENT, not a column, so a matrix input used
  # to yield length-1 x/y/z and therefore a keep mask of the wrong length --
  # silently wrong rather than an error. The mask must match nrow(coords).
  set.seed(3)
  n <- 400
  df <- data.frame(x = runif(n, 0, 10), y = runif(n, 0, 10), z = rnorm(n, 0, 0.05))
  df$z[1:5] <- df$z[1:5] + 12          # obvious floaters

  keep_df  <- despike_point_cloud(df,            methods = "sor")
  keep_mat <- despike_point_cloud(as.matrix(df), methods = "sor")

  expect_length(keep_df,  n)
  expect_length(keep_mat, n)
  expect_identical(keep_df, keep_mat)   # same answer either way
  expect_true(sum(!keep_mat) > 0)       # and it actually flagged the floaters
})

test_that("despike_point_cloud still rejects input without x/y/z", {
  expect_error(despike_point_cloud(matrix(1:9, ncol = 3)), "needs an x column")
})
