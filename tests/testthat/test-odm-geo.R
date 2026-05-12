# Fixtures: write tiny GeoScan-style files to a tempdir and exercise
# the four exported helpers + the run_odm_project auto-detect path.

make_cameras_fixture <- function(path) {
  lines <- c(
    "# file\tWGS84_lat\tWGS84_lon\tWGS84_H\troll\tpitch\tyaw\ttime\tStd Dev n (m)\tStd Dev e (m)\tStd Dev u (m)\tStd Dev Hz (m)",
    "img_001.JPG\t59.84507804\t31.46683755\t225.157\t1.81\t3.67\t-179.65\t2019.08.06 09:26:06\t0.0131\t0.0095\t0.018\t0.0162",
    "img_002.JPG\t59.84484605\t31.46683651\t224.290\t3.70\t4.26\t179.75\t2019.08.06 09:26:07\t0.0131\t0.0095\t0.018\t0.0162",
    "img_003.JPG\t59.84461494\t31.46684118\t224.386\t1.57\t5.51\t179.32\t2019.08.06 09:26:08\t0.0131\t0.0095\t0.018\t0.0162"
  )
  writeLines(lines, path)
}

make_offset_fixture <- function(path,
                                X = 0.3682, Y = -0.1815, Z = -0.032) {
  writeLines(c(sprintf("X= %s", X),
               sprintf("Y= %s", Y),
               sprintf("Z= %s", Z)), path)
}

test_that("read_geoscan_cameras parses header + numeric columns", {
  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp))
  make_cameras_fixture(tmp)
  cams <- read_geoscan_cameras(tmp)
  expect_equal(nrow(cams), 3L)
  expect_setequal(c("file", "lat", "lon", "H", "roll", "pitch", "yaw",
                    "time", "std_n", "std_e", "std_u", "std_hz"),
                  names(cams))
  expect_equal(cams$file[1L], "img_001.JPG")
  expect_equal(cams$lat[1L], 59.84507804, tolerance = 1e-8)
  expect_equal(cams$lon[1L], 31.46683755, tolerance = 1e-8)
  expect_equal(cams$H[1L], 225.157, tolerance = 1e-3)
})

test_that("read_geoscan_cameras drops rows with missing coords", {
  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp))
  writeLines(c(
    "# file\tlat\tlon\tH",
    "good.JPG\t59.84\t31.46\t220.0",
    "bad.JPG\t\t\t\t"  # blanks
  ), tmp)
  cams <- read_geoscan_cameras(tmp)
  expect_equal(nrow(cams), 1L)
  expect_equal(cams$file, "good.JPG")
})

test_that("read_geoscan_cameras errors on missing path", {
  expect_error(read_geoscan_cameras(file.path(tempdir(), "no_such_file.txt")),
               "Cameras file not found")
})

test_that("read_geoscan_gnss_offset parses XYZ lines", {
  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp))
  make_offset_fixture(tmp)
  off <- read_geoscan_gnss_offset(tmp)
  expect_equal(off[["X"]], 0.3682)
  expect_equal(off[["Y"]], -0.1815)
  expect_equal(off[["Z"]], -0.032)
})

test_that("read_geoscan_gnss_offset returns zero offset when file missing", {
  off <- read_geoscan_gnss_offset(file.path(tempdir(), "no_offset.txt"))
  expect_equal(unname(off), c(0, 0, 0))
})

test_that("convert_geoscan_to_odm_geo writes a valid ODM geo.txt", {
  tmp_cams <- tempfile(fileext = ".txt")
  tmp_off  <- tempfile(fileext = ".txt")
  tmp_geo  <- tempfile(fileext = ".txt")
  on.exit(unlink(c(tmp_cams, tmp_off, tmp_geo)))
  make_cameras_fixture(tmp_cams)
  make_offset_fixture(tmp_off)

  out <- convert_geoscan_to_odm_geo(tmp_cams, tmp_geo, gnss_offset = tmp_off)
  expect_equal(normalizePath(out), normalizePath(tmp_geo))

  lines <- readLines(tmp_geo)
  expect_equal(lines[1L], "EPSG:4326")
  expect_equal(length(lines), 4L)  # header + 3 cameras

  fields <- strsplit(lines[2L], "\\s+")[[1L]]
  expect_equal(fields[1L], "img_001.JPG")
  # lon comes before lat in ODM geo.txt
  expect_equal(as.numeric(fields[2L]), 31.466837, tolerance = 1e-4)  # lon + tiny offset
  expect_equal(as.numeric(fields[3L]), 59.845076, tolerance = 1e-4)  # lat + tiny offset
  # H corrected by GNSS offset Z (-0.032)
  expect_equal(as.numeric(fields[4L]), 225.157 - 0.032, tolerance = 1e-3)
  # Has IMU + accuracy columns (9 fields total)
  expect_equal(length(fields), 9L)
})

test_that("convert_geoscan_to_odm_geo respects gnss_offset = NULL", {
  tmp_cams <- tempfile(fileext = ".txt")
  tmp_geo  <- tempfile(fileext = ".txt")
  on.exit(unlink(c(tmp_cams, tmp_geo)))
  make_cameras_fixture(tmp_cams)

  convert_geoscan_to_odm_geo(tmp_cams, tmp_geo, gnss_offset = NULL)
  lines <- readLines(tmp_geo)
  fields <- strsplit(lines[2L], "\\s+")[[1L]]
  # No offset -> lon/lat/H match input exactly
  expect_equal(as.numeric(fields[2L]), 31.46683755, tolerance = 1e-8)
  expect_equal(as.numeric(fields[3L]), 59.84507804, tolerance = 1e-8)
  expect_equal(as.numeric(fields[4L]), 225.157, tolerance = 1e-3)
})

test_that("detect_geoscan_metadata walks up the directory tree", {
  root <- tempfile("geoscan_")
  dir.create(file.path(root, "Images"), recursive = TRUE)
  dir.create(file.path(root, "Metadata"), recursive = TRUE)
  make_cameras_fixture(file.path(root, "Metadata", "Cameras_WGS84.txt"))
  make_offset_fixture(file.path(root, "Metadata", "GNSS_offset.txt"))
  on.exit(unlink(root, recursive = TRUE))

  meta <- detect_geoscan_metadata(file.path(root, "Images"))
  expect_false(is.null(meta))
  expect_true(file.exists(meta$cameras_path))
  expect_true(file.exists(meta$gnss_offset_path))
  expect_equal(basename(meta$metadata_dir), "Metadata")
})

test_that("detect_geoscan_metadata returns NULL when nothing matches", {
  tmp <- tempfile("plain_")
  dir.create(file.path(tmp, "Images"), recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE))
  expect_null(detect_geoscan_metadata(file.path(tmp, "Images")))
})

test_that("detect_geoscan_metadata returns NULL for non-existent dir", {
  expect_null(detect_geoscan_metadata(file.path(tempdir(), "no_such_path")))
  expect_null(detect_geoscan_metadata(""))
  expect_null(detect_geoscan_metadata(NA_character_))
})

test_that("run_odm_project auto-attaches --geo when GeoScan is detected", {
  # Build a synthetic dataset that mimics the GeoScan layout:
  #   <root>/Images/   - JPGs (we fake-create three)
  #   <root>/Metadata/ - Cameras_WGS84.txt + GNSS_offset.txt
  # Then point a dronebio_project at <root>/Images and call
  # run_odm_project(run = FALSE) to inspect the docker command.

  root <- tempfile("rgb_geoscan_")
  dir.create(file.path(root, "Images"), recursive = TRUE)
  dir.create(file.path(root, "Metadata"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE))
  for (i in 1:3) {
    file.create(file.path(root, "Images", sprintf("img_%03d.JPG", i)))
  }
  make_cameras_fixture(file.path(root, "Metadata", "Cameras_WGS84.txt"))
  make_offset_fixture(file.path(root, "Metadata", "GNSS_offset.txt"))

  proj_root <- tempfile("dronebio_proj_")
  dir.create(proj_root, recursive = TRUE)
  on.exit(unlink(proj_root, recursive = TRUE), add = TRUE)

  proj <- dronebio_project(project_dir = proj_root)
  proj$images_dir <- file.path(root, "Images")

  res <- run_odm_project(proj, run = FALSE, camera_type = "rgb")
  expect_true(grepl("--geo", res$command, fixed = TRUE))
  expect_true(grepl("--matcher-neighbors\\b", res$command))
  expect_true(file.exists(file.path(proj$odm_project_dir, "geo.txt")))
})

test_that("run_odm_project skips auto-geoscan when disabled or multispectral", {
  root <- tempfile("rgb_geoscan_")
  dir.create(file.path(root, "Images"), recursive = TRUE)
  dir.create(file.path(root, "Metadata"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE))
  for (i in 1:3) {
    file.create(file.path(root, "Images", sprintf("img_%03d.JPG", i)))
  }
  make_cameras_fixture(file.path(root, "Metadata", "Cameras_WGS84.txt"))

  proj_root <- tempfile("dronebio_proj_")
  dir.create(proj_root, recursive = TRUE)
  on.exit(unlink(proj_root, recursive = TRUE), add = TRUE)
  proj <- dronebio_project(project_dir = proj_root)
  proj$images_dir <- file.path(root, "Images")

  # Explicit opt-out
  res <- run_odm_project(proj, run = FALSE, camera_type = "rgb",
                         auto_geoscan = FALSE)
  expect_false(grepl("--geo", res$command, fixed = TRUE))
  expect_false(file.exists(file.path(proj$odm_project_dir, "geo.txt")))
})
