# Real DJI Mavic 3M .MRK lines from the user's ifasbahia10 flight.
# Tab-separated, comma-glued numeric/label fields.
real_mrk_lines <- c(
  "1\t494452.918260\t[2416]\t   -31,N\t     0,E\t    89,V\t27.39880752,Lat\t-81.94304897,Lon\t34.586,Ellh\t0.026421, 0.030302, 0.083266\t50,Q",
  "2\t494454.434656\t[2416]\t   -13,N\t    -3,E\t    80,V\t27.39877465,Lat\t-81.94304900,Lon\t34.622,Ellh\t0.026538, 0.030513, 0.083610\t50,Q",
  "3\t494456.419406\t[2416]\t   -40,N\t   -10,E\t    83,V\t27.39872199,Lat\t-81.94304882,Lon\t34.600,Ellh\t0.026385, 0.030330, 0.082958\t50,Q"
)

write_real_mrk <- function() {
  dir <- tempfile("djim3m-")
  dir.create(dir)
  # Match the user's actual filename pattern so the detector picks it.
  mrk <- file.path(
    dir,
    "DJI_202605011317_006_DJISmartFarmWeb-ifaslimpo4_Timestamp.MRK"
  )
  writeLines(real_mrk_lines, mrk)
  list(dir = dir, mrk = mrk)
}

test_that("parse_djim3m_mrk handles the real Mavic 3M format", {
  bundle <- write_real_mrk()
  df <- parse_djim3m_mrk(bundle$mrk)
  expect_equal(nrow(df), 3L)
  expect_equal(df$photo_num, 1:3)
  # Lat/Lon/Alt parsed from the "27.39880752,Lat" style fields.
  expect_equal(df$lat[1L], 27.39880752, tolerance = 1e-7)
  expect_equal(df$lon[1L], -81.94304897, tolerance = 1e-7)
  expect_equal(df$alt[1L], 34.586, tolerance = 1e-3)
  # Std deviations come from "0.026421, 0.030302, 0.083266" (commas
  # AND spaces).
  expect_equal(df$lat_std[1L], 0.026421, tolerance = 1e-6)
  expect_equal(df$lon_std[1L], 0.030302, tolerance = 1e-6)
  expect_equal(df$alt_std[1L], 0.083266, tolerance = 1e-6)
  # Fix quality 50 = DJI RTK Fixed.
  expect_equal(df$fix_quality, c(50L, 50L, 50L))
  # GPS time/week parsed from "[2416]" and the second column.
  expect_equal(df$gps_week, c(2416L, 2416L, 2416L))
  expect_equal(df$gps_time_sec[1L], 494452.918260, tolerance = 1e-3)
})

test_that("detect_djim3m_ppk_files finds .MRK / .bin / .nav by suffix", {
  dir <- tempfile("djim3m-detect-")
  dir.create(dir)
  file.create(file.path(dir, "DJI_x_Timestamp.MRK"))
  file.create(file.path(dir, "DJI_x_PPKRAW.bin"))
  file.create(file.path(dir, "DJI_x_PPKNAV.nav"))
  files <- detect_djim3m_ppk_files(dir)
  expect_length(files$mrk, 1L)
  expect_length(files$bin, 1L)
  expect_length(files$nav, 1L)
  expect_true(files$has_mrk)
  expect_true(files$has_ppk_inputs)
})

test_that("detect_djim3m_ppk_files reports has_ppk_inputs FALSE when missing", {
  dir <- tempfile("djim3m-no-ppk-")
  dir.create(dir)
  file.create(file.path(dir, "DJI_x_Timestamp.MRK"))
  files <- detect_djim3m_ppk_files(dir)
  expect_true(files$has_mrk)
  expect_false(files$has_ppk_inputs)
})

test_that("parse_djim3m_mrk_folder merges multiple .MRK files and dedups by best quality", {
  dir <- tempfile("djim3m-multi-")
  dir.create(dir)
  # First mission: photos 1 and 2 with fix quality 50 (RTK Fixed).
  writeLines(real_mrk_lines[1:2],
             file.path(dir, "DJI_a_Timestamp.MRK"))
  # Second mission: photo 1 with fix quality 1 (single GNSS) plus a
  # fresh photo 3. The merger must prefer the fix-quality-50 row for
  # photo 1 and include photo 3.
  bad_row_1 <- sub("50,Q", "1,Q", real_mrk_lines[1])
  writeLines(c(bad_row_1, real_mrk_lines[3]),
             file.path(dir, "DJI_b_Timestamp.MRK"))
  df <- parse_djim3m_mrk_folder(dir)
  expect_equal(nrow(df), 3L)
  expect_setequal(df$photo_num, 1:3)
  expect_equal(df$fix_quality[df$photo_num == 1L], 50L)
})

test_that("write_djim3m_geo_txt produces an ODM-shaped file", {
  bundle <- write_real_mrk()
  filenames <- c(
    "DJI_20260501132033_0001_D.JPG",
    "DJI_20260501132034_0002_D.JPG",
    "DJI_20260501132036_0003_D.JPG"
  )
  out <- tempfile(fileext = ".txt")
  result <- write_djim3m_geo_txt(
    images_dir      = bundle$dir,
    image_filenames = filenames,
    geo_txt_path    = out
  )
  expect_true(file.exists(out))
  lines <- readLines(out)
  # First line must be the SRS header so ODM picks the right datum.
  expect_match(lines[1L], "EPSG:4326")
  # Three rows, one per image, with lon BEFORE lat (ODM convention).
  expect_length(lines, 4L)
  expect_match(lines[2L], filenames[1L], fixed = TRUE)
  expect_match(lines[2L], "-81\\.94304897")  # lon comes first
  expect_match(lines[2L], "27\\.39880752")   # lat next
  expect_equal(length(result$matched), 3L)
  expect_equal(length(result$unmatched), 0L)
})

test_that("write_djim3m_geo_txt drops rows below min_fix_quality", {
  bundle <- write_real_mrk()
  # Rewrite the middle row to fix quality 1 (single GNSS).
  writeLines(c(
    real_mrk_lines[1L],
    sub("50,Q", "1,Q", real_mrk_lines[2L]),
    real_mrk_lines[3L]
  ), bundle$mrk)

  filenames <- c(
    "DJI_x_0001_D.JPG",
    "DJI_x_0002_D.JPG",
    "DJI_x_0003_D.JPG"
  )
  out <- tempfile(fileext = ".txt")
  result <- write_djim3m_geo_txt(
    images_dir      = bundle$dir,
    image_filenames = filenames,
    geo_txt_path    = out,
    min_fix_quality = 4L
  )
  # Photo 2 (single GNSS) dropped; photos 1 and 3 (RTK Fixed) kept.
  expect_equal(length(result$matched), 2L)
  expect_equal(length(result$dropped_quality), 1L)
  expect_true("DJI_x_0002_D.JPG" %in% result$dropped_quality)
})

test_that("photo_num_from_filename pulls the 4+ digit segment between underscores", {
  fn <- DroneBioR:::photo_num_from_filename
  expect_equal(fn("DJI_20260501132033_0001_D.JPG"),    1L)
  expect_equal(fn("DJI_20260501132034_0042_MS_NIR.TIF"), 42L)
  # No matching pattern -> NA, not an error.
  expect_true(is.na(fn("not-a-dji-file.jpg")))
})

test_that("inspect_djim3m_mrk prints a useful summary", {
  bundle <- write_real_mrk()
  out <- testthat::capture_messages(df <- inspect_djim3m_mrk(bundle$dir))
  joined <- paste(out, collapse = "\n")
  expect_match(joined, "Found 1 .MRK file")
  expect_match(joined, "Latitude range")
  expect_match(joined, "RTK Fixed")
  expect_equal(nrow(df), 3L)
})

test_that("ppk_cli_rtklib_dji errors clearly when CLI tools are missing", {
  base_obs <- tempfile(fileext = ".obs")
  file.create(base_obs)
  hook <- ppk_cli_rtklib_dji(
    base_obs_path        = base_obs,
    dji_bin_to_rinex_cmd = "definitely_not_a_real_command_xyz",
    rnx2rtkp_cmd         = "also_not_a_real_command_xyz"
  )
  expect_error(
    hook(images_dir = tempdir(),
         bin_paths  = "fake.bin",
         nav_paths  = "fake.nav",
         mrk_paths  = "fake.MRK"),
    "not found"
  )
})

test_that("odm_log_has_exifread_crash detects the EXIF MakerNote signature", {
  fn <- DroneBioR:::odm_log_has_exifread_crash

  crash_log <- tempfile(fileext = ".log")
  writeLines(c(
    "[INFO]    Running dataset stage",
    "[INFO]    Loading 121 images",
    "Traceback (most recent call last):",
    "  File \"/code/venv/lib/python3.12/site-packages/exifread/__init__.py\", line 121, in process_file",
    "    hdr.decode_maker_note()",
    "IndexError: list index out of range"
  ), crash_log)
  expect_true(fn(crash_log))

  # A normal log (no exifread crash) must not trip the detector.
  ok_log <- tempfile(fileext = ".log")
  writeLines(c(
    "[INFO]    Running dataset stage",
    "[INFO]    Finished dataset stage",
    "[INFO]    Running opensfm stage"
  ), ok_log)
  expect_false(fn(ok_log))

  # Missing / empty inputs are FALSE, not errors.
  expect_false(fn(tempfile(fileext = ".log")))
  expect_false(fn(NULL))
})

test_that("sanitize_dji_exif_makernotes errors clearly when exiftool is missing", {
  skip_if(nzchar(Sys.which("exiftool")),
          "exiftool is installed; cannot test the missing-tool path")
  img <- tempfile(fileext = ".jpg")
  file.create(img)
  expect_error(
    DroneBioR:::sanitize_dji_exif_makernotes(img),
    "exiftool is required"
  )
})

test_that("sanitize_dji_exif_makernotes is a no-op on an empty path set", {
  expect_equal(DroneBioR:::sanitize_dji_exif_makernotes(character()), 0L)
  expect_equal(
    DroneBioR:::sanitize_dji_exif_makernotes(tempfile(fileext = ".jpg")),
    0L
  )
})

test_that("populate_band_images_dir never hardlinks when sanitizing", {
  # When sanitize_exif = TRUE the function must make real copies, not
  # hardlinks — a hardlink shares the inode with the source, so an
  # in-place exiftool rewrite would corrupt the user's originals.
  skip_if_not(nzchar(Sys.which("exiftool")),
              "exiftool not installed; sanitize path is unreachable")
  src <- tempfile("src-"); dir.create(src)
  img <- file.path(src, "DJI_20260501120009_0001_D.JPG")
  # A 1x1 JPEG so exiftool has a valid file to operate on.
  writeBin(as.raw(c(
    0xff,0xd8,0xff,0xe0,0x00,0x10,0x4a,0x46,0x49,0x46,0x00,0x01,
    0x01,0x00,0x00,0x01,0x00,0x01,0x00,0x00,0xff,0xd9
  )), img)
  manifest <- data.frame(file = img, filename = basename(img),
                         stringsAsFactors = FALSE)
  dest <- tempfile("dest-")
  DroneBioR:::populate_band_images_dir(manifest, dest, sanitize_exif = TRUE)
  copied <- file.path(dest, basename(img))
  expect_true(file.exists(copied))
  # Different inode => a real copy, not a hardlink.
  expect_false(isTRUE(file.info(copied)$ino == file.info(img)$ino))
})

test_that("has_djim3m_images detects the Mavic 3M filename pattern", {
  yes <- tempfile("djim3m-yes-"); dir.create(yes)
  file.create(file.path(yes, "DJI_20260501132033_0001_D.JPG"))
  file.create(file.path(yes, "DJI_20260501132034_0002_MS_NIR.TIF"))
  expect_true(has_djim3m_images(yes))

  no_files <- tempfile("djim3m-no-"); dir.create(no_files)
  file.create(file.path(no_files, "IMG_0001_1.tif"))  # MicaSense-shaped
  file.create(file.path(no_files, "random.jpg"))
  expect_false(has_djim3m_images(no_files))

  missing_dir <- tempfile("never-")
  expect_false(has_djim3m_images(missing_dir))
})

test_that("run_one_dji_band passes --geo to ODM when .MRK is present", {
  body_str <- paste(
    deparse(body(DroneBioR:::run_one_dji_band)),
    collapse = "\n"
  )
  expect_match(body_str, "detect_djim3m_ppk_files")
  expect_match(body_str, "write_djim3m_geo_txt")
  expect_match(body_str, "use_ppk_mrk")
  expect_match(body_str, "ppk_cli")
  expect_match(body_str, '"--geo"')
  expect_match(body_str, "--gps-accuracy")
})

test_that("ppk_cli defaults to \"auto\" on both engines", {
  expect_identical(eval(formals(DroneBioR::run_odm_dji_mavic_3m)$ppk_cli), "auto")
  expect_identical(eval(formals(DroneBioR:::run_one_dji_band)$ppk_cli),    "auto")
})

test_that("resolve_ppk_cli_auto returns NULL with a clear message when tools are missing", {
  # Clear any environment override the user may have set, then point
  # at a tempdir with no `base/` subfolder so the base-obs probe also
  # fails. We expect a `message()` listing what is missing and a
  # NULL return — the run_one_dji_band caller takes that as "use
  # .MRK only".
  withr::with_envvar(c(DRONEBIOR_PPK_BASE_OBS = ""), {
    withr::with_options(list(dronebior.ppk_base_obs = NULL), {
      tmp <- tempfile("ppk-auto-")
      dir.create(tmp)
      out <- testthat::capture_messages(
        result <- DroneBioR:::resolve_ppk_cli_auto(tmp)
      )
      expect_null(result)
      joined <- paste(out, collapse = "\n")
      expect_match(joined, "auto-detection skipped")
      expect_match(joined, "Base-station RINEX")
      expect_match(joined, "Falling back to the .MRK")
    })
  })
})

test_that("resolve_ppk_base_obs reads DRONEBIOR_PPK_BASE_OBS env first", {
  base <- tempfile(fileext = ".obs")
  file.create(base)
  withr::with_envvar(c(DRONEBIOR_PPK_BASE_OBS = base), {
    found <- DroneBioR:::resolve_ppk_base_obs(tempfile("nope-"))
    expect_equal(normalizePath(found), normalizePath(base))
  })
})

test_that("resolve_ppk_base_obs falls back to <images_dir>/base/*.obs", {
  images <- tempfile("withbase-")
  dir.create(file.path(images, "base"), recursive = TRUE)
  base <- file.path(images, "base", "cors_2026_05_01.obs")
  file.create(base)
  withr::with_envvar(c(DRONEBIOR_PPK_BASE_OBS = ""), {
    withr::with_options(list(dronebior.ppk_base_obs = NULL), {
      found <- DroneBioR:::resolve_ppk_base_obs(images)
      expect_equal(normalizePath(found), normalizePath(base))
    })
  })
})
