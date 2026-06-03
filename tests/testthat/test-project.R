test_that("dronebio_project builds normalized paths and has the right class", {
  p <- dronebio_project(project_dir = tempdir())
  expect_s3_class(p, "dronebio_project")
  expect_true(startsWith(p$odm_orthomosaic, p$project_dir))
  expect_equal(basename(p$odm_orthomosaic), "odm_orthophoto.tif")
  expect_equal(p$odm_project_name, "micasense")
})

test_that("dronebio_project honors custom subdirs", {
  p <- dronebio_project(
    project_dir       = tempdir(),
    images_subdir     = "raw_imgs",
    output_subdir     = "my_outputs",
    odm_project_name  = "flightA"
  )
  expect_equal(basename(p$images_dir), "raw_imgs")
  expect_equal(basename(p$output_dir), "my_outputs")
  expect_equal(p$odm_project_name, "flightA")
})

test_that("odm_product_paths returns all expected products", {
  p <- dronebio_project(project_dir = tempdir())
  paths <- odm_product_paths(p)
  expected <- c("orthomosaic", "dsm", "dtm", "chm",
                "dtm_csf", "chm_csf",
                "point_cloud_las",
                "point_cloud_laz", "point_cloud_copc",
                "point_cloud_ply", "mesh_ply",
                "textured_obj", "textured_obj_25d",
                "textured_glb", "textured_glb_25d",
                "tiles_3d", "map_tiles_dir", "report")
  expect_true(all(expected %in% names(paths)))
})

test_that("odm_product_paths exposes CSF DTM/CHM next to the SMRF originals", {
  p <- dronebio_project(project_dir = tempdir())
  paths <- odm_product_paths(p)
  # The CSF variants must live in the same directory as the SMRF
  # originals (otherwise build_chm_from_dsm_dtm can't find both with
  # one path lookup), and the filenames must differ so improve_dtm_csf
  # cannot accidentally clobber dtm.tif / chm.tif.
  expect_equal(dirname(paths[["dtm_csf"]]), dirname(paths[["dtm"]]))
  expect_equal(dirname(paths[["chm_csf"]]), dirname(paths[["chm"]]))
  expect_false(basename(paths[["dtm_csf"]]) == basename(paths[["dtm"]]))
  expect_false(basename(paths[["chm_csf"]]) == basename(paths[["chm"]]))
})

test_that("pick_best_textured_obj prefers 3D over 2.5D when present", {
  tmp <- tempfile("pick_obj_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE))
  p <- dronebio_project(project_dir = tmp,
                        odm_dataset_subdir = "ds", odm_project_name = "proj")

  # Neither file exists -> returns the 3D path as the sensible default
  expect_equal(pick_best_textured_obj(p),
               file.path(p$odm_project_dir, "odm_texturing", "odm_textured_model_geo.obj"))

  # Only 2.5D exists -> returns it
  d25 <- file.path(p$odm_project_dir, "odm_texturing_25d")
  dir.create(d25, recursive = TRUE)
  file.create(file.path(d25, "odm_textured_model_geo.obj"))
  expect_equal(pick_best_textured_obj(p),
               file.path(d25, "odm_textured_model_geo.obj"))

  # Both exist -> 3D wins
  d3 <- file.path(p$odm_project_dir, "odm_texturing")
  dir.create(d3, recursive = TRUE)
  file.create(file.path(d3, "odm_textured_model_geo.obj"))
  expect_equal(pick_best_textured_obj(p),
               file.path(d3, "odm_textured_model_geo.obj"))
})

test_that("pick_best_point_cloud picks COPC > LAZ > LAS > PLY", {
  tmp <- tempfile("pick_cloud_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE))
  p <- dronebio_project(project_dir = tmp,
                        odm_dataset_subdir = "ds", odm_project_name = "proj")
  paths <- odm_product_paths(p)
  for (k in c("point_cloud_las", "point_cloud_laz",
              "point_cloud_copc", "point_cloud_ply")) {
    dir.create(dirname(paths[[k]]), recursive = TRUE, showWarnings = FALSE)
  }

  # Nothing on disk -> default to COPC path
  expect_equal(pick_best_point_cloud(p), unname(paths[["point_cloud_copc"]]))

  # Only LAS exists
  file.create(paths[["point_cloud_las"]])
  expect_equal(pick_best_point_cloud(p), unname(paths[["point_cloud_las"]]))

  # Add LAZ -> wins
  file.create(paths[["point_cloud_laz"]])
  expect_equal(pick_best_point_cloud(p), unname(paths[["point_cloud_laz"]]))

  # Add COPC -> wins
  file.create(paths[["point_cloud_copc"]])
  expect_equal(pick_best_point_cloud(p), unname(paths[["point_cloud_copc"]]))
})

test_that("summarize_odm_products reports nothing available for an empty project", {
  p <- dronebio_project(project_dir = tempdir())
  s <- summarize_odm_products(p)
  expect_true(all(c("product", "available", "size_mb", "path") %in% names(s)))
  expect_false(any(s$available))
})

test_that("validate_odm_outputs reports all products as missing on empty project", {
  p <- dronebio_project(project_dir = tempdir())
  v <- validate_odm_outputs(p)
  expect_s3_class(v, "data.frame")
  expect_true(all(c("product", "exists", "valid", "notes") %in% names(v)))
  expect_false(any(v$exists))
  expect_false(any(v$valid))
  expect_true(all(v$notes == "missing"))
})

test_that("validate_odm_outputs flags a degenerate raster as invalid", {
  tmp <- tempfile("dronebio_validate_"); dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE))
  p <- dronebio_project(project_dir = tmp,
                        odm_dataset_subdir = "ds", odm_project_name = "proj")
  # Tiny 10x10 raster - smaller than the 100-pixel / 50-m threshold.
  ortho_path <- file.path(p$odm_project_dir, "odm_orthophoto", "odm_orthophoto.tif")
  dir.create(dirname(ortho_path), recursive = TRUE)
  r <- terra::rast(nrows = 10, ncols = 10,
                   xmin = 0, xmax = 5, ymin = 0, ymax = 5)
  terra::values(r) <- runif(100, 0, 1)
  terra::writeRaster(r, ortho_path, overwrite = TRUE)

  v <- validate_odm_outputs(p)
  ortho_row <- v[grepl("Orthomosaic", v$product), ]
  expect_true(ortho_row$exists)
  expect_false(ortho_row$valid)
  expect_true(grepl("degenerate", ortho_row$notes))
})

test_that("validate_odm_outputs marks a well-formed raster as ok", {
  tmp <- tempfile("dronebio_validate_"); dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE))
  p <- dronebio_project(project_dir = tmp,
                        odm_dataset_subdir = "ds", odm_project_name = "proj")
  ortho_path <- file.path(p$odm_project_dir, "odm_orthophoto", "odm_orthophoto.tif")
  dir.create(dirname(ortho_path), recursive = TRUE)
  # 200x150 grid spanning 200x150 m (>50 m, >100 px both axes).
  r <- terra::rast(nrows = 200, ncols = 150,
                   xmin = 0, xmax = 150, ymin = 0, ymax = 200)
  terra::values(r) <- runif(200 * 150, 0, 1)
  terra::writeRaster(r, ortho_path, overwrite = TRUE)

  v <- validate_odm_outputs(p)
  ortho_row <- v[grepl("Orthomosaic", v$product), ]
  expect_true(ortho_row$exists)
  expect_true(ortho_row$valid)
  expect_equal(ortho_row$notes, "ok")
  expect_true(grepl("150 x 200", ortho_row$dimensions))
})

test_that("validate_odm_outputs labels orthomosaic as RGB or Multispectral", {
  tmp <- tempfile("dronebio_label_"); dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE))
  p <- dronebio_project(project_dir = tmp,
                        odm_dataset_subdir = "ds", odm_project_name = "proj")
  ortho_path <- file.path(p$odm_project_dir, "odm_orthophoto", "odm_orthophoto.tif")
  dir.create(dirname(ortho_path), recursive = TRUE)

  # 4 layers -> RGB
  r <- terra::rast(nrows = 200, ncols = 200,
                   xmin = 0, xmax = 200, ymin = 0, ymax = 200, nlyrs = 4)
  terra::values(r) <- runif(200 * 200 * 4, 0, 1)
  terra::writeRaster(r, ortho_path, overwrite = TRUE)
  v <- validate_odm_outputs(p)
  expect_true("RGB Orthomosaic" %in% v$product)
  expect_false("Multispectral Orthomosaic" %in% v$product)

  # 5 layers -> Multispectral
  r5 <- terra::rast(nrows = 200, ncols = 200,
                    xmin = 0, xmax = 200, ymin = 0, ymax = 200, nlyrs = 5)
  terra::values(r5) <- runif(200 * 200 * 5, 0, 1)
  terra::writeRaster(r5, ortho_path, overwrite = TRUE)
  v2 <- validate_odm_outputs(p)
  expect_true("Multispectral Orthomosaic" %in% v2$product)
})

test_that("build_chm_raster computes CHM = DSM - DTM, clamped to 0", {
  tmp <- tempfile("dronebio_chm_"); dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE))
  p <- dronebio_project(project_dir = tmp,
                        odm_dataset_subdir = "ds", odm_project_name = "proj")
  dem_dir <- file.path(p$odm_project_dir, "odm_dem")
  dir.create(dem_dir, recursive = TRUE)

  dsm <- terra::rast(nrows = 50, ncols = 50,
                     xmin = 0, xmax = 50, ymin = 0, ymax = 50)
  dtm <- dsm
  terra::values(dsm) <- 100 + matrix(runif(2500, 0, 10), 50, 50)
  terra::values(dtm) <- 100  # flat ground
  # Introduce a tiny negative region (DSM < DTM) -> should clamp to 0
  vals_dsm <- terra::values(dsm)
  vals_dsm[1:10] <- 99  # 1 m below DTM
  terra::values(dsm) <- vals_dsm
  terra::writeRaster(dsm, file.path(dem_dir, "dsm.tif"), overwrite = TRUE)
  terra::writeRaster(dtm, file.path(dem_dir, "dtm.tif"), overwrite = TRUE)

  out <- build_chm_raster(p, cache_aware = FALSE)
  expect_true(file.exists(out))
  chm <- terra::rast(out)
  mm <- terra::minmax(chm)
  expect_gte(mm[1, 1], 0)        # min clamped to 0
  expect_lte(mm[2, 1], 15)       # max bounded by max DSM - min DTM
  expect_equal(names(chm), "CHM")
})

test_that("build_chm_raster clips the upper-tail outliers above the percentile", {
  tmp <- tempfile("dronebio_chm_out_"); dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE))
  p <- dronebio_project(project_dir = tmp,
                        odm_dataset_subdir = "ds", odm_project_name = "proj")
  dem_dir <- file.path(p$odm_project_dir, "odm_dem")
  dir.create(dem_dir, recursive = TRUE)

  # Flat ground; canopy mostly 0-2 m with a handful of impossible
  # spikes (200 m) standing in for reconstruction noise.
  dtm <- terra::rast(nrows = 100, ncols = 100,
                     xmin = 0, xmax = 100, ymin = 0, ymax = 100)
  terra::values(dtm) <- 100
  dsm <- dtm
  sv <- 100 + matrix(runif(10000, 0, 2), 100, 100)  # 0-2 m canopy
  sv[1:20] <- 300                                    # 200 m spikes (0.2%)
  terra::values(dsm) <- sv
  terra::writeRaster(dsm, file.path(dem_dir, "dsm.tif"), overwrite = TRUE)
  terra::writeRaster(dtm, file.path(dem_dir, "dtm.tif"), overwrite = TRUE)

  # With the default P99.5 clip, the 200 m spikes (0.2% of pixels)
  # become NA, so the max drops to the real canopy ceiling (~2 m).
  out <- build_chm_raster(p, cache_aware = FALSE, force = TRUE)
  mm <- terra::minmax(terra::rast(out))
  expect_lt(mm[2, 1], 10)       # spikes removed -> max is the real canopy
  expect_gte(mm[1, 1], 0)

  # Disabling the clip keeps the spikes (max ~200 m).
  out2 <- build_chm_raster(p, cache_aware = FALSE, force = TRUE,
                           outlier_percentile = 100)
  mm2 <- terra::minmax(terra::rast(out2))
  expect_gt(mm2[2, 1], 150)
})

test_that("build_chm_raster errors when DSM or DTM missing", {
  tmp <- tempfile("dronebio_chm_"); dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE))
  p <- dronebio_project(project_dir = tmp,
                        odm_dataset_subdir = "ds", odm_project_name = "proj")
  expect_error(build_chm_raster(p), "CHM needs DSM \\+ DTM")
})

test_that("configure_proj_database returns a logical without raising", {
  # Returns TRUE invisibly when proj.db is reachable, FALSE with a warning
  # otherwise. Both outcomes are valid for this smoke test.
  result <- suppressWarnings(configure_proj_database(verbose = FALSE))
  expect_type(result, "logical")
})

test_that("despike_dem removes isolated spikes but keeps coherent surface", {
  # Flat-ish surface (~0 m) with a few isolated 40 m needle spikes.
  r <- terra::rast(nrows = 40, ncols = 40, xmin = 0, xmax = 40,
                   ymin = 0, ymax = 40)
  terra::values(r) <- matrix(rnorm(1600, 0, 0.1), 40, 40)  # smooth surface
  # Plant isolated spikes far from each other.
  spike_cells <- c(205, 410, 820, 1200)
  rv <- terra::values(r); rv[spike_cells] <- 40; terra::values(r) <- rv

  expect_gt(terra::minmax(r)[2, 1], 30)          # spikes present before
  out <- despike_dem(r, window = 5, max_deviation = 3, fill = "median")
  expect_lt(terra::minmax(out)[2, 1], 5)         # spikes gone after
  # The smooth surface is essentially unchanged away from spikes.
  expect_lt(abs(terra::global(out, "mean", na.rm = TRUE)[[1]]), 0.2)
})

test_that("despike_dem removes wide towers via height-above-ground", {
  # A flat ground (DTM = 0) with a WIDE tower (10x10 cluster at 60 m)
  # that the small local-median pass cannot see, plus genuine 12 m
  # canopy that must survive a 20 m ceiling.
  dtm <- terra::rast(nrows = 60, ncols = 60, xmin = 0, xmax = 60,
                     ymin = 0, ymax = 60)
  terra::values(dtm) <- 0
  dsm <- dtm
  m <- matrix(0, 60, 60)
  m[10:21, 10:21] <- 60      # 12x12 wide tower (too wide for a 5x5 median)
  m[40:50, 40:50] <- 12      # genuine 12 m canopy block (must survive)
  terra::values(dsm) <- m

  # Local-median pass alone barely dents the wide tower.
  only_local <- despike_dem(dsm, window = 5, max_deviation = 3)
  expect_gt(terra::minmax(only_local)[2, 1], 40)

  # With the height-above-ground pass (DTM as ground, 20 m ceiling) the
  # tower goes and the 12 m canopy stays.
  cleaned <- despike_dem(dsm, ground = dtm, max_height_above_ground = 20)
  expect_lt(terra::minmax(cleaned)[2, 1], 20)            # tower removed
  vals <- terra::values(cleaned)
  expect_true(any(abs(vals[!is.na(vals)] - 12) < 1))     # 12 m canopy survived
})

test_that("despike_dem fills downward pits below the ground", {
  # A DSM is the top surface: a pixel 15 m BELOW the terrain is an
  # artifact (the downward spikes seen in 3D). It must be filled back
  # to ground.
  dtm <- terra::rast(nrows = 30, ncols = 30, xmin = 0, xmax = 30,
                     ymin = 0, ymax = 30)
  terra::values(dtm) <- 0
  dsm <- dtm
  m <- matrix(1, 30, 30)        # 1 m canopy everywhere
  m[15, 15] <- -15              # a downward pit
  terra::values(dsm) <- m
  expect_lt(terra::minmax(dsm)[1, 1], -10)               # pit present before
  cleaned <- despike_dem(dsm, ground = dtm,
                         max_height_above_ground = 20,
                         max_depth_below_ground = 2)
  expect_gte(terra::minmax(cleaned)[1, 1], -2)           # pit filled to ground
})

test_that("despike_dem can write to disk and NA-fill", {
  r <- terra::rast(nrows = 20, ncols = 20)
  terra::values(r) <- 0
  rv <- terra::values(r); rv[100] <- 50; terra::values(r) <- rv
  out_path <- tempfile(fileext = ".tif")
  despike_dem(r, max_deviation = 3, fill = "NA", out_path = out_path)
  expect_true(file.exists(out_path))
  cleaned <- terra::rast(out_path)
  expect_true(is.nan(terra::values(cleaned)[100]) ||
              is.na(terra::values(cleaned)[100]))
})
