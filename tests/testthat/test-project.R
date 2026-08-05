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

  out <- build_chm_raster(p)
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
  out <- build_chm_raster(p, force = TRUE)
  mm <- terra::minmax(terra::rast(out))
  expect_lt(mm[2, 1], 10)       # spikes removed -> max is the real canopy
  expect_gte(mm[1, 1], 0)

  # Two independent filters run, so disabling the percentile clip alone is not
  # enough to keep the spikes: the local despiker still catches them, because
  # each 200 m pixel stands far above its own neighbourhood.
  out2 <- suppressMessages(build_chm_raster(p, force = TRUE, outlier_percentile = 100))
  expect_lt(terra::minmax(terra::rast(out2))[2, 1], 10)

  # Disabling both keeps the raw difference (max ~200 m).
  out3 <- build_chm_raster(p, force = TRUE, outlier_percentile = 100, despike = FALSE)
  expect_gt(terra::minmax(terra::rast(out3))[2, 1], 150)
})

test_that("build_chm_raster errors when DSM or DTM missing", {
  tmp <- tempfile("dronebio_chm_"); dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE))
  p <- dronebio_project(project_dir = tmp,
                        odm_dataset_subdir = "ds", odm_project_name = "proj")
  expect_error(build_chm_raster(p), "CHM needs DSM \\+ DTM")
})

test_that("quick_outputs_check reads only the project, never a stray cache", {
  # Regression: product availability used to fall back to
  # ~/.dronebior/cache/<slug>/<basename>. Since the slug was just the
  # project folder name, a new set of images under the same root inherited
  # a previous run's cached files and reported products as present when the
  # project had none. quick_outputs_check must now see only the real files.
  home <- tempfile("home_"); dir.create(home)
  withr::local_envvar(HOME = home)

  tmp <- tempfile("dronebio_qc_"); dir.create(tmp, recursive = TRUE)
  on.exit(unlink(c(tmp, home), recursive = TRUE), add = TRUE)
  p <- dronebio_project(project_dir = tmp,
                        odm_dataset_subdir = "ds", odm_project_name = "proj")
  paths <- odm_product_paths(p)

  # Plant a >1 MB file in the would-be cache dir under the project's slug.
  slug <- gsub("[^A-Za-z0-9._-]+", "_", basename(p$project_dir))
  cache_dir <- file.path(home, ".dronebior", "cache", slug)
  dir.create(cache_dir, recursive = TRUE)
  blob <- as.raw(rep(0L, 1024 * 1024 + 16))
  for (key in c("orthomosaic", "dsm", "dtm")) {
    writeBin(blob, file.path(cache_dir, basename(unname(paths[[key]]))))
  }

  # No canonical products on disk -> everything FALSE, cache notwithstanding.
  qc <- quick_outputs_check(p)
  expect_false(qc[["orthomosaic"]])
  expect_false(qc[["dsm"]])
  expect_false(qc[["dtm"]])
  expect_false(qc[["outputs_complete"]])

  # Now write the real orthomosaic; only then does it read as present.
  ortho <- unname(paths[["orthomosaic"]])
  dir.create(dirname(ortho), recursive = TRUE, showWarnings = FALSE)
  writeBin(blob, ortho)
  expect_true(quick_outputs_check(p)[["orthomosaic"]])
})

test_that("quick_outputs_check counts the staged .ply cloud, not only LAS/LAZ", {
  # The staged flow (build_point_cloud_only / Process step 2) writes
  # odm_filterpoints/point_cloud.ply; the LAS/LAZ/COPC exports only appear
  # after a full ODM run. The "Point cloud" status pill must go green off
  # the .ply alone, otherwise it never lights up for the staged path.
  tmp <- tempfile("dronebio_qc_ply_"); dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  p <- dronebio_project(project_dir = tmp,
                        odm_dataset_subdir = "ds", odm_project_name = "proj")
  paths <- odm_product_paths(p)
  blob <- as.raw(rep(0L, 1024 * 1024 + 16))  # > 1 MB

  # No cloud of any kind yet.
  expect_false(quick_outputs_check(p)[["point_cloud"]])

  # Only the .ply exists (no LAS/LAZ/COPC) -> still counts as present.
  ply <- unname(paths[["point_cloud_ply"]])
  dir.create(dirname(ply), recursive = TRUE, showWarnings = FALSE)
  writeBin(blob, ply)
  expect_true(quick_outputs_check(p)[["point_cloud"]])
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

test_that("area_open_chm_spikes flattens small isolated spikes, keeps large canopy", {
  # CHM with a 1-cell isolated spike (a cone) and a wide contiguous canopy
  # block, both taller than the height threshold. The area-opening must drop
  # the isolated spike and keep the block.
  chm <- terra::rast(nrows = 60, ncols = 60, xmin = 0, xmax = 6,
                     ymin = 0, ymax = 6)                 # 0.1 m cells -> 0.01 m2
  m <- matrix(0, 60, 60)
  m[5, 5] <- 8                        # isolated spike: 1 cell = 0.01 m2
  m[20:39, 20:39] <- 6                # contiguous canopy: 20x20 = 4 m2
  terra::values(chm) <- m

  out <- DroneBioR:::area_open_chm_spikes(chm, min_height = 1.5,
                                          max_area_m2 = 1, dilate_cells = 0)
  v <- terra::values(out); v <- v[!is.na(v)]
  expect_false(any(abs(v - 8) < 0.5))                    # isolated spike flattened
  expect_equal(sum(abs(v - 6) < 0.5), 400L)              # the 4 m2 canopy kept whole

  # max_spike_area_m2 = 0 disables the filter (no-op).
  same <- DroneBioR:::area_open_chm_spikes(chm, max_area_m2 = 0)
  expect_equal(terra::minmax(same)[2, 1], 8)
})

test_that("despike_dem iterations converge a wide pit and never worsen it", {
  # A wide deep pit (12x12 cluster at -40 m) on a flat surface, with a
  # trend cell larger than the pit. Two iterations must clear it and
  # never do worse than one.
  r <- terra::rast(nrows = 80, ncols = 80, xmin = 0, xmax = 80,
                   ymin = 0, ymax = 80)
  m <- matrix(0, 80, 80)
  m[30:41, 30:41] <- -40        # wide deep pit, 12x12
  terra::values(r) <- m
  expect_lt(terra::minmax(r)[1, 1], -30)             # pit present

  one <- despike_dem(r, max_height_above_ground = 20,
                     max_depth_below_ground = 2, trend_cell_m = 30,
                     iterations = 1)
  two <- despike_dem(r, max_height_above_ground = 20,
                     max_depth_below_ground = 2, trend_cell_m = 30,
                     iterations = 2)
  # Two passes are never deeper than one, and clear the pit near 0.
  expect_gte(terra::minmax(two)[1, 1], terra::minmax(one)[1, 1])
  expect_gte(terra::minmax(two)[1, 1], -5)
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

test_that("harmonize_dem_products guarantees DSM >= DTM and CHM >= 0", {
  # DSM that dips BELOW the DTM in places (the ODM inconsistency) plus
  # a tower; DTM with its own bump.
  dtm <- terra::rast(nrows = 40, ncols = 40, xmin = 0, xmax = 40,
                     ymin = 0, ymax = 40)
  terra::values(dtm) <- 0
  dsm <- dtm
  m <- matrix(0.5, 40, 40)      # ~0.5 m canopy
  m[5:8, 5:8]   <- -3           # DSM below ground (impossible) -> CHM<0
  m[20:22, 20:22] <- 60         # tower
  m[28:33, 28:33] <- 12         # a real 12 m tree (coherent cluster, not a needle)
  terra::values(dsm) <- m

  res <- harmonize_dem_products(dsm = dsm, dtm = dtm, write = FALSE,
                                canopy_ceiling = 25)
  d <- terra::values(res$dsm - res$dtm); d <- d[!is.na(d)]
  chm <- terra::values(res$chm); chm <- chm[!is.na(chm)]
  expect_true(all(d >= -1e-6))               # DSM never below DTM
  expect_true(all(chm >= -1e-6))             # CHM never negative
  expect_lt(max(chm), 25)                    # tower removed
  expect_true(any(abs(chm - 12) < 2))        # 12 m tree preserved
})

test_that("harmonize_dem_products writes the three consistent rasters", {
  dtm <- terra::rast(nrows = 20, ncols = 20); terra::values(dtm) <- 0
  dsm <- dtm; terra::values(dsm) <- 1
  out <- tempfile("harmon_"); dir.create(out)
  res <- harmonize_dem_products(dsm = dsm, dtm = dtm, out_dir = out)
  expect_true(all(file.exists(unlist(res$paths))))
  expect_setequal(basename(unlist(res$paths)),
                  c("dsm_consistent.tif", "dtm_consistent.tif",
                    "chm_consistent.tif"))
})

test_that("harmonize_project_dems_inplace backs up raw and writes consistent canonical DEMs", {
  tmp <- tempfile("inplace_"); dir.create(tmp)
  p <- dronebio_project(project_dir = tmp, odm_dataset_subdir = "ds",
                        odm_project_name = "proj")
  dem <- file.path(p$odm_project_dir, "odm_dem"); dir.create(dem, recursive = TRUE)
  dtm <- terra::rast(nrows = 40, ncols = 40); terra::values(dtm) <- 0
  dsm <- dtm
  m <- matrix(0.5, 40, 40); m[5:8, 5:8] <- -3; m[20:22, 20:22] <- 60
  terra::values(dsm) <- m
  terra::writeRaster(dsm, file.path(dem, "dsm.tif"))
  terra::writeRaster(dtm, file.path(dem, "dtm.tif"))

  ok <- DroneBioR:::harmonize_project_dems_inplace(p, canopy_ceiling = 20)
  expect_true(isTRUE(ok))
  # Raw backups exist; canonical files now consistent.
  expect_true(file.exists(file.path(dem, "dsm_raw.tif")))
  expect_true(file.exists(file.path(dem, "dtm_raw.tif")))
  d <- terra::values(terra::rast(file.path(dem, "dsm.tif")) -
                     terra::rast(file.path(dem, "dtm.tif")))
  d <- d[!is.na(d)]
  expect_true(all(d >= -1e-6))                      # DSM >= DTM now
  expect_true(file.exists(file.path(dem, "chm.tif")))

  # Idempotent: a second call reads the raw backup, so the result is
  # identical (cleaning is not compounded).
  before <- terra::minmax(terra::rast(file.path(dem, "dsm.tif")))
  DroneBioR:::harmonize_project_dems_inplace(p, canopy_ceiling = 20)
  after <- terra::minmax(terra::rast(file.path(dem, "dsm.tif")))
  expect_equal(after, before)
})

test_that("run_odm_dji_mavic_3m harmonizes by default", {
  expect_true(eval(formals(DroneBioR::run_odm_dji_mavic_3m)$harmonize))
  expect_equal(eval(formals(DroneBioR::run_odm_dji_mavic_3m)$canopy_ceiling), 18)
})

test_that("finalize_dronebio_products flattens products and removes scaffolding", {
  tmp <- tempfile("final_"); dir.create(tmp)
  p <- dronebio_project(project_dir = tmp, odm_dataset_subdir = "odm_dataset",
                        odm_project_name = "flight",
                        output_subdir = "dronebior_analysis")
  dem  <- file.path(p$odm_project_dir, "odm_dem")
  orth <- file.path(p$odm_project_dir, "odm_orthophoto")
  dir.create(dem, recursive = TRUE); dir.create(orth, recursive = TRUE)
  dir.create(p$output_dir, recursive = TRUE)
  mk <- function(path, n = 1) {
    r <- terra::rast(nrows = 8, ncols = 8, nlyrs = n)
    terra::values(r) <- runif(64 * n)
    terra::writeRaster(r, path)
  }
  mk(file.path(dem, "dsm.tif")); mk(file.path(dem, "dtm.tif"))
  mk(file.path(dem, "chm.tif")); mk(file.path(dem, "dsm_raw.tif"))
  mk(file.path(orth, "odm_orthophoto.tif"), 4)
  mk(file.path(orth, "odm_orthophoto_dji.tif"), 7)
  mk(file.path(p$output_dir, "spectral_indices.tif"), 16)
  mk(file.path(p$output_dir, "biomass_index_proxy.tif"))

  out <- finalize_dronebio_products(p, extra_metadata = list(flight = "t"))
  prod_dir <- file.path(tmp, "products")
  # Flat, simply-named products present.
  expect_setequal(
    sort(list.files(prod_dir, pattern = "\\.tif$")),
    sort(c("orthomosaic.tif", "dsm.tif", "dtm.tif", "chm.tif",
           "spectral_indices.tif", "biomass_proxy.tif")))
  expect_true(file.exists(file.path(prod_dir, "metadata.json")))
  # The 7-band DJI stack was chosen as the orthomosaic.
  expect_equal(terra::nlyr(terra::rast(file.path(prod_dir, "orthomosaic.tif"))), 7L)
  # Scaffolding + intermediates gone.
  expect_false(dir.exists(p$odm_dataset_dir))
  expect_false(dir.exists(p$output_dir))
})

# Build a project whose ODM tree carries the full set of 3D deliverables
# alongside the rasters: point clouds in every format ODM writes, the
# filtered/meshed PLYs, a textured OBJ with its .mtl + texture PNG, the
# GLB the Shiny 3D tab consumes, a 3D tile set and the report PDF.
make_3d_project <- function(prefix = "final_3d_") {
  tmp <- tempfile(prefix); dir.create(tmp)
  p <- dronebio_project(project_dir = tmp, odm_dataset_subdir = "odm_dataset",
                        odm_project_name = "flight",
                        output_subdir = "dronebior_analysis")
  d <- p$odm_project_dir
  for (sub in c("odm_dem", "odm_orthophoto", "odm_georeferencing",
                "odm_filterpoints", "odm_meshing", "odm_texturing",
                "odm_report", file.path("3d_tiles", "tiles"))) {
    dir.create(file.path(d, sub), recursive = TRUE, showWarnings = FALSE)
  }
  mk_rast <- function(path, n = 1) {
    r <- terra::rast(nrows = 8, ncols = 8, nlyrs = n)
    terra::values(r) <- runif(64 * n)
    terra::writeRaster(r, path)
  }
  mk_bin <- function(path, n = 512) writeBin(as.raw(seq_len(n) %% 256L), path)

  mk_rast(file.path(d, "odm_dem", "dsm.tif"))
  mk_rast(file.path(d, "odm_dem", "dtm.tif"))
  mk_rast(file.path(d, "odm_dem", "chm.tif"))
  mk_rast(file.path(d, "odm_orthophoto", "odm_orthophoto_dji.tif"), 7)

  geo <- file.path(d, "odm_georeferencing")
  mk_bin(file.path(geo, "odm_georeferenced_model.las"), 700)
  mk_bin(file.path(geo, "odm_georeferenced_model.laz"), 300)
  mk_bin(file.path(geo, "odm_georeferenced_model.copc.laz"), 400)
  mk_bin(file.path(d, "odm_filterpoints", "point_cloud.ply"), 600)
  mk_bin(file.path(d, "odm_meshing", "odm_25dmesh.ply"), 200)

  tex <- file.path(d, "odm_texturing")
  writeLines(c("mtllib odm_textured_model_geo.mtl", "v 0 0 0", "v 1 0 0",
               "v 0 1 0", "f 1 2 3"),
             file.path(tex, "odm_textured_model_geo.obj"))
  writeLines(c("newmtl material0000",
               "map_Kd odm_textured_model_geo_material0000_map_Kd.png"),
             file.path(tex, "odm_textured_model_geo.mtl"))
  mk_bin(file.path(tex, "odm_textured_model_geo_material0000_map_Kd.png"), 128)
  mk_bin(file.path(tex, "odm_textured_model_geo.glb"), 900)

  writeLines('{"asset":{"version":"1.0"}}',
             file.path(d, "3d_tiles", "tileset.json"))
  mk_bin(file.path(d, "3d_tiles", "tiles", "0.b3dm"), 64)
  mk_bin(file.path(d, "odm_report", "report.pdf"), 256)
  p
}

test_that("finalize_dronebio_products keeps point clouds and 3D models", {
  p <- make_3d_project()
  prod_dir <- file.path(p$project_dir, "products")

  out <- finalize_dronebio_products(p, extra_metadata = list(flight = "t"))

  # The scaffolding is gone, so products/ is now the only copy.
  expect_false(dir.exists(p$odm_dataset_dir))

  # Every point-cloud format survived, under its real extension. The
  # two-part .copc.laz in particular must not be truncated to .laz.
  for (f in c("point_cloud.las", "point_cloud.laz", "point_cloud.copc.laz",
              "point_cloud.ply", "mesh.ply", "textured_model.glb",
              "report.pdf")) {
    expect_true(file.exists(file.path(prod_dir, f)), info = f)
  }
  expect_equal(file.info(file.path(prod_dir, "point_cloud.las"))$size, 700)
  expect_equal(file.info(file.path(prod_dir, "textured_model.glb"))$size, 900)

  # The textured OBJ travelled as a group under its original filenames,
  # so `mtllib` and the .mtl's map_Kd still resolve, and the Shiny 3D
  # tab's dirname()/<stem>.mtl layout still holds.
  obj <- file.path(prod_dir, "textured_model", "odm_textured_model_geo.obj")
  expect_true(file.exists(obj))
  expect_true(file.exists(file.path(dirname(obj), "odm_textured_model_geo.mtl")))
  expect_true(file.exists(file.path(
    dirname(obj), "odm_textured_model_geo_material0000_map_Kd.png")))
  expect_true(any(grepl("^mtllib odm_textured_model_geo\\.mtl$",
                        readLines(obj, warn = FALSE))))

  # Tile sets travelled whole -- the index alone would point at nothing.
  expect_true(file.exists(file.path(prod_dir, "3d_tiles", "tileset.json")))
  expect_true(file.exists(file.path(prod_dir, "3d_tiles", "tiles", "0.b3dm")))

  # Returned paths are keyed by odm_product_paths() key and all exist.
  expect_true(all(c("point_cloud_las", "point_cloud_copc", "textured_obj",
                    "textured_glb", "mesh_ply", "tiles_3d", "report")
                  %in% names(out)))
  expect_true(all(file.exists(out)))

  # metadata.json inventories the non-raster deliverables too, sizing the
  # OBJ group by its folder rather than by the .obj alone.
  skip_if_not_installed("jsonlite")
  meta <- jsonlite::fromJSON(file.path(prod_dir, "metadata.json"),
                             simplifyVector = FALSE)
  expect_equal(meta$products$textured_obj$file, "textured_model")
  expect_equal(meta$products$textured_obj$files, 3L)
  expect_equal(meta$products$point_cloud_las$bytes, 700)
  expect_equal(meta$products$orthomosaic$bands, 7L)
})

test_that("finalize_dronebio_products keeps scaffolding when a copy fails", {
  p <- make_3d_project("final_fail_")
  prod_dir <- file.path(p$project_dir, "products")
  # Block one destination with a directory of the same name: file.copy()
  # will happily copy *into* it and report success, and only the size
  # check catches that the product did not land where it belongs.
  dir.create(file.path(prod_dir, "point_cloud.las"), recursive = TRUE)

  expect_warning(out <- finalize_dronebio_products(p),
                 "could not be copied")
  # Nothing deleted, so the 1 GB-class originals are still recoverable.
  expect_true(dir.exists(p$odm_dataset_dir))
  expect_true(file.exists(file.path(p$odm_project_dir, "odm_georeferencing",
                                    "odm_georeferenced_model.las")))
  expect_false("point_cloud_las" %in% names(out))
  # The products that did copy are still there.
  expect_true(file.exists(file.path(prod_dir, "textured_model.glb")))
})

test_that("finalize_dronebio_products will not delete products it was told to skip", {
  p <- make_3d_project("final_skip_")

  expect_warning(
    out <- finalize_dronebio_products(
      p, products = c("orthomosaic", "dsm", "dtm", "chm")),
    "excluded by"
  )
  expect_true(dir.exists(p$odm_dataset_dir))
  expect_false("point_cloud_las" %in% names(out))

  # Excluding products that do not exist on disk is not a loss, so a
  # narrowed run over an ortho-only project still cleans up.
  tmp <- tempfile("final_skip_ok_"); dir.create(tmp)
  q <- dronebio_project(project_dir = tmp, odm_dataset_subdir = "odm_dataset",
                        odm_project_name = "flight",
                        output_subdir = "dronebior_analysis")
  dir.create(file.path(q$odm_project_dir, "odm_orthophoto"), recursive = TRUE)
  r <- terra::rast(nrows = 8, ncols = 8); terra::values(r) <- runif(64)
  terra::writeRaster(r, file.path(q$odm_project_dir, "odm_orthophoto",
                                  "odm_orthophoto.tif"))
  expect_silent(suppressMessages(
    finalize_dronebio_products(q, products = "orthomosaic")))
  expect_false(dir.exists(q$odm_dataset_dir))
})

test_that("finalize_dronebio_products warns only about expected-but-missing products", {
  mkproj <- function() {
    tmp <- tempfile("final_miss_"); dir.create(tmp)
    p <- dronebio_project(project_dir = tmp, odm_dataset_subdir = "odm_dataset",
                          odm_project_name = "flight",
                          output_subdir = "dronebior_analysis")
    orth <- file.path(p$odm_project_dir, "odm_orthophoto")
    dem  <- file.path(p$odm_project_dir, "odm_dem")
    dir.create(orth, recursive = TRUE); dir.create(dem, recursive = TRUE)
    mk <- function(path, n = 1) {
      r <- terra::rast(nrows = 8, ncols = 8, nlyrs = n)
      terra::values(r) <- runif(64 * n); terra::writeRaster(r, path)
    }
    mk(file.path(orth, "odm_orthophoto_dji.tif"), 7)
    mk(file.path(dem, "dsm.tif")); mk(file.path(dem, "dtm.tif"))
    mk(file.path(dem, "chm.tif"))
    p   # note: NO spectral_indices.tif / biomass written
  }

  # Indices expected but missing -> a clear warning, products/ still written.
  expect_warning(
    out <- finalize_dronebio_products(mkproj(),
                                      expect = c("spectral_indices", "biomass_proxy")),
    "expected product"
  )
  expect_true("orthomosaic" %in% names(out))
  expect_false("spectral_indices" %in% names(out))

  # Same missing indices but NOT expected (ortho-only run) -> no such warning.
  msgs <- character()
  withCallingHandlers(
    finalize_dronebio_products(mkproj()),
    warning = function(w) { msgs <<- c(msgs, conditionMessage(w)); invokeRestart("muffleWarning") }
  )
  expect_false(any(grepl("expected product", msgs)))
})

test_that("odm_product_paths prefers the DJI multispectral stack over the RGB ortho", {
  # A DJI Mavic 3M run leaves odm_orthophoto_dji.tif (7 bands) beside the
  # RGB-only odm_orthophoto.tif. Returning the latter hides NIR/RedEdge, so
  # NDVI, NDRE and EVI disappear from a multispectral flight.
  dir <- tempfile("djiproj-")
  p <- dronebio_project(dir)
  od <- dirname(p$odm_orthomosaic)
  dir.create(od, recursive = TRUE, showWarnings = FALSE)

  file.create(p$odm_orthomosaic)
  expect_equal(unname(odm_product_paths(p)[["orthomosaic"]]), p$odm_orthomosaic)

  stack <- file.path(od, "odm_orthophoto_dji.tif")
  file.create(stack)
  expect_equal(unname(odm_product_paths(p)[["orthomosaic"]]), stack)
})

# ---- build_chm_raster() removes local needles -------------------------------

# A needle is a cell standing far above its own neighbourhood. It is typically
# nowhere near the tallest cell in the survey, which is exactly why a global
# percentile clip cannot separate it from real canopy.
make_spiky_project <- function(spike_height = 25, canopy_height = 30) {
  root <- tempfile("chmproj"); dir.create(root, recursive = TRUE)
  proj <- dronebio_project(project_dir = root,
                           odm_dataset_subdir = "ds", odm_project_name = "proj")
  d <- file.path(proj$odm_project_dir, "odm_dem")
  dir.create(d, recursive = TRUE)
  dtm <- terra::rast(nrows = 60, ncols = 60, xmin = 0, xmax = 60, ymin = 0, ymax = 60)
  terra::values(dtm) <- 100                       # flat ground
  dsm <- dtm
  terra::values(dsm) <- 102                       # 2 m of low vegetation
  crown  <- terra::cells(dsm, terra::ext(10, 19, 10, 19))  # a real 9 x 9 crown
  dsm[crown] <- 100 + canopy_height
  needle <- terra::cellFromXY(dsm, cbind(45.5, 45.5))      # one isolated cell
  dsm[needle] <- 100 + spike_height
  terra::writeRaster(dtm, file.path(d, "dtm.tif"))
  terra::writeRaster(dsm, file.path(d, "dsm.tif"))
  list(project = proj, needle = needle, crown = crown)
}

test_that("the needle goes and the taller real crown stays", {
  p <- make_spiky_project()
  out <- suppressMessages(
    build_chm_raster(p$project, force = TRUE, outlier_percentile = 100))
  chm <- terra::rast(out)
  expect_lt(chm[p$needle][[1]], 5)                  # back down to its neighbours
  expect_gt(stats::median(chm[p$crown][, 1]), 25)   # the contiguous crown survives
})

test_that("despike = FALSE keeps the raw difference", {
  p <- make_spiky_project()
  out <- suppressMessages(
    build_chm_raster(p$project, force = TRUE,
                     outlier_percentile = 100, despike = FALSE))
  expect_equal(terra::rast(out)[p$needle][[1]], 25, tolerance = 1e-6)
})

test_that("a global percentile clip cannot do what the local filter does", {
  # the needle (25 m) sits below the crown (30 m), so no percentile that keeps
  # the crown can remove the needle - which is the whole point of the filter
  p <- make_spiky_project(spike_height = 25, canopy_height = 30)
  out <- suppressMessages(
    build_chm_raster(p$project, force = TRUE,
                     outlier_percentile = 99.5, despike = FALSE))
  expect_gt(terra::rast(out)[p$needle][[1]], 20)
})

test_that("the despiking parameters reach despike_dem()", {
  p <- make_spiky_project(spike_height = 25)
  out <- suppressMessages(
    build_chm_raster(p$project, force = TRUE,
                     outlier_percentile = 100, despike_max_deviation = 100))
  expect_gt(terra::rast(out)[p$needle][[1]], 20)   # tolerance above its prominence
})
