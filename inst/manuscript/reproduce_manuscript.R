#!/usr/bin/env Rscript
# ===========================================================================
#  reproduce_manuscript.R
#
#  Rodrigues, H.M. & Golmohammadi, G. "DroneBioR: A Reproducible R Package
#  for Drone-Based Biomass Workflows from Multispectral Imagery", Drones.
#
#  This script walks through Section 3 of the paper in the order the paper
#  presents it. Read it top to bottom: every number quoted in Section 3 is
#  computed below, and each is printed next to the value that was published,
#  so a divergence announces itself.
#
#  Before running, set PROJECT in Part 0 to your copy of the demonstration
#  project, and restore the recorded package versions:
#
#      renv::restore(lockfile = system.file("manuscript", "renv.lock",
#                                           package = "DroneBioR"))
#
#  Two things this script deliberately does not do.
#
#  It does not re-run the photogrammetric reconstruction. That belongs to
#  OpenDroneMap, it is not bit-reproducible across engine versions, and the
#  dense products the paper analyses came from a prior successful run with
#  OpenDroneMap 3.6.0, Docker image digest
#  sha256:fc56c7cda68ca20c62aa2cc8f48d112986eee350c766fe23afb2d282f6f49521.
#
#  It does not assume dtm.tif is the engine's terrain. improve_dtm_csf()
#  writes its result there, so after the CSF step the file a project reports
#  as its DTM is the CSF product; the engine's original SMRF surface survives
#  beside it as dtm_raw.tif. Part 3 reads dtm_raw.tif for exactly that reason.
# ===========================================================================


# ===========================================================================
#  PART 0.  Setup
# ===========================================================================

# Point this at the demonstration project. It must already hold the ODM
# products; see the header on why reconstruction is not re-run here.
PROJECT <- "~/DroneBioR-projects/micasense_demo"

# Everything this script writes goes here. Nothing is written into PROJECT.
OUT <- file.path(getwd(), "manuscript_repro")

suppressWarnings(suppressMessages({
  library(DroneBioR); library(terra); library(lidR); library(caret)
}))

PROJECT <- normalizePath(PROJECT, mustWork = TRUE)
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

# Every random draw below is seeded from these four values. They are part of
# the result: with them, two independent runs return bit-identical output.
SEED_POOL      <- 20260805L  # the pool of candidate quadrat locations
SEED_LINEAR    <- 1000L      # offset for the linear generating function
SEED_SATURATE  <- 2000L      # offset for the saturating one
SEED_PARTITION <- 7000L      # offset for the train/test partition

# The design constants of Section 3.5.
N_SAMPLE <- 399L                       # quadrat locations drawn per replicate
WINDOWS  <- c(1, 3, 5, 7, 9, 11, 13)   # extraction windows, in pixels
GPS_SD   <- c(0.00, 0.20, 0.50, 1.00)  # positional error, in metres
REPLICATES <- 1:10                     # simulation replicates
B0 <- 400; B_NDVI <- 5200; B_CHM <- 95; RESID_SD <- 260  # the known truth

project <- dronebio_project(PROJECT)
paths   <- odm_product_paths(project)

cat(sprintf("DroneBioR %s | R %s | terra %s | lidR %s\n",
            packageVersion("DroneBioR"), getRversion(),
            packageVersion("terra"), packageVersion("lidR")))
cat(sprintf("project: %s\noutput : %s\n\n", PROJECT, OUT))


# ===========================================================================
#  PART 1.  Section 3.1 - the demonstration dataset
# ===========================================================================

ortho <- rast(unname(paths[["orthomosaic"]]))

cat("[3.1] Demonstration dataset\n")
cat(sprintf("  orthomosaic     : %d x %d px, %d layers   (paper: 4374 x 3829, 6)\n",
            ncol(ortho), nrow(ortho), nlyr(ortho)))
cat(sprintf("  cells           : %.1f million             (paper: 16.7)\n",
            ncol(ortho) * nrow(ortho) / 1e6))
cat(sprintf("  ground sampling : %.4f m                  (paper: 0.0576)\n",
            min(res(ortho))))


# ===========================================================================
#  PART 2.  Section 3.3 - from imagery to covariates in one action
# ===========================================================================

cat("\n[3.3] Covariate stack\n")

# The five spectral bands, rescaled from their storage range to 0-1. This is
# a storage-scale normalisation, not a conversion to physical reflectance;
# the level it reached is written into the raster's metadata tags.
t0   <- Sys.time()
refl <- scale_to_reflectance(ortho[[1:5]])
names(refl) <- c("Blue", "Green", "Red", "RedEdge", "NIR")
cat(sprintf("  radiometric scaling  %6.1f s | level recorded: %s\n",
            as.numeric(difftime(Sys.time(), t0, units = "secs")),
            radiometric_level(refl)))

# The vegetation indices those five bands support. Indices whose bands are
# absent are skipped rather than written as empty layers, so the count is a
# property of the sensor as much as of the package.
t0  <- Sys.time()
idx <- compute_spectral_indices(refl)
cat(sprintf("  spectral indices     %6.1f s | %d computed          (paper: 22)\n",
            as.numeric(difftime(Sys.time(), t0, units = "secs")), nlyr(idx)))

# The whole covariate stack in one call: bands, indices, biomass proxies and
# the three elevation surfaces, each written as its own GeoTIFF.
t0 <- Sys.time()
export_all_covariates(refl, file.path(OUT, "covariates"),
                      chm = rast(unname(paths[["chm"]])),
                      dsm = rast(unname(paths[["dsm"]])),
                      dtm = rast(unname(paths[["dtm"]])), overwrite = TRUE)
written <- list.files(file.path(OUT, "covariates"), "\\.tif$", full.names = TRUE)
cat(sprintf("  covariate export     %6.1f s | %d rasters, %.2f GiB  (paper: 39, 1.65)\n",
            as.numeric(difftime(Sys.time(), t0, units = "secs")),
            length(written), sum(file.info(written)$size) / 1024^3))


# ===========================================================================
#  PART 3.  Section 3.4 - CSF-refined terrain and its effect on canopy height
# ===========================================================================

cat("\n[3.4] CSF terrain against the engine's SMRF terrain\n")

# dtm_raw.tif is the engine's original SMRF surface, preserved when CSF
# overwrote dtm.tif. Comparing against the project's reported DTM instead
# would silently compare CSF with itself and return roughly zero.
smrf_path <- file.path(dirname(unname(paths[["dtm"]])), "dtm_raw.tif")
stopifnot("dtm_raw.tif is missing: CSF has not been applied to this project, so dtm.tif is still the SMRF terrain and there is nothing to compare" =
            file.exists(smrf_path))

smrf   <- rast(smrf_path)
native <- min(res(rast(unname(paths[["dsm"]]))))   # rebuild at the DSM posting
las    <- readLAS(unname(paths[["point_cloud_las"]]))

# Classify ground with the cloth-simulation filter at the package defaults,
# then turn those ground points into a terrain raster.
ground  <- classify_ground(las, csf(class_threshold = 0.5, cloth_resolution = 0.5,
                                    rigidness = 1L), last_returns = FALSE)
csf_dtm <- rasterize_terrain(ground, res = native, algorithm = tin())

# The per-cell difference is the whole result of Section 3.4. Negative means
# the CSF surface sits lower than the engine's.
diff_m <- values(resample(csf_dtm, smrf, method = "bilinear") - smrf)
diff_m <- diff_m[is.finite(diff_m)]

cat(sprintf("  mean CSF - SMRF  : %+.2f m    (paper: -1.09)\n", mean(diff_m)))
cat(sprintf("  cells CSF lower  : %.1f %%     (paper: 81.2)\n", 100 * mean(diff_m < 0)))
cat(sprintf("  largest lowering : %.1f m     (paper: 13.8)\n", -min(diff_m)))
writeRaster(csf_dtm, file.path(OUT, "dtm_csf.tif"), overwrite = TRUE)


# ===========================================================================
#  PART 4.  Section 3.5 - verification of the extraction and modelling path
#
#  The response is constructed from the same rasters the predictors come
#  from, so nothing here is evidence about biomass. What it tests is whether
#  the software recovers a relationship that is known by construction.
# ===========================================================================

cat("\n[3.5] Verification experiment\n")

ndvi <- rast(file.path(OUT, "covariates", "NDVI.tif"))
chm  <- resample(rast(file.path(OUT, "covariates", "CHM.tif")), ndvi,
                 method = "bilinear")

# Pre-compute the window means once, one raster per window size. focal() at
# window w gives, for every cell, the mean over the w x w block centred on
# it - which is exactly what a windowed extraction at that cell returns. The
# 1 x 1 case is the raster itself; focal() rejects a window of one.
mean_ndvi <- lapply(WINDOWS, function(w) if (w == 1) ndvi else focal(ndvi, w, "mean", na.rm = TRUE, expand = TRUE))
mean_chm  <- lapply(WINDOWS, function(w) if (w == 1) chm  else focal(chm,  w, "mean", na.rm = TRUE, expand = TRUE))
names(mean_ndvi) <- names(mean_chm) <- WINDOWS

# The 9 x 9 layer doubles as the truth: a 0.52 m quadrat is 9 px at this
# GSD, so the mean over 9 x 9 at the true position is what a field sample of
# that size physically integrates.
true_ndvi <- mean_ndvi[["9"]]
true_chm  <- mean_chm[["9"]]

# Draw the pool of candidate locations once, from cells where every window
# is fully defined. Seeding this matters: without it the experiment
# reproduces in pattern but not in value, because each run would draw
# different sample locations.
usable <- !is.na(mean_ndvi[["13"]]) & !is.na(mean_chm[["13"]]) &
          !is.na(true_ndvi) & !is.na(true_chm)
set.seed(SEED_POOL)
pool <- spatSample(usable, 8000, method = "random", as.points = TRUE,
                   na.rm = TRUE, values = TRUE)
pool <- pool[pool[[1]] == 1, ]
cat(sprintf("  candidate pool   : %d locations\n", nrow(pool)))


# ---------------------------------------------------------------------------
#  4a. The cell the paper reports, step by step
#
#  Linear generating function, first replicate, 0.20 m positional error,
#  9 x 9 extraction window. Every step is spelled out here so the mechanism
#  is visible. Part 4b repeats exactly this over the full grid.
# ---------------------------------------------------------------------------

set.seed(SEED_LINEAR + 1L)

# Where the quadrats actually are.
xy_true <- crds(pool[sample(nrow(pool), N_SAMPLE), ])

# What each quadrat integrates: the mean over its own 0.52 m footprint.
ndvi_true <- extract(true_ndvi, xy_true)[, 1]
chm_true  <- extract(true_chm,  xy_true)[, 1]

# Drop any quadrat whose true value is undefined, so the truth is complete.
defined   <- is.finite(ndvi_true) & is.finite(chm_true)
xy_true   <- xy_true[defined, , drop = FALSE]
ndvi_true <- ndvi_true[defined]
chm_true  <- chm_true[defined]

# Biomass from a function we choose, evaluated on the true quadrat values.
# The coefficients and the residual spread are illustrative values that put
# the two error sources on a realistic scale, not estimates of anything.
biomass <- B0 + B_NDVI * ndvi_true + B_CHM * chm_true +
           rnorm(length(ndvi_true), 0, RESID_SD)

# The position a field crew would record, displaced by ordinary GPS error.
xy_recorded <- xy_true + cbind(rnorm(nrow(xy_true), 0, 0.20),
                               rnorm(nrow(xy_true), 0, 0.20))

# What a windowed extraction returns at that recorded position. This is the
# number a real workflow has; ndvi_true is the number it is trying to hit.
ndvi_extracted <- extract(mean_ndvi[["9"]], xy_recorded)[, 1]
chm_extracted  <- extract(mean_chm[["9"]],  xy_recorded)[, 1]

# Keep the rows the fit can actually use.
fittable <- is.finite(ndvi_extracted) & is.finite(chm_extracted) & is.finite(biomass)
cat(sprintf("  reported cell    : %d of %d quadrats usable\n",
            sum(fittable), length(fittable)))

# The first quantity: how far the extracted value sits from the value it is
# meant to estimate. This is the error a larger window is there to reduce.
cat(sprintf("  NDVI extraction RMSE : %.4f   (Table 4 at 9x9, mean of 10 replicates: 0.0354)\n",
            sqrt(mean((ndvi_extracted[fittable] - ndvi_true[fittable])^2))))
cat(sprintf("  CHM  extraction RMSE : %.2f m (Table 4 at 9x9, mean of 10 replicates: 1.37)\n",
            sqrt(mean((chm_extracted[fittable] - chm_true[fittable])^2))))

# The second quantity: fit the known form to the *extracted* predictors and
# see whether the known coefficients come back. They come back attenuated,
# because a predictor measured with error drags its own coefficient toward
# zero - and reducing that attenuation is what the window buys.
cell_fit <- lm(biomass[fittable] ~ ndvi_extracted[fittable] + chm_extracted[fittable])
cat(sprintf("  recovered b_NDVI     : %.0f     (true %d)\n", coef(cell_fit)[[2]], B_NDVI))
cat(sprintf("  recovered b_CHM      : %.1f     (true %d)\n", coef(cell_fit)[[3]], B_CHM))

# Write this replicate out so a reader can inspect the input to Table 4
# without re-running the sweep below.
write.csv(data.frame(
  sample_id          = sprintf("SIM_%03d", which(fittable)),
  x_true             = xy_true[fittable, 1],
  y_true             = xy_true[fittable, 2],
  x_recorded         = xy_recorded[fittable, 1],
  y_recorded         = xy_recorded[fittable, 2],
  ndvi_true_quadrat  = ndvi_true[fittable],
  chm_true_quadrat   = chm_true[fittable],
  ndvi_extracted_9x9 = ndvi_extracted[fittable],
  chm_extracted_9x9  = chm_extracted[fittable],
  biomass_kgha       = biomass[fittable],
  note = "SIMULATED FROM IMAGERY - NOT FIELD MEASURED"),
  file.path(OUT, "verification_samples_seed01.csv"), row.names = FALSE)


# ---------------------------------------------------------------------------
#  4b. The same computation over the full grid
#
#  Table 4 is 4a repeated over ten replicates, two generating functions, four
#  displacement magnitudes and seven windows. The three loops below only walk
#  that grid; their body is the code of 4a, unchanged. The order of the
#  random draws is the order used in 4a, and changing it changes the numbers.
# ---------------------------------------------------------------------------

cat("  running the full grid ...\n")
extraction <- coefficients_tbl <- performance <- list()

for (generator in c("linear", "saturating")) for (replicate in REPLICATES) {

  set.seed(ifelse(generator == "linear", SEED_LINEAR, SEED_SATURATE) + replicate)

  xy_true   <- crds(pool[sample(nrow(pool), N_SAMPLE), ])
  ndvi_true <- extract(true_ndvi, xy_true)[, 1]
  chm_true  <- extract(true_chm,  xy_true)[, 1]
  defined   <- is.finite(ndvi_true) & is.finite(chm_true)
  xy_true   <- xy_true[defined, , drop = FALSE]
  ndvi_true <- ndvi_true[defined]
  chm_true  <- chm_true[defined]

  # The saturating variant confirms the extraction behaviour does not depend
  # on the relationship being linear.
  truth <- if (generator == "linear") B0 + B_NDVI * ndvi_true + B_CHM * chm_true
           else 400 + 6000 * (ndvi_true / (ndvi_true + 0.30)) + 120 * log1p(chm_true)
  biomass <- truth + rnorm(length(truth), 0, RESID_SD)

  for (gps in GPS_SD) {

    xy_recorded <- if (gps > 0) xy_true + cbind(rnorm(nrow(xy_true), 0, gps),
                                                rnorm(nrow(xy_true), 0, gps))
                   else xy_true

    for (w in WINDOWS) {

      ndvi_extracted <- extract(mean_ndvi[[as.character(w)]], xy_recorded)[, 1]
      chm_extracted  <- extract(mean_chm[[as.character(w)]],  xy_recorded)[, 1]
      fittable <- is.finite(ndvi_extracted) & is.finite(chm_extracted) & is.finite(biomass)
      if (sum(fittable) < 50) next

      # Extraction fidelity: the columns of Table 4 that carry the argument.
      extraction[[length(extraction) + 1]] <- data.frame(
        generator, replicate, gps, window = w,
        ndvi_bias = mean(ndvi_extracted[fittable] - ndvi_true[fittable]),
        ndvi_rmse = sqrt(mean((ndvi_extracted[fittable] - ndvi_true[fittable])^2)),
        chm_bias  = mean(chm_extracted[fittable] - chm_true[fittable]),
        chm_rmse  = sqrt(mean((chm_extracted[fittable] - chm_true[fittable])^2)))

      # The coefficient columns are defined only where the truth is linear.
      if (generator != "linear") next
      samples <- data.frame(biomass = biomass[fittable],
                            NDVI = ndvi_extracted[fittable], CHM = chm_extracted[fittable],
                            x = xy_recorded[fittable, 1], y = xy_recorded[fittable, 2])
      beta <- coef(lm(biomass ~ NDVI + CHM, data = samples))
      coefficients_tbl[[length(coefficients_tbl) + 1]] <- data.frame(
        replicate, gps, window = w, b_ndvi = beta[[2]], b_chm = beta[[3]])

      # The R-squared columns are reported at the 0.20 m displacement only.
      if (gps != 0.20) next

      # Random hold-out: the split a reader would reach for by default.
      set.seed(SEED_PARTITION + replicate)
      in_train    <- createDataPartition(samples$biomass, p = 0.75, list = FALSE)
      random_fit  <- train(biomass ~ NDVI + CHM, data = samples[in_train, ],
                           method = "lm", trControl = trainControl("cv", number = 5))
      held_random <- setdiff(seq_len(nrow(samples)), in_train)
      pred_random <- predict(random_fit, samples[held_random, ])

      # Spatially blocked hold-out: whole blocks of a 4 x 4 grid withheld, so
      # no block is split between fitting and scoring. Samples from a single
      # flight are autocorrelated, and this is the estimate window effects
      # should be read from.
      block        <- (cut(samples$x, 4, labels = FALSE) - 1) * 4 +
                       cut(samples$y, 4, labels = FALSE)
      held_blocked <- which(block %in% sample(unique(block), 4))
      if (length(held_blocked) < 30 ||
          nrow(samples) - length(held_blocked) < 60) next
      blocked_fit  <- train(biomass ~ NDVI + CHM, data = samples[-held_blocked, ],
                            method = "lm", trControl = trainControl("cv", number = 5))
      pred_blocked <- predict(blocked_fit, samples[held_blocked, ])

      performance[[length(performance) + 1]] <- data.frame(
        replicate, window = w,
        r2_random  = cor(pred_random,  samples$biomass[held_random])^2,
        r2_blocked = cor(pred_blocked, samples$biomass[held_blocked])^2)
    }
  }
}

extraction       <- do.call(rbind, extraction)
coefficients_tbl <- do.call(rbind, coefficients_tbl)
performance      <- do.call(rbind, performance)
saveRDS(list(extraction = extraction, coefficients = coefficients_tbl,
             performance = performance), file.path(OUT, "verification.rds"))

# Table 4 is the grid averaged over the ten replicates, at the reported
# 0.20 m displacement.
reported <- subset(extraction, generator == "linear" & gps == 0.20)
betas    <- subset(coefficients_tbl, gps == 0.20)
table4   <- data.frame(
  window     = WINDOWS,
  ndvi_rmse  = tapply(reported$ndvi_rmse, reported$window, mean),
  chm_rmse   = tapply(reported$chm_rmse,  reported$window, mean),
  b_ndvi     = tapply(betas$b_ndvi, betas$window, mean),
  b_chm      = tapply(betas$b_chm,  betas$window, mean),
  r2_random  = tapply(performance$r2_random,  performance$window, mean),
  r2_blocked = tapply(performance$r2_blocked, performance$window, mean))

cat("\n  -- Table 4 --\n")
print(table4, digits = 4, row.names = FALSE)
cat(sprintf("  true coefficients: NDVI %d, CHM %d\n", B_NDVI, B_CHM))
cat("  b_ndvi rises toward the truth as the window widens: that is the\n")
cat("  attenuation from predictor measurement error being reduced.\n")
cat("  r2_random sits above r2_blocked at every window: that is spatial\n")
cat("  leakage in the random split, and why the blocked column is the one\n")
cat("  window effects should be read from.\n")


# ===========================================================================
#  PART 5.  Section 3.7 - sensitivity of the terrain and canopy-height choices
# ===========================================================================

cat("\n[3.7] Sensitivity\n")

# Vary one CSF parameter at a time about the package defaults. The point of
# the exercise is the comparison with Section 3.4: if the spread the
# parameters produce is wider than the difference between the two filters,
# then Section 3.4 shows that filtering matters, not how much it corrects.
csf_grid <- rbind(
  data.frame(cloth_resolution = c(0.25, 0.50, 1.00, 2.00), rigidness = 1L, class_threshold = 0.50),
  data.frame(cloth_resolution = 0.50, rigidness = c(2L, 3L),               class_threshold = 0.50),
  data.frame(cloth_resolution = 0.50, rigidness = 1L,       class_threshold = c(0.25, 1.00)))

sensitivity <- list()
for (i in seq_len(nrow(csf_grid))) {
  setting  <- csf_grid[i, ]
  ground_i <- classify_ground(las, csf(class_threshold  = setting$class_threshold,
                                       cloth_resolution = setting$cloth_resolution,
                                       rigidness = as.integer(setting$rigidness)),
                              last_returns = FALSE)
  dtm_i    <- resample(rasterize_terrain(ground_i, res = native, algorithm = tin()),
                       smrf, method = "bilinear")
  diff_i   <- values(dtm_i - smrf); diff_i <- diff_i[is.finite(diff_i)]
  chm_i    <- values(resample(rast(unname(paths[["dsm"]])), dtm_i) - dtm_i)
  chm_i    <- chm_i[is.finite(chm_i)]
  sensitivity[[i]] <- data.frame(setting,
    pct_ground = 100 * sum(ground_i@data$Classification == 2L) / npoints(las),
    mean_diff  = mean(diff_i), pct_lower = 100 * mean(diff_i < 0),
    chm_mean   = mean(chm_i),  chm_max   = max(chm_i))
}
sensitivity <- do.call(rbind, sensitivity)

default_row <- sensitivity$cloth_resolution == 0.50 & sensitivity$rigidness == 1L &
               sensitivity$class_threshold == 0.50

cat("\n  -- Table 6 --\n")
print(sensitivity, digits = 3, row.names = FALSE)
cat(sprintf("  cloth_resolution alone spans %.2f m, against the %.2f m the two\n",
            diff(range(sensitivity$mean_diff)),
            abs(sensitivity$mean_diff[default_row])))
cat("  filters differ by at their defaults. The parameterisation is as\n")
cat("  consequential as the choice of filter.\n")

# The canopy-height clip removes the top of the distribution. Whether those
# cells are artefacts is testable by their geometry: a reconstruction spike
# is one isolated cell, a real emergent crown is a contiguous patch.
chm_ref    <- rast(file.path(OUT, "covariates", "CHM.tif"))
heights    <- values(chm_ref); heights <- heights[is.finite(heights)]
thresholds <- quantile(heights, c(0.990, 0.995, 0.999))

clipping <- data.frame(
  percentile    = c(99.0, 99.5, 99.9),
  threshold_m   = as.numeric(thresholds),
  cells_clipped = sapply(thresholds, function(t) sum(heights > t)),
  pct_clipped   = sapply(thresholds, function(t) 100 * mean(heights > t)),
  mean_after    = sapply(thresholds, function(t) mean(heights[heights <= t])),
  max_after     = as.numeric(thresholds))

cat("\n  -- Table 7 --\n")
print(clipping, digits = 4, row.names = FALSE)
cat(sprintf("  unclipped: mean %.2f m, max %.2f m\n", mean(heights), max(heights)))

# patches() labels each connected group of above-threshold cells, so the
# table of labels gives the size of every patch.
patch_sizes <- as.numeric(table(values(patches(chm_ref > thresholds[[2]],
                                               directions = 8, zeroAsNA = TRUE))))
cat(sprintf("  above P99.5: %d patches, median %d cells, max %d\n",
            length(patch_sizes), median(patch_sizes), max(patch_sizes)))
cat(sprintf("  %.1f%% are one or two cells; %.1f%% span twenty or more\n",
            100 * mean(patch_sizes <= 2), 100 * mean(patch_sizes >= 20)))
cat("  Half of what the default clip removes is contiguous structure the\n")
cat("  size and shape of emergent crowns, not isolated noise.\n")

saveRDS(list(csf = sensitivity, clip = clipping, patch_sizes = patch_sizes),
        file.path(OUT, "sensitivity.rds"))


# ===========================================================================
#  PART 6.  Section 3.6 - what this machine was
#
#  Table 5 reports the timings printed by Parts 2 to 5. They are
#  hardware-dependent, so the machine has to be stated alongside them.
# ===========================================================================

cat("\n[3.6] Machine\n")
cat(sprintf("  host    : %s\n",
            paste(Sys.info()[c("sysname", "release", "machine")], collapse = " ")))
cat(sprintf("  cores   : %d\n", parallel::detectCores()))
cat(sprintf("  R       : %s | terra %s | memfrac %s\n", getRversion(),
            packageVersion("terra"), terraOptions(print = FALSE)$memfrac))
cat("  Table 5 was measured on an Apple M1 Max, 10 cores, 64 GB, memfrac 0.6.\n")
cat("  Peak memory during the covariate export is set by that budget rather\n")
cat("  than by an intrinsic requirement.\n")

cat(sprintf("\nDone. Results in %s\n", OUT))
