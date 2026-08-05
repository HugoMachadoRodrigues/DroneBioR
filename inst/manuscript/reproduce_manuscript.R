#!/usr/bin/env Rscript
# ===========================================================================
#  reproduce_manuscript.R
#
#  Regenerates every number, figure and table of Section 3 of
#
#     Rodrigues, H.M. & Golmohammadi, G. "DroneBioR: A Reproducible R Package
#     for Drone-Based Biomass Workflows from Multispectral Imagery", Drones.
#
#  Usage
#  -----
#     Rscript reproduce_manuscript.R --project=/path/to/imagens [--out=DIR]
#                                    [--only=covariates,csf,verify,sensitivity,cost]
#
#  --project  the DroneBioR project folder holding the demonstration survey.
#             It must already contain the reconstructed ODM products; this
#             script does NOT re-run photogrammetry (see NOTE below).
#  --out      where results are written. Default: ./manuscript_repro
#  --only     comma-separated subset of blocks to run. Default: all.
#
#  Environment
#  -----------
#  Restore the recorded package versions first:
#
#     renv::restore(lockfile = system.file("manuscript", "renv.lock",
#                                          package = "DroneBioR"))
#
#  The lockfile declares the r-lidar r-universe repository, because lidR and
#  rlas were removed from CRAN on 2026-06-09 and are no longer installable
#  from CRAN.
#
#  NOTE on the reconstruction step
#  -------------------------------
#  Photogrammetric reconstruction is delegated to OpenDroneMap and is NOT
#  re-run here: it is the engine's cost, not the package's, and its output is
#  not bit-reproducible across engine versions. The products used in the paper
#  were built with OpenDroneMap 3.6.0 from the `opendronemap/odm` Docker image
#  with digest
#     sha256:fc56c7cda68ca20c62aa2cc8f48d112986eee350c766fe23afb2d282f6f49521
#  and the settings printed by `--only=cost`. To rebuild them, see
#  run_dronebio_workflow() in the package reference.
# ===========================================================================

suppressWarnings(suppressMessages({
  library(DroneBioR); library(terra); library(lidR); library(caret)
}))

## ---- arguments -----------------------------------------------------------
args    <- commandArgs(TRUE)
getarg  <- function(k, default = NULL) {
  hit <- grep(paste0("^--", k, "="), args, value = TRUE)
  if (!length(hit)) return(default)
  sub(paste0("^--", k, "="), "", hit[[1]])
}
PROJECT <- getarg("project")
OUT     <- getarg("out", file.path(getwd(), "manuscript_repro"))
ONLY    <- strsplit(getarg("only", "covariates,csf,verify,sensitivity,cost"), ",")[[1]]
if (is.null(PROJECT) || !dir.exists(PROJECT))
  stop("--project=<folder> is required and must exist.", call. = FALSE)
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
run <- function(tag) tag %in% ONLY

## ---- fixed seeds ---------------------------------------------------------
## Every random draw in Section 3 is seeded here. Changing these changes the
## numbers; leaving them reproduces the published tables exactly.
SEEDS <- list(
  pool       = 20260805L, # the candidate-location pool drawn once from the rasters
  simulation = 1:10,      # ten simulation replicates (Section 3.5, Table 4)
  partition  = 7000L,     # offset for the train/test partition within each seed
  gen_offset = c(linear = 1000L, saturating = 2000L)
)
QUAD_PX  <- 9L                       # 0.52 m quadrat at 5.76 cm GSD
WINDOWS  <- c(1, 3, 5, 7, 9, 11, 13)
GPS_SD   <- c(0.00, 0.20, 0.50, 1.00)
N_SAMPLE <- 399L
BETA     <- c(intercept = 400, ndvi = 5200, chm = 95, resid_sd = 260)

say <- function(...) cat(sprintf(...), "\n")
stamp <- function(lbl, expr) {
  t0 <- Sys.time(); v <- force(expr)
  say("  %-42s %7.1f s", lbl, as.numeric(difftime(Sys.time(), t0, units = "secs")))
  invisible(v)
}

p       <- dronebio_project(PROJECT)
paths   <- odm_product_paths(p)
COV_DIR <- file.path(OUT, "covariates")

say("DroneBioR %s | R %s | terra %s | lidR %s",
    packageVersion("DroneBioR"), getRversion(),
    packageVersion("terra"), packageVersion("lidR"))
say("project: %s", PROJECT)
say("output : %s\n", OUT)

## ===========================================================================
## 1. Section 3.3 - covariate stack (39 rasters)
## ===========================================================================
if (run("covariates")) {
  say("[covariates] Section 3.3")
  ortho <- rast(unname(paths[["orthomosaic"]]))
  refl  <- stamp("radiometric scaling", scale_to_reflectance(ortho[[1:5]]))
  names(refl) <- c("Blue", "Green", "Red", "RedEdge", "NIR")
  idx   <- stamp("spectral indices", compute_spectral_indices(refl))
  say("  indices computed: %d  (expected 22, Table S1)", nlyr(idx))
  chm <- rast(unname(paths[["chm"]]))
  stamp("export covariate GeoTIFFs",
        export_all_covariates(refl, COV_DIR, chm = chm,
                              dsm = rast(unname(paths[["dsm"]])),
                              dtm = rast(unname(paths[["dtm"]])), overwrite = TRUE))
  f <- list.files(COV_DIR, pattern = "\\.tif$", full.names = TRUE)
  say("  wrote %d rasters, %.2f GiB  (expected 39, 1.65 GiB)",
      length(f), sum(file.info(f)$size) / 1024^3)
}

## ===========================================================================
## 2. Section 3.4 - CSF vs SMRF terrain, and Figure 4
## ===========================================================================
if (run("csf")) {
  say("\n[csf] Section 3.4")
  las  <- readLAS(unname(paths[["point_cloud_las"]]))
  ## IMPORTANT: improve_dtm_csf() writes its result to dtm.tif, so the file the
  ## project reports as "dtm" is the CSF terrain once step 4 has run. The
  ## engine's original SMRF terrain is preserved beside it as dtm_raw.tif, and
  ## that is the correct baseline for this comparison.
  raw  <- file.path(dirname(unname(paths[["dtm"]])), "dtm_raw.tif")
  if (!file.exists(raw))
    stop("dtm_raw.tif not found: this project has not had CSF applied, so ",
         "dtm.tif is still the SMRF terrain and there is nothing to compare.",
         call. = FALSE)
  smrf   <- rast(raw)
  NATIVE <- min(res(rast(unname(paths[["dsm"]]))))
  say("  SMRF baseline    : %s", basename(raw))
  say("  CSF resolution   : %.4f m (DSM native)", NATIVE)
  cl   <- stamp("CSF ground classification",
                classify_ground(las, csf(class_threshold = 0.5,
                                         cloth_resolution = 0.5,
                                         rigidness = 1L), last_returns = FALSE))
  dtm  <- stamp("rasterize terrain",
                rasterize_terrain(cl, res = NATIVE, algorithm = tin()))
  d    <- resample(dtm, smrf, method = "bilinear") - smrf
  v    <- as.numeric(values(d)); v <- v[is.finite(v)]
  say("  mean CSF - SMRF   : %+.2f m   (paper: -1.09 m)", mean(v))
  say("  cells CSF lower   : %.1f%%    (paper: 81.2%%)", 100 * mean(v < 0))
  say("  largest lowering  : %.1f m    (paper: 13.8 m)", -min(v))
  writeRaster(dtm, file.path(OUT, "dtm_csf.tif"), overwrite = TRUE)
}

## ===========================================================================
## 3. Section 3.5 - verification experiment (Figure 5, Table 4)
## ===========================================================================
if (run("verify")) {
  say("\n[verify] Section 3.5")
  ndvi <- rast(file.path(COV_DIR, "NDVI.tif")); chm <- rast(file.path(COV_DIR, "CHM.tif"))
  if (!file.exists(file.path(COV_DIR, "NDVI.tif")))
    stop("run --only=covariates first, or point --out at an existing run.", call. = FALSE)
  chm <- resample(chm, ndvi, method = "bilinear")
  fmean <- function(r, w) if (w <= 1L) r else focal(r, w = w, fun = "mean", na.rm = TRUE, expand = TRUE)
  fm <- lapply(stats::setNames(unique(c(WINDOWS, QUAD_PX)), unique(c(WINDOWS, QUAD_PX))),
               function(w) list(ndvi = fmean(ndvi, w), chm = fmean(chm, w)))
  qd <- fm[[as.character(QUAD_PX)]]
  ok <- !is.na(fm[["13"]]$ndvi) & !is.na(fm[["13"]]$chm) & !is.na(qd$ndvi) & !is.na(qd$chm)
  ## The candidate pool must be seeded too: without this the sample locations
  ## differ between runs and the table is only reproducible in pattern, not value.
  set.seed(SEEDS$pool)
  pool <- spatSample(ok, 8000, method = "random", as.points = TRUE, na.rm = TRUE, values = TRUE)
  pool <- pool[pool[[1]] == 1, ]

  extr <- coefs <- perf <- list()
  for (gen in c("linear", "saturating")) for (s in SEEDS$simulation) {
    set.seed(SEEDS$gen_offset[[gen]] + s)
    sel <- pool[sample(nrow(pool), N_SAMPLE), ]; xy0 <- crds(sel)
    nd_t <- extract(qd$ndvi, xy0)[, 1]; ch_t <- extract(qd$chm, xy0)[, 1]
    keep <- is.finite(nd_t) & is.finite(ch_t)
    xy0 <- xy0[keep, , drop = FALSE]; nd_t <- nd_t[keep]; ch_t <- ch_t[keep]
    truth <- if (gen == "linear") BETA[["intercept"]] + BETA[["ndvi"]] * nd_t + BETA[["chm"]] * ch_t
             else 400 + 6000 * (nd_t / (nd_t + 0.30)) + 120 * log1p(ch_t)
    biomass <- truth + rnorm(length(truth), 0, BETA[["resid_sd"]])
    for (g in GPS_SD) {
      xy <- if (g > 0) xy0 + cbind(rnorm(nrow(xy0), 0, g), rnorm(nrow(xy0), 0, g)) else xy0
      for (w in WINDOWS) {
        ne <- extract(fm[[as.character(w)]]$ndvi, xy)[, 1]
        ce <- extract(fm[[as.character(w)]]$chm,  xy)[, 1]
        good <- is.finite(ne) & is.finite(ce) & is.finite(biomass)
        if (sum(good) < 50) next
        extr[[length(extr) + 1]] <- data.frame(gen, seed = s, gps = g, window = w,
          ndvi_bias = mean(ne[good] - nd_t[good]),
          ndvi_rmse = sqrt(mean((ne[good] - nd_t[good])^2)),
          chm_bias  = mean(ce[good] - ch_t[good]),
          chm_rmse  = sqrt(mean((ce[good] - ch_t[good])^2)))
        if (gen != "linear") next
        d <- data.frame(biomass = biomass[good], NDVI = ne[good], CHM = ce[good],
                        x = xy[good, 1], y = xy[good, 2])
        cf <- coef(lm(biomass ~ NDVI + CHM, data = d))
        coefs[[length(coefs) + 1]] <- data.frame(seed = s, gps = g, window = w,
          b0 = cf[[1]], b_ndvi = cf[[2]], b_chm = cf[[3]])
        if (g != 0.20) next
        set.seed(SEEDS$partition + s)
        idx <- createDataPartition(d$biomass, p = 0.75, list = FALSE)
        ev <- function(tr, te) {
          m <- train(biomass ~ NDVI + CHM, data = d[tr, ], method = "lm",
                     trControl = trainControl(method = "cv", number = 5))
          pr <- predict(m, d[te, ])
          c(r2 = cor(pr, d$biomass[te])^2, rmse = sqrt(mean((pr - d$biomass[te])^2)))
        }
        rnd <- ev(idx, setdiff(seq_len(nrow(d)), idx))
        blk <- (cut(d$x, 4, labels = FALSE) - 1) * 4 + cut(d$y, 4, labels = FALSE)
        hold <- blk %in% sample(unique(blk), 4)
        if (sum(hold) >= 30 && sum(!hold) >= 60) {
          spa <- ev(which(!hold), which(hold))
          perf[[length(perf) + 1]] <- data.frame(seed = s, window = w, split = "random",
                                                 r2 = rnd[["r2"]], rmse = rnd[["rmse"]])
          perf[[length(perf) + 1]] <- data.frame(seed = s, window = w, split = "spatial_block",
                                                 r2 = spa[["r2"]], rmse = spa[["rmse"]])
        }
      }
    }
    say("  done %s seed %d", gen, s)
  }
  E <- do.call(rbind, extr); C <- do.call(rbind, coefs); P <- do.call(rbind, perf)
  saveRDS(list(extraction = E, coefficients = C, performance = P),
          file.path(OUT, "verification.rds"))
  say("\n  -- Table 4: extraction fidelity and coefficient recovery --")
  ce <- subset(C, gps == 0.20)
  tab <- do.call(rbind, lapply(split(ce, ce$window), function(z) data.frame(
    window = z$window[1],
    ndvi_rmse = mean(subset(E, gen == "linear" & gps == 0.20 & window == z$window[1])$ndvi_rmse),
    b_ndvi = mean(z$b_ndvi), lo = min(z$b_ndvi), hi = max(z$b_ndvi), b_chm = mean(z$b_chm))))
  print(tab, digits = 4, row.names = FALSE)
  say("  (true coefficients: NDVI %g, CHM %g)", BETA[["ndvi"]], BETA[["chm"]])
  say("\n  -- random vs spatially blocked hold-out --")
  print(do.call(rbind, lapply(split(P, list(P$window, P$split), drop = TRUE), function(z)
    data.frame(window = z$window[1], split = z$split[1], r2 = mean(z$r2)))),
    digits = 3, row.names = FALSE)
}

## ===========================================================================
## 4. Section 3.7 - sensitivity of CSF parameters and the CHM clip
##                  (Tables 6 and 7)
## ===========================================================================
if (run("sensitivity")) {
  say("\n[sensitivity] Section 3.7")
  las  <- readLAS(unname(paths[["point_cloud_las"]]))
  dsm  <- rast(unname(paths[["dsm"]]))
  smrf <- rast(file.path(dirname(unname(paths[["dtm"]])), "dtm_raw.tif"))  # see [csf]
  NATIVE <- min(res(dsm))
  grid <- rbind(
    data.frame(cloth_resolution = c(0.25, 0.50, 1.00, 2.00), rigidness = 1L,  class_threshold = 0.50),
    data.frame(cloth_resolution = 0.50, rigidness = c(2L, 3L),                class_threshold = 0.50),
    data.frame(cloth_resolution = 0.50, rigidness = 1L,       class_threshold = c(0.25, 1.00)))
  A <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
    g  <- grid[i, ]
    cl <- classify_ground(las, csf(class_threshold = g$class_threshold,
                                   cloth_resolution = g$cloth_resolution,
                                   rigidness = as.integer(g$rigidness)), last_returns = FALSE)
    d2  <- resample(rasterize_terrain(cl, res = NATIVE, algorithm = tin()), smrf, method = "bilinear")
    dif <- as.numeric(values(d2 - smrf)); dif <- dif[is.finite(dif)]
    hv  <- as.numeric(values(resample(dsm, d2) - d2)); hv <- hv[is.finite(hv)]
    data.frame(g, pct_ground = 100 * sum(cl@data$Classification == 2L) / npoints(las),
               mean_diff = mean(dif), pct_lower = 100 * mean(dif < 0),
               chm_mean = mean(hv), chm_max = max(hv))
  }))
  print(A, digits = 3, row.names = FALSE)

  chm <- rast(file.path(COV_DIR, "CHM.tif"))
  v <- as.numeric(values(chm)); v <- v[is.finite(v)]
  B <- do.call(rbind, lapply(c(99.0, 99.5, 99.9), function(q) {
    thr <- unname(quantile(v, q / 100)); kept <- v[v <= thr]
    data.frame(percentile = q, threshold_m = thr, cells_clipped = sum(v > thr),
               pct = 100 * mean(v > thr), mean_after = mean(kept), max_after = max(kept))
  }))
  B <- rbind(B, data.frame(percentile = NA, threshold_m = NA, cells_clipped = 0,
                           pct = 0, mean_after = mean(v), max_after = max(v)))
  print(B, digits = 4, row.names = FALSE)
  # are the clipped cells isolated spikes, or contiguous crowns?
  pat <- patches(chm > unname(quantile(v, 0.995)), directions = 8, zeroAsNA = TRUE)
  sz  <- as.numeric(table(values(pat)[is.finite(values(pat))]))
  say("  above-P99.5: %d patches | median %d cells | max %d | %.1f%% are 1-2 cells | %.1f%% are >=20",
      length(sz), median(sz), max(sz), 100 * mean(sz <= 2), 100 * mean(sz >= 20))
  saveRDS(list(csf = A, clip = B, patch_sizes = sz), file.path(OUT, "sensitivity.rds"))
}

## ===========================================================================
## 5. Section 3.6 - computational cost (Table 5)
## ===========================================================================
if (run("cost")) {
  say("\n[cost] Section 3.6 - hardware and settings actually used")
  say("  host   : %s", paste(Sys.info()[c("sysname", "release", "machine")], collapse = " "))
  say("  cores  : %d", parallel::detectCores())
  say("  R      : %s | terra %s | memfrac %s", getRversion(),
      packageVersion("terra"), terraOptions(print = FALSE)$memfrac)
  say("  NOTE   : Table 5 timings were taken on an Apple M1 Max (10 cores, 64 GB)")
  say("           with terraOptions(memfrac = 0.6). Peak RSS is governed by that")
  say("           budget, not by an intrinsic requirement.")
  say("  NOTE   : the ODM reconstruction row is not reproduced here; see the")
  say("           header of this script for the engine version and image digest.")
}

say("\nDone. Results in %s", OUT)
