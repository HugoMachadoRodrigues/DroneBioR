# ---------------------------------------------------------------------------
# Sensitivity analysis (reviewer points 8 and 13):
#   A. CSF parameters -> terrain surface and the CHM derived from it
#   B. CHM upper-tail clipping -> how many cells, and does it remove real crowns?
# ---------------------------------------------------------------------------
suppressMessages({library(lidR); library(terra)})

# Set PROJECT to the demonstration project and OUTDIR to where the figure
# should be written. Both are the only paths this script needs.
PROJECT <- Sys.getenv("DRONEBIOR_PROJECT", "~/DroneBioR-projects/micasense_demo")
OUTDIR  <- Sys.getenv("DRONEBIOR_FIGDIR",  file.path(getwd(), "figures"))
PROJECT <- normalizePath(PROJECT, mustWork = TRUE)
dir.create(OUTDIR, recursive = TRUE, showWarnings = FALSE)
COV  <- file.path(PROJECT, "covariates")

LAS  <- file.path(PROJECT, "imagens/outputs/odm_micasense_dataset/micasense/odm_georeferencing/odm_georeferenced_model.las")
DEM  <- file.path(PROJECT, "outputs/odm_micasense_dataset/micasense/odm_dem")
las  <- readLAS(LAS)
NATIVE <- min(res(rast(file.path(DEM,"dsm.tif"))))
cat("native resolution:", round(NATIVE,4), "m\n")
dsm  <- rast(file.path(DEM,"dsm.tif"))
# dtm.tif was overwritten by improve_dtm_csf(); dtm_raw.tif is the engine's SMRF terrain
smrf <- rast(file.path(DEM,"dtm_raw.tif"))
stopifnot(file.exists(file.path(DEM,"dtm_raw.tif")))
cat("points:", npoints(las), "\n\n")

# ---- A. CSF parameter grid ------------------------------------------------
grid <- expand.grid(cloth_resolution = c(0.25, 0.5, 1.0, 2.0),
                    rigidness        = c(1L, 2L, 3L),
                    class_threshold  = c(0.25, 0.5, 1.0))
grid <- subset(grid, (rigidness == 1L & class_threshold == 0.5) |
                     (cloth_resolution == 0.5 & class_threshold == 0.5) |
                     (cloth_resolution == 0.5 & rigidness == 1L))
rows <- list()
for (i in seq_len(nrow(grid))) {
  g <- grid[i,]
  cl <- classify_ground(las, csf(class_threshold = g$class_threshold,
                                 cloth_resolution = g$cloth_resolution,
                                 rigidness = as.integer(g$rigidness)), last_returns = FALSE)
  ng <- sum(cl@data$Classification == 2L)
  dtm <- rasterize_terrain(cl, res = NATIVE, algorithm = tin())
  d2  <- resample(dtm, smrf, method = "bilinear")
  dif <- as.numeric(values(d2 - smrf)); dif <- dif[is.finite(dif)]
  chm <- resample(dsm, d2) - d2
  hv  <- as.numeric(values(chm)); hv <- hv[is.finite(hv)]
  rows[[i]] <- data.frame(cloth_resolution=g$cloth_resolution, rigidness=g$rigidness,
    class_threshold=g$class_threshold, ground_pts=ng, pct_ground=100*ng/npoints(las),
    mean_diff_vs_smrf=mean(dif), pct_lower=100*mean(dif<0),
    chm_mean=mean(hv), chm_p99=unname(quantile(hv,0.99)), chm_max=max(hv))
  cat(sprintf("  cr=%.2f rig=%d ct=%.2f | ground %5.1f%% | dCSF-SMRF %6.2f m | CHM mean %5.2f max %6.2f\n",
      g$cloth_resolution, g$rigidness, g$class_threshold, 100*ng/npoints(las),
      mean(dif), mean(hv), max(hv)))
}
A <- do.call(rbind, rows)

# ---- B. CHM upper-tail clipping -------------------------------------------
chm_ref <- rast(file.path(PROJECT, "covariates/CHM.tif"))
v <- as.numeric(values(chm_ref)); v <- v[is.finite(v)]
cat("\nCHM cells:", length(v), "| max", round(max(v),2), "m\n")
brows <- list()
for (p in c(99.0, 99.5, 99.9, 100)) {
  thr <- if (p >= 100) Inf else unname(quantile(v, p/100))
  n_clip <- sum(v > thr)
  kept <- v[v <= thr]
  brows[[length(brows)+1]] <- data.frame(percentile=p, threshold_m=ifelse(is.finite(thr),thr,NA),
    cells_clipped=n_clip, pct_clipped=100*n_clip/length(v),
    mean_after=mean(kept), max_after=max(kept))
  cat(sprintf("  P%.1f -> thr %6.2f m | clipped %6d cells (%.3f%%) | mean %.3f max %6.2f\n",
      p, ifelse(is.finite(thr),thr,NA), n_clip, 100*n_clip/length(v), mean(kept), max(kept)))
}
B <- do.call(rbind, brows)
# are the clipped cells isolated spikes or contiguous crowns?
thr995 <- unname(quantile(v, 0.995))
mask <- chm_ref > thr995
pat <- patches(mask, directions = 8, zeroAsNA = TRUE)
sz  <- as.numeric(table(values(pat)[is.finite(values(pat))]))
cat(sprintf("\nAbove-P99.5 cells form %d connected patches; median %d cells, 90th pct %d, max %d\n",
    length(sz), median(sz), round(quantile(sz,0.9)), max(sz)))
cat(sprintf("patches of 1-2 cells: %.1f%% | patches >= 20 cells: %.1f%%\n",
    100*mean(sz<=2), 100*mean(sz>=20)))
saveRDS(list(csf=A, clip=B, patch_sizes=sz), "sens.rds")
cat("\nsaved\n")
