#!/usr/bin/env Rscript
# Figure 4 - what CSF ground refinement does to the terrain, and therefore to
# the canopy height derived from it.
#
#   DRONEBIOR_PROJECT=/path/to/project DRONEBIOR_FIGDIR=/path/to/figures \
#     Rscript fig4_csf_vs_smrf.R
#
# The baseline is dtm_raw.tif, the engine's original SMRF terrain, preserved
# when improve_dtm_csf() overwrote dtm.tif. Comparing against the path the
# project reports as its DTM would compare CSF with itself.

suppressWarnings(suppressMessages({
  library(DroneBioR); library(terra); library(lidR)
}))

PROJECT <- Sys.getenv("DRONEBIOR_PROJECT", "~/DroneBioR-projects/micasense_demo")
OUTDIR  <- Sys.getenv("DRONEBIOR_FIGDIR",  file.path(getwd(), "figures"))
PROJECT <- normalizePath(PROJECT, mustWork = TRUE)
dir.create(OUTDIR, recursive = TRUE, showWarnings = FALSE)
COV <- file.path(PROJECT, "covariates")
OUT <- file.path(OUTDIR, "fig4_csf_vs_smrf.png")

paths <- odm_product_paths(dronebio_project(PROJECT))
smrf  <- rast(file.path(dirname(unname(paths[["dtm"]])), "dtm_raw.tif"))
dsm   <- rast(unname(paths[["dsm"]]))
native <- min(res(dsm))

# Rebuild the CSF terrain at the DSM's own posting, exactly as Section 3.4 does.
ground  <- classify_ground(readLAS(unname(paths[["point_cloud_las"]])),
                           csf(class_threshold = 0.5, cloth_resolution = 0.5,
                               rigidness = 1L), last_returns = FALSE)
csf_dtm <- resample(rasterize_terrain(ground, res = native, algorithm = tin()),
                    smrf, method = "bilinear")
diff_r  <- csf_dtm - smrf
d <- values(diff_r); d <- d[is.finite(d)]

cat(sprintf("mean %.2f m | %.1f%% lower | extreme %.1f m | SD %.2f | median %.2f\n",
            mean(d), 100 * mean(d < 0), min(d), sd(d), median(d)))

png(OUT, width = 2600, height = 1500, res = 200)
layout(matrix(1:4, 2, byrow = TRUE))
par(family = "serif", cex.main = 1.15)

# (a) the distribution of the per-cell difference
par(mar = c(4.6, 4.8, 3.2, 1.2))
hist(d[d > -12 & d < 3], breaks = 90, col = "#2f6b45", border = NA,
     main = "(a) Ground difference: CSF - SMRF",
     xlab = "Elevation difference (m)", ylab = "Cells")
abline(v = 0, col = "#b0242a", lwd = 2.4, lty = 2)
abline(v = mean(d), col = "#22405e", lwd = 2.4)
legend("topleft", c("0 (identical)", sprintf("mean = %.2f m", mean(d))),
       col = c("#b0242a", "#22405e"), lty = c(2, 1), lwd = 2.4, bty = "n")

# (b) the orthomosaic, so the reader can see what the site is
par(mar = c(0.4, 0.4, 3.2, 0.4))
rgb_stack <- c(rast(file.path(COV, "Red.tif")), rast(file.path(COV, "Green.tif")),
               rast(file.path(COV, "Blue.tif")))
plotRGB(aggregate(rgb_stack, 8, "mean", na.rm = TRUE), stretch = "lin",
        axes = FALSE, mar = c(0.4, 0.4, 3.2, 0.4))
title("(b) Orthomosaic (site reference)")

# (c) where the two surfaces disagree
par(mar = c(0.4, 0.4, 3.2, 3.4))
plot(aggregate(diff_r, 8, "mean", na.rm = TRUE),
     col = hcl.colors(100, "Blue-Red 3"), axes = FALSE,
     mar = c(0.4, 0.4, 3.2, 3.4), plg = list(title = "m", cex = 0.8))
title("(c) Where CSF lowers the ground (m)")

# (d) the canopy height model the refined terrain produces
par(mar = c(0.4, 0.4, 3.2, 3.4))
plot(aggregate(rast(file.path(COV, "CHM.tif")), 8, "mean", na.rm = TRUE),
     col = hcl.colors(100, "Viridis"), axes = FALSE,
     mar = c(0.4, 0.4, 3.2, 3.4), plg = list(title = "m", cex = 0.8))
title("(d) Resulting canopy height model (m)")
dev.off()
cat("wrote", OUT, "\n")
