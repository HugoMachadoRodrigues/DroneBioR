suppressMessages({library(DroneBioR); library(lidR); library(terra)})

# Set PROJECT to the demonstration project and OUTDIR to where the figure
# should be written. Both are the only paths this script needs.
PROJECT <- Sys.getenv("DRONEBIOR_PROJECT", "~/DroneBioR-projects/micasense_demo")
OUTDIR  <- Sys.getenv("DRONEBIOR_FIGDIR",  file.path(getwd(), "figures"))
PROJECT <- normalizePath(PROJECT, mustWork = TRUE)
dir.create(OUTDIR, recursive = TRUE, showWarnings = FALSE)
COV  <- file.path(PROJECT, "covariates")

# resolve the point cloud through the package rather than a literal path
LAS  <- unname(odm_product_paths(dronebio_project(PROJECT))[["point_cloud_las"]])
OUT  <- file.path(OUTDIR, "fig2_3d_capabilities.png")
las <- readLAS(LAS); xyz <- as.data.frame(las@data[, c("X","Y","Z")])
cat("points:", nrow(xyz), "\n")
# SOR outlier flag (same algorithm the package exposes)
lo <- classify_noise(las, sor(k = 16, m = 3)); flag <- lo@data$Classification == 18L
cat("flagged:", sum(flag), sprintf("(%.2f%%)\n", 100*mean(flag)))

# a tile that actually contains flagged points, for the zoom detail
fx <- xyz$X[flag]; fy <- xyz$Y[flag]
cx <- median(fx); cy <- median(fy); H <- 18
sel <- xyz$X > cx-H & xyz$X < cx+H & xyz$Y > cy-H & xyz$Y < cy+H
zx <- xyz[sel,]; zf <- flag[sel]
cat("zoom tile:", nrow(zx), "points,", sum(zf), "flagged\n")

png(OUT, width = 2700, height = 1380, res = 210)
layout(matrix(c(1,1,2,2,3,3, 4,4,4,5,5,5), nrow=2, byrow=TRUE), heights=c(1,0.72))
par(family="serif", cex.main=1.15, cex.axis=0.95, cex.lab=1.02)
GR<-"#1b6b4f"; RD<-"#b0242a"

# (a) whole cloud in plan view, outliers flagged - tight margins
par(mar=c(4.2,4.6,3.4,0.8))
k <- sample(which(!flag), min(90000, sum(!flag)))
plot(xyz$X[k], xyz$Y[k], pch=".", col="#9fb8ad", asp=1,
     xlim=range(xyz$X), ylim=range(xyz$Y),
     xlab="Easting (m)", ylab="Northing (m)",
     main="(a) Dense cloud in plan view:\nisolated outliers flagged")
points(fx, fy, pch=19, cex=0.22, col=RD)
rect(cx-H, cy-H, cx+H, cy+H, border="#111", lwd=2.2)
text(cx, cy-H, "detail in (b), (c)", pos=1, cex=0.9, font=2, offset=0.45)
legend("topleft", c(sprintf("retained (n = %s)", format(sum(!flag), big.mark=",")),
                    sprintf("SOR outliers (n = %s)", format(sum(flag), big.mark=","))),
       col=c("#9fb8ad",RD), pch=c(20,19), pt.cex=c(1.1,0.9), bty="n", cex=0.9)

# (b)/(c) BEFORE and AFTER, side elevation of the same tile
zl <- range(zx$Z)
par(mar=c(4.2,4.4,3.4,0.8))
plot(zx$X, zx$Z, pch=19, cex=0.16, col="#9fb8ad", ylim=zl,
     xlab="Easting (m)", ylab="Elevation (m)", main="(b) Detail before despiking")
points(zx$X[zf], zx$Z[zf], pch=19, cex=0.5, col=RD)
legend("topright", "to be removed", col=RD, pch=19, bty="n", cex=0.95)
plot(zx$X[!zf], zx$Z[!zf], pch=19, cex=0.16, col=GR, ylim=zl,
     xlab="Easting (m)", ylab="Elevation (m)", main="(c) The same detail after despiking")

# (d) canopy height in plan view, hillshaded; (e) the same model in 3-D
chm <- rast(file.path(COV,"CHM.tif"))
# 0.30 m cells. No display filter is applied: build_chm_raster() despikes the
# product itself, so what is drawn here is the raster the analysis uses.
ch_p <- aggregate(chm, max(1, round(0.30/min(res(chm)))), "mean", na.rm=TRUE)
par(mar=c(0.4,0.4,3.0,0.4))
slp <- terrain(ch_p, "slope", unit="radians"); asp <- terrain(ch_p, "aspect", unit="radians")
plot(shade(slp, asp, angle=35, direction=315), col=grey(0:100/100), legend=FALSE,
     axes=FALSE, mar=c(0.4,0.4,3.0,0.4),
     main="(d) Canopy height in plan view: forest on the margins, open ground in the centre")
plot(ch_p, col=adjustcolor(colorRampPalette(c("#d9c88c","#9fbe5e","#4d8f3f","#1c5c25"))(120), 0.62),
     legend=FALSE, axes=FALSE, add=TRUE)

# 1 m cells for the surface render: at finer spacing every crown-to-gap step
# becomes a near-vertical facet and persp() draws it as a spike.
ch_3d <- aggregate(chm, max(1, round(1.0/min(res(chm)))), "mean", na.rm=TRUE)
as_grid <- function(r){ m <- as.matrix(r, wide=TRUE); m[!is.finite(m)] <- NA
                        t(m)[, nrow(r):1, drop=FALSE] }
Zc <- as_grid(ch_3d); Zc[is.na(Zc)] <- 0
xm <- seq(0, ncol(ch_3d)*min(res(ch_3d)), length.out=nrow(Zc))
ym <- seq(0, nrow(ch_3d)*min(res(ch_3d)), length.out=ncol(Zc))
facet <- function(M)(M[-1,-1,drop=FALSE]+M[-1,-ncol(M),drop=FALSE]+
                     M[-nrow(M),-1,drop=FALSE]+M[-nrow(M),-ncol(M),drop=FALSE])/4
fz <- facet(Zc)
pal <- colorRampPalette(c("#cbb87e","#a8c169","#5d9a45","#20642a"))(200)
fcol <- pal[cut(as.numeric(fz), 200, labels=FALSE)]; fcol[is.na(fcol)] <- "#efefef"
dim(fcol) <- dim(fz)
par(mar=c(0.2,0.2,3.0,0.2))
persp(xm, ym, Zc*1.6, theta=30, phi=38, expand=1, border=NA, col=fcol, shade=0.5,
      ltheta=-50, lphi=45, box=FALSE, scale=FALSE, r=5,
      main="(e) The same canopy height model as a surface (1 m cells, 1.6x vertical)")
dev.off(); cat("wrote", OUT, "\n")
