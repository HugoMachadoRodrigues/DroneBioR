suppressMessages({library(terra); library(caret)})
# Set PROJECT to the demonstration project and OUTDIR to where the figure
# should be written. Both are the only paths this script needs.
PROJECT <- Sys.getenv("DRONEBIOR_PROJECT", "~/DroneBioR-projects/micasense_demo")
OUTDIR  <- Sys.getenv("DRONEBIOR_FIGDIR",  file.path(getwd(), "figures"))
PROJECT <- normalizePath(PROJECT, mustWork = TRUE)
dir.create(OUTDIR, recursive = TRUE, showWarnings = FALSE)
COV  <- file.path(PROJECT, "covariates")

S <- readRDS("repro_full/verification.rds")
E <- subset(S$extraction, gen=="linear" & gps==0.20); C <- subset(S$coefficients, gps==0.20); R <- S$performance
agg <- function(d,f,by) tapply(d[[f]], d[[by]], mean)

OUT <- file.path(OUTDIR, "fig5_modelling.png")

# ---- panel (d): prediction surface from the known generating function -------
nd <- rast(file.path(COV,"NDVI.tif")); ch <- rast(file.path(COV,"CHM.tif"))
ch <- resample(ch, nd, method="bilinear")
nd9 <- focal(nd, 9, "mean", na.rm=TRUE, expand=TRUE); ch9 <- focal(ch, 9, "mean", na.rm=TRUE, expand=TRUE)
bio <- 400 + 5200*nd9 + 95*ch9
bio <- clamp(bio, 1596, 9339, values=TRUE)
bio <- aggregate(bio, 4, fun="mean", na.rm=TRUE)

fam <- sort(table(sub("^(\\w+).*","\\1", getModelInfo() |> names()))[1:0])  # placeholder
mi <- getModelInfo(); nm <- names(mi)
lab <- sapply(nm, function(k) paste(mi[[k]]$label, collapse=" "))
grp <- rep("other", length(nm))
grp[grepl("Linear|Regression|glm|Least Squares|Ridge|Lasso|Elastic", lab, ignore.case=TRUE)] <- "linear /\nregularised"
grp[grepl("Tree|Forest|Boost|Bagg|Cubist|Rules", lab, ignore.case=TRUE)] <- "trees /\nensembles"
grp[grepl("Neural|Perceptron|Deep|Extreme Learning", lab, ignore.case=TRUE)] <- "neural"
grp[grepl("Spline|Additive|Polynomial|MARS", lab, ignore.case=TRUE)] <- "splines /\nadditive"
grp[grepl("Support Vector|Kernel|Gaussian Process|Relevance", lab, ignore.case=TRUE)] <- "kernel"
grp[grepl("Neighbor|Neighbour|Nearest", lab, ignore.case=TRUE)] <- "nearest\nneighbour"
grp[grepl("Fuzzy|Genetic|Rule-Based", lab, ignore.case=TRUE)] <- "fuzzy /\nrule-based"
reg <- sapply(mi, function(m) "Regression" %in% m$type); tb <- sort(table(grp[reg]), decreasing=TRUE)

png(OUT, width=2600, height=2050, res=210)
par(mfrow=c(2,2), mar=c(6.2,5.2,3.4,5.4), family="serif", cex.axis=0.95, cex.lab=1.05)
GR <- "#1b6b4f"; BR <- "#8c5a2b"; NV <- "#22405e"

# (a) caret families
bp <- barplot(tb, col=GR, border=NA, las=2, ylab="regression methods",
              main=sprintf("(a) caret regression methods reachable (%d)", sum(reg)), cex.names=0.8)
text(bp, tb, tb, pos=3, cex=0.85, xpd=NA)

# (b) extraction fidelity
w <- as.numeric(names(agg(E,"ndvi_rmse","window")))
plot(w, agg(E,"ndvi_rmse","window"), type="b", pch=19, col=GR, lwd=2.4, xlab="extraction window (pixels)",
     ylab="NDVI extraction RMSE", main="(b) Extraction error vs the true quadrat value", ylim=c(0.035,0.060))
par(new=TRUE); plot(w, agg(E,"chm_rmse","window"), type="b", pch=17, col=BR, lwd=2.4, lty=2,
                    axes=FALSE, xlab="", ylab="", ylim=c(1.45,2.15))
axis(4, col=BR, col.axis=BR, las=1); mtext("CHM extraction RMSE (m)", 4, line=3.4, col=BR, cex=0.85)
legend("topright", c("NDVI (left)","CHM (right)"), col=c(GR,BR), lty=c(1,2), pch=c(19,17), bty="n", cex=0.9)

# (c) coefficient recovery
mn <- tapply(C$b_ndvi, C$window, mean); lo <- tapply(C$b_ndvi, C$window, min); hi <- tapply(C$b_ndvi, C$window, max)
ww <- as.numeric(names(mn))
plot(ww, mn, type="n", ylim=range(c(lo,hi,5200))*c(0.99,1.01), xlab="extraction window (pixels)",
     ylab=expression(recovered~beta[NDVI]), main="(c) Recovery of the known coefficient")
polygon(c(ww,rev(ww)), c(lo,rev(hi)), col=adjustcolor(GR,0.18), border=NA)
abline(h=5200, col="#b0242a", lwd=2.2, lty=2)
lines(ww, mn, col=GR, lwd=2.6); points(ww, mn, pch=19, col=GR)
text(max(ww), 5200, "true value 5200", col="#b0242a", pos=1, cex=0.9, xpd=NA)
legend("bottomright", c("mean over 10 seeds","range over seeds"), col=c(GR,adjustcolor(GR,0.35)),
       lwd=c(2.6,8), bty="n", cex=0.9)

# (d) prediction map
pal <- hcl.colors(100,"Greens", rev=TRUE)
plot(bio, col=pal, main="(d) Predicted surface from the fitted model", axes=FALSE,
     plg=list(title=expression(kg~ha^-1), title.cex=0.8, cex=0.8), mar=c(2,2,3.4,6))
mtext("clamped to the calibration range", side=1, line=0.2, cex=0.8)
dev.off()
cat("wrote", OUT, "\n")
