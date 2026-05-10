# Regenerate the small synthetic fixtures shipped in inst/extdata/.
#
# These fixtures exist so the package's @examples and tests can run without
# needing real flight data. They are deliberately small (each raster is 32x32)
# and synthetic, so they should never be used for science.
#
# Run from the package root:
#   Rscript data-raw/build_fixtures.R
#
# All output paths are under inst/extdata/.

set.seed(42)

stopifnot(requireNamespace("terra", quietly = TRUE))

extdata <- file.path("inst", "extdata")
dir.create(extdata, recursive = TRUE, showWarnings = FALSE)

# --- Spatial frame ---------------------------------------------------------
# UTM 17N (EPSG:32617). Coordinates approximate UF REC-Ona, Hardee County, FL.
crs_utm17n <- "EPSG:32617"
ncol <- 32L
nrow <- 32L
xmin <- 392000   # meters
ymin <- 3033000
res  <- 0.5      # 0.5 m / pixel -> 16 m x 16 m footprint
ext <- terra::ext(xmin, xmin + ncol * res, ymin, ymin + nrow * res)

# --- Multispectral orthomosaic: 6 layers (R, G, B, NIR, RedEdge, alpha) ----
# Values are 16-bit-ish reflectance (MicaSense-style: divide by 32768 to get
# a 0-1 reflectance). The radial gradient mimics a vegetation patch with a
# darker bare-soil ring; alpha masks the corners as no-data.
make_band <- function(center, sigma, scale, noise = 0.02) {
  xx <- matrix(rep(seq_len(ncol), each = nrow), nrow, ncol)
  yy <- matrix(rep(seq_len(nrow), times = ncol), nrow, ncol)
  cx <- ncol / 2
  cy <- nrow / 2
  d  <- sqrt((xx - cx)^2 + (yy - cy)^2)
  patch <- center * exp(-((d - 6)^2) / (2 * sigma^2))
  patch <- patch + rnorm(length(patch), sd = noise * center)
  patch <- pmin(pmax(patch, 0), 1)
  round(patch * scale)
}

# Approximate healthy-vegetation reflectance shape:
#   NIR > RedEdge >> Green > Red > Blue
red    <- make_band(center = 0.08, sigma = 3, scale = 32768)
green  <- make_band(center = 0.12, sigma = 3, scale = 32768)
blue   <- make_band(center = 0.05, sigma = 3, scale = 32768)
nir    <- make_band(center = 0.55, sigma = 3, scale = 32768)
rededg <- make_band(center = 0.28, sigma = 3, scale = 32768)

# Alpha: 0 in the four corners (no data), 255 elsewhere
alpha <- matrix(255L, nrow, ncol)
alpha[1:3, 1:3]                                <- 0L
alpha[1:3, (ncol-2):ncol]                      <- 0L
alpha[(nrow-2):nrow, 1:3]                      <- 0L
alpha[(nrow-2):nrow, (ncol-2):ncol]            <- 0L

stack_matrices <- list(red, green, blue, nir, rededg, alpha)
ortho <- terra::rast(lapply(stack_matrices, function(m) {
  r <- terra::rast(nrows = nrow, ncols = ncol, extent = ext, crs = crs_utm17n)
  terra::values(r) <- as.vector(t(m))
  r
}))
names(ortho) <- c("Red", "Green", "Blue", "NIR", "RedEdge", "alpha")

terra::writeRaster(
  ortho,
  filename  = file.path(extdata, "micasense_subset.tif"),
  overwrite = TRUE,
  datatype  = "INT2U",
  gdal      = c("COMPRESS=DEFLATE", "PREDICTOR=2")
)

# --- DSM and DTM -----------------------------------------------------------
# DTM: gentle slope from west (50 m) to east (52 m).
# DSM: DTM + canopy bump (Gaussian, ~3 m tall) centered on the field.
xx <- matrix(rep(seq_len(ncol), each = nrow), nrow, ncol)
yy <- matrix(rep(seq_len(nrow), times = ncol), nrow, ncol)
dtm_mat <- 50 + (xx / ncol) * 2
canopy <- 3 * exp(-((xx - ncol/2)^2 + (yy - nrow/2)^2) / (2 * 5^2))
dsm_mat <- dtm_mat + canopy + rnorm(length(dtm_mat), sd = 0.05)

write_single <- function(mat, name) {
  r <- terra::rast(nrows = nrow, ncols = ncol, extent = ext, crs = crs_utm17n)
  terra::values(r) <- as.vector(t(mat))
  names(r) <- name
  terra::writeRaster(
    r,
    filename  = file.path(extdata, paste0(name, "_subset.tif")),
    overwrite = TRUE,
    datatype  = "FLT4S",
    gdal      = c("COMPRESS=DEFLATE", "PREDICTOR=3")
  )
}
write_single(dsm_mat, "dsm")
write_single(dtm_mat, "dtm")

# --- Field biomass samples -------------------------------------------------
# 8 samples placed within the orthomosaic footprint, plausible biomass values.
n_samples <- 8L
xs <- runif(n_samples, xmin + 2, xmin + ncol * res - 2)
ys <- runif(n_samples, ymin + 2, ymin + nrow * res - 2)
biomass <- round(2000 + 1500 * runif(n_samples) - 500, 1)
field <- data.frame(
  sample_id    = sprintf("S%02d", seq_len(n_samples)),
  biomass_kgha = biomass,
  x            = round(xs, 3),
  y            = round(ys, 3),
  stringsAsFactors = FALSE
)
utils::write.csv(field, file.path(extdata, "field_samples.csv"), row.names = FALSE)

# --- Report ---------------------------------------------------------------
files <- list.files(extdata, full.names = TRUE)
info  <- file.info(files)
report <- data.frame(
  file = basename(rownames(info)),
  bytes = info$size,
  stringsAsFactors = FALSE
)
cat("Fixtures written to ", extdata, ":\n", sep = "")
print(report, row.names = FALSE)
cat("Total: ", sum(info$size), " bytes\n", sep = "")
