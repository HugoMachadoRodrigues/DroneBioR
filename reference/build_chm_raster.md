# Build a Canopy Height Model (CHM) from the DSM and DTM

Computes `CHM = DSM - DTM`, clamps negatives to zero (small noise from
SMRF ground classification), and writes the result as a COG-style
GeoTIFF into the project's `odm_dem/` directory, alongside the DSM.

## Usage

``` r
build_chm_raster(
  project,
  force = FALSE,
  outlier_percentile = 99.5,
  despike = TRUE,
  despike_window = 5L,
  despike_max_deviation = 3
)
```

## Arguments

- project:

  A `dronebio_project` object.

- force:

  Logical. Recompute even when `chm.tif` already exists.

- outlier_percentile:

  Numeric in (0, 100\], default `99.5`. After differencing,
  canopy-height pixels strictly above this percentile are set to `NA`
  and a message reports how many were dropped. Photogrammetric
  reconstructions routinely leave a thin tail of physically impossible
  spikes (CHM pixels of tens to hundreds of metres over short pasture)
  from mis-reconstructed points at edges, water and low-texture areas.
  Even when they are well under 1% of pixels they wreck colour ramps and
  contaminate downstream biomass statistics. Clipping the extreme tail
  at a high percentile removes them while preserving genuine tall
  features (the percentile adapts to each survey). Set to `100` (or
  `NULL`) to disable and keep every pixel.

- despike:

  Logical, default `TRUE`. Run
  [`despike_dem()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/despike_dem.md)
  on the differenced canopy height model before the percentile clip, to
  remove isolated needles. This catches what `outlier_percentile`
  structurally cannot: a needle is a cell standing far above its own
  neighbourhood, and it is usually nowhere near the tallest cell in the
  survey, so no global threshold separates it from real canopy. Set
  `FALSE` to keep the raw difference.

- despike_window:

  Odd neighbourhood size in pixels for the local median, passed to
  [`despike_dem()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/despike_dem.md).
  Default `5`.

- despike_max_deviation:

  Metres a cell may stand above its local median before it is treated as
  a needle, passed to
  [`despike_dem()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/despike_dem.md).
  Default `3`.

## Value

Absolute path to the written `chm.tif`.

## Examples

``` r
if (FALSE) { # \dontrun{
  project <- dronebio_project("~/my_project")
  build_chm_raster(project)
  # keep every pixel, no outlier clipping:
  build_chm_raster(project, outlier_percentile = 100)
  # the raw difference, with neither filter:
  build_chm_raster(project, outlier_percentile = 100, despike = FALSE)
} # }
```
