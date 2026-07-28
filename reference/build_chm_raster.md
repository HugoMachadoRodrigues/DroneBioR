# Build a Canopy Height Model (CHM) from the DSM and DTM

Computes `CHM = DSM - DTM`, clamps negatives to zero (small noise from
SMRF ground classification), and writes the result as a COG-style
GeoTIFF. By default reads/writes from the local cache directory
(`~/.dronebior/cache/<slug>/`) when DSM + DTM are already cached there,
so we never touch the cloud-synced project folder. Falls back to writing
into the project's `odm_dem/` directory otherwise.

## Usage

``` r
build_chm_raster(
  project,
  force = FALSE,
  cache_aware = TRUE,
  outlier_percentile = 99.5
)
```

## Arguments

- project:

  A `dronebio_project` object.

- force:

  Logical. Recompute even when `chm.tif` already exists.

- cache_aware:

  Logical. Prefer the local cache when DSM + DTM already live there.

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

## Value

Absolute path to the written `chm.tif`.

## Examples

``` r
if (FALSE) { # \dontrun{
  project <- dronebio_project("~/my_project")
  build_chm_raster(project)
  # keep every pixel, no outlier clipping:
  build_chm_raster(project, outlier_percentile = 100)
} # }
```
