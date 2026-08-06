# Remove isolated spikes from a DSM / DTM / DEM

Photogrammetric surface models routinely contain a handful of isolated
"needle" spikes — single pixels (or tiny clusters) that jut tens of
metres above an otherwise locally-smooth surface, caused by
mis-reconstructed dense-cloud points where the imagery was blurry,
low-texture or reflective. They are devastating for 3D visualisation
(the surface sprouts towers) and for any slope / volume statistic, yet
they are a vanishing fraction of pixels.

## Usage

``` r
despike_dem(
  dem,
  window = 5,
  max_deviation = 3,
  max_height_above_ground = NULL,
  ground = NULL,
  max_depth_below_ground = 2,
  trend_cell_m = 15,
  iterations = 2L,
  fill = c("median", "NA"),
  out_path = NULL
)
```

## Arguments

- dem:

  A
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  (first layer used) or a path to a DEM GeoTIFF.

- window:

  Odd integer neighbourhood size in pixels for the local median. Default
  `5`.

- max_deviation:

  Maximum allowed absolute deviation (metres) from the local median
  before a cell is treated as a needle spike. Default `3`.

- max_height_above_ground:

  Optional numeric (metres). When set, also flag cells whose height
  above the ground reference exceeds this — the wide-tower detector.
  `NULL` (default) disables it.

- ground:

  Optional ground reference for the height-above-ground filter: a
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  or path (typically the DTM). When `NULL`, a coarse trend is built from
  the DEM itself.

- max_depth_below_ground:

  Numeric (metres), default `2`. Used only when the height-above-ground
  filter is active. A DSM is the *top* surface, so a cell more than this
  far **below** the ground is impossible — a downward spike. Such cells
  are flagged and filled from the ground surface too. Set `NULL` to
  ignore downward spikes.

- trend_cell_m:

  Coarse-trend cell size in metres used when `ground` is not supplied.
  Default `15` — should comfortably exceed the width of the towers you
  want removed.

- iterations:

  Integer, default `2`. Number of detect-and-fill passes. A single pass
  cannot fully clean a wide blob (while it is present it drags the local
  trend toward itself, hiding its deepest core); a second pass over the
  now-mostly-cleaned surface removes the residual. The loop stops early
  once a pass changes nothing.

- fill:

  One of `"median"` (replace flagged cells with the local median /
  ground — keeps a continuous surface, best for 3D viz) or `"NA"` (drop
  them to NoData). Default `"median"`.

- out_path:

  Optional path to write the cleaned DEM. When `NULL` (default) nothing
  is written and the cleaned raster is returned in memory.

## Value

The cleaned
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
(invisibly when `out_path` is written).

## Details

Two complementary detectors run, and a cell flagged by either is
cleaned:

1.  **Local needle filter** (always on): compares each cell to the
    median of its `window`x`window` neighbourhood and flags cells whose
    absolute deviation exceeds `max_deviation` metres. This catches
    single-pixel / tiny-cluster spikes that are taller than their
    immediate surroundings.

2.  **Height-above-ground filter** (opt-in via
    `max_height_above_ground`): some reconstruction artifacts are not
    needles but *wide towers* — coherent blobs several metres across
    where a blurry / low-texture patch ballooned upward. A small
    neighbourhood median cannot see those (the tower's own pixels
    dominate the window), so this second pass measures each cell's
    height above the ground and flags anything taller than
    `max_height_above_ground` metres. The ground reference is the
    `ground` raster (pass the DTM) when supplied, otherwise a coarse
    trend surface built by aggregating the DEM to ~`trend_cell_m`-metre
    cells (large enough to average over the towers). Use this for a
    survey where you know the real surface ceiling — e.g. a pasture
    whose canopy tops out near 15 m but whose DSM sprouts 50-130 m
    towers.

Because real terrain and vegetation are spatially coherent, both
detectors leave genuine features intact; a global percentile clip could
not make that distinction.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Needles only:
  despike_dem("odm_dem/dsm.tif", out_path = "odm_dem/dsm_clean.tif")
  # Wide towers too, using the DTM as ground (pasture canopy <= 20 m):
  despike_dem("odm_dem/dsm.tif", ground = "odm_dem/dtm.tif",
              max_height_above_ground = 20,
              out_path = "odm_dem/dsm_clean.tif")
} # }
```
