# Survey-grade volume calculations over a region of interest

Computes the volume between a "top" surface (a DSM, a CHM, or any other
height raster) and a chosen base reference, broken into cut (height
above the base), fill (height below the base) and net volumes. The base
reference is chosen with `method`, and matches the conventions used by
photogrammetric surveyors for stockpiles, earthworks and biomass
calculations.

## Usage

``` r
compute_survey_volumes(
  top,
  roi,
  method = c("dtm", "min_z", "mean_z", "ground_quantile", "user_plane", "perimeter_tin"),
  dtm = NULL,
  base_z = NULL,
  ground_quantile = 0.05
)
```

## Arguments

- top:

  A
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  containing the top surface (DSM, CHM, etc.). Must be in a projected
  CRS with linear units; otherwise volumes are not meaningful.

- roi:

  A data frame with `x` and `y` columns describing the ROI polygon in
  the same CRS as `top`. At least three vertices. The ring is closed
  automatically.

- method:

  Base-reference method. One of `"dtm"`, `"min_z"`, `"mean_z"`,
  `"ground_quantile"`, `"user_plane"`, `"perimeter_tin"`.

- dtm:

  Required when `method = "dtm"`. A
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  bare-earth DTM. Resampled to the `top` grid if geometries differ.

- base_z:

  Required when `method = "user_plane"`. Numeric.

- ground_quantile:

  Quantile in `[0, 1]` used by `method = "ground_quantile"`. Default
  `0.05`.

## Value

A list with class `"dronebio_survey_volume"`:

- `method`:

  Method used.

- `cut_volume_m3`:

  Volume above the base (m^3).

- `fill_volume_m3`:

  Volume below the base (m^3, positive).

- `net_volume_m3`:

  `cut - fill` (m^3).

- `surface_area_planar_m2`:

  Projected (planimetric) area of valid cells inside the ROI.

- `surface_area_draped_m2`:

  3D surface area via the cell-wise slope tangent; `NA` if
  [`terra::terrain()`](https://rspatial.github.io/terra/reference/terrain.html)
  cannot be computed (e.g. ROI \< 3x3 cells).

- `perimeter_m`:

  Planimetric polygon perimeter.

- `cell_count`:

  Number of valid `top`-cells inside the ROI.

- `cell_area_m2`:

  Grid cell area, m^2.

- `top_z_summary`:

  Named vector `min/median/mean/max` of the top surface inside the ROI.

- `base_z_summary`:

  Same for the base surface.

- `height_summary`:

  Same for `(top - base)`.

- `base_reference_text`:

  Human-readable description of the base method.

## Details

The volume math is the standard raster integral \$\$V = \sum_i
(z_i^{top} - z_i^{base}) \cdot A\_{cell}\$\$ clipped to positive (cut)
and negative (fill) contributions, where the sum runs over every cell of
`top` whose centre lies inside the ROI polygon. The base surface is
built on the same grid as the clipped `top`, so cut and fill are
computed cell-by-cell with no resampling artefact between top and base.

Base-reference methods:

- `"dtm"`:

  Use a separate DTM (bare-earth model) as the base. This is the correct
  method for canopy biomass when a DTM is available: the cut volume
  reduces to \\\int (DSM - DTM) dA\\, i.e. the canopy height model
  integrated over the ROI.

- `"min_z"`:

  Constant plane at the minimum top-surface value inside the ROI. The
  classic stockpile method when no separate bare-earth model exists.

- `"mean_z"`:

  Constant plane at the mean top-surface value.

- `"ground_quantile"`:

  Constant plane at a low quantile of the top surface (default 5th
  percentile). A robust proxy for the ground when there is no DTM and
  `"min_z"` is too sensitive to a single dark pixel.

- `"user_plane"`:

  Constant plane at the user-supplied `base_z`.

- `"perimeter_tin"`:

  Triangulated irregular network built from the top-surface values
  sampled at the ROI's perimeter vertices. This is the industry standard
  for stockpile surveys (used by Pix4D Mapper, Bentley ContextCapture,
  Trimble Business Center): the perimeter rests on the surveyed "natural
  ground line", a Delaunay triangulation interpolates linearly between
  those vertices, and the volume is the integral of `top - TIN` inside
  the ROI. Requires the optional `interp` package.

Both planimetric and 3D draped surface areas are reported. The draped
area is the sum of cell areas inflated by the secant of the local slope,
\\A\_{drape} = \sum_i A\_{cell} \cdot \sec(\theta_i)\\, which is the
standard formula in photogrammetric reporting.

## Examples

``` r
dsm <- terra::rast(system.file("extdata", "dsm_subset.tif", package = "DroneBioR"))
dtm <- terra::rast(system.file("extdata", "dtm_subset.tif", package = "DroneBioR"))
roi <- data.frame(
  x = c(392004, 392012, 392012, 392004),
  y = c(3033004, 3033004, 3033012, 3033012)
)
result <- compute_survey_volumes(top = dsm, roi = roi, method = "dtm", dtm = dtm)
result$cut_volume_m3
#> [1] 93.41468

# Constant-plane stockpile-style:
result_min <- compute_survey_volumes(top = dsm, roi = roi, method = "min_z")
result_min$cut_volume_m3
#> [1] 98.7074
```
