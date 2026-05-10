# Dense point clouds, CHM and ROI metrics

## Inputs

This vignette assumes you already have one or more of these products
from an external photogrammetry engine:

- a dense point cloud in LAS, LAZ or PLY;
- a DSM and a DTM as GeoTIFFs;
- a region of interest (a plot polygon, or a set of points to hull).

## Reading a point cloud

[`read_full_point_cloud()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/read_full_point_cloud.md)
dispatches on file extension and returns a tidy data frame with `x`,
`y`, `z`, optional classification and a display color.

``` r

library(DroneBioR)

pc <- read_full_point_cloud(
  "outputs/odm_micasense_dataset/micasense/odm_georeferencing/odm_georeferenced_model.laz"
)
nrow(pc)
```

For format-specific control:

``` r

las <- read_las_point_cloud("dense.las")
ply <- read_ply_point_cloud("preview.ply", max_points = 50000)
```

The built-in LAS reader handles uncompressed `.las`. The PLY reader is
deliberately small for app previews — it expects the ODM/FPCFilter
binary little-endian layout (float x/y/z + uchar r/g/b/views). For very
large LAZ files, install the suggested `lidR` dependency, and
[`read_full_point_cloud()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/read_full_point_cloud.md)
will use it transparently.

## CHM from DSM and DTM

``` r

chm <- build_chm_from_dsm_dtm("export/dsm.tif", "export/dtm.tif")
```

When you only have a point cloud and no DTM, attach above-ground heights
using a low-quantile ground reference within each neighborhood:

``` r

pc <- add_point_heights(pc, ground_quantile = 0.05)
```

When a CHM is already available,
[`add_chm_heights()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/add_chm_heights.md)
samples it at each point so the heights are consistent with the raster
product.

``` r

pc <- add_chm_heights(pc, chm)
```

## Region-of-interest selection

A user selection is typically a convex hull around clicked points (in
the Shiny app) or a bounding box read from a field GIS. Both go through
the same polygon abstraction.

``` r

roi <- build_roi_polygon(pc[sample(nrow(pc), 200), ], method = "hull")
# Or a bounding box:
roi_bbox <- build_roi_polygon(pc, method = "bbox")

inside <- points_in_roi(pc$x, pc$y, roi)
sel    <- filter_points_by_roi(pc, roi)
```

## CHM metrics for a ROI

``` r

metrics <- compute_chm_roi_metrics(chm, roi)
metrics
```

Returned columns include area, mean and percentile heights, useful as
plot-level summaries to merge with field biomass measurements.

## Selection metrics and vertical profile

``` r

sel <- add_point_heights(sel)

compute_selection_metrics(sel, voxel_size = 0.5)
compute_vertical_profile(sel, bin_size = 1)
```

[`compute_selection_metrics()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/compute_selection_metrics.md)
returns a one-row data frame summarizing the selection: n points,
bounding-box area, max pairwise distance, height percentiles and
occupied voxel volume.
[`compute_vertical_profile()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/compute_vertical_profile.md)
returns binned point counts useful for canopy structure plots.

## Individual tree candidates

``` r

trees <- derive_tree_candidates(pc, min_height = 1.5)
```

The detector is intentionally simple — it produces seed locations for
downstream segmentation, not a final crown delineation.

## Exporting a selection

``` r

export_point_selection(
  points     = sel,
  output_dir = "outputs/selections",
  base_name  = "plot_03"
)
```

Output files include a CSV table and a small PLY preview suitable for
the Shiny 3D viewer.

## When to reach for `lidR`

For full LAZ analytics — tiled processing, advanced ground
classification, DTM modelling, terrain normalization, individual tree
segmentation — the `lidR` package is the right tool. DroneBioR’s
point-cloud functions are deliberately scoped to the in-memory analyses
that drive the Shiny app and plot-level summaries.
