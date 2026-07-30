# Flag reconstruction spikes in a point cloud

Photogrammetric dense reconstruction of low-texture vegetation leaves
two kinds of noise the ODM `--pc-filter` stage does not remove: sparse
floating points / needle tips, and thin vertical "needles" that shoot up
well above the local canopy from mis-matched features. This is the
classic point-cloud denoising problem, and this function applies the two
standard filters used by CloudCompare, PDAL (`filters.outlier`), PCL and
lidR:

## Usage

``` r
despike_point_cloud(
  coords,
  methods = c("sor", "surface"),
  sor_k = 16L,
  sor_mult = 2,
  cell = 1,
  ground_q = 0.1,
  smooth_w = 5L,
  height_cap = NULL,
  surface_mult = 3
)
```

## Arguments

- coords:

  A data frame or matrix with numeric columns `x`, `y`, `z`
  (case-insensitive), one row per point.

- methods:

  Which passes to apply: any of `"sor"` and `"surface"`.

- sor_k, sor_mult:

  SOR neighbour count and robust-deviation multiplier. Lower `sor_mult`
  is more aggressive. Ignored unless `"sor"` is in `methods`.

- cell:

  Grid cell size (metres) for the surface estimate.

- ground_q:

  Quantile of z per cell taken as the local surface base (`0.1` = 10th
  percentile). Lower is closer to bare ground.

- smooth_w:

  Odd window (in cells) used to smooth / gap-fill the surface.

- height_cap:

  Maximum height (metres) a point may stand above the local surface
  before it is flagged. `NULL` (default) uses a robust cut instead
  (`median + surface_mult * MAD` of the residuals), which adapts to the
  flight; set an explicit value (e.g. `3`) to cap vegetation height
  directly.

- surface_mult:

  Robust-deviation multiplier used when `height_cap` is `NULL`. Lower is
  more aggressive.

## Value

A logical vector, one per input row, `TRUE` for points to **keep** and
`FALSE` for flagged spikes. Suitable as the `keep` mask for
[`write_ply_subset()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/write_ply_subset.md)
once you take `which(keep)`.

## Details

- **Statistical Outlier Removal (SOR)** – for each point, the mean
  distance to its `sor_k` nearest neighbours (in 3D). Points whose mean
  distance is a robust outlier (above `median + sor_mult * MAD`) are
  flagged. This clears sparse floaters and the isolated tips of needles.

- **Height-above-surface filter** – a robust local ground/canopy-base
  surface is estimated on a grid (a low quantile of z per `cell`,
  smoothed across a neighbourhood so a cell full of spikes borrows its
  neighbours' base), and a point standing more than `height_cap` metres
  above that surface – or, when `height_cap` is `NULL`, more than
  `surface_mult` robust deviations above the residual distribution – is
  flagged. Unlike a neighbour-mean reference, a *cluster* of tall spike
  points cannot hide itself here: its base comes from the low quantile /
  surrounding cells, so the whole clump is measured against the real
  surface and removed. This is what actually knocks down the tall
  needles, not just the scattered noise.

The SOR pass is robust (median / MAD, not mean / sd) so a few gross
spikes do not inflate the threshold and mask the rest. Run either or
both.

## Examples

``` r
set.seed(1)
ground <- data.frame(x = runif(2000, 0, 10), y = runif(2000, 0, 10))
ground$z <- 0.2 * ground$x + rnorm(2000, 0, 0.05)     # a gentle sloped surface
spikes <- data.frame(x = runif(20, 0, 10), y = runif(20, 0, 10),
                     z = runif(20, 5, 9))              # tall needles
pc <- rbind(ground, spikes)
keep <- despike_point_cloud(pc, methods = "sor")
sum(!keep)   # the scattered spikes
#> [1] 102
```
