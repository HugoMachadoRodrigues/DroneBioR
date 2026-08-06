# Produce physically consistent DSM, DTM and CHM

ODM generates the DSM (top of the dense cloud) and the DTM
(SMRF-classified ground, interpolated) by independent processes, so they
are not pixel-consistent: on bare ground the interpolated DTM routinely
sits a few centimetres *above* the DSM, which makes `DSM - DTM` (the
canopy height) negative over a large fraction of a short-canopy survey.
Despiking the two rasters separately can widen that gap further. This
helper rebuilds all three products so they obey the physical constraints
`CHM >= 0` and `DSM >= DTM` everywhere, by construction:

## Usage

``` r
harmonize_dem_products(
  project = NULL,
  dsm = NULL,
  dtm = NULL,
  out_dir = NULL,
  canopy_ceiling = 30,
  trend_cell_m = 30,
  max_depth_below_ground = 2,
  iterations = 2L,
  dtm_max_bump = 5,
  spike_min_height = 1.5,
  max_spike_area_m2 = 10,
  spike_dilate_cells = 15L,
  write = TRUE
)
```

## Arguments

- project:

  Optional `dronebio_project`; when supplied the DSM and DTM are taken
  from
  [`odm_product_paths()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/odm_product_paths.md)
  and outputs default to the same `odm_dem/` folder.

- dsm, dtm:

  Raster paths or
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)s.
  Required when `project` is not given.

- out_dir:

  Output directory. Defaults to the DSM's folder. The function writes
  `dsm_consistent.tif`, `dtm_consistent.tif` and `chm_consistent.tif`
  there when `write = TRUE`.

- canopy_ceiling:

  Height (m) above the local canopy trend beyond which a CHM cell is
  treated as a tower spike and removed. Default `30` — keeps genuine
  tall trees, drops the reconstruction towers.

- trend_cell_m, max_depth_below_ground, iterations:

  Passed to
  [`despike_dem()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/despike_dem.md)
  for the DTM and CHM cleaning. Defaults `30`, `2`, `2`.

- dtm_max_bump:

  Height (m) above its own trend beyond which a DTM cell is treated as a
  spike. Default `5`.

- spike_min_height, max_spike_area_m2, spike_dilate_cells:

  Area-opening that removes isolated SfM "cone"/needle spikes the
  height-based `canopy_ceiling` filter misses (they are shorter than the
  ceiling). A CHM cell taller than `spike_min_height` m (default `1.5`)
  is "tall"; contiguous tall patches of at most `max_spike_area_m2` m^2
  (default `10`) are flattened to ground, larger patches (real canopy)
  are kept. The spike mask is grown `spike_dilate_cells` cells (default
  `15`) to also catch the cone skirt. Set `max_spike_area_m2 = 0` (or
  `NULL`) to disable.

- write:

  Logical. Write the three GeoTIFFs. Default `TRUE`.

## Value

Invisibly, a list with the cleaned `dsm`, `dtm`, `chm`
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)s
and, when written, their `paths`.

## Details

1.  Despike the DTM (the ground) with
    [`despike_dem()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/despike_dem.md).

2.  Form `CHM = DSM - DTM_clean`, clamp it to `>= 0` (which turns the
    DSM's downward pits — where the surface dipped below ground — back
    into bare ground) and despike the result to remove canopy-height
    towers / needles.

3.  Rebuild `DSM = DTM_clean + CHM_clean`. Because the cleaned CHM is
    non-negative, the rebuilt DSM is never below the DTM.

## Examples

``` r
if (FALSE) { # \dontrun{
  project <- dronebio_project("~/flight")
  harmonize_dem_products(project)            # writes *_consistent.tif
} # }
```
