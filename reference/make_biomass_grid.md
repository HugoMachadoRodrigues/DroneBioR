# Build a grid of biomass predictors from indices and a CHM

Aggregates a fine-resolution spectral index stack and Canopy Height
Model onto a coarser management grid (default 1 m, matching the quadrat
/ grid size used by Page et al. 2025 and Vahidi et al. 2023). Per grid
cell it returns the spectral index means plus the structural statistics
both papers rely on - CHM mean, median, max, standard deviation and
variance - and the Page vegetation volume (mean height x cell area).

## Usage

``` r
make_biomass_grid(indices, chm = NULL, grid_m = 1, indices_keep = NULL)
```

## Arguments

- indices:

  Spectral index `SpatRaster` from
  [`compute_spectral_indices()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/compute_spectral_indices.md).

- chm:

  Optional CHM `SpatRaster` (m above ground). Resampled onto the index
  grid when geometries differ. When `NULL` only spectral means are
  returned.

- grid_m:

  Target grid-cell size in metres. Use `NA` or a size at/below the
  native resolution to skip aggregation.

- indices_keep:

  Optional character vector restricting which index layers are
  aggregated (default: all).

## Value

A `SpatRaster` of per-cell predictors.

## Details

Calibration extraction and wall-to-wall prediction both read from this
same stack, so the features a model is trained on are defined
identically to the features it is mapped over.

## Examples

``` r
if (FALSE) { # \dontrun{
  refl <- scale_to_reflectance(read_multispectral_orthomosaic(path)$bands)
  ix   <- compute_spectral_indices(refl)
  chm  <- terra::rast("chm.tif")
  grid <- make_biomass_grid(ix, chm, grid_m = 1)
  names(grid)
} # }
```
