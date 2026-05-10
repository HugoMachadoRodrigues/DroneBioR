# Vegetation indices in DroneBioR

## Reflectance is required

All indices in DroneBioR assume that input bands are scaled to
reflectance in the 0 – 1 range. MicaSense RedEdge orthomosaics are
typically stored as 16-bit integers scaled by 32768; route them through
[`scale_to_reflectance()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/scale_to_reflectance.md)
before computing indices.

``` r

library(DroneBioR)

ortho <- read_multispectral_orthomosaic("path/to/orthomosaic.tif")
refl  <- scale_to_reflectance(ortho$bands)
```

The named layers in `refl` must be `Blue`, `Green`, `Red`, `RedEdge` and
`NIR`.
[`read_multispectral_orthomosaic()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/read_multispectral_orthomosaic.md)
enforces this naming when the band map is correct.

## The index catalogue

[`compute_spectral_indices()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/compute_spectral_indices.md)
returns a
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
with the layers listed below, all computed with a small-denominator
guard (`eps = 1e-6`) to avoid division blow-ups in shadow or water
pixels.

| Layer | Formula | Typical use |
|----|----|----|
| NDVI | (NIR - Red) / (NIR + Red) | Greenness, general vigor; saturates at high LAI |
| NDRE | (NIR - RedEdge) / (NIR + RedEdge) | Chlorophyll / nitrogen status in dense canopies |
| EVI | 2.5 \* (NIR - Red) / (NIR + 6*Red - 7.5*Blue + 1) | High-biomass canopies, atmosphere-resistant |
| SAVI | (1 + L) \* (NIR - Red) / (NIR + Red + L), L = 0.5 | Sparse canopies where soil reflectance is visible |
| NDWI | (Green - NIR) / (Green + NIR) | Water / canopy water content; flags wet pixels |
| GNDVI | (NIR - Green) / (NIR + Green) | Chlorophyll, sensitive at low-to-mid biomass |
| CIrededge | (NIR / RedEdge) - 1 | Chlorophyll without saturation at high LAI |
| MSAVI2 | 0.5 \* (2*NIR + 1 - sqrt((2*NIR + 1)^2 - 8\*(NIR - Red))) | Soil-adjusted, no tuned L parameter |
| VARI | (Green - Red) / (Green + Red - Blue) | RGB-only proxy when NIR is unreliable |

## Computing the stack

``` r

indices <- compute_spectral_indices(refl)
names(indices)
#> [1] "NDVI" "NDRE" "EVI" "SAVI" "NDWI" "GNDVI" "CIrededge" "MSAVI2" "VARI"

summarize_spatraster(indices)
```

## Biomass proxy

[`compute_biomass_proxy()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/compute_biomass_proxy.md)
blends a small subset of indices into a single rescaled layer useful for
quick visual screening. It is a proxy, not a calibrated estimate —
always validate against field samples before reporting biomass values.

``` r

proxy <- compute_biomass_proxy(indices)
```

## Choosing predictors for a biomass model

[`fit_biomass_lm()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/fit_biomass_lm.md)
defaults to whichever of `NDVI`, `NDRE`, `EVI`, `SAVI`, `NDWI`, `NIR`
and `RedEdge` are present in the joined field/predictor table. Pass
`predictors = ...` to constrain the model — for dense canopies,
`c("NDRE", "CIrededge")` often outperforms NDVI alone.

``` r

joined <- extract_field_spectral_data(field, indices)
model  <- fit_biomass_lm(joined, predictors = c("NDRE", "CIrededge"))
summary(model)
```

## Practical tips

- Always inspect the indices for hot pixels at the orthomosaic edges —
  the alpha mask in
  `read_multispectral_orthomosaic(..., use_alpha = TRUE)` removes most
  of them.
- When comparing flights, scale to reflectance first; otherwise NDVI
  shifts across dates reflect the engine’s scaling, not the canopy.
- Index choice should follow the canopy stage. NDVI plateaus once LAI is
  above ~3; CIrededge and NDRE keep responding.
