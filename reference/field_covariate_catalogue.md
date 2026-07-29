# Catalogue the covariates available for field modelling

Pure metadata: no raster is read, so the covariate picker can be
rendered before any product is loaded into memory. Proxy rows appear
only when their prerequisites hold, so the user cannot tick a covariate
that extraction would then fail to supply.

## Usage

``` r
field_covariate_catalogue(
  band_names = character(),
  index_names = character(),
  custom_index_name = NULL,
  has_chm = FALSE,
  has_dsm = FALSE,
  has_dtm = FALSE
)
```

## Arguments

- band_names:

  Layer names of the reflectance stack.

- index_names:

  Layer names of the spectral-index stack.

- custom_index_name:

  Optional name of a user-defined index layer.

- has_chm, has_dsm, has_dtm:

  Whether those terrain products exist.

## Value

A data frame with `id`, `label`, `group`, `source`, `recommended` and
`note`. `group` is one of `Reflectance bands`, `Spectral indices`,
`Biomass proxies`, `Terrain`.

## Examples

``` r
field_covariate_catalogue(
  band_names = c("Red", "NIR"),
  index_names = c("NDVI", "SAVI", "NDRE"),
  has_chm = TRUE
)
#>                    id                                     label
#> 1                 Red                           Red reflectance
#> 2                 NIR                           NIR reflectance
#> 3                NDVI                                      NDVI
#> 4                SAVI                                      SAVI
#> 5                NDRE                                      NDRE
#> 6    Biomass_Spectral    Biomass_Spectral (mean NDVI/SAVI/NDRE)
#> 7  Biomass_NDVI_x_CHM Biomass_NDVI_x_CHM (NDVI x canopy height)
#> 8  Biomass_NDRE_x_CHM Biomass_NDRE_x_CHM (NDRE x canopy height)
#> 9  Biomass_SAVI_x_CHM Biomass_SAVI_x_CHM (SAVI x canopy height)
#> 10              CHM_m                         Canopy height (m)
#>                group             source recommended
#> 1  Reflectance bands        Orthomosaic       FALSE
#> 2  Reflectance bands        Orthomosaic       FALSE
#> 3   Spectral indices Spectral Analytics        TRUE
#> 4   Spectral indices Spectral Analytics        TRUE
#> 5   Spectral indices Spectral Analytics        TRUE
#> 6    Biomass proxies    Biomass proxies       FALSE
#> 7    Biomass proxies    Biomass proxies       FALSE
#> 8    Biomass proxies    Biomass proxies       FALSE
#> 9    Biomass proxies    Biomass proxies       FALSE
#> 10           Terrain  3D Modeling (CHM)        TRUE
#>                                               note
#> 1                   Per-pixel surface reflectance.
#> 2                   Per-pixel surface reflectance.
#> 3         Computed per pixel at native resolution.
#> 4         Computed per pixel at native resolution.
#> 5         Computed per pixel at native resolution.
#> 6  Mean of NDVI, SAVI and NDRE clipped to [-1, 1].
#> 7            NDVI multiplied by the CHM in metres.
#> 8            NDRE multiplied by the CHM in metres.
#> 9            SAVI multiplied by the CHM in metres.
#> 10       Canopy Height Model, metres above ground.
```
