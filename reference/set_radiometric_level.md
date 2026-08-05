# Record or read the radiometric state of a raster

Dividing digital numbers by a constant does not produce reflectance, and
conflating the two is easy. These functions write the state into the
raster's metadata tags, where it survives a GeoTIFF round trip, so a
product can always say what was done to it.

## Usage

``` r
set_radiometric_level(x, level, scale_factor = NA_real_)

radiometric_level(x)
```

## Arguments

- x:

  A
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html).

- level:

  One of the levels above.

- scale_factor:

  The divisor applied, when one was. `NA` otherwise.

## Value

`set_radiometric_level()` returns `x` with the tags set.
`radiometric_level()` returns a length-1 character, `"unknown"` when the
raster carries no tag.

## Details

The recognised levels are `"normalised_dn"` (DroneBioR divided by a
detected storage scale), `"engine_scaled"` (the raster already arrived
in 0-1, carrying whatever the photogrammetry engine applied),
`"panel_calibrated"` (tied to a reflectance standard through panel
regions or a downwelling-light sensor, and the only level that is
reflectance) and `"unknown"`.

## Examples

``` r
r <- terra::rast(nrows = 4, ncols = 4)
terra::values(r) <- seq_len(16) * 1000
radiometric_level(scale_to_reflectance(r))
#> [1] "normalised_dn"
```
