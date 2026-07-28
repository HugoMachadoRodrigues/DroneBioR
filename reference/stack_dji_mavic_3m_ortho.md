# Stack RGB + per-band MS orthomosaics into a single GeoTIFF

Reads the 3-band RGB ortho plus up to four single-band MS orthos and
resamples each MS layer onto the RGB grid (bilinear). The output band
order is `Red, Green, Blue, MS_G, MS_R, MS_RE, MS_NIR` — matching
[`default_dji_mavic_3m_band_map()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/default_dji_mavic_3m_band_map.md)
— so downstream
[`read_multispectral_orthomosaic()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/read_multispectral_orthomosaic.md)
auto-detects it.

## Usage

``` r
stack_dji_mavic_3m_ortho(rgb_ortho, ms_orthos, out_path)
```

## Arguments

- rgb_ortho:

  Path to the RGB ortho (3 bands; ODM convention is R/G/B).

- ms_orthos:

  Named character vector of paths to MS-band orthos. Names must be among
  `MS_G`, `MS_R`, `MS_RE`, `MS_NIR`. Missing bands are silently skipped
  (the resulting stack still works, it just exposes fewer indices).

- out_path:

  Destination GeoTIFF path.

## Value

The `out_path`, invisibly.
