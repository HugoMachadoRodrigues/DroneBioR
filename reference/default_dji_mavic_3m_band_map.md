# Default band map for the DJI Mavic 3M 7-band stacked orthomosaic

[`run_odm_dji_mavic_3m()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/run_odm_dji_mavic_3m.md)
stacks five separate ODM outputs into a single GeoTIFF with band order
Red, Green, Blue (from the RGB run) followed by MS_G, MS_R, MS_RE,
MS_NIR (from the four per-band MS runs). For spectral-index computation
we want the *radiometrically calibrated* MS bands and nothing else: this
band map points Green/Red/RedEdge/NIR at the MS layers (4-7) and **drops
the Blue channel entirely** because the Mavic 3M does not capture a
calibrated blue MS band — the only Blue available is the uncalibrated
RGB JPG channel, and mixing it with calibrated MS bands inside EVI /
VARI / ExG / GLI / TGI / RGBVI produces a hybrid number that is not
comparable to the values in the literature. With Blue absent,
[`compute_spectral_indices()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/compute_spectral_indices.md)
automatically skips the six Blue-dependent indices and returns the 16
indices the MS bands can support honestly.

## Usage

``` r
default_dji_mavic_3m_band_map()
```

## Value

Named integer vector with `Green`, `Red`, `RedEdge`, `NIR`.

## Details

Users who specifically want the visible-band indices on the RGB JPG
channels can override the band map manually with
`c(Blue = 3, Green = 2, Red = 1)` and pass it to
[`read_multispectral_orthomosaic()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/read_multispectral_orthomosaic.md).
