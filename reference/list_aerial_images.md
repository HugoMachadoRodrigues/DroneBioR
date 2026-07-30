# List generic aerial images for an ODM project

Permissive image lister for non-MicaSense flights (Sony RX1R, DJI
Phantom / Mavic, Phase One, generic RGB). Returns a
[`list_micasense_images()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/list_micasense_images.md)-
shaped manifest so
[`copy_images_for_odm()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/copy_images_for_odm.md)
and
[`run_odm_project()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/run_odm_project.md)
can consume it transparently, but without enforcing the
`^(.+)_([0-9]+)\.[A-Za-z0-9]+$` capture/band filename pattern. Accepts
`.jpg`, `.jpeg`, `.png`, `.tif` and `.tiff` (case-insensitive).

## Usage

``` r
list_aerial_images(images_dir)
```

## Arguments

- images_dir:

  Folder containing raw image files.

## Value

A data frame with the same columns as
[`list_micasense_images()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/list_micasense_images.md):
`file`, `filename`, `capture_id`, `band_id`, `file_size_mb`. For aerial
RGB sets, `capture_id` is the base filename without extension and
`band_id` is always `1L`.

## Details

**DJI Mavic 3M datasets** drop their multispectral
`_MS_{G,R,RE,NIR}.TIF` siblings from the returned manifest when at least
one matching `_D.JPG` is also present. This keeps the existing
[`run_odm_project()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/run_odm_project.md)
flow viable (ODM only sees the RGB JPGs for SfM, which is what it can
actually handle), and the returned data frame gets an attribute
`dji_visible_multispectral = TRUE` so callers know the MS TIFs were
filtered out. For full DJI Mavic 3M processing — including the four MS
bands — use
[`list_dji_mavic_3m_images()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/list_dji_mavic_3m_images.md)
and
[`run_odm_dji_mavic_3m()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/run_odm_dji_mavic_3m.md).

## Examples

``` r
tmp <- tempfile("aerial-"); dir.create(tmp)
for (i in 1:3) file.create(file.path(tmp, paste0("DJI_", sprintf("%04d", i), ".JPG")))
head(list_aerial_images(tmp))
#>                                               file     filename capture_id
#> 1 /tmp/RtmpT26ue6/aerial-22a26ad8975a/DJI_0001.JPG DJI_0001.JPG   DJI_0001
#> 2 /tmp/RtmpT26ue6/aerial-22a26ad8975a/DJI_0002.JPG DJI_0002.JPG   DJI_0002
#> 3 /tmp/RtmpT26ue6/aerial-22a26ad8975a/DJI_0003.JPG DJI_0003.JPG   DJI_0003
#>   band_id file_size_mb
#> 1       1            0
#> 2       1            0
#> 3       1            0
```
