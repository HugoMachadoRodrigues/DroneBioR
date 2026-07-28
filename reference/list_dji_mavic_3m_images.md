# List DJI Mavic 3M images grouped by camera band

Splits a folder of DJI Mavic 3M raw images into the five camera streams
the platform produces per capture: the RGB visible (`D`, `.JPG`) plus
the four single-band multispectral TIFFs (`MS_G`, `MS_R`, `MS_RE`,
`MS_NIR`). Each stream is returned as a
[`list_aerial_images()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/list_aerial_images.md)-style
manifest so
[`copy_images_for_odm()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/copy_images_for_odm.md)
/
[`run_odm_project()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/run_odm_project.md)
can consume it transparently in a per-band ODM workflow (see
[`run_odm_dji_mavic_3m()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/run_odm_dji_mavic_3m.md)).

## Usage

``` r
list_dji_mavic_3m_images(images_dir)
```

## Arguments

- images_dir:

  Folder containing raw DJI Mavic 3M images.

## Value

A named list with up to five elements - `D`, `MS_G`, `MS_R`, `MS_RE`,
`MS_NIR` - each a data frame in the
[`list_aerial_images()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/list_aerial_images.md)
shape. Bands that have no matching images are omitted from the list.

## Details

Files that do not match the DJI Mavic 3M naming convention are ignored.
A folder that contains zero `_D.JPG` *and* zero `_MS_*.TIF` is treated
as an error.

## Examples

``` r
tmp <- tempfile("djim3m-"); dir.create(tmp)
for (i in 1:3) {
  stem <- sprintf("DJI_20260501132033_%04d", i)
  file.create(file.path(tmp, paste0(stem, "_D.JPG")))
  for (b in c("G", "R", "RE", "NIR"))
    file.create(file.path(tmp, paste0(stem, "_MS_", b, ".TIF")))
}
names(list_dji_mavic_3m_images(tmp))
#> [1] "D"      "MS_G"   "MS_R"   "MS_RE"  "MS_NIR"
```
