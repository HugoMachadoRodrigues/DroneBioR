# Copy images into an ODM project folder

Copy images into an ODM project folder

## Usage

``` r
copy_images_for_odm(manifest, odm_images_dir)
```

## Arguments

- manifest:

  Data frame from
  [`list_micasense_images()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/list_micasense_images.md).

- odm_images_dir:

  ODM `images` folder.

## Value

Invisibly returns the destination paths.

## Examples

``` r
src <- tempfile("src-"); dir.create(src)
for (cap in sprintf("IMG_%04d", 1:2))
  for (band in 1:5)
    file.create(file.path(src, paste0(cap, "_", band, ".tif")))
manifest <- list_micasense_images(src)
dest <- tempfile("odm-images-")
copy_images_for_odm(manifest, dest)
length(list.files(dest))
#> [1] 10
```
