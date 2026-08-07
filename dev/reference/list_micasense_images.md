# List MicaSense image files

List MicaSense image files

## Usage

``` r
list_micasense_images(images_dir)
```

## Arguments

- images_dir:

  Folder containing raw image files.

## Value

A data frame with file path, file name, capture id, band id and size.

## Examples

``` r
tmp <- tempfile("micasense-"); dir.create(tmp)
for (cap in sprintf("IMG_%04d", 1:3))
  for (band in 1:5)
    file.create(file.path(tmp, paste0(cap, "_", band, ".tif")))
head(list_micasense_images(tmp))
#>                                                  file       filename capture_id
#> 1 /tmp/RtmpatLiBZ/micasense-2324ab16ee/IMG_0001_1.tif IMG_0001_1.tif   IMG_0001
#> 2 /tmp/RtmpatLiBZ/micasense-2324ab16ee/IMG_0001_2.tif IMG_0001_2.tif   IMG_0001
#> 3 /tmp/RtmpatLiBZ/micasense-2324ab16ee/IMG_0001_3.tif IMG_0001_3.tif   IMG_0001
#> 4 /tmp/RtmpatLiBZ/micasense-2324ab16ee/IMG_0001_4.tif IMG_0001_4.tif   IMG_0001
#> 5 /tmp/RtmpatLiBZ/micasense-2324ab16ee/IMG_0001_5.tif IMG_0001_5.tif   IMG_0001
#> 6 /tmp/RtmpatLiBZ/micasense-2324ab16ee/IMG_0002_1.tif IMG_0002_1.tif   IMG_0002
#>   band_id file_size_mb
#> 1       1            0
#> 2       2            0
#> 3       3            0
#> 4       4            0
#> 5       5            0
#> 6       1            0
```
