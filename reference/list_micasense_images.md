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
#>                                                    file       filename
#> 1 /tmp/Rtmp2iFtP8/micasense-27776274626f/IMG_0001_1.tif IMG_0001_1.tif
#> 2 /tmp/Rtmp2iFtP8/micasense-27776274626f/IMG_0001_2.tif IMG_0001_2.tif
#> 3 /tmp/Rtmp2iFtP8/micasense-27776274626f/IMG_0001_3.tif IMG_0001_3.tif
#> 4 /tmp/Rtmp2iFtP8/micasense-27776274626f/IMG_0001_4.tif IMG_0001_4.tif
#> 5 /tmp/Rtmp2iFtP8/micasense-27776274626f/IMG_0001_5.tif IMG_0001_5.tif
#> 6 /tmp/Rtmp2iFtP8/micasense-27776274626f/IMG_0002_1.tif IMG_0002_1.tif
#>   capture_id band_id file_size_mb
#> 1   IMG_0001       1            0
#> 2   IMG_0001       2            0
#> 3   IMG_0001       3            0
#> 4   IMG_0001       4            0
#> 5   IMG_0001       5            0
#> 6   IMG_0002       1            0
```
