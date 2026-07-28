# Write an ODM `geo.txt` from a .MRK folder + a list of filenames

For each entry in `image_filenames`, resolves the photo number from the
filename, looks up the matching row in the merged .MRK data, and emits
one geo.txt row:

## Usage

``` r
write_djim3m_geo_txt(
  images_dir,
  image_filenames,
  geo_txt_path,
  min_fix_quality = 4L
)
```

## Arguments

- images_dir:

  Folder holding the .MRK file(s).

- image_filenames:

  Character vector of image *basenames* (not full paths). Each is looked
  up by its photo-number suffix.

- geo_txt_path:

  Destination path for the ODM geo.txt.

- min_fix_quality:

  Drop rows whose fix quality is below this threshold. Default 4 (RTK
  Float) — anything below would not constrain the bundle adjustment
  usefully. Set to 0 to keep everything.

## Value

Invisibly a list with `written` (the path), `matched`, `unmatched`
(filenames that had no .MRK row), `dropped_quality` (filenames whose row
was below `min_fix_quality`).

## Details

    <filename> <lon> <lat> <alt> <yaw> <pitch> <roll> <horiz_acc> <vert_acc>

Yaw/pitch/roll are emitted as `0` because the .MRK does not record them
— ODM treats those columns as optional anyway. Accuracy columns come
straight from the .MRK std deviations (horiz_acc = sqrt(lat_std^2 +
lon_std^2)).

## Examples

``` r
if (FALSE) { # \dontrun{
  files <- list_aerial_images("/path/to/ifasbahia10")
  write_djim3m_geo_txt(
    images_dir      = "/path/to/ifasbahia10",
    image_filenames = files$filename,
    geo_txt_path    = "/tmp/geo.txt"
  )
} # }
```
