# Parse and merge every .MRK in a folder

Concatenates the rows from each .MRK file, then collapses duplicates on
`photo_num` by keeping the row with the **highest fix quality**,
breaking ties with the smallest combined horizontal standard deviation.
This is the single source of truth the geo.txt writer consumes.

## Usage

``` r
parse_djim3m_mrk_folder(images_dir)
```

## Arguments

- images_dir:

  Folder holding `.MRK` files.

## Value

A data.frame in the same shape as
[`parse_djim3m_mrk()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/parse_djim3m_mrk.md),
with one row per unique `photo_num`.
