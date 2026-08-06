# Inspect a DJI Mavic 3M .MRK folder

Prints a human-readable summary of every .MRK rolled up: row count,
fix-quality breakdown, lat / lon / altitude spans, and typical standard
deviations. Use this before plumbing the file into ODM to confirm the
RTK quality is good.

## Usage

``` r
inspect_djim3m_mrk(images_dir)
```

## Arguments

- images_dir:

  Folder holding `.MRK` files.

## Value

Invisibly, the merged data.frame from
[`parse_djim3m_mrk_folder()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/parse_djim3m_mrk_folder.md).

## Examples

``` r
if (FALSE) { # \dontrun{
  inspect_djim3m_mrk("/path/to/ifasbahia10")
} # }
```
