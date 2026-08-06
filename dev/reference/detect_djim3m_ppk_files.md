# Detect DJI Mavic 3M PPK / RTK sidecar files

Scans `images_dir` for the three families of DJI Mavic 3M sidecar files:
`*_Timestamp.MRK` (trigger events + RTK positions), `*_PPKRAW.bin` (raw
GNSS observables) and `*_PPKNAV.nav` (broadcast ephemerides). One
mission produces one file per family; a folder can hold several missions
and therefore several files of each.

## Usage

``` r
detect_djim3m_ppk_files(images_dir)
```

## Arguments

- images_dir:

  Folder containing the raw DJI Mavic 3M images.

## Value

A named list with character vectors `mrk`, `bin`, `nav` (each empty when
nothing matches). The `has_ppk_inputs` element is `TRUE` when at least
one `.bin` AND one `.nav` are present — the minimum needed by an
external PPK CLI.

## Examples

``` r
if (FALSE) { # \dontrun{
  files <- detect_djim3m_ppk_files("/path/to/ifasbahia10")
  files$mrk
} # }
```
