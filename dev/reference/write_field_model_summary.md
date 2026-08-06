# Write a plain-text summary of a fitted field model

Creates intermediate directories. The Shiny studio writes this to
`<project_dir>/outputs/biomass_model_summary.txt`, which is the path the
workflow stepper checks for its Field step.

## Usage

``` r
write_field_model_summary(model, path)
```

## Arguments

- model:

  A `dronebio_field_model`.

- path:

  Output text file.

## Value

`path`, invisibly.
