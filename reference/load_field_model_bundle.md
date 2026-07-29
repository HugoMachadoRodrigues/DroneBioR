# Load a field model bundle written by save_field_model_bundle()

Load a field model bundle written by save_field_model_bundle()

## Usage

``` r
load_field_model_bundle(path, check_packages = TRUE)
```

## Arguments

- path:

  Bundle `.zip` path.

- check_packages:

  Warn when the caret backend the model needs is not installed here.

## Value

The `dronebio_field_model`, with a `bundle_dir` attribute pointing at
the unpacked files.
