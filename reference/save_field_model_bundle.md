# Save a trained field model as a self-contained zip bundle

A bare [`saveRDS()`](https://rdrr.io/r/base/readRDS.html) is not enough
to run a model later outside the app, so the bundle also carries the
metrics, the samples, a README naming the exact package versions and
covariate order, and a runnable scoring script.

## Usage

``` r
save_field_model_bundle(model, path, samples = NULL, map_path = NULL)
```

## Arguments

- model:

  A `dronebio_field_model`.

- path:

  Destination `.zip` path.

- samples:

  Optional extraction table to include as `samples.csv`.

- map_path:

  Optional predicted-map GeoTIFF to include.

## Value

`path`, invisibly.
