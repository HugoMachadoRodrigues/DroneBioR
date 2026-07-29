# Predict from a fitted field model

Predict from a fitted field model

## Usage

``` r
# S3 method for class 'dronebio_field_model'
predict(object, newdata, na.action = stats::na.pass, ...)
```

## Arguments

- object:

  A `dronebio_field_model`.

- newdata:

  Data frame containing every covariate in `object$predictors`.

- na.action:

  Forced to [`stats::na.pass`](https://rdrr.io/r/stats/na.fail.html) by
  default: `predict.train()` defaults to `na.omit` and silently returns
  fewer rows than supplied.

- ...:

  Passed to `predict.train()`.

## Value

Numeric vector with one prediction per row of `newdata`.
