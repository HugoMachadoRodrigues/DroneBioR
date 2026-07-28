# Predict biomass from plate-meter heights

Predict biomass from plate-meter heights

## Usage

``` r
# S3 method for class 'dronebio_plate_meter'
predict(object, newdata, ...)
```

## Arguments

- object:

  A `dronebio_plate_meter` from
  [`fit_plate_meter()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/fit_plate_meter.md).

- newdata:

  Data frame containing the height (and group) column(s).

- ...:

  Unused.

## Value

Numeric vector of predicted biomass (kg/ha), clamped at 0.
