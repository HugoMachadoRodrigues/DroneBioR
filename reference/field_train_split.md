# Build the train / test partition and CV folds for a model sweep

Computed **once per sweep**, outside the model loop, and passed into
every
[`fit_field_caret_model()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/fit_field_caret_model.md)
call. Sharing one fold assignment is what makes the leaderboard a fair
comparison and the whole sweep reproducible from the seed shown in the
sidebar.

## Usage

``` r
field_train_split(
  data,
  response = "biomass_kgha",
  holdout = 0.25,
  folds = 10L,
  seed = 42L
)
```

## Arguments

- data:

  Extraction table with the response column.

- response:

  Response column name.

- holdout:

  Fraction held out as an independent test set (0 to 0.5). `0` disables
  the holdout.

- folds:

  Number of cross-validation folds (requirement: k = 10).

- seed:

  Random seed.

## Value

A list with `train_idx`, `test_idx`, `folds` (in-fold row positions
**within the training block**), `folds_out`, `holdout`, `folds_k` and
`seed`.

## Examples

``` r
if (requireNamespace("caret", quietly = TRUE)) {
  d <- data.frame(biomass_kgha = runif(60, 500, 4000))
  split <- field_train_split(d, holdout = 0.25, folds = 10)
  length(split$train_idx)
}
#> [1] 48
```
