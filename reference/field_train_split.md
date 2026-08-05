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
  seed = 42L,
  blocking = c("random", "spatial"),
  coords = c("x", "y"),
  block_size = NULL
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

- blocking:

  How the partition and the folds are formed. `"random"` (default)
  reproduces the previous behaviour. `"spatial"` groups samples into
  square blocks and assigns whole blocks, which is the appropriate
  choice when the samples come from one flight.

- coords:

  Length-2 character vector naming the coordinate columns used for
  spatial blocking. Ignored when `blocking = "random"`.

- block_size:

  Side length of a block, in the units of `coords`. When `NULL`
  (default) a size is chosen so that roughly `5 * folds` blocks are
  occupied, which keeps the folds balanced without making blocks so
  small that neighbouring samples still straddle them.

## Value

A list with `train_idx`, `test_idx`, `folds` (in-fold row positions
**within the training block**), `folds_out`, `holdout`, `folds_k` and
`seed`. When `blocking = "spatial"` it also carries `blocking`,
`block_id` (one block label per row of `data`), `block_size` and
`n_blocks`.

## Details

Samples drawn from a single flight are spatially autocorrelated, so a
random partition can place neighbouring quadrats on both sides of the
split and return an optimistic score. Set `blocking = "spatial"` to lay
a regular grid over the sample coordinates and keep whole blocks
together: the hold-out withholds entire blocks, and the cross-validation
folds are grouped by block as well, so no block is ever split between
fitting and scoring.

## Examples

``` r
if (requireNamespace("caret", quietly = TRUE)) {
  d <- data.frame(biomass_kgha = runif(60, 500, 4000))
  split <- field_train_split(d, holdout = 0.25, folds = 10)
  length(split$train_idx)

  # spatially blocked: neighbouring samples stay on the same side
  d$x <- runif(60, 0, 100)
  d$y <- runif(60, 0, 100)
  sp <- field_train_split(d, holdout = 0.25, folds = 5,
                          blocking = "spatial")
  sp$n_blocks
}
#> [1] 21
```
