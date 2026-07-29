# Catalogue caret models of a given type

All 137 regression-capable caret models are offered; the remaining 102
are classification-only and cannot fit a continuous biomass response.
`ready` says whether every backend package the model needs is already
installed, which is what the picker groups on.

## Usage

``` r
caret_model_catalogue(
  type = "Regression",
  installed = rownames(utils::installed.packages())
)
```

## Arguments

- type:

  caret model type, normally `"Regression"`.

- installed:

  Character vector of installed package names.

## Value

A data frame with `method`, `label`, `packages`, `missing`, `ready`,
`tags`, `n_params` and `needs_scaling`. Zero rows (with a warning) when
caret is not installed.

## Examples

``` r
if (requireNamespace("caret", quietly = TRUE)) {
  cat <- caret_model_catalogue()
  nrow(cat)
  head(cat[cat$ready, c("method", "label")])
}
#>    method                                      label
#> 17 avNNet              Model Averaged Neural Network
#> 18    bag                               Bagged Model
#> 21    bam   Generalized Additive Model using Splines
#> 42    gam   Generalized Additive Model using Splines
#> 52    glm                   Generalized Linear Model
#> 53 glm.nb Negative Binomial Generalized Linear Model
```
