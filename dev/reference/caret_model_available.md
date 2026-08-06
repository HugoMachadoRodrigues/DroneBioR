# Check whether a caret model can be trained here

Must be called before every
[`caret::train()`](https://rdrr.io/pkg/caret/man/train.html) call.
caret's own install check opens an interactive
[`menu()`](https://rdrr.io/r/utils/menu.html) prompt when a backend is
missing, which hangs a Shiny session started from an interactive R
process.

## Usage

``` r
caret_model_available(
  method,
  installed = rownames(utils::installed.packages())
)
```

## Arguments

- method:

  caret method id.

- installed:

  Character vector of installed package names.

## Value

A list with `ok`, `missing` (packages) and `install_call` (a
ready-to-paste
[`install.packages()`](https://rdrr.io/r/utils/install.packages.html)
line, `NA` when nothing is missing).

## Examples

``` r
if (requireNamespace("caret", quietly = TRUE)) {
  caret_model_available("lm")$ok
}
#> [1] TRUE
```
