# Canonicalise covariate / layer names

Applied identically to the training frame and to every prediction stack,
so a CHM that arrived as `CHM`, `chm` or `CHM_m` becomes one name and
[`terra::predict()`](https://rspatial.github.io/terra/reference/predict.html)
never fails on a mismatch.

## Usage

``` r
normalize_covariate_names(x, warn = TRUE)
```

## Arguments

- x:

  Character vector of names.

- warn:

  Emit a warning listing the renames.

## Value

A named character vector mapping each original name to its normalised
form.

## Details

True duplicates are an error rather than a silent rename: a stack with
two layers called `CHM_m` would make
[`terra::predict()`](https://rspatial.github.io/terra/reference/predict.html)
fail much later with `duplicate names in SpatRaster`.

## Examples

``` r
normalize_covariate_names(c("NDVI", "chm", "dsm"), warn = FALSE)
#>    NDVI     chm     dsm 
#>  "NDVI" "CHM_m"   "DSM" 
```
