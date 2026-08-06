# Read field biomass data

Read field biomass data

## Usage

``` r
read_field_data(path)
```

## Arguments

- path:

  CSV path.

## Value

A data frame.

## Examples

``` r
field_path <- system.file("extdata", "field_samples.csv", package = "DroneBioR")
field <- read_field_data(field_path)
head(field)
#>   sample_id biomass_kgha        x       y
#> 1       S01       1763.3 392004.0 3033007
#> 2       S02       2446.1 392012.1 3033012
#> 3       S03       1551.4 392006.7 3033007
#> 4       S04       1763.5 392013.6 3033003
#> 5       S05       1939.4 392005.4 3033003
#> 6       S06       2299.4 392002.8 3033005
```
