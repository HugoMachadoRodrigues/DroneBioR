# The band map [`read_multispectral_orthomosaic()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/read_multispectral_orthomosaic.md) picks for a layer count

Kept as one function so the reader and anything that has to predict what
the reader will do – band availability in the app, for one – cannot
drift apart. A 7-layer DJI stack maps only the calibrated bands, which
is why Blue is absent there even though the stack carries an RGB
triplet.

## Usage

``` r
default_band_map_for_layers(n_layers)
```

## Arguments

- n_layers:

  Number of layers in the orthomosaic.

## Value

A named integer vector of layer positions.

## Examples

``` r
names(default_band_map_for_layers(7))
#> [1] "Green"   "Red"     "RedEdge" "NIR"    
names(default_band_map_for_layers(5))
#> [1] "Red"     "Green"   "Blue"    "NIR"     "RedEdge"
```
