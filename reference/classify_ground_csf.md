# Classify ground points in a LAS file using lidR's CSF algorithm

Bridge to
[`lidR::classify_ground()`](https://rdrr.io/pkg/lidR/man/classify.html)
with the Cloth Simulation Filter (Zhang et al., 2016). Returns a
[`lidR::LAS`](https://rdrr.io/pkg/lidR/man/LAS-class.html) object whose
`Classification` attribute marks ground points as 2 and the rest as 1.

## Usage

``` r
classify_ground_csf(
  las_path,
  sloop_smooth = FALSE,
  class_threshold = 0.5,
  cloth_resolution = 0.5,
  rigidness = 1L,
  ...
)
```

## Arguments

- las_path:

  Path to a LAS or LAZ file.

- sloop_smooth:

  Logical, passed to
  [`lidR::csf()`](https://rdrr.io/pkg/lidR/man/gnd_csf.html). Smooth
  before building the simulated cloth.

- class_threshold:

  Numeric distance in meters from cloth to point; below this, the point
  is ground.

- cloth_resolution:

  Numeric cloth grid resolution in meters.

- rigidness:

  Integer 1, 2, or 3. Cloth rigidness.

- ...:

  Additional arguments passed to
  [`lidR::csf()`](https://rdrr.io/pkg/lidR/man/gnd_csf.html).

## Value

A classified [`lidR::LAS`](https://rdrr.io/pkg/lidR/man/LAS-class.html)
object.

## Details

Requires the optional `lidR` package (a Suggests dependency). The
function does not implement CSF in R itself - it just wraps the upstream
algorithm so users can stay inside the DroneBioR API surface.

## Examples

``` r
if (FALSE) { # \dontrun{
las <- classify_ground_csf(
  "outputs/odm_micasense_dataset/micasense/odm_georeferencing/odm_georeferenced_model.las",
  class_threshold = 0.5,
  cloth_resolution = 0.5
)
} # }
```
