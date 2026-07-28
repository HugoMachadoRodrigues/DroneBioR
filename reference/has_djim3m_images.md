# Does this folder hold a DJI Mavic 3M image set?

Checks whether *any* file in `images_dir` matches the DJI Mavic 3M
filename pattern (`DJI_<datetime>_<NNNN>_<D|MS_(G|R|RE|NIR)>.<ext>`).
Used by the Drone Biomass Studio app to gate which manifest / processing
engine to dispatch — the legacy
[`list_micasense_images()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/list_micasense_images.md)
path errors out on DJI names, while
[`run_odm_dji_mavic_3m()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/run_odm_dji_mavic_3m.md)
handles them natively.

## Usage

``` r
has_djim3m_images(images_dir)
```

## Arguments

- images_dir:

  Folder containing raw images.

## Value

`TRUE` when at least one filename matches the DJI Mavic 3M pattern,
`FALSE` otherwise (including when the directory does not exist or is
empty).

## Examples

``` r
if (FALSE) { # \dontrun{
  has_djim3m_images("/path/to/ifasbahia10")
} # }
```
