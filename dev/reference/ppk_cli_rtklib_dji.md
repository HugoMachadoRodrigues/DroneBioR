# PPK CLI hook factory using rtklib + a DJI .bin -\> RINEX converter

Returns a function suitable for the `ppk_cli` argument of
[`run_odm_dji_mavic_3m()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/run_odm_dji_mavic_3m.md).
When called, the function:

## Usage

``` r
ppk_cli_rtklib_dji(
  base_obs_path,
  dji_bin_to_rinex_cmd,
  rnx2rtkp_cmd = "rnx2rtkp",
  rnx2rtkp_extra = character()
)
```

## Arguments

- base_obs_path:

  Path to the base station RINEX observation file. Required.

- dji_bin_to_rinex_cmd:

  Command (name or full path) that converts a DJI Mavic 3M `_PPKRAW.bin`
  to RINEX. Must accept the .bin path as its first positional argument
  and write the RINEX `.obs` to its standard output **or** to a file
  named `<bin>.obs`.

- rnx2rtkp_cmd:

  Command for rtklib's PPK runner. Default `"rnx2rtkp"`.

- rnx2rtkp_extra:

  Extra arguments forwarded to `rnx2rtkp`. For example `c("-p", "0")` to
  force static positioning, or `c("-c", "/path/to/config.conf")` for a
  custom configuration.

## Value

A function with signature
`function(images_dir, bin_paths, nav_paths, mrk_paths)`, ready to pass
as `ppk_cli` to
[`run_odm_dji_mavic_3m()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/run_odm_dji_mavic_3m.md).

## Details

1.  Converts the DJI `_PPKRAW.bin` rover observations into RINEX using
    `dji_bin_to_rinex_cmd` (a user-supplied CLI that knows DJI's
    proprietary binary; **rtklib's `convbin` does NOT handle DJI .bin
    out of the box**, so this must be a tool the user installed
    separately — for example
    [`klauppk`](https://github.com/heliopas/klauppk) or any DJI Smart
    Farm-compatible converter).

2.  Runs rtklib's `rnx2rtkp` with the rover RINEX, the `_PPKNAV.nav`
    ephemerides and the **user-supplied base station RINEX** to produce
    a positioning solution.

3.  Rewrites each `_Timestamp.MRK` in place with the corrected lat / lon
    / alt per trigger event by matching the .MRK GPS time-of-week column
    to the solution's timestamps.

If any step fails the hook emits a
[`warning()`](https://rdrr.io/r/base/warning.html) and leaves the .MRK
files unchanged;
[`run_odm_dji_mavic_3m()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/run_odm_dji_mavic_3m.md)
then falls back to the .MRK-as-shipped path.

Tooling expectations:

- `rnx2rtkp` must be on `PATH`. On macOS: `brew install rtklib`.

- `dji_bin_to_rinex_cmd` must be on `PATH` or a full path. Pass the
  user-installed converter's name explicitly — there is no universal
  default.

- `base_obs_path` is the base station RINEX observation file (`.YYo` /
  `.obs`). The user typically downloads this from a public CORS network
  for the flight day + a base receiver covering the survey area.

## Examples

``` r
if (FALSE) { # \dontrun{
  hook <- ppk_cli_rtklib_dji(
    base_obs_path        = "~/ppk/base_2026_05_01.26o",
    dji_bin_to_rinex_cmd = "klauppk_dji_to_rinex"
  )
  run_odm_dji_mavic_3m(project, ppk_cli = hook)
} # }
```
