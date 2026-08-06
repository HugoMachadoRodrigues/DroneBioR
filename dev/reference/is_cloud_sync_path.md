# Detect whether a path lives inside a cloud-sync provider folder

OneDrive / iCloud Drive / Google Drive paths on macOS live under
`~/Library/CloudStorage/`. Reads against those folders can trigger
background up/downloads ("Files On-Demand"), which is painful for a
GeoTIFF-heavy app. This helper returns the provider name so the Shiny UI
can warn the user and offer a local-cache migration.

## Usage

``` r
is_cloud_sync_path(path)
```

## Arguments

- path:

  Filesystem path to inspect.

## Value

`NA_character_` when not under a cloud-sync folder, otherwise the
provider name (e.g. "OneDrive", "GoogleDrive").

## Examples

``` r
is_cloud_sync_path("/Users/me/Documents")
#> [1] NA
is_cloud_sync_path("/Users/me/Library/CloudStorage/OneDrive-Acme/proj")
#> [1] "OneDrive"
```
