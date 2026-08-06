# Convert ODM undistorted Float TIFFs to UInt16 for texturing

MVS-Texturing inside ODM occasionally fails on float undistorted images
produced from MicaSense reflectance TIFFs. This helper rewrites the
undistorted TIFFs as UInt16 (0-65535) so MVS-Texturing can consume them
on a re-run from the `mvs_texturing` stage.

## Usage

``` r
convert_undistorted_tiffs_for_texturing(odm_project_dir)
```

## Arguments

- odm_project_dir:

  ODM project folder.

## Value

Number of files converted.

## Examples

``` r
if (FALSE) { # \dontrun{
n <- convert_undistorted_tiffs_for_texturing("/path/to/odm_project")
} # }
```
