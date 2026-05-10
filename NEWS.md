# DroneBioR (development version)

# DroneBioR 0.2.0

This release prepares the package for an open-source release for the academic
and research community.

## Major changes

* Documentation, README and metadata overhauled for a public release on
  GitHub.
* `DESCRIPTION`: declared `terra` and `sf` as `Imports` (they were used by
  exported functions but listed only in `Suggests`); added `methods` to
  `Imports`; added `URL`, `BugReports`, `SystemRequirements`, `VignetteBuilder`
  and `Language`; normalized `Authors@R` to a single canonical entry.
* License file `LICENSE.md` added (full GNU AGPL v3 text). License field
  standardized to `AGPL (>= 3)`.
* New `.gitignore` and updated `.Rbuildignore`.
* `convert_undistorted_tiffs_for_texturing()` now exported (its `.Rd` was
  orphaned in 0.1.0).
* `NAMESPACE` rebuilt with proper `importFrom()` directives for base packages.
* GitHub Actions workflow `R-CMD-check.yaml` added.
* New `CONTRIBUTING.md`, `CODE_OF_CONDUCT.md` and `inst/CITATION` files.
* Vignettes added under `vignettes/`:
  - `dronebior-overview` - end-to-end workflow.
  - `external-engines` - reading products from ODM, WebODM, Pix4Dmapper and
    Agisoft Metashape.
  - `spectral-indices` - the index catalogue and recommended use.
  - `point-clouds-and-chm` - dense cloud, DSM/DTM and CHM analyses.

## Internal

* Added a package-level `_PACKAGE` documentation with overview and engine
  references.

# DroneBioR 0.1.0

* Initial development release. Scope: read MicaSense images, drive ODM via
  Docker, read multispectral orthomosaics, scale reflectance, compute
  vegetation indices, extract field samples, fit a baseline biomass model,
  and provide a Shiny app prototype.
