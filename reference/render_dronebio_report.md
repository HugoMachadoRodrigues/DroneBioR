# Render a DroneBioR biomass report

Renders the bundled RMarkdown template `inst/report/biomass_report.Rmd`
against a DroneBioR project. The report contains the ODM product
inventory, per-band reflectance and per-index summaries, index
histograms, the canopy height model and (when a field CSV is supplied)
the baseline biomass model.

## Usage

``` r
render_dronebio_report(
  project,
  output_file = NULL,
  field_csv = NULL,
  use_alpha = TRUE,
  roi_geojson = NULL,
  snapshot_path = NULL
)
```

## Arguments

- project:

  A `dronebio_project` object or a project directory path.

- output_file:

  Output HTML file path. Defaults to `DroneBioR_report.html` in the
  project directory.

- field_csv:

  Optional path to a field biomass CSV. When supplied, the report
  includes the baseline biomass model section.

- use_alpha:

  Logical. Use the orthomosaic alpha band as a valid-data mask.

- roi_geojson:

  Optional path to a GeoJSON file containing one or more ROI polygons
  (the format `studio_assets/rois.geojson` produces). When supplied, the
  report adds a "Survey-grade volumes" section that runs
  [`compute_survey_volumes()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/compute_survey_volumes.md)
  with four base-reference methods (DTM, min Z, mean Z, perimeter TIN)
  for each ROI. Defaults to `<project>/studio_assets/rois.geojson` when
  that file exists.

- snapshot_path:

  Optional PNG of the 3D viewer (e.g. from the "Screenshot" toolbar
  button in Drone Biomass Studio). When supplied, the report embeds it
  in the "3D scene documentation" section. Otherwise the section falls
  back to a server-side
  [`persp()`](https://rdrr.io/r/graphics/persp.html) rendering of the
  DSM.

## Value

Invisibly returns the absolute path to the rendered file.

## Details

Requires the `rmarkdown` package (a Suggests dependency).

## Examples

``` r
# \donttest{
project <- dronebio_sample_project(target_dir = tempfile("dronebior-sample-"))
out <- render_dronebio_report(
  project     = project,
  output_file = file.path(tempdir(), "demo_report.html"),
  field_csv   = file.path(project$project_dir, "field_samples.csv")
)
file.exists(out)
#> [1] TRUE
# }
```
