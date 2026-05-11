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
  use_alpha = TRUE
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
