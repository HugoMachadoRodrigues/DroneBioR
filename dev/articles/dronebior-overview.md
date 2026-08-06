# DroneBioR overview: end-to-end drone biomass workflow

## Purpose

This vignette walks through the canonical DroneBioR pipeline for a
MicaSense RedEdge mission, from a folder of raw images to a baseline
biomass model. DroneBioR delegates Structure-from-Motion and Multi-View
Stereo to an external photogrammetry engine (OpenDroneMap, WebODM,
Pix4Dmapper or Agisoft Metashape) and contributes the scientific layer
in R.

## Setup

``` r

library(DroneBioR)

# Optional on macOS, when sf or terra cannot find proj.db
configure_proj_database(verbose = TRUE)
```

## 1. Describe the project

[`dronebio_project()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/dronebio_project.md)
returns a list of normalized paths that the other functions consume.
Defaults assume MicaSense images under `imagens/micasense/` and outputs
under `outputs/dronebior_analysis/`.

``` r

project <- dronebio_project(
  project_dir = "/path/to/Drone_Biomass"
)
project
```

## 2. (Optional) Drive ODM via Docker

If you have not produced an orthomosaic yet, DroneBioR can prepare the
dataset folder and run OpenDroneMap inside Docker. Skip this section if
you are bringing products from Pix4Dmapper or Metashape — see the
`external-engines` vignette for those workflows.

``` r

manifest <- list_micasense_images(project$images_dir)
copy_images_for_odm(manifest, project$odm_images_dir)

run_odm_project(project, multispectral = TRUE)
summarize_odm_products(project)
```

## 3. End-to-end scientific workflow

[`run_dronebio_workflow()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/run_dronebio_workflow.md)
chains the scientific steps: read the orthomosaic, apply the alpha mask
when a sixth layer is present, scale to reflectance, compute the index
stack, derive a biomass proxy and write outputs to disk.

``` r

result <- run_dronebio_workflow(project, use_alpha = TRUE)
names(result)
summarize_spatraster(result$reflectance)
```

The returned list contains, among others, `reflectance`, `indices`,
`biomass_proxy`, `alpha` and the `output_dir` where rasters were
written.

## 4. Field samples and baseline biomass model

The field CSV needs `sample_id`, `biomass_kgha` and either `x`/`y` in
the predictors’ CRS or `longitude`/`latitude` in WGS84.

``` r

field <- read_field_data("data/field_samples.csv")

joined <- extract_field_spectral_data(
  field_data = field,
  predictors = result$indices
)

model <- fit_biomass_lm(joined)
summary(model)
```

By default
[`fit_biomass_lm()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/fit_biomass_lm.md)
picks the available columns among NDVI, NDRE, EVI, SAVI, NDWI, NIR and
RedEdge. Pass `predictors = c("NDVI", "NDRE")` to constrain the model.

## 5. Interactive exploration

``` r

run_drone_biomass_studio(project_dir = project$project_dir)
```

## Where to go next

- `external-engines` — read products from ODM, WebODM, Pix4Dmapper or
  Agisoft Metashape without re-running photogrammetry.
- `spectral-indices` — the index catalogue and recommendations per crop
  / canopy density.
- `point-clouds-and-chm` — CHM, ROI selection and dense cloud analyses.
