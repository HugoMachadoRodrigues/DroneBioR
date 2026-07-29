# Create a DroneBioR project description

Create a DroneBioR project description

## Usage

``` r
dronebio_project(
  project_dir = getwd(),
  images_subdir = file.path("imagens", "micasense"),
  output_subdir = file.path("outputs", "dronebior_analysis"),
  odm_dataset_subdir = file.path("outputs", "odm_micasense_dataset"),
  odm_project_name = "micasense"
)
```

## Arguments

- project_dir:

  Root folder for the drone biomass project.

- images_subdir:

  Relative folder containing raw MicaSense images.

- output_subdir:

  Relative folder for DroneBioR outputs.

- odm_dataset_subdir:

  Relative folder mounted into ODM Docker.

- odm_project_name:

  ODM project name inside the dataset folder.

## Value

A list with normalized project paths.

## Examples

``` r
project <- dronebio_project(project_dir = tempdir())
project$project_dir
#> [1] "/tmp/RtmpTDYSwi"
project$odm_orthomosaic
#> [1] "/tmp/RtmpTDYSwi/outputs/odm_micasense_dataset/micasense/odm_orthophoto/odm_orthophoto.tif"
```
