# Package index

## Project setup

Build the project description used by the rest of the package, seed a
clickable sample project from bundled fixtures, and configure PROJ on
systems where terra or sf cannot find it.

- [`dronebio_project()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/dronebio_project.md)
  : Create a DroneBioR project description
- [`dronebio_sample_project()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/dronebio_sample_project.md)
  : Seed a clickable sample DroneBioR project from bundled fixtures
- [`configure_proj_database()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/configure_proj_database.md)
  : Configure PROJ paths for terra and sf

## External photogrammetry engines

Stage MicaSense images for ODM, drive ODM via Docker, and read products
from ODM, WebODM, Pix4Dmapper and Agisoft Metashape.

- [`list_micasense_images()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/list_micasense_images.md)
  : List MicaSense image files
- [`copy_images_for_odm()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/copy_images_for_odm.md)
  : Copy images into an ODM project folder
- [`build_odm_args()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/build_odm_args.md)
  : Build an ODM Docker command
- [`run_odm_project()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/run_odm_project.md)
  : Run ODM through Docker for a DroneBioR project
- [`convert_undistorted_tiffs_for_texturing()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/convert_undistorted_tiffs_for_texturing.md)
  : Convert ODM undistorted Float TIFFs to UInt16 for texturing
- [`odm_product_paths()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/odm_product_paths.md)
  : Return expected ODM product paths
- [`summarize_odm_products()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/summarize_odm_products.md)
  : Summarize available ODM products

## Multispectral orthomosaic

Read and scale a multispectral orthomosaic, summarize layers and write
DroneBioR raster products to disk.

- [`read_multispectral_orthomosaic()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/read_multispectral_orthomosaic.md)
  : Read a multispectral orthomosaic
- [`scale_to_reflectance()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/scale_to_reflectance.md)
  : Scale raster values to reflectance-like 0-1 values
- [`summarize_spatraster()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/summarize_spatraster.md)
  : Summarize a SpatRaster by layer
- [`write_dronebio_rasters()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/write_dronebio_rasters.md)
  : Write DroneBioR raster products

## Vegetation indices and biomass proxy

- [`compute_spectral_indices()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/compute_spectral_indices.md)
  : Compute spectral vegetation indices
- [`compute_biomass_proxy()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/compute_biomass_proxy.md)
  : Compute an image-only biomass proxy

## Classification

- [`classify_ground_vegetation()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/classify_ground_vegetation.md)
  : Rule-based ground / vegetation classification from NDVI (and CHM)
- [`classify_ground_csf()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/classify_ground_csf.md)
  : Classify ground points in a LAS file using lidR's CSF algorithm

## Field data and baseline biomass model

- [`read_field_data()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/read_field_data.md)
  : Read field biomass data
- [`extract_field_spectral_data()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/extract_field_spectral_data.md)
  : Extract raster values at field sample points
- [`fit_biomass_lm()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/fit_biomass_lm.md)
  : Fit a baseline biomass linear model

## Point clouds and CHM

Read dense point clouds, build a canopy height model from DSM/DTM and
attach heights to selected points.

- [`read_full_point_cloud()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/read_full_point_cloud.md)
  : Read a full-resolution LAS/LAZ/COPC point cloud
- [`read_las_point_cloud()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/read_las_point_cloud.md)
  : Read an uncompressed LAS point cloud
- [`read_ply_point_cloud()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/read_ply_point_cloud.md)
  : Read a binary little-endian PLY point cloud sample
- [`build_chm_from_dsm_dtm()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/build_chm_from_dsm_dtm.md)
  : Build a canopy height model from DSM and DTM rasters
- [`add_chm_heights()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/add_chm_heights.md)
  : Add CHM-derived heights to selected points
- [`add_point_heights()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/add_point_heights.md)
  : Add local height above a ground proxy to point cloud data
- [`compute_chm_roi_metrics()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/compute_chm_roi_metrics.md)
  : Compute CHM metrics for a polygon ROI

## Region of interest and selection metrics

- [`build_roi_polygon()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/build_roi_polygon.md)
  : Build a 2D ROI polygon from selected points
- [`points_in_roi()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/points_in_roi.md)
  : Test whether coordinates are inside a polygon ROI
- [`filter_points_by_roi()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/filter_points_by_roi.md)
  : Filter point cloud data by a polygon ROI
- [`compute_selection_metrics()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/compute_selection_metrics.md)
  : Compute selection metrics for a point cloud ROI
- [`compute_vertical_profile()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/compute_vertical_profile.md)
  : Compute a vertical point-density profile
- [`derive_tree_candidates()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/derive_tree_candidates.md)
  : Derive approximate tree candidates from a point cloud
- [`export_point_selection()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/export_point_selection.md)
  : Export a selected point cloud ROI

## Reports

- [`render_dronebio_report()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/render_dronebio_report.md)
  : Render a DroneBioR biomass report

## Time series across flights

- [`default_flight_registry()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/default_flight_registry.md)
  : Default location of the DroneBioR flight registry

- [`register_flight()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/register_flight.md)
  : Register a flight in the time-series registry

- [`list_flights()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/list_flights.md)
  : List flights registered in the time-series registry

- [`flight_time_series()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/flight_time_series.md)
  : Compute a time series of a custom flight summary

- [`flight_ndvi_mean()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/flight_summary_helpers.md)
  [`flight_biomass_proxy_mean()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/flight_summary_helpers.md)
  [`flight_chm_mean()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/flight_summary_helpers.md)
  :

  Stock summary helpers for
  [`flight_time_series()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/flight_time_series.md)

## Workflow and Shiny app

- [`run_dronebio_workflow()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/run_dronebio_workflow.md)
  : Run the DroneBioR orthomosaic analysis workflow
- [`run_drone_biomass_studio()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/run_drone_biomass_studio.md)
  : Start Drone Biomass Studio
- [`with_error_toast()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/with_error_toast.md)
  : Run an expression and report errors as Shiny toasts
