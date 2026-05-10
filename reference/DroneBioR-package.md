# DroneBioR: Reproducible Drone Biomass Workflows for Multispectral Imagery

DroneBioR wraps external photogrammetry engines (OpenDroneMap, WebODM,
Pix4Dmapper, Agisoft Metashape) to produce orthomosaics, DSM/DTM, dense
point clouds and meshes, then provides the scientific layer in R: alpha
and no-data masking, radiometric scaling to reflectance, vegetation
indices, canopy height models, point-cloud ROI analysis, individual tree
candidate detection, field sample extraction, baseline biomass models,
and a Shiny application for interactive exploration.

## Reading the products of external engines

DroneBioR can read products from any photogrammetry engine that produces
standard formats:

- GeoTIFF orthomosaics (multi-band MicaSense layout, with optional
  alpha);

- GeoTIFF DSM and DTM rasters;

- LAS / LAZ / COPC point clouds (PDAL/lidR optional);

- PLY point clouds and meshes;

- OBJ textured models.

## Engines

- OpenDroneMap (ODM) via Docker: see
  [`run_odm_project()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/run_odm_project.md).

- WebODM, Pix4Dmapper, Agisoft Metashape: just point DroneBioR at the
  output folder produced by those tools.

## See also

Useful links:

- <https://github.com/HugoMachadoRodrigues/DroneBioR>

- Report bugs at
  <https://github.com/HugoMachadoRodrigues/DroneBioR/issues>

## Author

**Maintainer**: Hugo Machado Rodrigues
<rodrigues.machado.hugo@gmail.com> \[copyright holder\]
