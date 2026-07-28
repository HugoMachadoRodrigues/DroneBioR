# Download an asset from a WebODM task

Common asset names: `orthophoto.tif`, `dsm.tif`, `dtm.tif`,
`point_cloud.laz`, `textured_model.zip`, `all.zip` (everything bundled).

## Usage

``` r
webodm_download_asset(
  base_url,
  token,
  project_id,
  task_id,
  asset_name,
  target_path
)
```

## Arguments

- base_url:

  Root URL of the WebODM server.

- token:

  JWT from
  [`webodm_authenticate()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/webodm_authenticate.md).

- project_id:

  Integer project ID from
  [`webodm_create_project()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/webodm_create_project.md).

- task_id:

  Task UUID from
  [`webodm_submit_task()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/webodm_submit_task.md).

- asset_name:

  Asset filename as listed in `webodm_task_status()$available_assets`.

- target_path:

  Local destination file.

## Value

Invisibly returns the absolute local path written.
