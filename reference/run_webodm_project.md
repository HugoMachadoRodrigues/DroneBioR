# Run a DroneBioR project through a remote WebODM instance

Submits the project's MicaSense or aerial images to a WebODM server,
polls the task until completion, then downloads `orthophoto.tif`,
`dsm.tif`, `dtm.tif` and `point_cloud.laz` (each only if produced) into
the project's expected ODM-shaped folders.

## Usage

``` r
run_webodm_project(
  project,
  base_url,
  username,
  password,
  project_name = NULL,
  camera_type = c("multispectral", "rgb"),
  poll_seconds = 60,
  ...
)
```

## Arguments

- project:

  A `dronebio_project` object.

- base_url, username, password:

  WebODM server URL and credentials.

- project_name:

  WebODM project name (default: from `project`).

- camera_type:

  `"multispectral"` or `"rgb"` (see
  [`build_odm_args()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/build_odm_args.md)).

- poll_seconds:

  Status-poll interval in seconds. Default 60.

- ...:

  Forwarded to
  [`as_webodm_options()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/as_webodm_options.md).

## Value

Invisibly returns a list with `task_id`, `final_status`, and a named
character vector of downloaded local paths.

## Details

This is a blocking call: WebODM tasks can take many hours. Set
`poll_seconds` to control how often the function checks status.

## Examples

``` r
if (FALSE) { # \dontrun{
project <- dronebio_project("/path/to/flight")
run_webodm_project(
  project,
  base_url = "http://localhost:8000",
  username = "admin",
  password = "secret",
  camera_type = "rgb",
  build_dsm = TRUE,
  build_dtm = TRUE
)
} # }
```
