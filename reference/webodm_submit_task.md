# Submit a task to a WebODM project

Uploads the supplied image files as multipart form data alongside an
`options` JSON array (the WebODM format:
`[{"name": "dsm", "value": true}, ...]`). Returns the task id;
processing happens asynchronously on the WebODM server.

## Usage

``` r
webodm_submit_task(
  base_url,
  token,
  project_id,
  image_paths,
  options = list(),
  name = NULL
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

- image_paths:

  Character vector of local image file paths to upload.

- options:

  A list of WebODM option `name = value` pairs. Use
  [`as_webodm_options()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/as_webodm_options.md)
  to translate from
  [`build_odm_args()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/build_odm_args.md)-style
  arguments.

- name:

  Optional task name (default: `"DroneBioR task <timestamp>"`).

## Value

Character task UUID.

## Examples

``` r
if (FALSE) { # \dontrun{
task_id <- webodm_submit_task(
  base_url, token, project_id,
  image_paths = list.files("/path/to/images", full.names = TRUE),
  options = as_webodm_options(camera_type = "rgb", build_dsm = TRUE)
)
} # }
```
