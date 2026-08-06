# Poll the status of a WebODM task

Poll the status of a WebODM task

## Usage

``` r
webodm_task_status(base_url, token, project_id, task_id)
```

## Arguments

- base_url:

  Root URL of the WebODM server.

- token:

  JWT from
  [`webodm_authenticate()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/webodm_authenticate.md).

- project_id:

  Integer project ID from
  [`webodm_create_project()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/webodm_create_project.md).

- task_id:

  Task UUID from
  [`webodm_submit_task()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/webodm_submit_task.md).

## Value

A list with `status` (10 queued / 20 running / 30 failed / 40 completed
/ 50 canceled), `progress` (0-100), `processing_time`, `last_error`. The
integer status codes are the WebODM convention.
