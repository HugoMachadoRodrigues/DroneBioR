# Create or look up a WebODM project

Calls `POST /api/projects/`. WebODM allows multiple projects to share
the same name; this helper does not enforce uniqueness, it just creates
a new one. To list existing projects use
[`webodm_list_projects()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/webodm_list_projects.md).

## Usage

``` r
webodm_create_project(
  base_url,
  token,
  name,
  description = "Created by DroneBioR"
)
```

## Arguments

- base_url:

  Root URL of the WebODM server.

- token:

  JWT from
  [`webodm_authenticate()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/webodm_authenticate.md).

- name:

  Project name visible in the WebODM dashboard.

- description:

  Optional project description.

## Value

Integer project ID.

## Examples

``` r
if (FALSE) { # \dontrun{
id <- webodm_create_project(base_url, token, name = "Drone biomass 2026-05-11")
} # }
```
