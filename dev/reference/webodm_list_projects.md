# List WebODM projects

List WebODM projects

## Usage

``` r
webodm_list_projects(base_url, token)
```

## Arguments

- base_url:

  Root URL of the WebODM server.

- token:

  JWT from
  [`webodm_authenticate()`](https://hugomachadorodrigues.github.io/DroneBioR/dev/reference/webodm_authenticate.md).

## Value

A data frame with `id`, `name`, `description`, `created_at`.
