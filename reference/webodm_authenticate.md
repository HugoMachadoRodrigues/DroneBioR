# Authenticate with a WebODM instance

Calls `POST /api/token-auth/` on a WebODM server and returns the JWT
that subsequent calls must place in `Authorization: JWT <token>`.

## Usage

``` r
webodm_authenticate(base_url, username, password)
```

## Arguments

- base_url:

  Root URL of the WebODM server, with or without a trailing slash (e.g.
  `"http://localhost:8000"` or `"https://webodm.example.org"`).

- username, password:

  WebODM credentials.

## Value

A character string token. Throws an informative error on non-200
responses.

## Examples

``` r
if (FALSE) { # \dontrun{
token <- webodm_authenticate("http://localhost:8000", "admin", "secret")
} # }
```
