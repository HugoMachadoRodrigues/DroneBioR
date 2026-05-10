# Contributing to DroneBioR

Thank you for considering a contribution to **DroneBioR**. The package
is research software for the academic and drone-remote-sensing
community, and contributions of any size are welcome — bug reports,
documentation, new indices, new readers for photogrammetry products,
examples, vignettes, tests, code reviews.

## Code of conduct

This project follows the [Contributor Covenant Code of
Conduct](https://hugomachadorodrigues.github.io/DroneBioR/CODE_OF_CONDUCT.md).
By participating you are expected to uphold it.

## Where to start

- **Found a bug?** Open an issue with a minimal reproducible example
  (`reprex::reprex()` is great), the version of DroneBioR
  (`packageVersion("DroneBioR")`), the version of R, and any relevant
  external software (Docker, ODM, GDAL).
- **Have an enhancement?** Open an issue first so we can discuss design.
  Pull requests on issues tagged `help wanted` or `good first issue` are
  especially welcome.
- **Documentation gap?** Even a typo fix is a valid PR. Submit it
  directly.

## Development workflow

1.  Fork the repository and create a feature branch:

    ``` sh
    git checkout -b feat/short-description
    ```

2.  Install development dependencies:

    ``` r

    install.packages(c("devtools", "roxygen2", "testthat", "knitr", "rmarkdown"))
    install.packages(c("terra", "sf"))
    # Optional, for Shiny app and 3D viewer:
    install.packages(c("shiny", "bslib", "leaflet", "htmltools"))
    ```

3.  Load the package and run tests:

    ``` r

    devtools::load_all()
    devtools::test()
    ```

4.  Update documentation if you touched `R/*.R`:

    ``` r

    devtools::document()
    ```

5.  Run the full check before pushing:

    ``` r

    devtools::check()
    ```

6.  Commit with a descriptive message and push your branch.

7.  Open a pull request against `main`, describe the *why*, and link the
    issue when applicable.

## Code style

- Follow the [tidyverse style guide](https://style.tidyverse.org/) where
  reasonable. The package uses base-R idioms in core paths to keep the
  dependency footprint small for academic users — please match the style
  of the file you are editing.
- Two-space indentation, snake_case function names, explicit
  `pkg::fun()` for non-base packages.
- Document every exported function with roxygen2 (`@param`, `@return`,
  `@examples` when meaningful, `@export`).
- Add a `tests/testthat/test-<feature>.R` for every new exported
  function, even a small smoke test. Use `skip_if_not_installed()` for
  tests that require optional dependencies (terra, sf, lidR, jsonlite,
  etc.).

## Scope guidance

DroneBioR delegates Structure-from-Motion / Multi-View Stereo / mesh
generation / texturing to **external photogrammetry engines** (ODM,
WebODM, Pix4Dmapper, Agisoft Metashape). It does not re-implement those
algorithms in R. Pull requests that add new readers for engine outputs,
additional scientific analyses on the R side, vignettes, reproducible
examples and QA tools are highly welcome. Please open an issue first
before proposing to bring SfM/MVS code into the package itself.

## Releasing (maintainers)

- Bump version in `DESCRIPTION` and add a section to `NEWS.md`.
- `devtools::check()` must pass clean.
- Tag the release: `git tag -a v0.x.0 -m "DroneBioR 0.x.0"` and push
  tags.
- Create a GitHub release from the tag with the `NEWS.md` entry as body.

## Citation

If your work uses DroneBioR, please cite it (`citation("DroneBioR")`).
