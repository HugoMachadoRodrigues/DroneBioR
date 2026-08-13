# cran-comments

## Submission

DroneBioR 0.6.0. This is a new submission.

## Test environments

* macOS 26.6 (aarch64), R 4.6.1 — `R CMD check --as-cran`, local
* Ubuntu 24.04, R release / R devel / R oldrel-1 — GitHub Actions
* macOS latest, R release — GitHub Actions
* Windows Server 2022, R release — GitHub Actions

## R CMD check results

0 errors | 0 warnings | 2 notes

### Note 1 — new submission and a Suggests package outside the mainstream repositories

```
Maintainer: 'Hugo Machado Rodrigues <rodrigues.machado.hugo@gmail.com>'

New submission

Suggests or Enhances not in mainstream repositories:
  lidR
Availability using Additional_repositories specification:
  lidR   yes   https://r-lidar.r-universe.dev
```

The first line is expected for a first submission.

`lidR` and its dependency `rlas` were archived from CRAN on 9 June 2026 and are
now distributed by their maintainer through r-universe. `lidR` is used only for
point-cloud reading, ground classification and terrain rasterisation, and is
declared in `Suggests` with `Additional_repositories` pointing at the
maintainer's repository, which the check confirms resolves.

Every use of it is conditional, by one of two mechanisms:

* `R/full_pointcloud.R` calls `requireNamespace("lidR", quietly = TRUE)` and
  returns `NULL` when it is absent, so the reader falls back to the package's
  own LAS parser rather than failing.
* `R/classify.R` routes both cloth-simulation-filter entry points through an
  internal guard that stops with a message naming the package and the
  r-universe install line, since there is no fallback for that algorithm.

The package installs, checks and runs its full test suite with `lidR` absent;
the tests that need it use `skip_if_not_installed()`. No vignette chunk
evaluates `lidR` code — the vignettes discuss it in prose only. Nothing in
`Imports` depends on it.

### Note 2 — HTML manual validation skipped

```
checking HTML version of manual ... NOTE
Skipping checking HTML validation: 'tidy' doesn't look like recent enough HTML Tidy.
```

This is the age of HTML Tidy on the machine that ran the check, not a property
of the package. It does not appear on the CI runners.

## Reverse dependencies

None; this is a new submission.

## Notes for the reviewer

* The package drives external photogrammetry engines (OpenDroneMap via Docker,
  and it reads the products of WebODM, Pix4Dmapper and Agisoft Metashape).
  Nothing in the examples or tests requires Docker, a network connection or an
  external engine to be present: the examples that would need one are wrapped in
  `\dontrun{}`, and everything reachable without an engine runs.
* No function writes outside `tempdir()` unless the caller supplies a path.
  Persistent state - a flight registry, two caches and a run record - lives
  under `tools::R_user_dir("DroneBioR", ...)`. Nothing is written to the home
  directory; a regression test asserts it, and the path helpers no longer
  create their directory as a side effect of being called.
* `SystemRequirements` lists GDAL, PROJ and GEOS, which come in through `sf` and
  `terra`, plus the optional external tools, each of which is optional at
  run time and absent from the checks.
