# Figure and table sources for the manuscript

Everything the paper displays is generated from here, so a figure can be
re-derived rather than only re-used.

| File | Produces |
|---|---|
| `fig2_pointcloud_and_canopy.R` | Figure 2 — dense cloud, despiking detail, canopy height |
| `fig4_csf_vs_smrf.R` | Figure 4 — CSF terrain against the engine's SMRF terrain |
| `fig5_verification.R` | Figure 5 — the verification experiment |
| `sensitivity_tables_6_7.R` | the data behind Tables 6 and 7 |
| `render_html_tables.R` | every `table*.html` and `fig1_architecture.html` → PNG |
| `*.html` | the table sources; captions are part of the image |

`reproduce_manuscript.R` in the parent directory regenerates the **numbers**;
these scripts regenerate the **images**.

## Paths

Every script reads its paths from the environment, so none of them assumes a
directory layout:

```sh
DRONEBIOR_PROJECT=/path/to/project \
DRONEBIOR_REPRO=/path/to/manuscript_repro \
DRONEBIOR_FIGDIR=/path/to/figures \
  Rscript fig4_csf_vs_smrf.R
```

| Variable | Meaning | Default |
|---|---|---|
| `DRONEBIOR_PROJECT` | the project holding the ODM products | `~/DroneBioR-projects/micasense_demo` |
| `DRONEBIOR_REPRO` | the directory `reproduce_manuscript.R` wrote | `./manuscript_repro` |
| `DRONEBIOR_FIGDIR` | where images are written | `./figures` |

**Run `reproduce_manuscript.R` first.** Figures 2, 4 and 5 read the covariate
rasters and `verification.rds` out of `DRONEBIOR_REPRO` rather than recomputing
them. `sensitivity_tables_6_7.R` recomputes nothing at all: it reads
`sensitivity.rds` and renders Tables 6 and 7 from it, so it needs
`DRONEBIOR_REPRO` and `DRONEBIOR_FIGDIR` but not `DRONEBIOR_PROJECT`.

Point-cloud paths are resolved through `odm_product_paths()`, which composes
both the `.las` and `.laz` names whether or not the file exists. The scripts
take whichever is actually on disk, because the distributed products carry the
compressed one.

## Two things that will bite you

* **The screenshot selector must match the page.** `render_html_tables.R` uses
  `.fig` for the architecture diagram and `table` for everything else. If the
  selector is absent, `chromote::screenshot()` writes no file and reports no
  error, so the old PNG survives and looks like the render "worked".
* **`file://` pages are cached.** The script disables the cache; without that,
  editing an HTML source and re-rendering silently reproduces the old image.
