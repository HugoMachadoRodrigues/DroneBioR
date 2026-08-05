# Figure and table sources for the manuscript

Everything the paper displays is generated from here, so a figure can be
re-derived rather than only re-used.

| File | Produces |
|---|---|
| `fig2_pointcloud_and_canopy.R` | Figure 2 — dense cloud, despiking detail, canopy height |
| `fig5_verification.R` | Figure 5 — the verification experiment |
| `sensitivity_tables_6_7.R` | the data behind Tables 6 and 7 |
| `render_html_tables.R` | every `table*.html` and `fig1_architecture.html` → PNG |
| `*.html` | the table sources; captions are part of the image |

`reproduce_manuscript.R` in the parent directory regenerates the **numbers**;
these scripts regenerate the **images**. Both need the demonstration project.

Two things that will bite you:

* **The screenshot selector must match the page.** `render_html_tables.R` uses
  `.fig` for the architecture diagram and `table` for everything else. If the
  selector is absent, `chromote::screenshot()` writes no file and reports no
  error, so the old PNG survives and looks like the render "worked".
* **`file://` pages are cached.** The script disables the cache; without that,
  editing an HTML source and re-rendering silently reproduces the old image.
