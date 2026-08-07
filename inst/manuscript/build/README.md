# Manuscript build machinery

The manuscript is Markdown, its tables are the HTML files in `../figures/`, and
pandoc joins them. Three pieces do that:

| file | what it does |
|---|---|
| `table_from_html.lua` | pandoc filter: replaces each `![](figures/tableN.png)` placeholder with the table parsed from `figures/tableN.html`, so a table enters Word as a table rather than as a picture of one. Also restores the bold that lives in the HTML's stylesheet, which pandoc's HTML reader drops. |
| `table_geometry.py` | decides each table's column widths, and its type size when the columns cannot otherwise hold their longest word. Character widths were fitted by least squares to bold text spans measured off a rendered PDF of this template. |
| `docx_postprocess.py` | puts the result on the MDPI template's own styles, and defines the monospace character style the template lacks. |

The HTML is the single source of truth for every table: nothing is transcribed
into the Markdown, so the document cannot drift from it. `build.sh` refuses to
run if the copy under `Manuscripts/DroneBioR_paper/figures/` and the copy here
have diverged.

The PNG renderings in `figures/` are no longer embedded in the manuscript. They
remain useful as a standalone preview of a table and are what
`render_html_tables.R` produces.
