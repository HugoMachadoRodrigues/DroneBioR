#!/usr/bin/env Rscript
# Render the table HTML sources beside this file into the PNGs the manuscript
# embeds. Tables are images because the journal template defines no table style
# that pandoc's output resolves against, so a native table loses its structure
# in some viewers.
#
#   Rscript render_html_tables.R [name ...]      # default: every *.html here
#
# Two silent failure modes this guards against, both of which produced wrong
# figures in this project before the guards existed:
#
#   * A viewport narrower than the page's own .wrap width crops the right-hand
#     columns, and the render still reports success. The viewport is therefore
#     derived from the HTML rather than passed in.
#   * setDeviceMetricsOverride(deviceScaleFactor = ) does NOT apply to
#     screenshot(selector = ); only the `scale` argument does. Using the former
#     silently yields a 1x image at roughly 100 dpi.
#   * A selector that matches nothing makes screenshot() write no file and
#     report no error, so the previous PNG survives and the render looks like
#     it worked. The selector is checked before the shot is taken.
#
# The width of every render is checked against the expected 3x and reported.

library(chromote)

# By default the sources beside this script are rendered. DRONEBIOR_FIGDIR
# points it at another directory holding the same *.html - the manuscript build
# keeps its own copy, and rendering into only one of the two is how a corrected
# table ends up absent from the document that embeds it.
here <- normalizePath(dirname(sub("^--file=", "",
          grep("^--file=", commandArgs(FALSE), value = TRUE)[1])), mustWork = FALSE)
if (is.na(here) || !nzchar(here)) here <- getwd()
here <- normalizePath(Sys.getenv("DRONEBIOR_FIGDIR", here), mustWork = TRUE)

names_ <- commandArgs(TRUE)
if (!length(names_)) {
  names_ <- sub("\\.html$", "", basename(Sys.glob(file.path(here, "*.html"))))
}

for (nm in names_) {
  page <- paste(readLines(file.path(here, paste0(nm, ".html")), warn = FALSE),
                collapse = "")

  # the page states its own width; the viewport has to clear it
  hit  <- regmatches(page, regexpr("\\.wrap\\{width:[0-9]+px", page))
  wrap <- if (length(hit)) as.integer(gsub("\\D", "", hit)) else 1200L
  viewport <- wrap + 120L

  # the architecture diagram is not a table
  selector <- if (grepl("^fig1", nm)) ".fig" else "table"

  b <- ChromoteSession$new(width = viewport, height = 1600)
  b$Network$setCacheDisabled(cacheDisabled = TRUE)
  b$Page$navigate(paste0("file://", file.path(here, paste0(nm, ".html"))),
                  wait_ = FALSE)
  Sys.sleep(2.5)

  present <- b$Runtime$evaluate(
    sprintf("document.querySelector('%s') !== null", selector))$result$value
  if (!isTRUE(present)) {
    cat("  SKIP", nm, "- selector", selector, "matched nothing\n")
    b$close(); next
  }

  out <- file.path(here, paste0(nm, ".png"))
  b$screenshot(filename = out, selector = selector, scale = 3)
  px <- dim(png::readPNG(out))[2]
  cat(sprintf("  %-26s wrap %4d viewport %4d -> %5d px  %s\n",
              nm, wrap, viewport, px,
              if (px >= 3 * (wrap - 60)) "ok" else "** SHORT: check for cropping **"))
  b$close()
}
