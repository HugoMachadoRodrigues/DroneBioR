#!/usr/bin/env Rscript
# Render the table HTML sources beside this file into the PNGs the manuscript
# embeds. Tables are images because the journal template defines no table style
# that pandoc's output resolves against, so a native table loses its structure
# in some viewers.
#
#   Rscript render_html_tables.R [viewport_px] [name ...]
#
# viewport_px must be wide enough for the table's own .wrap width, or the
# right-hand columns are silently cropped. scale = 3 gives ~300 dpi in print.
library(chromote)
args <- commandArgs(TRUE)
width <- if (length(args)) as.integer(args[[1]]) else 1300L
here  <- normalizePath(dirname(sub("^--file=", "",
           grep("^--file=", commandArgs(FALSE), value = TRUE)[1])), mustWork = FALSE)
if (is.na(here) || !nzchar(here)) here <- getwd()
names_ <- if (length(args) > 1) args[-1] else
  sub("\\.html$", "", basename(Sys.glob(file.path(here, "*.html"))))
for (nm in names_) {
  b <- ChromoteSession$new(width = width, height = 1400)
  b$Network$setCacheDisabled(cacheDisabled = TRUE)   # file:// pages are cached
  b$Page$navigate(paste0("file://", file.path(here, paste0(nm, ".html"))), wait_ = FALSE)
  Sys.sleep(2.5)
  # the selector must exist in the page or screenshot() writes nothing and
  # still returns quietly - .fig for the architecture diagram, table elsewhere
  sel <- if (grepl("^fig1", nm)) ".fig" else "table"
  b$screenshot(filename = file.path(here, paste0(nm, ".png")), selector = sel, scale = 3)
  cat("rendered", nm, "\n"); b$close()
}
