#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# Tables 6 and 7 of the manuscript:
#   Table 6  CSF parameters -> terrain surface, and the CHM derived from it
#   Table 7  CHM upper-tail clipping -> how many cells, and are they crowns?
#
# This script does NOT recompute either analysis. It reads the results
# reproduce_manuscript.R already wrote to sensitivity.rds and writes them into
# the table sources, so that the number printed by the script and the number
# printed in the table cannot disagree.
#
# That is not merely tidier. The two used to be computed independently, and
# they diverged: classify_ground(csf()) does not return an identical
# classification run to run, so an independent re-run moved the last digit of
# every cell and shifted the cloth_resolution range between 1.34 m and 1.35 m.
# A table transcribed from one run and prose quoted from another cannot both
# be right.
#
#   DRONEBIOR_REPRO   the directory reproduce_manuscript.R wrote
#   DRONEBIOR_FIGDIR  where the table sources live (default: alongside this file)
# ---------------------------------------------------------------------------

REPRO  <- Sys.getenv("DRONEBIOR_REPRO", file.path(getwd(), "manuscript_repro"))
HERE   <- normalizePath(dirname(sub("^--file=", "",
            grep("^--file=", commandArgs(FALSE), value = TRUE)[1])), mustWork = FALSE)
HERE   <- if (is.na(HERE) || !nzchar(HERE)) getwd() else HERE
OUTDIR <- Sys.getenv("DRONEBIOR_FIGDIR", HERE)
REPRO  <- normalizePath(REPRO, mustWork = TRUE)

S <- readRDS(file.path(REPRO, "sensitivity.rds"))
A <- S$csf; B <- S$clip; sz <- S$patch_sizes

# --- Table 6 ---------------------------------------------------------------
# The published row order interleaves the default among the cloth_resolution
# rows, so it is spelled out rather than derived from the grid order.
ord <- list(list("cloth_resolution", 0.25, 1, 0.50), list("default", 0.50, 1, 0.50),
            list("cloth_resolution", 1.00, 1, 0.50), list("cloth_resolution", 2.00, 1, 0.50),
            list("rigidness",        0.50, 2, 0.50), list("rigidness",        0.50, 3, 0.50),
            list("class_threshold",  0.50, 1, 0.25), list("class_threshold",  0.50, 1, 1.00))

row6 <- function(o) {
  r <- A[abs(A$cloth_resolution - o[[2]]) < 1e-9 & A$rigidness == o[[3]] &
         abs(A$class_threshold - o[[4]]) < 1e-9, ]
  lab <- if (o[[1]] == "default") "<b>default</b>" else o[[1]]
  tr  <- if (o[[1]] == "default") '<tr style="font-weight:700">' else "<tr>"
  sprintf(paste0('%s<td class="w">%s</td><td>%.2f</td><td>%d</td><td>%.2f</td>',
                 '<td>%.1f</td><td>%.2f</td><td>%.1f</td><td>%.2f</td><td>%.2f</td></tr>'),
          tr, lab, r$cloth_resolution, r$rigidness, r$class_threshold,
          r$pct_ground, r$mean_diff, r$pct_lower, r$chm_mean, r$chm_max)
}

# --- Table 7 ---------------------------------------------------------------
lab7 <- c("P99.0", "P99.5", "P99.9", "none")
row7 <- function(i) {
  r   <- B[i, ]
  tr  <- if (lab7[i] == "P99.5") '<tr style="font-weight:700">' else "<tr>"
  thr <- if (is.na(r$threshold_m)) "&mdash;" else sprintf("%.2f", r$threshold_m)
  sprintf('%s<td class="w">%s</td><td>%s</td><td>%s</td><td>%.3f</td><td>%.2f</td><td>%.2f</td></tr>',
          tr, lab7[i], thr, formatC(r$cells_clipped, format = "d", big.mark = ","),
          r$pct_clipped, r$mean_after, r$max_after)
}

# --- write both bodies into their sources ----------------------------------
patch <- function(file, body, caption_subs = list()) {
  path <- file.path(OUTDIR, file)
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")
  html <- sub("(?s)(<tbody>).*?(</tbody>)", paste0("\\1", body, "\\2"), html, perl = TRUE)
  # Captions quote figures from the same data, so they are rewritten here too.
  # Leaving them to be edited by hand is how Table 6 came to claim a 1.34 m
  # range above a body that showed 1.35 m.
  for (s in caption_subs) {
    stopifnot("caption pattern not found" = grepl(s$find, html, perl = TRUE))
    html <- sub(s$find, s$replace, html, perl = TRUE)
  }
  writeLines(html, path)
  cat("  wrote", file, "\n")
}

d_default <- A$mean_diff[abs(A$cloth_resolution - 0.5) < 1e-9 & A$rigidness == 1L &
                         abs(A$class_threshold - 0.5) < 1e-9]

patch("table6_sensitivity.html",
      paste(vapply(ord, row6, character(1)), collapse = ""),
      list(list(find    = "moves &Delta; across a [0-9.]+ m range",
                replace = sprintf("moves &Delta; across a %.2f m range",
                                  diff(range(A$mean_diff)))),
           list(find    = "The default row is the &minus;[0-9.]+ m and [0-9.]+%",
                replace = sprintf("The default row is the &minus;%.2f m and %.1f%%",
                                  abs(d_default),
                                  A$pct_lower[abs(A$cloth_resolution - 0.5) < 1e-9 &
                                              A$rigidness == 1L &
                                              abs(A$class_threshold - 0.5) < 1e-9]))))

patch("table7_clipping.html",
      paste(vapply(seq_len(nrow(B)), row7, character(1)), collapse = ""),
      list(list(find    = "removes [0-9,]+ cells and lowers the maximum by [0-9.]+ m while moving the mean by [0-9.]+ m",
                replace = sprintf("removes %s cells and lowers the maximum by %.1f m while moving the mean by %.2f m",
                                  formatC(B$cells_clipped[2], format = "d", big.mark = ","),
                                  B$max_after[4] - B$max_after[2],
                                  B$mean_after[4] - B$mean_after[2])),
           list(find    = "they form [0-9,]+ connected patches with a median of [0-9,]+ cells and a maximum of [0-9,]+, and [0-9.]+% of the patches",
                replace = sprintf("they form %s connected patches with a median of %s cells and a maximum of %s, and %.1f%% of the patches",
                                  formatC(length(sz), format = "d", big.mark = ","),
                                  formatC(median(sz), format = "d", big.mark = ","),
                                  formatC(max(sz), format = "d", big.mark = ","),
                                  100 * mean(sz >= 20)))))

# --- the figures Section 3.7 quotes in prose -------------------------------
d <- A$mean_diff[abs(A$cloth_resolution - 0.5) < 1e-9 & A$rigidness == 1L &
                 abs(A$class_threshold - 0.5) < 1e-9]
cat(sprintf("\nprose values for Section 3.7:\n"))
cat(sprintf("  cloth_resolution spans %.2f m against the %.2f m at defaults\n",
            diff(range(A$mean_diff)), abs(d)))
cat(sprintf("  clip P99.5: %s of %s cells, max %.2f -> %.2f m, mean %.2f -> %.2f m\n",
            formatC(B$cells_clipped[2], format = "d", big.mark = ","),
            formatC(round(B$cells_clipped[2] / (B$pct_clipped[2] / 100)), format = "d", big.mark = ","),
            B$max_after[4], B$max_after[2], B$mean_after[4], B$mean_after[2]))
cat(sprintf("  %d patches, median %d cells, max %d; %.1f%% are 1-2 cells, %.1f%% span >= 20\n",
            length(sz), median(sz), max(sz), 100 * mean(sz <= 2), 100 * mean(sz >= 20)))
cat("\nNow re-render with:  Rscript render_html_tables.R table6_sensitivity table7_clipping\n")
