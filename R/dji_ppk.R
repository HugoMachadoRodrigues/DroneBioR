# DJI Mavic 3M PPK / RTK sidecar handling.
#
# The Mavic 3M ships a `_Timestamp.MRK` file alongside the photos for
# every "mission" the SmartFarm app uploads. Each row of the .MRK
# records the *RTK-quality* lat / lon / ellipsoidal height of one
# trigger event, plus the receiver's fix quality. When the drone had
# an RTK Fixed solution during the flight (fix_quality = 50), the
# positions in the .MRK are already centimetre-accurate — equivalent
# in quality to post-processed PPK output.
#
# The plain DJI JPG / TIF EXIF GPS suffers from a documented altitude
# bug on the Mavic 3M (negative heights, off by hundreds of metres).
# That makes OpenSfM's bundle adjuster diverge on multi-band flights:
# the reconstruction sprawls to many kilometres and `odm_orthophoto`
# OOMs trying to write a kilometre-wide orthomosaic at centimetre
# resolution. Feeding ODM the clean .MRK coordinates via a `geo.txt`
# sidecar fixes the root cause.
#
# The helpers in this file do exactly that:
#   1. detect_djim3m_ppk_files()  - locate .MRK / .bin / .nav siblings
#   2. parse_djim3m_mrk()         - parse one .MRK into a tidy data.frame
#   3. inspect_djim3m_mrk()       - human-readable summary (fix quality
#                                   counts, position span, std dev range)
#   4. write_djim3m_geo_txt()     - resolve photo numbers to actual
#                                   image filenames and write an ODM
#                                   geo.txt
# `run_odm_dji_mavic_3m()` calls them automatically; advanced users
# can also use them directly.

#' Does this folder hold a DJI Mavic 3M image set?
#'
#' Checks whether *any* file in `images_dir` matches the DJI Mavic 3M
#' filename pattern (`DJI_<datetime>_<NNNN>_<D|MS_(G|R|RE|NIR)>.<ext>`).
#' Used by the Drone Biomass Studio app to gate which manifest /
#' processing engine to dispatch — the legacy `list_micasense_images()`
#' path errors out on DJI names, while `run_odm_dji_mavic_3m()`
#' handles them natively.
#'
#' @param images_dir Folder containing raw images.
#' @return `TRUE` when at least one filename matches the DJI Mavic 3M
#'   pattern, `FALSE` otherwise (including when the directory does not
#'   exist or is empty).
#' @examples
#' \dontrun{
#'   has_djim3m_images("/path/to/ifasbahia10")
#' }
#' @export
has_djim3m_images <- function(images_dir) {
  if (!is.character(images_dir) || !length(images_dir) ||
      !nzchar(images_dir) || !dir.exists(images_dir)) {
    return(FALSE)
  }
  files <- list.files(images_dir,
                      pattern = "^DJI_[0-9]+_[0-9]+_(D|MS_(G|R|RE|NIR))\\.[A-Za-z]+$",
                      ignore.case = TRUE)
  length(files) > 0L
}

#' Detect DJI Mavic 3M PPK / RTK sidecar files
#'
#' Scans `images_dir` for the three families of DJI Mavic 3M sidecar
#' files: `*_Timestamp.MRK` (trigger events + RTK positions),
#' `*_PPKRAW.bin` (raw GNSS observables) and `*_PPKNAV.nav` (broadcast
#' ephemerides). One mission produces one file per family; a folder
#' can hold several missions and therefore several files of each.
#'
#' @param images_dir Folder containing the raw DJI Mavic 3M images.
#' @return A named list with character vectors `mrk`, `bin`, `nav`
#'   (each empty when nothing matches). The `has_ppk_inputs` element
#'   is `TRUE` when at least one `.bin` AND one `.nav` are present —
#'   the minimum needed by an external PPK CLI.
#' @examples
#' \dontrun{
#'   files <- detect_djim3m_ppk_files("/path/to/ifasbahia10")
#'   files$mrk
#' }
#' @export
detect_djim3m_ppk_files <- function(images_dir) {
  if (!dir.exists(images_dir)) {
    stop("Image directory not found: ", images_dir, call. = FALSE)
  }
  mrk <- list.files(images_dir, pattern = "_Timestamp\\.MRK$",
                    full.names = TRUE, ignore.case = TRUE)
  bin <- list.files(images_dir, pattern = "_PPKRAW\\.bin$",
                    full.names = TRUE, ignore.case = TRUE)
  nav <- list.files(images_dir, pattern = "_PPKNAV\\.nav$",
                    full.names = TRUE, ignore.case = TRUE)
  list(
    mrk = mrk, bin = bin, nav = nav,
    has_mrk        = length(mrk) > 0L,
    has_ppk_inputs = length(bin) > 0L && length(nav) > 0L
  )
}

#' Parse a DJI Mavic 3M `_Timestamp.MRK` file
#'
#' The .MRK is tab-separated and uses a comma to glue each numeric
#' value to a one-letter label (e.g. `27.39880752,Lat`). Schema:
#'
#' | col | example          | meaning                                  |
#' |-----|------------------|------------------------------------------|
#' | 1   | `1`              | photo number (1-based)                   |
#' | 2   | `494452.918260`  | GPS time of week, seconds                |
#' | 3   | `[2416]`         | GPS week, in brackets                    |
#' | 4   | `-31,N`          | delta-N in millimetres (RTK offset)      |
#' | 5   | `0,E`            | delta-E in millimetres                   |
#' | 6   | `89,V`           | delta-V (vertical) in millimetres        |
#' | 7   | `27.3988,Lat`    | latitude in decimal degrees (WGS84)      |
#' | 8   | `-81.943,Lon`    | longitude in decimal degrees (WGS84)     |
#' | 9   | `34.586,Ellh`    | ellipsoidal height in metres             |
#' | 10  | `0.026, 0.030, 0.083` | std dev (lat, lon, alt) in metres   |
#' | 11  | `50,Q`           | RTK fix quality (50 = Fixed)             |
#'
#' Fix quality codes (DJI / NMEA convention): 0 = no fix, 1 = single,
#' 2 = DGPS, 4 = RTK Float, 5 = RTK Fixed, 50 = DJI's own "Fixed"
#' code. Anything < 50 is degraded; anything >= 50 is centimetre-class.
#'
#' @param mrk_path Path to a `_Timestamp.MRK` file.
#' @return A data.frame with columns `photo_num`, `gps_time_sec`,
#'   `gps_week`, `lat`, `lon`, `alt`, `lat_std`, `lon_std`, `alt_std`,
#'   `fix_quality`, plus a `source` column carrying the .MRK basename
#'   so rows from multiple .MRK files remain distinguishable when
#'   merged.
#' @examples
#' \dontrun{
#'   df <- parse_djim3m_mrk("DJI_..._Timestamp.MRK")
#'   head(df)
#' }
#' @export
parse_djim3m_mrk <- function(mrk_path) {
  if (!file.exists(mrk_path)) {
    stop(".MRK file not found: ", mrk_path, call. = FALSE)
  }
  lines <- readLines(mrk_path, warn = FALSE)
  lines <- lines[nzchar(trimws(lines))]
  if (!length(lines)) {
    return(empty_mrk_df())
  }

  # `before_comma()` returns the numeric chunk in fields like
  # "27.39880752,Lat" — strips the comma and the alphabetic suffix.
  before_comma <- function(s) {
    s <- trimws(s)
    parts <- strsplit(s, ",", fixed = TRUE)[[1L]]
    suppressWarnings(as.numeric(parts[1L]))
  }

  parse_row <- function(line) {
    fields <- strsplit(line, "\t", fixed = TRUE)[[1L]]
    if (length(fields) < 11L) return(NULL)
    stds <- suppressWarnings(as.numeric(trimws(
      strsplit(fields[10L], ",", fixed = TRUE)[[1L]]
    )))
    if (length(stds) < 3L) stds <- c(stds, rep(NA_real_, 3L - length(stds)))
    fix_q <- suppressWarnings(as.integer(
      strsplit(fields[11L], ",", fixed = TRUE)[[1L]][1L]
    ))
    week_str <- gsub("\\[|\\]", "", fields[3L])
    data.frame(
      photo_num    = suppressWarnings(as.integer(fields[1L])),
      gps_time_sec = suppressWarnings(as.numeric(fields[2L])),
      gps_week     = suppressWarnings(as.integer(week_str)),
      lat          = before_comma(fields[7L]),
      lon          = before_comma(fields[8L]),
      alt          = before_comma(fields[9L]),
      lat_std      = stds[1L],
      lon_std      = stds[2L],
      alt_std      = stds[3L],
      fix_quality  = fix_q,
      source       = basename(mrk_path),
      stringsAsFactors = FALSE
    )
  }

  rows <- lapply(lines, parse_row)
  rows <- rows[!vapply(rows, is.null, logical(1L))]
  if (!length(rows)) return(empty_mrk_df())
  do.call(rbind, rows)
}

empty_mrk_df <- function() {
  data.frame(
    photo_num    = integer(),
    gps_time_sec = numeric(),
    gps_week     = integer(),
    lat          = numeric(),
    lon          = numeric(),
    alt          = numeric(),
    lat_std      = numeric(),
    lon_std      = numeric(),
    alt_std      = numeric(),
    fix_quality  = integer(),
    source       = character(),
    stringsAsFactors = FALSE
  )
}

#' Parse and merge every .MRK in a folder
#'
#' Concatenates the rows from each .MRK file, then collapses
#' duplicates on `photo_num` by keeping the row with the **highest
#' fix quality**, breaking ties with the smallest combined horizontal
#' standard deviation. This is the single source of truth the
#' geo.txt writer consumes.
#'
#' @param images_dir Folder holding `.MRK` files.
#' @return A data.frame in the same shape as [parse_djim3m_mrk()],
#'   with one row per unique `photo_num`.
#' @export
parse_djim3m_mrk_folder <- function(images_dir) {
  files <- detect_djim3m_ppk_files(images_dir)$mrk
  if (!length(files)) {
    return(empty_mrk_df())
  }
  all_rows <- do.call(rbind, lapply(files, parse_djim3m_mrk))
  if (!nrow(all_rows)) return(all_rows)

  # Best row per photo_num: max fix_quality, tie-broken by smallest
  # sqrt(lat_std^2 + lon_std^2).
  horiz <- sqrt(all_rows$lat_std^2 + all_rows$lon_std^2)
  ord <- order(all_rows$photo_num,
               -all_rows$fix_quality,
               horiz)
  all_rows <- all_rows[ord, , drop = FALSE]
  all_rows[!duplicated(all_rows$photo_num), , drop = FALSE]
}

#' Inspect a DJI Mavic 3M .MRK folder
#'
#' Prints a human-readable summary of every .MRK rolled up: row
#' count, fix-quality breakdown, lat / lon / altitude spans, and
#' typical standard deviations. Use this before plumbing the file
#' into ODM to confirm the RTK quality is good.
#'
#' @param images_dir Folder holding `.MRK` files.
#' @return Invisibly, the merged data.frame from
#'   [parse_djim3m_mrk_folder()].
#' @examples
#' \dontrun{
#'   inspect_djim3m_mrk("/path/to/ifasbahia10")
#' }
#' @export
inspect_djim3m_mrk <- function(images_dir) {
  files <- detect_djim3m_ppk_files(images_dir)
  if (!files$has_mrk) {
    message("No .MRK files found in ", images_dir)
    return(invisible(empty_mrk_df()))
  }
  df <- parse_djim3m_mrk_folder(images_dir)
  message(sprintf("Found %d .MRK file(s), %d unique photo records:",
                  length(files$mrk), nrow(df)))
  message("  Source files:")
  for (f in files$mrk) message("    - ", basename(f))
  if (!nrow(df)) return(invisible(df))
  fq <- table(df$fix_quality, useNA = "ifany")
  message("  Fix quality breakdown:")
  for (k in names(fq)) {
    label <- switch(as.character(k),
                    "0" = " (no fix)",
                    "1" = " (single GNSS)",
                    "2" = " (DGPS)",
                    "4" = " (RTK Float)",
                    "5" = " (RTK Fixed)",
                    "50" = " (DJI RTK Fixed)",
                    "")
    message(sprintf("    %s%s: %d photos", k, label, fq[[k]]))
  }
  message(sprintf("  Latitude range : %.6f -> %.6f", min(df$lat),  max(df$lat)))
  message(sprintf("  Longitude range: %.6f -> %.6f", min(df$lon),  max(df$lon)))
  message(sprintf("  Altitude range : %.2f m -> %.2f m (ellipsoidal)",
                  min(df$alt), max(df$alt)))
  message(sprintf("  Std dev typical: lat ~%.3f m, lon ~%.3f m, alt ~%.3f m",
                  stats::median(df$lat_std, na.rm = TRUE),
                  stats::median(df$lon_std, na.rm = TRUE),
                  stats::median(df$alt_std, na.rm = TRUE)))
  invisible(df)
}

#' Resolve a .MRK row to an image filename
#'
#' DJI Mavic 3M filenames follow `DJI_<datetime>_<NNNN>_<band>.<ext>`
#' where `<datetime>` is 14 digits (YYYYMMDDHHMMSS), `<NNNN>` is the
#' 4-digit photo number that matches the .MRK row, and `<band>` is
#' `D` for the RGB JPG or `MS_(G|R|RE|NIR)` for the multispectral
#' TIFs. The naive regex `[0-9]{4,}` matches the datetime instead of
#' the photo number, so we anchor explicitly on the band-suffix tail
#' to lock onto the right group. Filenames that do not match the
#' Mavic 3M pattern (mock test inputs, third-party tools' renames)
#' fall through to a permissive `[0-9]+_[^_]+\\.<ext>$` capture that
#' grabs the last underscore-delimited numeric run before the
#' extension, so a barebones `DJI_x_0001_D.JPG` still resolves.
#'
#' @noRd
photo_num_from_filename <- function(filenames) {
  out <- rep(NA_integer_, length(filenames))
  if (!length(filenames)) return(out)

  # Strict Mavic 3M pattern: DJI_<datetime>_<NNNN>_<band>.<ext>
  strict <- regmatches(
    filenames,
    regexec("^DJI_[0-9]+_([0-9]{1,})_(?:D|MS_(?:G|R|RE|NIR))\\.[A-Za-z]+$",
            filenames, ignore.case = TRUE)
  )
  for (i in seq_along(filenames)) {
    if (length(strict[[i]]) >= 2L && nzchar(strict[[i]][2L])) {
      out[i] <- suppressWarnings(as.integer(strict[[i]][2L]))
    }
  }

  # Permissive fallback for non-Mavic-3M-shaped names. Captures the
  # last numeric run before _<something>.<ext>.
  todo <- is.na(out)
  if (any(todo)) {
    fallback <- regmatches(
      filenames[todo],
      regexec("([0-9]+)_[^_]+\\.[A-Za-z]+$", filenames[todo])
    )
    for (k in seq_along(fallback)) {
      if (length(fallback[[k]]) >= 2L && nzchar(fallback[[k]][2L])) {
        out[which(todo)[k]] <- suppressWarnings(as.integer(fallback[[k]][2L]))
      }
    }
  }
  out
}

#' Write an ODM `geo.txt` from a .MRK folder + a list of filenames
#'
#' For each entry in `image_filenames`, resolves the photo number
#' from the filename, looks up the matching row in the merged
#' .MRK data, and emits one geo.txt row:
#'
#'     <filename> <lon> <lat> <alt> <yaw> <pitch> <roll> <horiz_acc> <vert_acc>
#'
#' Yaw/pitch/roll are emitted as `0` because the .MRK does not
#' record them — ODM treats those columns as optional anyway.
#' Accuracy columns come straight from the .MRK std deviations
#' (horiz_acc = sqrt(lat_std^2 + lon_std^2)).
#'
#' @param images_dir Folder holding the .MRK file(s).
#' @param image_filenames Character vector of image *basenames* (not
#'   full paths). Each is looked up by its photo-number suffix.
#' @param geo_txt_path Destination path for the ODM geo.txt.
#' @param min_fix_quality Drop rows whose fix quality is below this
#'   threshold. Default 4 (RTK Float) — anything below would not
#'   constrain the bundle adjustment usefully. Set to 0 to keep
#'   everything.
#' @return Invisibly a list with `written` (the path), `matched`,
#'   `unmatched` (filenames that had no .MRK row), `dropped_quality`
#'   (filenames whose row was below `min_fix_quality`).
#' @examples
#' \dontrun{
#'   files <- list_aerial_images("/path/to/ifasbahia10")
#'   write_djim3m_geo_txt(
#'     images_dir      = "/path/to/ifasbahia10",
#'     image_filenames = files$filename,
#'     geo_txt_path    = "/tmp/geo.txt"
#'   )
#' }
#' @export
write_djim3m_geo_txt <- function(images_dir,
                                 image_filenames,
                                 geo_txt_path,
                                 min_fix_quality = 4L) {
  mrk <- parse_djim3m_mrk_folder(images_dir)
  if (!nrow(mrk)) {
    stop("No .MRK rows parsed from ", images_dir, call. = FALSE)
  }

  photo_nums <- photo_num_from_filename(image_filenames)
  idx <- match(photo_nums, mrk$photo_num)
  matched_mask <- !is.na(idx)
  unmatched <- image_filenames[!matched_mask]

  mrk_for_files <- mrk[idx[matched_mask], , drop = FALSE]
  files_kept    <- image_filenames[matched_mask]

  # Drop rows that did not reach the minimum fix quality. Keeping
  # them would let degraded positions distort the bundle adjustment.
  quality_ok <- !is.na(mrk_for_files$fix_quality) &
                mrk_for_files$fix_quality >= as.integer(min_fix_quality)
  dropped_quality <- files_kept[!quality_ok]
  mrk_for_files <- mrk_for_files[quality_ok, , drop = FALSE]
  files_kept    <- files_kept[quality_ok]

  if (!length(files_kept)) {
    stop("No image filenames resolved to a .MRK row at or above ",
         "fix quality >= ", min_fix_quality, ".", call. = FALSE)
  }

  horiz_acc <- sqrt(mrk_for_files$lat_std^2 + mrk_for_files$lon_std^2)
  vert_acc  <- mrk_for_files$alt_std

  # ODM geo.txt format:
  #   <header line:  EPSG code OR proj4 string>
  #   <filename> <x> <y> <z> [yaw] [pitch] [roll] [horiz_acc] [vert_acc]
  # See https://docs.opendronemap.org/tutorials/#using-geo-txt
  body <- sprintf(
    "%s %.10f %.10f %.4f 0 0 0 %.4f %.4f",
    files_kept,
    mrk_for_files$lon,
    mrk_for_files$lat,
    mrk_for_files$alt,
    horiz_acc,
    vert_acc
  )
  dir.create(dirname(geo_txt_path), recursive = TRUE, showWarnings = FALSE)
  writeLines(c("EPSG:4326", body), geo_txt_path)

  message(sprintf(
    "[%s] Wrote ODM geo.txt with %d positions (%d images unmatched, %d below fix quality %d) -> %s",
    "PPK",
    length(files_kept),
    length(unmatched),
    length(dropped_quality),
    as.integer(min_fix_quality),
    geo_txt_path
  ))
  invisible(list(
    written         = geo_txt_path,
    matched         = files_kept,
    unmatched       = unmatched,
    dropped_quality = dropped_quality
  ))
}

#' PPK CLI hook factory using rtklib + a DJI .bin -> RINEX converter
#'
#' Returns a function suitable for the `ppk_cli` argument of
#' [run_odm_dji_mavic_3m()]. When called, the function:
#'
#'   1. Converts the DJI `_PPKRAW.bin` rover observations into RINEX
#'      using `dji_bin_to_rinex_cmd` (a user-supplied CLI that knows
#'      DJI's proprietary binary; **rtklib's `convbin` does NOT
#'      handle DJI .bin out of the box**, so this must be a tool the
#'      user installed separately — for example
#'      [`klauppk`](https://github.com/heliopas/klauppk) or any DJI
#'      Smart Farm-compatible converter).
#'   2. Runs rtklib's `rnx2rtkp` with the rover RINEX, the
#'      `_PPKNAV.nav` ephemerides and the **user-supplied base
#'      station RINEX** to produce a positioning solution.
#'   3. Rewrites each `_Timestamp.MRK` in place with the corrected
#'      lat / lon / alt per trigger event by matching the .MRK GPS
#'      time-of-week column to the solution's timestamps.
#'
#' If any step fails the hook emits a `warning()` and leaves the
#' .MRK files unchanged; [run_odm_dji_mavic_3m()] then falls back to
#' the .MRK-as-shipped path.
#'
#' Tooling expectations:
#' * `rnx2rtkp` must be on `PATH`. On macOS: `brew install rtklib`.
#' * `dji_bin_to_rinex_cmd` must be on `PATH` or a full path. Pass
#'   the user-installed converter's name explicitly — there is no
#'   universal default.
#' * `base_obs_path` is the base station RINEX observation file
#'   (`.YYo` / `.obs`). The user typically downloads this from a
#'   public CORS network for the flight day + a base receiver
#'   covering the survey area.
#'
#' @param base_obs_path Path to the base station RINEX observation
#'   file. Required.
#' @param dji_bin_to_rinex_cmd Command (name or full path) that
#'   converts a DJI Mavic 3M `_PPKRAW.bin` to RINEX. Must accept
#'   the .bin path as its first positional argument and write the
#'   RINEX `.obs` to its standard output **or** to a file named
#'   `<bin>.obs`.
#' @param rnx2rtkp_cmd Command for rtklib's PPK runner. Default
#'   `"rnx2rtkp"`.
#' @param rnx2rtkp_extra Extra arguments forwarded to `rnx2rtkp`.
#'   For example `c("-p", "0")` to force static positioning, or
#'   `c("-c", "/path/to/config.conf")` for a custom configuration.
#' @return A function with signature
#'   `function(images_dir, bin_paths, nav_paths, mrk_paths)`,
#'   ready to pass as `ppk_cli` to [run_odm_dji_mavic_3m()].
#' @examples
#' \dontrun{
#'   hook <- ppk_cli_rtklib_dji(
#'     base_obs_path        = "~/ppk/base_2026_05_01.26o",
#'     dji_bin_to_rinex_cmd = "klauppk_dji_to_rinex"
#'   )
#'   run_odm_dji_mavic_3m(project, ppk_cli = hook)
#' }
#' @export
ppk_cli_rtklib_dji <- function(base_obs_path,
                               dji_bin_to_rinex_cmd,
                               rnx2rtkp_cmd  = "rnx2rtkp",
                               rnx2rtkp_extra = character()) {
  base_obs_path <- normalizePath(base_obs_path, mustWork = TRUE)
  force(dji_bin_to_rinex_cmd)
  force(rnx2rtkp_cmd)
  force(rnx2rtkp_extra)

  function(images_dir, bin_paths, nav_paths, mrk_paths) {
    if (!nzchar(Sys.which(rnx2rtkp_cmd))) {
      stop(sprintf(
        "rtklib's `%s` was not found on PATH. Install rtklib (e.g. `brew install rtklib`) and retry.",
        rnx2rtkp_cmd
      ), call. = FALSE)
    }
    if (!nzchar(Sys.which(dji_bin_to_rinex_cmd))) {
      stop(sprintf(
        "DJI .bin -> RINEX converter `%s` was not found on PATH. Install a Mavic 3M-compatible converter (e.g. KlauPPK) and pass its command name.",
        dji_bin_to_rinex_cmd
      ), call. = FALSE)
    }

    # We iterate per .bin so that flights with several missions get
    # several rover RINEX files + several `rnx2rtkp` invocations.
    pos_files <- character()
    for (bin in bin_paths) {
      obs_path <- paste0(bin, ".obs")
      # 1) DJI .bin -> RINEX
      conv_status <- suppressWarnings(system2(
        dji_bin_to_rinex_cmd,
        args   = shQuote(bin),
        stdout = obs_path,
        stderr = "",
        timeout = 600
      ))
      if (!identical(as.integer(conv_status), 0L) || !file.exists(obs_path)) {
        stop(sprintf(
          "DJI .bin -> RINEX converter failed on %s (exit %s).",
          bin, conv_status
        ), call. = FALSE)
      }
      # 2) rnx2rtkp (PPK)
      nav_for_bin <- nav_paths[which.min(abs(
        as.numeric(file.info(nav_paths)$mtime) -
          as.numeric(file.info(bin)$mtime)
      ))]
      if (!length(nav_for_bin)) {
        nav_for_bin <- nav_paths[1L]
      }
      pos_path <- paste0(bin, ".pos")
      rtk_status <- suppressWarnings(system2(
        rnx2rtkp_cmd,
        args = c(
          rnx2rtkp_extra,
          "-o", shQuote(pos_path),
          shQuote(obs_path),
          shQuote(base_obs_path),
          shQuote(nav_for_bin)
        ),
        timeout = 1800
      ))
      if (!identical(as.integer(rtk_status), 0L) || !file.exists(pos_path)) {
        stop(sprintf(
          "rnx2rtkp failed on %s (exit %s).",
          bin, rtk_status
        ), call. = FALSE)
      }
      pos_files <- c(pos_files, pos_path)
    }

    # 3) Rewrite each .MRK in place by matching `gps_time_sec` to
    #    the closest epoch in the corresponding .pos file. We keep
    #    this within an internal helper so the rewriting logic stays
    #    encapsulated and is unit-testable on its own.
    update_mrk_from_pos_files(mrk_paths, pos_files)

    invisible(TRUE)
  }
}

#' Replace .MRK positions with the closest match from rtklib .pos files
#'
#' Reads each `_Timestamp.MRK`, locates the closest GPS time-of-week
#' epoch in any of the supplied .pos files, and rewrites the .MRK
#' rows in place with the PPK lat / lon / alt + the .pos std-devs +
#' a fix-quality code mapped from the .pos Q column (Q=1 RTK Fix ->
#' 50, Q=2 Float -> 4, else -> 1).
#'
#' @noRd
update_mrk_from_pos_files <- function(mrk_paths, pos_files) {
  pos_rows <- do.call(rbind, lapply(pos_files, parse_rtklib_pos))
  if (!nrow(pos_rows)) {
    warning("No epochs parsed from .pos files; .MRK left untouched.",
            call. = FALSE)
    return(invisible(FALSE))
  }
  for (mrk in mrk_paths) {
    df <- parse_djim3m_mrk(mrk)
    if (!nrow(df)) next
    lines <- readLines(mrk, warn = FALSE)
    new_lines <- character(length(lines))
    for (i in seq_along(lines)) {
      row <- df[i, , drop = FALSE]
      if (!nrow(row) || !is.finite(row$gps_time_sec)) {
        new_lines[i] <- lines[i]; next
      }
      d <- abs(pos_rows$gps_time_sec - row$gps_time_sec)
      j <- which.min(d)
      if (!length(j) || d[j] > 1.0) {
        # No epoch within 1 s — keep the original row rather than
        # falsifying it.
        new_lines[i] <- lines[i]; next
      }
      fix_q <- switch(as.character(pos_rows$q[j]),
                      "1" = 50L,   # RTK Fix
                      "2" = 4L,    # RTK Float
                      1L)          # Single / DGPS / unknown
      new_lines[i] <- sprintf(
        "%d\t%.6f\t[%d]\t%d,N\t%d,E\t%d,V\t%.8f,Lat\t%.8f,Lon\t%.3f,Ellh\t%.6f, %.6f, %.6f\t%d,Q",
        row$photo_num, row$gps_time_sec, row$gps_week,
        0L, 0L, 0L,
        pos_rows$lat[j], pos_rows$lon[j], pos_rows$alt[j],
        pos_rows$lat_std[j], pos_rows$lon_std[j], pos_rows$alt_std[j],
        fix_q
      )
    }
    writeLines(new_lines, mrk)
  }
  invisible(TRUE)
}

#' Parse an rtklib .pos output file (header + space-delimited rows)
#' @noRd
parse_rtklib_pos <- function(pos_path) {
  if (!file.exists(pos_path)) return(empty_pos_df())
  lines <- readLines(pos_path, warn = FALSE)
  # Strip rtklib header (lines starting with `%`).
  data_lines <- lines[!startsWith(lines, "%") & nzchar(trimws(lines))]
  if (!length(data_lines)) return(empty_pos_df())
  # rtklib default output: GPS_week GPS_tow lat lon height Q ns sdn sde sdu ...
  rows <- lapply(data_lines, function(line) {
    f <- strsplit(trimws(line), "\\s+")[[1L]]
    if (length(f) < 10L) return(NULL)
    data.frame(
      gps_week     = suppressWarnings(as.integer(f[1L])),
      gps_time_sec = suppressWarnings(as.numeric(f[2L])),
      lat          = suppressWarnings(as.numeric(f[3L])),
      lon          = suppressWarnings(as.numeric(f[4L])),
      alt          = suppressWarnings(as.numeric(f[5L])),
      q            = suppressWarnings(as.integer(f[6L])),
      ns           = suppressWarnings(as.integer(f[7L])),
      lat_std      = suppressWarnings(as.numeric(f[8L])),
      lon_std      = suppressWarnings(as.numeric(f[9L])),
      alt_std      = suppressWarnings(as.numeric(f[10L])),
      stringsAsFactors = FALSE
    )
  })
  rows <- rows[!vapply(rows, is.null, logical(1L))]
  if (!length(rows)) return(empty_pos_df())
  do.call(rbind, rows)
}

empty_pos_df <- function() {
  data.frame(
    gps_week = integer(), gps_time_sec = numeric(),
    lat = numeric(), lon = numeric(), alt = numeric(),
    q = integer(), ns = integer(),
    lat_std = numeric(), lon_std = numeric(), alt_std = numeric(),
    stringsAsFactors = FALSE
  )
}

#' Candidate command names for the DJI .bin -> RINEX converter
#'
#' rtklib's mainstream `convbin` does not handle DJI's proprietary
#' Mavic 3M `.bin`; the user must install a Mavic 3M-compatible
#' converter separately. Auto-detection probes these names in order.
#' Override at the call site by passing an explicit `ppk_cli`.
#' @noRd
dji_bin_to_rinex_candidates <- function() {
  c(
    # KlauPPK ships several wrapper names depending on the install.
    "klauppk_dji_to_rinex",
    "klauppk",
    # Community CLI ports — names users in the OpenDroneMap community
    # tend to install.
    "dji_to_rinex",
    "djiparsekit",
    "djirinexconverter",
    # convbin with a DJI-aware fork (some rtklib forks add support).
    "convbin"
  )
}

#' Find the base-station RINEX observation file for an auto PPK run
#'
#' Searches, in order:
#'   1. The `DRONEBIOR_PPK_BASE_OBS` environment variable.
#'   2. The `dronebior.ppk_base_obs` R option.
#'   3. Files matching `*.obs`, `*.YYo`, `*.??o` inside `<images_dir>/base/`.
#'      Convention: the user drops the day's base RINEX next to the
#'      flight images under `base/`.
#'
#' @return Absolute path to a base obs file, or `NA_character_` when
#'   nothing is found.
#' @noRd
resolve_ppk_base_obs <- function(images_dir) {
  env <- Sys.getenv("DRONEBIOR_PPK_BASE_OBS", unset = "")
  if (nzchar(env) && file.exists(env)) {
    return(normalizePath(env, mustWork = FALSE))
  }
  opt <- getOption("dronebior.ppk_base_obs", default = NULL)
  if (is.character(opt) && length(opt) == 1L && file.exists(opt)) {
    return(normalizePath(opt, mustWork = FALSE))
  }
  base_dir <- file.path(images_dir, "base")
  if (dir.exists(base_dir)) {
    # Common RINEX obs extensions: .obs, .25o (year-suffixed), .25O ...
    candidates <- list.files(
      base_dir,
      pattern = "\\.(obs|[0-9]{2}[oO])$",
      full.names = TRUE,
      ignore.case = TRUE
    )
    if (length(candidates) > 0L) {
      return(normalizePath(candidates[1L], mustWork = FALSE))
    }
  }
  NA_character_
}

#' Resolve `ppk_cli = "auto"` into a real hook (or NULL)
#'
#' Looks for everything `ppk_cli_rtklib_dji()` needs (rnx2rtkp, a DJI
#' converter command, a base obs file). If every piece is found,
#' returns a configured `ppk_cli` function. Otherwise emits a single
#' message naming what is missing and returns NULL, in which case
#' the caller proceeds with the .MRK-as-shipped path.
#'
#' @noRd
resolve_ppk_cli_auto <- function(images_dir) {
  missing <- character()

  if (!nzchar(Sys.which("rnx2rtkp"))) {
    missing <- c(missing,
                 "rnx2rtkp (install rtklib: `brew install rtklib`)")
  }

  dji_cmd <- NA_character_
  for (cand in dji_bin_to_rinex_candidates()) {
    if (nzchar(Sys.which(cand))) {
      dji_cmd <- cand
      break
    }
  }
  if (is.na(dji_cmd)) {
    missing <- c(missing, paste0(
      "DJI .bin -> RINEX converter (tried: ",
      paste(dji_bin_to_rinex_candidates(), collapse = ", "),
      "; install KlauPPK or equivalent and put it on PATH)"
    ))
  }

  base_obs <- resolve_ppk_base_obs(images_dir)
  if (is.na(base_obs)) {
    missing <- c(missing, paste0(
      "Base-station RINEX observation file (looked at ",
      "DRONEBIOR_PPK_BASE_OBS env, dronebior.ppk_base_obs option, ",
      "and ", file.path(images_dir, "base"), "/*.obs|*.YYo)"
    ))
  }

  if (length(missing)) {
    message("[PPK] CLI auto-detection skipped because:\n  - ",
            paste(missing, collapse = "\n  - "),
            "\n  Falling back to the .MRK-as-shipped path. Set the missing pieces to enable the full PPK CLI on the next run.")
    return(NULL)
  }

  message(sprintf(
    "[PPK] CLI auto-detection ok:\n  - rnx2rtkp -> %s\n  - DJI converter -> %s\n  - base obs -> %s\n  The .MRK files will be refined via PPK before being read.",
    Sys.which("rnx2rtkp"), Sys.which(dji_cmd), base_obs
  ))
  ppk_cli_rtklib_dji(
    base_obs_path        = base_obs,
    dji_bin_to_rinex_cmd = dji_cmd
  )
}
