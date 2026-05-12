#' Read GeoScan-style camera position file
#'
#' GeoScan drones ship Sony JPGs without GPS EXIF; instead, an external
#' `Cameras_WGS84.txt` records per-image WGS84 lat/lon/H + IMU + std-dev.
#' This reader parses the tab-separated file into a data frame the ODM
#' geo-txt writer understands.
#'
#' @param path Absolute path to `Cameras_WGS84.txt`.
#' @return A data frame with columns `file`, `lat`, `lon`, `H`, `roll`,
#'   `pitch`, `yaw`, `time`, `std_n`, `std_e`, `std_u`, `std_hz`.
#' @examples
#' \dontrun{
#'   read_geoscan_cameras("Metadata/Cameras_WGS84.txt")
#' }
#' @export
read_geoscan_cameras <- function(path) {
  if (!file.exists(path)) {
    stop("Cameras file not found: ", path, call. = FALSE)
  }
  raw <- readLines(path, warn = FALSE)
  raw <- raw[nzchar(trimws(raw))]
  # Skip header comment line(s); GeoScan files start with `#`.
  data_lines <- raw[!grepl("^#", raw)]
  parts <- strsplit(data_lines, "\\s+")
  # Each row should have at least 12 columns (file lat lon H roll pitch
  # yaw time std_n std_e std_u std_hz). Trim trailing empty fields.
  n_cols <- max(vapply(parts, length, integer(1)))
  pad <- function(p, n) c(p, rep(NA_character_, n - length(p)))
  m <- do.call(rbind, lapply(parts, pad, n = n_cols))
  num <- function(j) suppressWarnings(as.numeric(m[, j]))
  out <- data.frame(
    file   = m[, 1L],
    lat    = num(2L),
    lon    = num(3L),
    H      = num(4L),
    roll   = num(5L),
    pitch  = num(6L),
    yaw    = num(7L),
    time   = m[, 8L],
    std_n  = num(9L),
    std_e  = num(10L),
    std_u  = num(11L),
    std_hz = num(12L),
    stringsAsFactors = FALSE
  )
  out[!is.na(out$lat) & !is.na(out$lon), , drop = FALSE]
}

#' Read GeoScan GNSS-to-camera offset file
#'
#' `GNSS_offset.txt` records the lever-arm offset between the GNSS
#' antenna and the camera optical centre, in metres (X east, Y north,
#' Z up). Pass to [convert_geoscan_to_odm_geo()] to correct camera
#' positions before writing geo.txt.
#'
#' @param path Absolute path to `GNSS_offset.txt`.
#' @return A named numeric vector with `X`, `Y`, `Z` offsets in metres.
#' @examples
#' \dontrun{
#'   read_geoscan_gnss_offset("Metadata/GNSS_offset.txt")
#' }
#' @export
read_geoscan_gnss_offset <- function(path) {
  if (!file.exists(path)) {
    return(c(X = 0, Y = 0, Z = 0))
  }
  raw <- readLines(path, warn = FALSE)
  vals <- c(X = 0, Y = 0, Z = 0)
  for (line in raw) {
    m <- regmatches(line, regexec("^\\s*([XYZ])\\s*=\\s*(-?[0-9.eE+-]+)", line))[[1L]]
    if (length(m) >= 3L) {
      vals[m[2L]] <- as.numeric(m[3L])
    }
  }
  vals
}

#' Convert GeoScan camera metadata to ODM `geo.txt` and write to disk
#'
#' Builds an OpenDroneMap-compatible `geo.txt` from GeoScan's per-image
#' WGS84 records. Output schema (one image per line, space-separated):
#'
#' ```
#' EPSG:4326
#' image_name lon lat H yaw pitch roll horz_acc vert_acc
#' ```
#'
#' GeoScan stores lat/lon (in that order); ODM expects lon/lat. We
#' translate accordingly. The optional GNSS-to-camera offset is applied
#' as a small (sub-metre) correction in metres at the local latitude.
#'
#' @param cameras_path Path to `Cameras_WGS84.txt`.
#' @param geo_txt_path Output path for the ODM `geo.txt`.
#' @param gnss_offset Either a path to `GNSS_offset.txt` or a named numeric
#'   vector `c(X, Y, Z)` in metres. Pass `NULL` to skip the correction.
#' @return Invisibly, the path written.
#' @examples
#' \dontrun{
#'   convert_geoscan_to_odm_geo(
#'     "Metadata/Cameras_WGS84.txt",
#'     "aerial_geoscan/geo.txt",
#'     gnss_offset = "Metadata/GNSS_offset.txt"
#'   )
#' }
#' @export
convert_geoscan_to_odm_geo <- function(cameras_path, geo_txt_path,
                                       gnss_offset = NULL) {
  cams <- read_geoscan_cameras(cameras_path)
  off  <- if (is.character(gnss_offset)) read_geoscan_gnss_offset(gnss_offset)
          else if (is.numeric(gnss_offset)) gnss_offset
          else c(X = 0, Y = 0, Z = 0)

  # Convert ENU metre offsets into deg-lat / deg-lon at the local
  # latitude. WGS84 mean radius ~6371 km; 1 deg lat ~ 111320 m.
  meters_per_deg_lat <- 111320
  meters_per_deg_lon <- 111320 * cos(cams$lat * pi / 180)
  cams$lon <- cams$lon + (off[["X"]] / meters_per_deg_lon)
  cams$lat <- cams$lat + (off[["Y"]] / meters_per_deg_lat)
  cams$H   <- cams$H   + off[["Z"]]

  # Crude accuracy: horizontal hz-std (already meters), vertical = std_u.
  horz_acc <- ifelse(is.finite(cams$std_hz), cams$std_hz, 0.05)
  vert_acc <- ifelse(is.finite(cams$std_u),  cams$std_u,  0.10)

  header <- "EPSG:4326"
  rows <- sprintf("%s %.10f %.10f %.4f %.3f %.3f %.3f %.4f %.4f",
                  cams$file, cams$lon, cams$lat, cams$H,
                  cams$yaw %||% 0, cams$pitch %||% 0, cams$roll %||% 0,
                  horz_acc, vert_acc)
  dir.create(dirname(geo_txt_path), recursive = TRUE, showWarnings = FALSE)
  writeLines(c(header, rows), geo_txt_path)
  invisible(geo_txt_path)
}
