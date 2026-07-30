# =============================================================================
# Field-sample ingest for the Field Models tab
#
# Three concerns kept apart so the Shiny layer stays a thin caller:
#   1. stage_uploaded_vector()  - put a browser upload back on disk under its
#      real file names (Shiny renames every part to `0.dbf 1.prj 2.shp ...`).
#   2. field_source_columns()   - probe a file for column names / CRS without
#      committing to a mapping, so the wizard can offer choices.
#   3. read_field_points() + prepare_field_table() - the single ingest entry
#      point and the single renaming / unit-conversion step.
#
# The CRS discipline here is deliberate: a CSV of bare `x` / `y` numbers is
# meaningless without a CRS, and stamping the orthomosaic's CRS on lon/lat
# degrees silently produces garbage extractions. read_field_points() refuses
# to guess.
# =============================================================================

# Column-name heuristics. Deliberately loose: ESRI shapefiles truncate DBF
# field names to 10 characters (`sample_id` -> `sampl_d`), so exact matches
# are useless and the wizard must always let the user override the guess.
.guess_field_columns <- function(cols) {
  pick <- function(pattern) {
    hit <- grep(pattern, cols, ignore.case = TRUE, value = TRUE)
    if (length(hit) == 0) NULL else hit[[1]]
  }
  list(
    id      = pick("sample_?id|^id$|plot_?id|name|sampl"),
    biomass = pick("biomass|yield|kgha|kg_ha|agb|bmss"),
    x       = pick("^x$|^lon|longitude|easting|^x_"),
    y       = pick("^y$|^lat|latitude|northing|^y_")
  )
}

# Supported biomass units and their multiplier onto kg/ha. "unknown" passes
# the numbers through untouched, which is what a user with pre-converted
# data wants.
# TRUE when both coordinate columns fall inside geographic bounds
# (longitude in [-180, 180], latitude in [-90, 90]); FALSE when they look
# projected; NA when the columns are missing or non-numeric. Used to pick a
# sane default CRS for a plain CSV, which carries none.
.coords_look_geographic <- function(tab, x_col, y_col) {
  if (is.null(x_col) || is.null(y_col) ||
      !all(c(x_col, y_col) %in% names(tab))) {
    return(NA)
  }
  x <- suppressWarnings(as.numeric(tab[[x_col]]))
  y <- suppressWarnings(as.numeric(tab[[y_col]]))
  if (!any(is.finite(x)) || !any(is.finite(y))) return(NA)
  max(abs(x), na.rm = TRUE) <= 180 && max(abs(y), na.rm = TRUE) <= 90
}

.biomass_unit_factors <- c(
  "kg/ha"   = 1,
  "Mg/ha"   = 1000,
  "g/m^2"   = 10,
  "unknown" = 1
)

.biomass_to_kgha <- function(x, units = "kg/ha") {
  units <- match.arg(units, names(.biomass_unit_factors))
  as.numeric(x) * .biomass_unit_factors[[units]]
}

.vector_extensions <- c("shp", "gpkg", "geojson", "json", "kml", "gml", "sqlite")
.table_extensions  <- c("csv", "txt", "tsv")

# Lower-case extension without a `tools` dependency.
.file_ext <- function(path) {
  base <- basename(as.character(path))
  ifelse(grepl("\\.[^.]+$", base), tolower(sub("^.*\\.", "", base)), "")
}

#' Stage a Shiny multi-file upload back onto disk
#'
#' Shiny writes uploaded files to its own temporary names (`0.dbf`, `1.prj`,
#' `2.shp`, `3.shx`), and GDAL cannot open a shapefile whose sidecars no
#' longer share the base name - `sf::st_read()` fails with
#' `Unable to open .../2.shx`. This helper copies each upload back under its
#' original `name` so the parts line up again, and unpacks a single `.zip`
#' upload the same way.
#'
#' @param name Character vector of original file names (`input$file$name`).
#' @param datapath Character vector of temporary paths
#'   (`input$file$datapath`), same length as `name`.
#' @param dir Directory to stage into. Created if missing.
#' @return The path of the dataset to open, with attributes `staged_dir`
#'   (the directory holding every part) and `crs_known` (`FALSE` when a
#'   shapefile arrived without a `.prj`).
#' @examples
#' tmp <- tempfile(fileext = ".csv")
#' utils::write.csv(data.frame(sample_id = "S01", biomass_kgha = 1000,
#'                             x = 1, y = 2), tmp, row.names = FALSE)
#' staged <- stage_uploaded_vector("field.csv", tmp)
#' basename(staged)
#' @export
stage_uploaded_vector <- function(name, datapath, dir = tempfile("dronebio_field_")) {
  name <- as.character(name)
  datapath <- as.character(datapath)
  if (length(name) == 0L) {
    stop("No files were supplied.", call. = FALSE)
  }
  if (length(name) != length(datapath)) {
    stop("`name` and `datapath` must have the same length.", call. = FALSE)
  }
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }

  for (i in seq_along(name)) {
    target <- file.path(dir, basename(name[[i]]))
    ok <- file.copy(datapath[[i]], target, overwrite = TRUE)
    if (!ok) {
      stop("Could not stage uploaded file: ", name[[i]], call. = FALSE)
    }
  }

  exts <- .file_ext(name)
  if (length(name) == 1L && identical(exts, "zip")) {
    zip_path <- file.path(dir, basename(name[[1L]]))
    utils::unzip(zip_path, exdir = dir)
    unlink(zip_path)
  }

  .locate_staged_dataset(dir)
}

# Walk a staged directory and decide which file is "the" dataset.
.locate_staged_dataset <- function(dir) {
  files <- list.files(dir, recursive = TRUE, full.names = TRUE)
  files <- files[!dir.exists(files)]
  if (length(files) == 0L) {
    stop("The upload contained no readable files.", call. = FALSE)
  }
  exts <- .file_ext(files)

  shp <- files[exts == "shp"]
  if (length(shp) > 1L) {
    stop("The upload contains ", length(shp), " shapefiles (",
         paste(basename(shp), collapse = ", "),
         "). Upload one shapefile at a time.", call. = FALSE)
  }
  if (length(shp) == 1L) {
    base <- sub("\\.[^.]+$", "", shp)
    sidecars <- c("shx", "dbf")
    present <- tolower(sub("^.*\\.", "", files[startsWith(files, base)]))
    missing <- setdiff(sidecars, present)
    if (length(missing) > 0L) {
      stop("Shapefile is missing required part(s): ",
           paste0(".", missing, collapse = ", "),
           ". Select the .shp, .shx, .dbf (and .prj) together, or upload a .zip.",
           call. = FALSE)
    }
    return(structure(
      shp,
      staged_dir = dir,
      crs_known  = "prj" %in% present
    ))
  }

  other <- files[exts %in% c(setdiff(.vector_extensions, "shp"), .table_extensions)]
  if (length(other) == 0L) {
    stop("No CSV, shapefile or vector dataset was found in the upload.",
         call. = FALSE)
  }
  structure(other[[1L]], staged_dir = dir, crs_known = TRUE)
}

#' Probe a field-sample file for columns, CRS and a column-mapping guess
#'
#' Reads only what is needed to populate the column-mapping wizard. The
#' returned `columns` are the names as they actually come back from the
#' driver - DBF truncates to 10 characters, so downstream code must never
#' match hard-coded names.
#'
#' @param path Path to a CSV or vector dataset.
#' @param layer Optional layer name for multi-layer sources (GeoPackage).
#' @return A list with `kind` (`"table"` or `"vector"`), `columns`,
#'   `classes`, `n_missing`, `n_rows`, `crs`, `epsg`, `has_geometry`,
#'   `geom_type` and `guess` (`id` / `biomass` / `x` / `y`).
#' @examples
#' path <- system.file("extdata", "field_samples.csv", package = "DroneBioR")
#' probe <- field_source_columns(path)
#' probe$kind
#' probe$guess$biomass
#' @export
field_source_columns <- function(path, layer = NULL) {
  if (!file.exists(path)) {
    stop("Field data file not found: ", path, call. = FALSE)
  }
  ext <- .file_ext(path)
  if (ext %in% .table_extensions) {
    tab <- utils::read.csv(path, stringsAsFactors = FALSE)
    guess <- .guess_field_columns(names(tab))
    return(list(
      kind         = "table",
      columns      = names(tab),
      classes      = vapply(tab, function(z) class(z)[[1L]], character(1)),
      n_missing    = vapply(tab, function(z) sum(is.na(z)), integer(1)),
      n_rows       = nrow(tab),
      crs          = NA_character_,
      epsg         = NA_integer_,
      has_geometry = FALSE,
      geom_type    = NA_character_,
      guess        = guess,
      # Do the guessed coordinate columns look like longitude / latitude
      # degrees, or projected metres? A plain CSV carries no CRS, and defaulting
      # projected eastings/northings (~5e5 / ~5e6) to EPSG:4326 places every
      # point off the planet -> silently empty extractions. NA when the guess is
      # missing / non-numeric so the caller can fall back sensibly.
      xy_geographic = .coords_look_geographic(tab, guess$x, guess$y)
    ))
  }

  layer_arg <- if (is.null(layer)) list() else list(layer = layer)
  pts <- do.call(sf::st_read, c(list(dsn = path, quiet = TRUE), layer_arg))
  crs <- sf::st_crs(pts)
  tab <- sf::st_drop_geometry(pts)
  list(
    kind         = "vector",
    columns      = names(tab),
    classes      = vapply(tab, function(z) class(z)[[1L]], character(1)),
    n_missing    = vapply(tab, function(z) sum(is.na(z)), integer(1)),
    n_rows       = nrow(tab),
    crs          = if (is.na(crs)) NA_character_ else crs$wkt,
    epsg         = if (is.na(crs)) NA_integer_ else as.integer(crs$epsg),
    has_geometry = TRUE,
    geom_type    = as.character(unique(sf::st_geometry_type(pts)))[[1L]],
    guess        = .guess_field_columns(names(tab))
  )
}

#' Read field sample points from a CSV or vector file
#'
#' The single ingest entry point for the Field Models tab. Column names are
#' never renamed here - see [prepare_field_table()] for that.
#'
#' A CSV **must** be given a `crs`: bare `x` / `y` numbers carry no spatial
#' meaning, and adopting the orthomosaic's CRS for what are actually
#' longitude / latitude degrees produces silently wrong extractions.
#'
#' @param path Path to a CSV or vector dataset (see [stage_uploaded_vector()]).
#' @param crs CRS of the coordinates: an EPSG code, a WKT / PROJ string, or
#'   `NULL`. Required for CSV input, and used for a vector file whose CRS is
#'   undefined (a shapefile with no `.prj`).
#' @param x_col,y_col Coordinate columns for CSV input. Guessed with
#'   [field_source_columns()] when `NULL`.
#' @param layer Optional layer name for multi-layer sources.
#' @return An `sf` POINT layer. When polygons were reduced to centroids the
#'   result carries a `centroid_note` attribute.
#' @examples
#' path <- system.file("extdata", "field_samples.csv", package = "DroneBioR")
#' pts <- read_field_points(path, crs = 32617)
#' nrow(pts)
#' @export
read_field_points <- function(path, crs = NULL, x_col = NULL, y_col = NULL,
                              layer = NULL) {
  if (!file.exists(path)) {
    stop("Field data file not found: ", path, call. = FALSE)
  }
  ext <- .file_ext(path)

  if (ext %in% .table_extensions) {
    tab <- utils::read.csv(path, stringsAsFactors = FALSE)
    guess <- .guess_field_columns(names(tab))
    if (is.null(x_col)) x_col <- guess$x
    if (is.null(y_col)) y_col <- guess$y
    if (is.null(x_col) || is.null(y_col)) {
      stop("No coordinate columns were found. Pass `x_col` and `y_col` ",
           "(available columns: ", paste(names(tab), collapse = ", "), ").",
           call. = FALSE)
    }
    missing <- setdiff(c(x_col, y_col), names(tab))
    if (length(missing) > 0L) {
      stop("Coordinate column(s) not in the file: ",
           paste(missing, collapse = ", "), call. = FALSE)
    }
    if (is.null(crs)) {
      stop("A CRS is required for tabular field data: '", x_col, "' / '", y_col,
           "' are plain numbers. Pass the EPSG code the coordinates were ",
           "recorded in (for example 4326 for longitude/latitude).",
           call. = FALSE)
    }
    bad <- !is.finite(suppressWarnings(as.numeric(tab[[x_col]]))) |
      !is.finite(suppressWarnings(as.numeric(tab[[y_col]])))
    if (any(bad)) {
      stop("Non-numeric or missing coordinates in row(s): ",
           paste(which(bad), collapse = ", "), call. = FALSE)
    }
    tab[[x_col]] <- as.numeric(tab[[x_col]])
    tab[[y_col]] <- as.numeric(tab[[y_col]])
    pts <- sf::st_as_sf(tab, coords = c(x_col, y_col), crs = crs, remove = FALSE)
    return(pts)
  }

  layer_arg <- if (is.null(layer)) list() else list(layer = layer)
  pts <- do.call(sf::st_read, c(list(dsn = path, quiet = TRUE), layer_arg))

  if (is.na(sf::st_crs(pts))) {
    if (is.null(crs)) {
      stop("The vector file has no CRS (a shapefile without a .prj). ",
           "Pass `crs` with the EPSG code the coordinates were recorded in.",
           call. = FALSE)
    }
    sf::st_crs(pts) <- crs
  }

  # Drop Z/M so downstream st_transform / terra::vect stay in 2D.
  pts <- sf::st_zm(pts, drop = TRUE, what = "ZM")

  gtype <- as.character(sf::st_geometry_type(pts))
  centroid_note <- NULL
  if (any(gtype %in% c("POLYGON", "MULTIPOLYGON"))) {
    n_poly <- sum(gtype %in% c("POLYGON", "MULTIPOLYGON"))
    pts <- suppressWarnings(sf::st_centroid(pts))
    centroid_note <- sprintf(
      "%d polygon feature(s) were reduced to their centroid.", n_poly
    )
  } else if (any(gtype == "MULTIPOINT")) {
    pts <- suppressWarnings(sf::st_cast(pts, "POINT"))
  }

  if (!all(as.character(sf::st_geometry_type(pts)) == "POINT")) {
    pts <- suppressWarnings(sf::st_cast(pts, "POINT"))
  }
  if (!is.null(centroid_note)) attr(pts, "centroid_note") <- centroid_note
  pts
}

#' Rename, convert and reproject field points for modelling
#'
#' Produces the canonical field table every downstream function expects:
#' `sample_id` (character) and `biomass_kgha` (numeric), in the raster CRS.
#' All other attributes are preserved untouched.
#'
#' @param points An `sf` POINT layer from [read_field_points()].
#' @param id_col Column holding the sample identifier.
#' @param biomass_col Column holding the biomass measurement.
#' @param units Units of `biomass_col`: `"kg/ha"`, `"Mg/ha"`, `"g/m^2"` or
#'   `"unknown"` (passed through unchanged).
#' @param target_crs CRS of the raster stack the samples will be extracted
#'   against.
#' @return An `sf` POINT layer of class `dronebio_field_points`, carrying a
#'   `dropped_na` attribute with the number of rows removed for a missing
#'   biomass value.
#' @examples
#' path <- system.file("extdata", "field_samples.csv", package = "DroneBioR")
#' pts <- read_field_points(path, crs = 32617)
#' tab <- prepare_field_table(pts, "sample_id", "biomass_kgha",
#'                            units = "kg/ha", target_crs = "EPSG:32617")
#' names(tab)[1:2]
#' @export
prepare_field_table <- function(points, id_col, biomass_col,
                                units = c("kg/ha", "Mg/ha", "g/m^2", "unknown"),
                                target_crs) {
  if (!inherits(points, "sf")) {
    stop("`points` must be an sf layer from read_field_points().", call. = FALSE)
  }
  units <- match.arg(units)
  missing <- setdiff(c(id_col, biomass_col), names(points))
  if (length(missing) > 0L) {
    stop("Column(s) not found in the field data: ",
         paste(missing, collapse = ", "),
         " (available: ", paste(setdiff(names(points), attr(points, "sf_column")),
                                collapse = ", "), ").",
         call. = FALSE)
  }

  raw_biomass <- points[[biomass_col]]
  numeric_biomass <- suppressWarnings(as.numeric(as.character(raw_biomass)))
  bad <- is.na(numeric_biomass) & !is.na(raw_biomass) & nzchar(trimws(as.character(raw_biomass)))
  if (any(bad)) {
    stop("Non-numeric biomass value(s) in row(s): ",
         paste(utils::head(which(bad), 10L), collapse = ", "),
         if (sum(bad) > 10L) " ..." else "", call. = FALSE)
  }

  points[[id_col]] <- as.character(points[[id_col]])
  points[[biomass_col]] <- .biomass_to_kgha(numeric_biomass, units)

  # A file that already carries a column called `sample_id` or `biomass_kgha`
  # would end up with two of that name, and every later `$biomass_kgha` read
  # returns the FIRST one -- the untouched original, not the unit-converted
  # column -- so the response would be silently wrong. Refuse instead.
  clash <- intersect(
    c("sample_id", "biomass_kgha"),
    setdiff(names(points), c(id_col, biomass_col))
  )
  if (length(clash) > 0L) {
    stop("The field file already has a column named ",
         paste(sprintf("`%s`", clash), collapse = " and "),
         ", which would collide with the canonical name(s) used here. ",
         "Rename it in the source file, or map it as the biomass/id column.",
         call. = FALSE)
  }

  # Rename in place so column order (and every other attribute) survives.
  nms <- names(points)
  nms[nms == id_col] <- "sample_id"
  nms[nms == biomass_col] <- "biomass_kgha"
  names(points) <- nms

  keep <- !is.na(points$biomass_kgha)
  dropped <- sum(!keep)
  points <- points[keep, , drop = FALSE]
  if (nrow(points) == 0L) {
    stop("Every field sample has a missing biomass value.", call. = FALSE)
  }

  points <- sf::st_transform(points, target_crs)
  attr(points, "dropped_na") <- as.integer(dropped)
  class(points) <- unique(c("dronebio_field_points", class(points)))
  points
}
