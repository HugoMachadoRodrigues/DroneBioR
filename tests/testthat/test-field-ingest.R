# Tests for field-sample ingest: stage_uploaded_vector -> field_source_columns
# -> read_field_points -> prepare_field_table.
#
# The two behaviours worth pinning are the Shiny upload mangling (parts
# renamed to 0.dbf / 1.prj / 2.shp / 3.shx, which GDAL cannot open) and the
# CRS discipline for tabular input.

write_test_shapefile <- function(dir = tempfile()) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  d <- data.frame(
    sample_id = c("S01", "S02", "S03"),
    biomass_kgha = c(1000, 2000, 3000),
    x = c(392004, 392012, 392006),
    y = c(3033007, 3033012, 3033003),
    stringsAsFactors = FALSE
  )
  pts <- sf::st_as_sf(d, coords = c("x", "y"), crs = 32617)
  shp <- file.path(dir, "plots.shp")
  suppressWarnings(sf::st_write(pts, shp, quiet = TRUE))
  shp
}

# Reproduce what Shiny's FileUploadOperation does to a multi-file upload.
mangle_like_shiny <- function(shp) {
  dir <- dirname(shp)
  parts <- sort(list.files(dir, full.names = TRUE))
  upload <- tempfile("upload_")
  dir.create(upload, recursive = TRUE, showWarnings = FALSE)
  datapath <- file.path(upload, sprintf("%d.%s", seq_along(parts) - 1L,
                                        tolower(sub("^.*\\.", "", parts))))
  file.copy(parts, datapath)
  list(name = basename(parts), datapath = datapath)
}

test_that("sf cannot open a Shiny-mangled shapefile but staging repairs it", {
  shp <- write_test_shapefile()
  upload <- mangle_like_shiny(shp)
  mangled_shp <- upload$datapath[grepl("\\.shp$", upload$datapath)]

  expect_error(suppressWarnings(sf::st_read(mangled_shp, quiet = TRUE)))

  staged <- stage_uploaded_vector(upload$name, upload$datapath)
  expect_true(grepl("\\.shp$", staged))
  expect_true(isTRUE(attr(staged, "crs_known")))

  pts <- read_field_points(staged)
  expect_equal(nrow(pts), 3L)
  expect_equal(sf::st_crs(pts)$epsg, 32617L)
})

test_that("DBF-truncated column names come back verbatim and are guessed", {
  shp <- write_test_shapefile()
  probe <- field_source_columns(shp)
  # GDAL abbreviates DBF field names to 10 characters, so the wizard can
  # never match hard-coded names.
  expect_true("sampl_d" %in% probe$columns)
  expect_true("bmss_kg" %in% probe$columns)
  expect_equal(probe$kind, "vector")
  expect_equal(probe$guess$id, "sampl_d")
  expect_equal(probe$guess$biomass, "bmss_kg")
  expect_equal(probe$epsg, 32617L)
})

test_that("a zipped shapefile is unpacked and located", {
  skip_if(!nzchar(Sys.which(Sys.getenv("R_ZIPCMD", "zip"))), "no zip command")
  shp <- write_test_shapefile()
  zip_path <- tempfile(fileext = ".zip")
  old <- setwd(dirname(shp))
  on.exit(setwd(old), add = TRUE)
  utils::zip(zipfile = zip_path, files = list.files("."),
             flags = c("-r9X", "-q"))
  setwd(old)

  staged <- stage_uploaded_vector("plots.zip", zip_path)
  expect_true(grepl("\\.shp$", staged))
  expect_equal(nrow(read_field_points(staged)), 3L)
})

test_that("a shapefile missing its .shx is rejected by name", {
  shp <- write_test_shapefile()
  upload <- mangle_like_shiny(shp)
  keep <- !grepl("\\.shx$", upload$name)
  expect_error(
    stage_uploaded_vector(upload$name[keep], upload$datapath[keep]),
    "\\.shx"
  )
})

test_that("a CSV without a CRS is refused rather than silently stamped", {
  path <- system.file("extdata", "field_samples.csv", package = "DroneBioR")
  # Regression test for the dead field_crs_epsg input: lon/lat pasted into
  # x/y columns used to be stamped with the raster CRS and extract garbage.
  expect_error(read_field_points(path, crs = NULL), "CRS is required")

  pts <- read_field_points(path, crs = 4326)
  expect_equal(sf::st_crs(pts)$epsg, 4326L)
  expect_true(all(c("sample_id", "biomass_kgha", "x", "y") %in% names(pts)))
})

test_that("prepare_field_table renames, converts units and reprojects", {
  path <- system.file("extdata", "field_samples.csv", package = "DroneBioR")
  pts <- read_field_points(path, crs = 32617)

  tab <- prepare_field_table(pts, "sample_id", "biomass_kgha",
                             units = "kg/ha", target_crs = "EPSG:4326")
  expect_s3_class(tab, "dronebio_field_points")
  expect_equal(sf::st_crs(tab)$epsg, 4326L)
  expect_type(tab$sample_id, "character")
  expect_equal(attr(tab, "dropped_na"), 0L)

  mg <- prepare_field_table(pts, "sample_id", "biomass_kgha",
                            units = "Mg/ha", target_crs = "EPSG:32617")
  expect_equal(mg$biomass_kgha, tab$biomass_kgha * 1000)

  gm2 <- prepare_field_table(pts, "sample_id", "biomass_kgha",
                             units = "g/m^2", target_crs = "EPSG:32617")
  expect_equal(gm2$biomass_kgha, tab$biomass_kgha * 10)
})

test_that("prepare_field_table drops NA biomass and counts it", {
  d <- data.frame(sample_id = c("a", "b", "c"),
                  agb = c(1000, NA, 3000),
                  x = c(392004, 392006, 392008),
                  y = c(3033007, 3033008, 3033009),
                  stringsAsFactors = FALSE)
  pts <- sf::st_as_sf(d, coords = c("x", "y"), crs = 32617, remove = FALSE)
  tab <- prepare_field_table(pts, "sample_id", "agb",
                             target_crs = "EPSG:32617")
  expect_equal(nrow(tab), 2L)
  expect_equal(attr(tab, "dropped_na"), 1L)
  expect_true("biomass_kgha" %in% names(tab))
  expect_false("agb" %in% names(tab))
})

test_that("prepare_field_table names the rows with non-numeric biomass", {
  d <- data.frame(sample_id = c("a", "b"),
                  agb = c("1000", "not a number"),
                  x = c(392004, 392006), y = c(3033007, 3033008),
                  stringsAsFactors = FALSE)
  pts <- sf::st_as_sf(d, coords = c("x", "y"), crs = 32617, remove = FALSE)
  expect_error(prepare_field_table(pts, "sample_id", "agb",
                                   target_crs = "EPSG:32617"),
               "row\\(s\\): 2")
})

test_that("a shapefile without a .prj asks for a CRS", {
  shp <- write_test_shapefile()
  unlink(sub("\\.shp$", ".prj", shp))
  expect_error(read_field_points(shp), "no CRS")
  pts <- read_field_points(shp, crs = 32617)
  expect_equal(sf::st_crs(pts)$epsg, 32617L)
})

test_that("polygons are reduced to centroids with a note", {
  square <- sf::st_polygon(list(cbind(
    c(392000, 392010, 392010, 392000, 392000),
    c(3033000, 3033000, 3033010, 3033010, 3033000)
  )))
  layer <- sf::st_sf(sample_id = "P1", biomass_kgha = 1500,
                     geometry = sf::st_sfc(square, crs = 32617))
  path <- tempfile(fileext = ".gpkg")
  suppressWarnings(sf::st_write(layer, path, quiet = TRUE))

  pts <- read_field_points(path)
  expect_equal(as.character(sf::st_geometry_type(pts)), "POINT")
  expect_match(attr(pts, "centroid_note"), "centroid")
})

test_that("field_source_columns probes a CSV without committing to a mapping", {
  probe <- field_source_columns(
    system.file("extdata", "field_samples.csv", package = "DroneBioR")
  )
  expect_equal(probe$kind, "table")
  expect_false(probe$has_geometry)
  expect_equal(probe$guess$id, "sample_id")
  expect_equal(probe$guess$biomass, "biomass_kgha")
  expect_equal(probe$guess$x, "x")
  expect_equal(probe$guess$y, "y")
})

test_that("prepare_field_table refuses a file that already uses the canonical names", {
  # Renaming into a name the file already uses leaves two columns of that name,
  # and `$biomass_kgha` then returns the untouched original instead of the
  # unit-converted column -- a silently wrong response variable.
  csv <- tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(plot = c("a", "b", "c"), biomass_kgha = c(-99, -98, -97),
               dry_mg_ha = c(1, 2, 3), x = c(1, 2, 3), y = c(1, 2, 3)),
    csv, row.names = FALSE
  )
  pts <- read_field_points(csv, crs = 32636, x_col = "x", y_col = "y")
  expect_error(
    prepare_field_table(pts, "plot", "dry_mg_ha", units = "Mg/ha", target_crs = 32636),
    "already has a column named"
  )
})

test_that("prepare_field_table still converts units when there is no clash", {
  csv <- tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(plot = c("a", "b", "c"), dry_mg_ha = c(1, 2, 3),
               x = c(1, 2, 3), y = c(1, 2, 3)),
    csv, row.names = FALSE
  )
  pts <- read_field_points(csv, crs = 32636, x_col = "x", y_col = "y")
  tab <- prepare_field_table(pts, "plot", "dry_mg_ha", units = "Mg/ha", target_crs = 32636)
  expect_equal(sum(names(tab) == "biomass_kgha"), 1L)
  expect_equal(tab$biomass_kgha, c(1000, 2000, 3000))
})
