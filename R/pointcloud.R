find_header_end <- function(raw_file) {
  marker <- charToRaw("end_header\n")
  n <- length(marker)
  last_start <- length(raw_file) - n + 1L
  for (i in seq_len(last_start)) {
    if (identical(raw_file[i:(i + n - 1L)], marker)) {
      return(i + n - 1L)
    }
  }
  stop("Could not find PLY header end.", call. = FALSE)
}
# PLY scalar types and their byte widths, including the int8/uint16 aliases
# some writers emit. `signed` drives readBin(); `what` picks its storage mode.
.ply_types <- list(
  char   = list(size = 1L, what = "integer", signed = TRUE),
  int8   = list(size = 1L, what = "integer", signed = TRUE),
  uchar  = list(size = 1L, what = "integer", signed = FALSE),
  uint8  = list(size = 1L, what = "integer", signed = FALSE),
  short  = list(size = 2L, what = "integer", signed = TRUE),
  int16  = list(size = 2L, what = "integer", signed = TRUE),
  ushort = list(size = 2L, what = "integer", signed = FALSE),
  uint16 = list(size = 2L, what = "integer", signed = FALSE),
  int    = list(size = 4L, what = "integer", signed = TRUE),
  int32  = list(size = 4L, what = "integer", signed = TRUE),
  uint   = list(size = 4L, what = "integer", signed = FALSE),
  uint32 = list(size = 4L, what = "integer", signed = FALSE),
  float  = list(size = 4L, what = "numeric", signed = TRUE),
  float32 = list(size = 4L, what = "numeric", signed = TRUE),
  double = list(size = 8L, what = "numeric", signed = TRUE),
  float64 = list(size = 8L, what = "numeric", signed = TRUE)
)

#' Parse the header of a binary little-endian PLY file
#'
#' Returns the vertex layout so readers and writers agree on the record
#' stride. Assuming a fixed stride is how a reader silently produces garbage:
#' ODM's `odm_filterpoints/point_cloud.ply` carries x/y/z + nx/ny/nz +
#' red/blue/green/views, 28 bytes per vertex, while its
#' `odm_georeferencing` cloud omits the normals and uses 16.
#'
#' @param path Path to a `.ply` file.
#' @return A list with `header_end` (bytes), `header_text`, `n_vertices`,
#'   `props` (a data frame of `name`, `type`, `size`, `offset`) and `stride`.
#' @examples
#' \dontrun{
#' h <- parse_ply_header("odm_filterpoints/point_cloud.ply")
#' h$stride
#' h$props$name
#' }
#' @export
parse_ply_header <- function(path) {
  if (!file.exists(path)) stop("PLY file not found: ", path, call. = FALSE)
  # The header is ASCII and short; read a bounded prefix rather than the file.
  n_probe <- min(as.numeric(file.info(path)$size), 65536)
  probe <- readBin(path, what = "raw", n = n_probe)
  header_end <- find_header_end(probe)
  if (is.na(header_end)) {
    stop("Could not find 'end_header' in the first 64 kB of ", path,
         call. = FALSE)
  }
  header <- rawToChar(probe[seq_len(header_end)])
  if (!grepl("format binary_little_endian", header, fixed = TRUE)) {
    stop("Only binary_little_endian PLY files are supported.", call. = FALSE)
  }
  lines <- trimws(strsplit(header, "\n", fixed = TRUE)[[1]])

  # Properties belong to the element that precedes them; only the vertex
  # element is decoded here.
  el <- grep("^element ", lines)
  if (!length(el)) stop("PLY header declares no elements.", call. = FALSE)
  v_at <- grep("^element vertex ", lines)
  if (!length(v_at)) stop("PLY header has no vertex element.", call. = FALSE)
  v_at <- v_at[[1L]]
  n_vertices <- as.integer(sub("^element vertex ", "", lines[[v_at]]))
  if (!is.finite(n_vertices) || n_vertices < 0) {
    stop("Could not read the PLY vertex count.", call. = FALSE)
  }
  nxt <- el[el > v_at]
  stop_at <- if (length(nxt)) nxt[[1L]] else length(lines) + 1L
  prop_lines <- grep("^property ", lines[(v_at + 1L):(stop_at - 1L)], value = TRUE)
  if (any(grepl("^property list", prop_lines))) {
    stop("List properties on the vertex element are not supported.", call. = FALSE)
  }
  parts <- strsplit(prop_lines, "[[:space:]]+")
  types <- vapply(parts, `[`, character(1), 2L)
  names_ <- vapply(parts, `[`, character(1), 3L)
  unknown <- setdiff(types, names(.ply_types))
  if (length(unknown)) {
    stop("Unsupported PLY property type(s): ", paste(unknown, collapse = ", "),
         call. = FALSE)
  }
  sizes <- vapply(types, function(t) .ply_types[[t]]$size, integer(1))
  offsets <- c(0L, cumsum(sizes)[-length(sizes)])
  list(
    header_end  = header_end,
    header_text = header,
    n_vertices  = n_vertices,
    props = data.frame(name = names_, type = types, size = as.integer(sizes),
                       offset = as.integer(offsets), stringsAsFactors = FALSE),
    stride = as.integer(sum(sizes))
  )
}


#' Read a binary little-endian PLY point cloud sample
#'
#' This reader supports the ODM/FPCFilter PLY layout used in the current project:
#' float x/y/z followed by uchar red/blue/green/views. It is intentionally small
#' and dependency-free for app previews; full LAZ analytics should use PDAL/lidR.
#'
#' @param path Path to a PLY file.
#' @param max_points Maximum number of points to return.
#' @param seed Random seed used when sampling.
#' @return A data frame with x, y, z, RGB values and display color.
#' @examples
#' \dontrun{
#' pc <- read_ply_point_cloud("preview.ply", max_points = 50000)
#' }
#' @export
read_ply_point_cloud <- function(path, max_points = 50000, seed = 42) {
  h <- parse_ply_header(path)
  n_vertices <- h$n_vertices
  stride     <- h$stride
  if (n_vertices == 0L) {
    return(data.frame(point_id = integer(), x = numeric(), y = numeric(),
                      z = numeric(), red = integer(), green = integer(),
                      blue = integer(), views = integer(), color = character(),
                      stringsAsFactors = FALSE))
  }

  if (n_vertices > max_points) {
    set.seed(seed)
    point_index <- sort(sample.int(n_vertices, max_points))
  } else {
    point_index <- seq_len(n_vertices)
  }
  n_sel <- length(point_index)

  raw_file <- readBin(path, what = "raw", n = file.info(path)$size)
  body_at  <- h$header_end
  if (n_sel == n_vertices) {
    recs <- raw_file[(body_at + 1L):(body_at + stride * n_vertices)]
  } else {
    # One strided gather instead of a per-point connection: build the byte
    # positions of every selected record and slice once.
    starts <- body_at + (point_index - 1) * stride
    idx <- rep(starts, each = stride) + rep(seq_len(stride), times = n_sel)
    recs <- raw_file[idx]
  }
  rm(raw_file)

  # Decode each property from its own offset, and match colours BY NAME:
  # ODM's filtered cloud declares red, blue, green in that order, so reading
  # the three uchars positionally silently swaps blue and green.
  take <- function(nm) {
    row <- h$props[h$props$name == nm, , drop = FALSE]
    if (!nrow(row)) return(NULL)
    sz <- row$size[[1L]]; ty <- .ply_types[[row$type[[1L]]]]
    pos <- rep((seq_len(n_sel) - 1L) * stride + row$offset[[1L]], each = sz) +
           rep(seq_len(sz), times = n_sel)
    readBin(recs[pos], what = ty$what, n = n_sel, size = sz,
            signed = ty$signed, endian = "little")
  }
  xyz <- lapply(c("x", "y", "z"), take)
  if (any(vapply(xyz, is.null, logical(1)))) {
    stop("The PLY vertex element is missing x/y/z.", call. = FALSE)
  }
  grab <- function(nm, default) {
    v <- take(nm)
    if (is.null(v)) rep(default, n_sel) else as.integer(v)
  }
  red   <- grab("red",   0L)
  green <- grab("green", 0L)
  blue  <- grab("blue",  0L)
  views <- grab("views", 0L)

  data.frame(
    point_id = point_index,
    x = xyz[[1L]], y = xyz[[2L]], z = xyz[[3L]],
    red = red, green = green, blue = blue, views = views,
    color = grDevices::rgb(red, green, blue, maxColorValue = 255),
    stringsAsFactors = FALSE
  )
}

#' Derive approximate tree candidates from a point cloud
#'
#' This is a preview-grade canopy object detector. It bins high points into a
#' regular grid and estimates height, crown diameter and crown volume. Scientific
#' tree metrics should later use a CHM from DSM-DTM and validated segmentation.
#'
#' @param points Data frame from `read_ply_point_cloud()`.
#' @param grid_size Grid size in source coordinate units.
#' @param min_height Minimum height above local ground proxy.
#' @param min_points Minimum number of points per candidate.
#' @param max_trees Maximum number of candidates to return.
#' @return A data frame of approximate tree objects.
#' @examples
#' set.seed(1)
#' n <- 300
#' pts <- data.frame(
#'   x = c(rnorm(n/3, 5, 0.5), rnorm(n/3, 15, 0.5), rnorm(n/3, 25, 0.5)),
#'   y = c(rnorm(n/3, 5, 0.5), rnorm(n/3, 5, 0.5), rnorm(n/3, 15, 0.5)),
#'   z = c(rnorm(n/3, 55, 0.2), rnorm(n/3, 57, 0.2), rnorm(n/3, 54, 0.2))
#' )
#' derive_tree_candidates(pts)
#' @export
derive_tree_candidates <- function(points,
                                   grid_size = 4,
                                   min_height = 1.5,
                                   min_points = 5,
                                   max_trees = 80) {
  if (nrow(points) == 0) {
    return(data.frame())
  }

  ground <- stats::quantile(points$z, probs = 0.05, na.rm = TRUE, names = FALSE)
  points$height <- pmax(points$z - ground, 0)
  high <- points[points$height >= min_height, , drop = FALSE]
  if (nrow(high) == 0) {
    return(data.frame())
  }

  high$gx <- floor((high$x - min(high$x, na.rm = TRUE)) / grid_size)
  high$gy <- floor((high$y - min(high$y, na.rm = TRUE)) / grid_size)
  high$cell <- paste(high$gx, high$gy, sep = "_")

  cells <- split(high, high$cell)
  cells <- cells[vapply(cells, nrow, integer(1)) >= min_points]
  if (length(cells) == 0) {
    return(data.frame())
  }

  metrics <- lapply(cells, function(d) {
    height <- max(d$height, na.rm = TRUE)
    crown_diameter <- max(
      sqrt(diff(range(d$x, na.rm = TRUE))^2 + diff(range(d$y, na.rm = TRUE))^2),
      grid_size * 0.6,
      na.rm = TRUE
    )
    crown_volume <- pi * (crown_diameter / 2)^2 * height * 0.5
    data.frame(
      x = mean(d$x, na.rm = TRUE),
      y = mean(d$y, na.rm = TRUE),
      z = max(d$z, na.rm = TRUE),
      height_m = height,
      crown_diameter_m = crown_diameter,
      crown_volume_m3 = crown_volume,
      point_count = nrow(d)
    )
  })

  out <- do.call(rbind, metrics)
  out <- out[order(out$height_m, decreasing = TRUE), , drop = FALSE]
  out <- utils::head(out, max_trees)
  out$tree_id <- seq_len(nrow(out))
  out[, c("tree_id", "x", "y", "z", "height_m", "crown_diameter_m", "crown_volume_m3", "point_count")]
}

#' Write a filtered copy of a binary PLY, preserving its exact vertex layout
#'
#' Copies `path` to `out_path` keeping only the vertices in `keep`, byte for
#' byte. Every property the source declares survives, including the normals
#' (`nx`, `ny`, `nz`) that ODM's screened-Poisson meshing needs -- dropping
#' them would leave a file ODM still reads but meshes badly.
#'
#' Vertices are streamed in chunks, so a 900 MB cloud does not have to fit in
#' memory, and the output is assembled in a temporary file and moved into
#' place only once it is complete. A run interrupted halfway therefore leaves
#' the original intact rather than a truncated cloud.
#'
#' @param path Source `.ply`.
#' @param out_path Destination. Writing back over `path` is allowed and is the
#'   normal case when handing an edited cloud back to ODM; the temporary file
#'   makes that safe.
#' @param keep Either a logical vector of length `n_vertices`, or the integer
#'   indices of the vertices to keep (1-based, as `point_id` reports them).
#' @param backup When `TRUE` (the default) and `out_path` is the same file as
#'   `path`, the original is copied to `<path>.orig` first, unless that copy
#'   already exists -- so the very first edit is always recoverable and later
#'   edits never overwrite that safety net.
#' @param chunk_size Vertices per streamed chunk.
#' @return Invisibly, the number of vertices written.
#' @examples
#' \dontrun{
#' pc <- read_ply_point_cloud("odm_filterpoints/point_cloud.ply", max_points = 5e4)
#' drop <- pc$point_id[pc$z > 400]          # obvious blunders
#' n <- write_ply_subset("odm_filterpoints/point_cloud.ply",
#'                       "odm_filterpoints/point_cloud.ply",
#'                       keep = setdiff(seq_len(2371187), drop))
#' }
#' @export
write_ply_subset <- function(path, out_path, keep, backup = TRUE,
                             chunk_size = 500000L) {
  h <- parse_ply_header(path)
  n <- h$n_vertices
  if (is.logical(keep)) {
    if (length(keep) != n) {
      stop("`keep` is a logical vector of length ", length(keep),
           " but the cloud has ", n, " vertices.", call. = FALSE)
    }
    keep_idx <- which(keep)
  } else {
    keep_idx <- sort(unique(as.integer(keep)))
    bad <- keep_idx[keep_idx < 1L | keep_idx > n]
    if (length(bad)) {
      stop("`keep` has ", length(bad), " index/indices outside 1..", n,
           " (first: ", bad[[1L]], "). Indices must be `point_id` values ",
           "from the same cloud.", call. = FALSE)
    }
  }
  n_keep <- length(keep_idx)
  if (n_keep == 0L) {
    stop("Refusing to write an empty point cloud: `keep` selected no vertices.",
         call. = FALSE)
  }

  same_file <- normalizePath(path, mustWork = FALSE) ==
               normalizePath(out_path, mustWork = FALSE)
  if (isTRUE(backup) && same_file) {
    orig <- paste0(path, ".orig")
    if (!file.exists(orig)) file.copy(path, orig, overwrite = FALSE)
  }

  # The header is ASCII; only the vertex count changes.
  new_header <- sub(paste0("element vertex ", n),
                    paste0("element vertex ", n_keep),
                    h$header_text, fixed = TRUE)
  if (identical(new_header, h$header_text) && n_keep != n) {
    stop("Could not rewrite the vertex count in the PLY header.", call. = FALSE)
  }

  tmp <- tempfile(tmpdir = dirname(normalizePath(out_path, mustWork = FALSE)),
                  fileext = ".ply.part")
  on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)

  con_out <- file(tmp, open = "wb")
  con_in  <- file(path, open = "rb")
  # try(): after the explicit close() below these handles are invalid, and
  # isOpen() on an invalid connection errors rather than returning FALSE.
  on.exit({
    try(close(con_out), silent = TRUE)
    try(close(con_in), silent = TRUE)
  }, add = TRUE)

  writeBin(charToRaw(new_header), con_out)
  seek(con_in, where = h$header_end, origin = "start")

  stride <- h$stride
  written <- 0L
  start <- 1L
  while (start <= n) {
    stop_at <- min(start + chunk_size - 1L, n)
    block <- readBin(con_in, what = "raw", n = stride * (stop_at - start + 1L))
    sel <- keep_idx[keep_idx >= start & keep_idx <= stop_at]
    if (length(sel)) {
      local_start <- (sel - start) * stride
      idx <- rep(local_start, each = stride) + rep(seq_len(stride), times = length(sel))
      writeBin(block[idx], con_out)
      written <- written + length(sel)
    }
    start <- stop_at + 1L
  }
  close(con_out); close(con_in)

  if (written != n_keep) {
    stop("Wrote ", written, " vertices but selected ", n_keep,
         "; refusing to install a corrupt cloud.", call. = FALSE)
  }
  if (!file.rename(tmp, out_path)) {
    ok <- file.copy(tmp, out_path, overwrite = TRUE)
    if (!ok) stop("Could not move the filtered cloud into ", out_path, call. = FALSE)
    unlink(tmp)
  }
  invisible(written)
}
