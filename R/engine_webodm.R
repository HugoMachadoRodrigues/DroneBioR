# WebODM REST API client. WebODM (https://github.com/WebODM/WebODM) is the
# multi-user dashboard built on top of OpenDroneMap. It speaks a documented
# REST API on top of the same opendronemap/odm engine the package's ODM
# Docker driver uses, so the analysis layer below remains the same; only
# the submission mechanism changes from `docker run` to HTTP POST.
#
# Authentication uses JWT: POST username + password to /api/token-auth/
# and reuse the returned token in Authorization headers.

.require_httr <- function() {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop(
      "The 'httr' package is required for the WebODM client. ",
      "Install with install.packages('httr').",
      call. = FALSE
    )
  }
}

.webodm_normalize_url <- function(url) {
  if (!is.character(url) || length(url) != 1 || !nzchar(url)) {
    stop("`base_url` must be a non-empty character string.", call. = FALSE)
  }
  sub("/+$", "", url)
}

#' Authenticate with a WebODM instance
#'
#' Calls `POST /api/token-auth/` on a WebODM server and returns the JWT
#' that subsequent calls must place in `Authorization: JWT <token>`.
#'
#' @param base_url Root URL of the WebODM server, with or without a
#'   trailing slash (e.g. `"http://localhost:8000"` or
#'   `"https://webodm.example.org"`).
#' @param username,password WebODM credentials.
#' @return A character string token. Throws an informative error on
#'   non-200 responses.
#' @examples
#' \dontrun{
#' token <- webodm_authenticate("http://localhost:8000", "admin", "secret")
#' }
#' @export
webodm_authenticate <- function(base_url, username, password) {
  .require_httr()
  base_url <- .webodm_normalize_url(base_url)
  resp <- httr::POST(
    url    = paste0(base_url, "/api/token-auth/"),
    body   = list(username = username, password = password),
    encode = "json"
  )
  if (httr::status_code(resp) != 200L) {
    stop(
      "WebODM authentication failed (HTTP ", httr::status_code(resp),
      "). Check the URL and credentials.",
      call. = FALSE
    )
  }
  body <- httr::content(resp, as = "parsed", type = "application/json")
  if (is.null(body$token)) {
    stop("WebODM authentication response did not include a token.", call. = FALSE)
  }
  body$token
}

#' Create or look up a WebODM project
#'
#' Calls `POST /api/projects/`. WebODM allows multiple projects to share
#' the same name; this helper does not enforce uniqueness, it just creates
#' a new one. To list existing projects use [webodm_list_projects()].
#'
#' @param base_url Root URL of the WebODM server.
#' @param token JWT from [webodm_authenticate()].
#' @param name Project name visible in the WebODM dashboard.
#' @param description Optional project description.
#' @return Integer project ID.
#' @examples
#' \dontrun{
#' id <- webodm_create_project(base_url, token, name = "Drone biomass 2026-05-11")
#' }
#' @export
webodm_create_project <- function(base_url, token, name,
                                  description = "Created by DroneBioR") {
  .require_httr()
  base_url <- .webodm_normalize_url(base_url)
  resp <- httr::POST(
    url    = paste0(base_url, "/api/projects/"),
    httr::add_headers(Authorization = paste("JWT", token)),
    body   = list(name = name, description = description),
    encode = "json"
  )
  if (!(httr::status_code(resp) %in% c(200L, 201L))) {
    stop(
      "WebODM project creation failed (HTTP ", httr::status_code(resp), ").",
      call. = FALSE
    )
  }
  body <- httr::content(resp, as = "parsed", type = "application/json")
  as.integer(body$id)
}

#' List WebODM projects
#'
#' @inheritParams webodm_create_project
#' @return A data frame with `id`, `name`, `description`, `created_at`.
#' @export
webodm_list_projects <- function(base_url, token) {
  .require_httr()
  base_url <- .webodm_normalize_url(base_url)
  resp <- httr::GET(
    url = paste0(base_url, "/api/projects/"),
    httr::add_headers(Authorization = paste("JWT", token))
  )
  if (httr::status_code(resp) != 200L) {
    stop("WebODM projects listing failed (HTTP ", httr::status_code(resp), ").",
         call. = FALSE)
  }
  rows <- httr::content(resp, as = "parsed", type = "application/json")
  if (length(rows) == 0L) {
    return(data.frame(id = integer(), name = character(),
                      description = character(), created_at = character(),
                      stringsAsFactors = FALSE))
  }
  do.call(rbind, lapply(rows, function(p) {
    data.frame(
      id          = as.integer(p$id),
      name        = as.character(p$name %||% ""),
      description = as.character(p$description %||% ""),
      created_at  = as.character(p$created_at %||% ""),
      stringsAsFactors = FALSE
    )
  }))
}

#' Submit a task to a WebODM project
#'
#' Uploads the supplied image files as multipart form data alongside an
#' `options` JSON array (the WebODM format: `[{"name": "dsm",
#' "value": true}, ...]`). Returns the task id; processing happens
#' asynchronously on the WebODM server.
#'
#' @inheritParams webodm_create_project
#' @param project_id Integer project ID from [webodm_create_project()].
#' @param image_paths Character vector of local image file paths to upload.
#' @param options A list of WebODM option `name = value` pairs. Use
#'   [as_webodm_options()] to translate from `build_odm_args()`-style
#'   arguments.
#' @param name Optional task name (default: `"DroneBioR task <timestamp>"`).
#' @return Character task UUID.
#' @examples
#' \dontrun{
#' task_id <- webodm_submit_task(
#'   base_url, token, project_id,
#'   image_paths = list.files("/path/to/images", full.names = TRUE),
#'   options = as_webodm_options(camera_type = "rgb", build_dsm = TRUE)
#' )
#' }
#' @export
webodm_submit_task <- function(base_url, token, project_id,
                               image_paths,
                               options = list(),
                               name = NULL) {
  .require_httr()
  base_url <- .webodm_normalize_url(base_url)
  if (length(image_paths) == 0L) {
    stop("No image files supplied to submit.", call. = FALSE)
  }
  missing <- image_paths[!file.exists(image_paths)]
  if (length(missing) > 0L) {
    stop("Image files not found: ", paste(utils::head(missing, 3), collapse = ", "),
         call. = FALSE)
  }

  options_payload <- lapply(seq_along(options), function(i) {
    list(name = names(options)[[i]], value = options[[i]])
  })
  options_json <- jsonlite::toJSON(options_payload, auto_unbox = TRUE)

  task_name <- name %||% paste0(
    "DroneBioR task ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )

  # Build the multipart body: every image goes in a field literally named
  # "images". R named lists tolerate duplicate names; httr serialises each
  # entry as its own multipart part.
  body <- c(
    setNames(
      lapply(image_paths, httr::upload_file),
      rep("images", length(image_paths))
    ),
    list(name = task_name, options = as.character(options_json))
  )

  resp <- httr::POST(
    url    = paste0(base_url, "/api/projects/", project_id, "/tasks/"),
    httr::add_headers(Authorization = paste("JWT", token)),
    body   = body,
    encode = "multipart"
  )

  if (!(httr::status_code(resp) %in% c(200L, 201L))) {
    stop("WebODM task submission failed (HTTP ", httr::status_code(resp),
         "): ", httr::content(resp, as = "text", encoding = "UTF-8"),
         call. = FALSE)
  }
  parsed <- httr::content(resp, as = "parsed", type = "application/json")
  as.character(parsed$id)
}

#' Poll the status of a WebODM task
#'
#' @inheritParams webodm_submit_task
#' @param task_id Task UUID from [webodm_submit_task()].
#' @return A list with `status` (10 queued / 20 running / 30 failed /
#'   40 completed / 50 canceled), `progress` (0-100), `processing_time`,
#'   `last_error`. The integer status codes are the WebODM convention.
#' @export
webodm_task_status <- function(base_url, token, project_id, task_id) {
  .require_httr()
  base_url <- .webodm_normalize_url(base_url)
  resp <- httr::GET(
    url = paste0(base_url, "/api/projects/", project_id, "/tasks/", task_id, "/"),
    httr::add_headers(Authorization = paste("JWT", token))
  )
  if (httr::status_code(resp) != 200L) {
    stop("WebODM task status lookup failed (HTTP ", httr::status_code(resp), ").",
         call. = FALSE)
  }
  body <- httr::content(resp, as = "parsed", type = "application/json")
  list(
    status          = as.integer(body$status %||% NA_integer_),
    progress        = as.numeric(body$running_progress %||% NA_real_),
    processing_time = as.numeric(body$processing_time %||% NA_real_),
    last_error      = body$last_error %||% NA_character_,
    available_assets = body$available_assets %||% list()
  )
}

#' Download an asset from a WebODM task
#'
#' Common asset names: `orthophoto.tif`, `dsm.tif`, `dtm.tif`,
#' `point_cloud.laz`, `textured_model.zip`, `all.zip` (everything bundled).
#'
#' @inheritParams webodm_task_status
#' @param asset_name Asset filename as listed in
#'   `webodm_task_status()$available_assets`.
#' @param target_path Local destination file.
#' @return Invisibly returns the absolute local path written.
#' @export
webodm_download_asset <- function(base_url, token, project_id, task_id,
                                  asset_name, target_path) {
  .require_httr()
  base_url <- .webodm_normalize_url(base_url)
  url <- paste0(base_url, "/api/projects/", project_id, "/tasks/", task_id,
                "/download/", asset_name)
  dir.create(dirname(target_path), recursive = TRUE, showWarnings = FALSE)
  resp <- httr::GET(
    url = url,
    httr::add_headers(Authorization = paste("JWT", token)),
    httr::write_disk(target_path, overwrite = TRUE)
  )
  if (httr::status_code(resp) != 200L) {
    if (file.exists(target_path)) unlink(target_path)
    stop("WebODM asset download failed (HTTP ", httr::status_code(resp),
         ") for asset '", asset_name, "'.",
         call. = FALSE)
  }
  invisible(normalizePath(target_path, mustWork = TRUE))
}

#' Translate `build_odm_args()` arguments into a WebODM options list
#'
#' WebODM's `/api/projects/{id}/tasks/` endpoint takes options as
#' `[{"name": "dsm", "value": true}, ...]`. This helper maps the same
#' boolean / numeric flags the `build_odm_args()` ODM-CLI driver uses
#' so a Shiny form can drive either engine from one set of inputs.
#'
#' @param camera_type `"multispectral"` or `"rgb"`. Sets
#'   `radiometric-calibration = "camera+sun"` for multispectral; omits
#'   it for RGB.
#' @param orthophoto_resolution_cm Orthophoto resolution (cm).
#' @param fast_orthophoto,build_dsm,build_dtm,pc_las,pc_copc,pc_csv,tiles,three_d_tiles,gltf
#'   Same semantics as `build_odm_args()`.
#' @param extra A named list of additional WebODM options to merge.
#' @return A named list with one entry per WebODM option, ready for
#'   [webodm_submit_task()].
#' @export
as_webodm_options <- function(camera_type             = c("multispectral", "rgb"),
                              orthophoto_resolution_cm = 5,
                              fast_orthophoto         = TRUE,
                              build_dsm               = FALSE,
                              build_dtm               = FALSE,
                              pc_las                  = FALSE,
                              pc_copc                 = FALSE,
                              pc_csv                  = FALSE,
                              tiles                   = FALSE,
                              three_d_tiles           = FALSE,
                              gltf                    = FALSE,
                              extra                   = list()) {
  camera_type <- match.arg(camera_type)
  opts <- list(
    `orthophoto-resolution` = orthophoto_resolution_cm,
    `fast-orthophoto`       = isTRUE(fast_orthophoto),
    dsm                     = isTRUE(build_dsm),
    dtm                     = isTRUE(build_dtm),
    `pc-las`                = isTRUE(pc_las),
    `pc-copc`               = isTRUE(pc_copc),
    `pc-csv`                = isTRUE(pc_csv),
    tiles                   = isTRUE(tiles),
    `3d-tiles`              = isTRUE(three_d_tiles),
    gltf                    = isTRUE(gltf)
  )
  if (identical(camera_type, "multispectral")) {
    opts[["radiometric-calibration"]] <- "camera+sun"
  }
  if (length(extra) > 0L) {
    opts <- utils::modifyList(opts, extra)
  }
  opts
}

#' Run a DroneBioR project through a remote WebODM instance
#'
#' Submits the project's MicaSense or aerial images to a WebODM server,
#' polls the task until completion, then downloads `orthophoto.tif`,
#' `dsm.tif`, `dtm.tif` and `point_cloud.laz` (each only if produced)
#' into the project's expected ODM-shaped folders.
#'
#' This is a blocking call: WebODM tasks can take many hours. Set
#' `poll_seconds` to control how often the function checks status.
#'
#' @param project A `dronebio_project` object.
#' @param base_url,username,password WebODM server URL and credentials.
#' @param project_name WebODM project name (default: from `project`).
#' @param camera_type `"multispectral"` or `"rgb"` (see
#'   [build_odm_args()]).
#' @param poll_seconds Status-poll interval in seconds. Default 60.
#' @param ... Forwarded to [as_webodm_options()].
#' @return Invisibly returns a list with `task_id`, `final_status`, and
#'   a named character vector of downloaded local paths.
#' @examples
#' \dontrun{
#' project <- dronebio_project("/path/to/flight")
#' run_webodm_project(
#'   project,
#'   base_url = "http://localhost:8000",
#'   username = "admin",
#'   password = "secret",
#'   camera_type = "rgb",
#'   build_dsm = TRUE,
#'   build_dtm = TRUE
#' )
#' }
#' @export
run_webodm_project <- function(project,
                               base_url,
                               username,
                               password,
                               project_name = NULL,
                               camera_type  = c("multispectral", "rgb"),
                               poll_seconds = 60,
                               ...) {
  camera_type <- match.arg(camera_type)
  if (!inherits(project, "dronebio_project")) {
    stop("`project` must be a dronebio_project.", call. = FALSE)
  }

  token <- webodm_authenticate(base_url, username, password)

  manifest <- switch(camera_type,
                     multispectral = list_micasense_images(project$images_dir),
                     rgb           = list_aerial_images(project$images_dir))
  image_paths <- manifest$file

  pid <- webodm_create_project(
    base_url, token,
    name = project_name %||%
      paste0("DroneBioR ", basename(project$project_dir), " ", format(Sys.Date()))
  )

  options <- as_webodm_options(camera_type = camera_type, ...)
  task_id <- webodm_submit_task(base_url, token, pid,
                                image_paths = image_paths,
                                options = options)

  message("WebODM task submitted: ", task_id,
          " (project ", pid, "). Polling every ", poll_seconds, "s ...")
  repeat {
    st <- webodm_task_status(base_url, token, pid, task_id)
    if (identical(st$status, 40L)) {
      message("WebODM task completed.")
      break
    }
    if (identical(st$status, 30L)) {
      stop("WebODM task failed: ", st$last_error, call. = FALSE)
    }
    if (identical(st$status, 50L)) {
      stop("WebODM task was canceled.", call. = FALSE)
    }
    message(sprintf("  status=%d, progress=%.0f%%",
                    st$status %||% NA_integer_,
                    st$progress %||% 0))
    Sys.sleep(poll_seconds)
  }

  downloads <- list(
    orthophoto = list(asset = "orthophoto.tif",
                      target = project$odm_orthomosaic),
    dsm        = list(asset = "dsm.tif",
                      target = file.path(project$odm_project_dir, "odm_dem", "dsm.tif")),
    dtm        = list(asset = "dtm.tif",
                      target = file.path(project$odm_project_dir, "odm_dem", "dtm.tif")),
    point_cloud = list(asset = "georeferenced_model.laz",
                       target = file.path(project$odm_project_dir,
                                          "odm_georeferencing",
                                          "odm_georeferenced_model.laz"))
  )
  paths <- vapply(names(downloads), function(label) {
    spec <- downloads[[label]]
    tryCatch(
      webodm_download_asset(base_url, token, pid, task_id,
                            asset_name = spec$asset,
                            target_path = spec$target),
      error = function(e) {
        message("  skipping ", spec$asset, ": ", conditionMessage(e))
        NA_character_
      }
    )
  }, character(1))

  invisible(list(task_id = task_id, project_id = pid,
                 final_status = 40L, downloads = paths))
}
