# Running a reconstruction when the application is hosted for several users.
#
# Locally, the three reconstruction entry points shell out to Docker. That is
# correct on a laptop, where the person driving the application already owns the
# machine. It is not correct on a server: the project directory is user-supplied
# and lands in a `-v <dir>:/datasets` argument, so a container that can reach
# the Docker socket lets any user mount any host path into a container that runs
# as root. Giving a user-facing container the Docker socket is, in effect,
# handing out root on the host.
#
# So the hosted deployment does not give it one. When DRONEBIOR_JOB_DIR is set,
# the three entry points stop calling Docker and instead write a job request
# into a spool directory. A broker outside the user's reach - it holds the
# socket, the user's container cannot talk to it except by leaving a file -
# validates the request and runs the reconstruction.
#
# The security property that matters: **a request never carries a host path.**
# It carries a path as seen inside the user's own container, always under
# /data, and the broker recomputes the host path from the user it already knows
# from the spool directory the file arrived in. A request that asks for
# /etc, or for ../../another-user, is rejected by the broker rather than
# trusted, and nothing the user can write into the request changes which user
# the broker thinks they are.

#' Is this session running under the hosted deployment?
#'
#' @return `TRUE` when a job spool directory is configured.
#' @noRd
dronebior_hosted <- function() {
  nzchar(Sys.getenv("DRONEBIOR_JOB_DIR", unset = ""))
}

#' The spool directory this session writes job requests into.
#' @noRd
dronebior_job_dir <- function() {
  Sys.getenv("DRONEBIOR_JOB_DIR", unset = "")
}

#' The root under which this session's data is mounted.
#'
#' Everything a hosted user can reach lives beneath this. It is a constant of
#' the deployment, not something the user or the request can influence.
#' @noRd
dronebior_data_root <- function() {
  Sys.getenv("DRONEBIOR_DATA_ROOT", unset = "/data")
}

#' Reject a path that does not lie inside the session's own data root.
#'
#' Called on the client for a clear early error. It is *not* the security
#' boundary - the broker checks again, because a client check protects only a
#' client that chose to run it.
#'
#' @param path A directory as seen inside this container.
#' @return The normalised path, or an error.
#' @noRd
assert_within_data_root <- function(path) {
  root <- normalizePath(dronebior_data_root(), mustWork = FALSE)
  full <- normalizePath(path, mustWork = FALSE)
  if (!identical(full, root) &&
      !startsWith(full, paste0(root, .Platform$file.sep))) {
    stop("In the hosted application, a project must live under ", root,
         ". Got: ", full, call. = FALSE)
  }
  full
}

#' Submit a reconstruction to the broker and wait for it.
#'
#' @param kind One of `"point_cloud"`, `"odm_project"`, `"dji_mavic_3m"` -
#'   which of the three entry points the broker should run.
#' @param project A `dronebio_project`.
#' @param args Named list of parameters. Only names the broker allows are
#'   forwarded; anything else is dropped rather than passed through, so a new
#'   argument here cannot silently become a new argument to `docker run`.
#' @param on_progress Called with each new line of the broker's log.
#' @param poll_seconds How often to look for new output.
#' @return The broker's result list, invisibly.
#' @noRd
submit_odm_job <- function(kind, project, args = list(),
                           on_progress = NULL, poll_seconds = 2) {
  stopifnot(kind %in% c("point_cloud", "odm_project", "dji_mavic_3m"))
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("The hosted application needs the 'jsonlite' package.", call. = FALSE)
  }
  spool <- dronebior_job_dir()
  if (!dir.exists(spool)) {
    stop("The job spool directory does not exist: ", spool,
         ". This session thinks it is hosted but nothing is listening.",
         call. = FALSE)
  }

  project_dir <- assert_within_data_root(project$project_dir)
  id <- paste0(format(Sys.time(), "%Y%m%dT%H%M%S"), "-",
               paste(sample(c(letters, 0:9), 8, replace = TRUE), collapse = ""))

  request <- list(
    id                 = id,
    kind               = kind,
    project_dir        = project_dir,
    odm_dataset_subdir = project$odm_dataset_subdir %||% NULL,
    odm_project_name   = project$odm_project_name %||% NULL,
    args               = args
  )

  # Write beside the spool and rename into it, so the broker never sees a
  # half-written request: rename within a directory is atomic.
  tmp <- file.path(spool, paste0(".", id, ".json.part"))
  writeLines(jsonlite::toJSON(request, auto_unbox = TRUE, null = "null"), tmp)
  file.rename(tmp, file.path(spool, paste0(id, ".json")))

  await_odm_job(id, on_progress = on_progress, poll_seconds = poll_seconds)
}

#' Wait for a submitted job, streaming its log.
#' @noRd
await_odm_job <- function(id, on_progress = NULL, poll_seconds = 2) {
  spool  <- dronebior_job_dir()
  status <- file.path(spool, paste0(id, ".status.json"))
  logf   <- file.path(spool, paste0(id, ".log"))
  seen   <- 0L

  repeat {
    if (file.exists(logf) && is.function(on_progress)) {
      lines <- tryCatch(readLines(logf, warn = FALSE), error = function(e) character())
      if (length(lines) > seen) {
        for (l in lines[(seen + 1L):length(lines)]) on_progress(l)
        seen <- length(lines)
      }
    }
    if (file.exists(status)) {
      res <- tryCatch(jsonlite::fromJSON(status, simplifyVector = TRUE),
                      error = function(e) NULL)
      if (!is.null(res) && !identical(res$state, "running")) {
        if (identical(res$state, "rejected")) {
          stop("The reconstruction was refused: ", res$message %||% "no reason given",
               call. = FALSE)
        }
        return(invisible(res))
      }
    }
    Sys.sleep(poll_seconds)
  }
}

#' List the projects this hosted session can see.
#'
#' The hosted interface offers this instead of a free-text directory field: on
#' a shared server, a text box that accepts any path is a way to read the file
#' system, and there is no reason a user should be typing one.
#'
#' @return Character vector of directory names under the data root.
#' @noRd
list_hosted_projects <- function() {
  root <- dronebior_data_root()
  if (!dir.exists(root)) return(character())
  dirs <- list.dirs(root, recursive = FALSE, full.names = FALSE)
  sort(dirs[nzchar(dirs) & !startsWith(dirs, ".")])
}
