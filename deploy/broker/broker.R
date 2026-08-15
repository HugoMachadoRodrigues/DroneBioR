#!/usr/bin/env Rscript
#
# The reconstruction broker.
#
# This is the only process in the deployment that can talk to Docker, and no
# user can reach it. Users' containers leave a job request in their own spool
# directory; this loop picks it up, decides whether it is allowed, and runs the
# reconstruction on their behalf.
#
# The whole point is that the request is *data*, never instruction. Three rules
# follow from that, and every check below is one of them:
#
#   1. The user is whoever owns the spool directory the file arrived in. It is
#      never read from the request. A request that claims to be someone else is
#      simply a request from the person who wrote it, claiming something.
#
#   2. THE MOUNT SOURCE CONTAINS NO USER INPUT AT ALL. This is stronger than
#      "the path is checked", and the earlier version of this broker got it
#      wrong in a way worth recording: it validated `project_dir` and then
#      mounted `project_dir + odm_dataset_subdir`, a different path built from
#      another field of the same request. A symlink named in that field - which
#      the user may create inside their own tree - made the mount `-v /:/datasets`
#      into a container running as root, with the host Docker socket inside it.
#      Checking one path and mounting another is not a check.
#
#      Even validating the right path is not enough on its own: the user owns
#      everything inside their tree and can replace a directory with a symlink
#      between the check and the mount. So the mount is pinned to
#      USERS_ROOT/<user> - a directory whose PARENT is root-owned, so the user
#      cannot swap it - and the project is addressed inside the container with
#      --project-path. A symlink planted in the mount then resolves against the
#      container's filesystem, where it reaches nothing.
#
#   3. Parameters are chosen from a fixed set, not forwarded. An unknown name is
#      dropped. A known name is coerced and clamped. Nothing a user writes
#      becomes a token in a docker command line.
#
# Environment:
#   DRONEBIOR_SPOOL_ROOT  parent of the per-user spool directories (default /spool)
#   DRONEBIOR_USERS_ROOT  parent of the per-user data directories (default /srv/dronebior/users)
#   DRONEBIOR_POLL        seconds between sweeps (default 3)
#   DRONEBIOR_MAX_WORKERS ceiling on --max-concurrency (default 8)

suppressWarnings(suppressMessages({
  library(DroneBioR)
  library(jsonlite)
}))

SPOOL_ROOT  <- Sys.getenv("DRONEBIOR_SPOOL_ROOT", "/spool")
USERS_ROOT  <- Sys.getenv("DRONEBIOR_USERS_ROOT", "/srv/dronebior/users")
POLL        <- as.numeric(Sys.getenv("DRONEBIOR_POLL", "3"))
MAX_WORKERS <- as.integer(Sys.getenv("DRONEBIOR_MAX_WORKERS", "8"))
CONTAINER_DATA_ROOT <- "/data"

# Declare that every reconstruction in this process must name its mount
# explicitly. build_odm_args() then refuses to fall back to a caller-derived
# path, so a code path that forgets the pin fails loudly instead of mounting
# whatever the user pointed at.
Sys.setenv(DRONEBIOR_REQUIRE_PINNED_MOUNT = "1")

# The project layout is a constant of the deployment. These used to be taken
# from the request; that is exactly how a symlink component reached the mount.
DATASET_SUBDIR <- file.path("outputs", "odm_micasense_dataset")
PROJECT_NAME   <- "micasense"

say <- function(...) cat(format(Sys.time(), "%H:%M:%S"), "|", ..., "\n", sep = " ")

# Each user's data belongs to a different uid, and the container they drive
# runs as that uid (docker-user in application.yml). The broker has to agree:
# handing every user's products back to one shared uid would undo the
# separation the moment anyone ran a reconstruction.
#
# The map is configuration, not a guess - DRONEBIOR_UID_MAP is "user:uid,..."
# and a user who is not in it gets no run at all, because the alternative is
# writing files as root into a tree the application cannot then touch.
uid_map <- local({
  raw <- Sys.getenv("DRONEBIOR_UID_MAP", unset = "")
  out <- list()
  for (pair in strsplit(raw, ",", fixed = TRUE)[[1]]) {
    kv <- strsplit(trimws(pair), ":", fixed = TRUE)[[1]]
    if (length(kv) == 2L && nzchar(kv[1]) && grepl("^[0-9]+$", kv[2])) {
      out[[kv[1]]] <- kv[2]
    }
  }
  out
})

uid_for <- function(user) uid_map[[user]] %||% NULL
`%||%` <- function(a, b) if (is.null(a)) b else a

# ---- rule 3: parameters are chosen, not forwarded -------------------------
#
# Each entry says how to coerce the value and what range is acceptable. A name
# that is not in this table does not reach the reconstruction at all.
ALLOWED <- list(
  pc_quality       = function(v) match.arg(as.character(v)[1],
                                           c("lowest", "low", "medium", "high", "ultra")),
  pc_filter        = function(v) max(0, min(10, as.numeric(v)[1])),
  pc_rectify       = function(v) isTRUE(as.logical(v)[1]),
  max_concurrency  = function(v) max(1L, min(MAX_WORKERS, as.integer(v)[1])),
  feature_quality  = function(v) match.arg(as.character(v)[1],
                                           c("lowest", "low", "medium", "high", "ultra")),
  fast_orthophoto  = function(v) isTRUE(as.logical(v)[1]),
  skip_3dmodel     = function(v) isTRUE(as.logical(v)[1])
)

sanitise_args <- function(args) {
  if (!length(args)) return(list())
  keep <- intersect(names(args), names(ALLOWED))
  dropped <- setdiff(names(args), keep)
  if (length(dropped)) {
    say("  dropping parameters the broker does not accept:",
        paste(dropped, collapse = ", "))
  }
  out <- list()
  for (nm in keep) {
    v <- tryCatch(ALLOWED[[nm]](args[[nm]]), error = function(e) NULL)
    if (!is.null(v) && !is.na(v)) out[[nm]] <- v
  }
  out
}

# ---- rule 2: the host path is computed here -------------------------------
#
# Returns the host path, or NULL with a reason. normalizePath() resolves "..",
# symlinks and duplicate separators, so the containment test happens on what
# the path really is rather than on how it was spelled.
resolve_host_dir <- function(user, container_path) {
  if (!is.character(container_path) || length(container_path) != 1L ||
      !nzchar(container_path)) {
    return(list(NULL, "the request carries no project directory"))
  }
  root <- paste0(CONTAINER_DATA_ROOT, "/")
  if (!identical(container_path, CONTAINER_DATA_ROOT) &&
      !startsWith(container_path, root)) {
    return(list(NULL, paste0("a project must be under ", CONTAINER_DATA_ROOT)))
  }
  relative <- sub(paste0("^", CONTAINER_DATA_ROOT, "/?"), "", container_path)

  user_root <- normalizePath(file.path(USERS_ROOT, user), mustWork = FALSE)
  candidate <- normalizePath(file.path(user_root, relative), mustWork = FALSE)

  if (!identical(candidate, user_root) &&
      !startsWith(candidate, paste0(user_root, "/"))) {
    return(list(NULL, "that path resolves outside your own directory"))
  }
  if (!dir.exists(candidate)) {
    return(list(NULL, paste0("no such project: ", relative)))
  }
  # No component may be a symlink - and that has to mean EVERY component the
  # broker will touch, not just the ones the request named. Checking only
  # `relative` missed the layout appended afterwards: a request for /data has
  # an empty `relative`, so the walk ran zero times, and the user's own
  # `outputs` could be a symlink that the broker - running as root - then
  # created directories through and deleted files through, on the host.
  bad <- symlink_in_path(user_root, c(strsplit(relative, "/", fixed = TRUE)[[1]],
                                      strsplit(DATASET_SUBDIR, "/", fixed = TRUE)[[1]],
                                      PROJECT_NAME))
  if (!is.null(bad)) {
    return(list(NULL, paste0("a symbolic link is not allowed in a project path: ", bad)))
  }
  list(candidate, NULL, relative)
}

#' First component of `parts` under `base` that is a symbolic link, or NULL.
#'
#' A component that does not exist yet is fine - the broker creates it, and a
#' path it creates is not a link. What must never be followed is one that is
#' already there and points elsewhere.
#' @noRd
symlink_in_path <- function(base, parts) {
  walk <- base
  for (part in parts) {
    if (!nzchar(part) || identical(part, ".")) next
    walk <- file.path(walk, part)
    link <- Sys.readlink(walk)
    # Sys.readlink() answers three different ways and they must not be
    # conflated: "" for a real file that is not a link, the target for a link,
    # and NA for a path that does not exist. nzchar(NA) is TRUE, so testing
    # nzchar() alone called every not-yet-created directory a symlink - which
    # refused every legitimate project, since outputs/odm_micasense_dataset is
    # exactly what the run is about to create.
    if (!is.na(link) && nzchar(link)) return(walk)
  }
  NULL
}

write_status <- function(spool, id, state, ...) {
  payload <- c(list(id = id, state = state,
                    at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")), list(...))
  tmp <- file.path(spool, paste0(".", id, ".status.part"))
  writeLines(toJSON(payload, auto_unbox = TRUE, null = "null"), tmp)
  file.rename(tmp, file.path(spool, paste0(id, ".status.json")))
}

run_job <- function(user, spool, path) {
  id <- sub("[.]json$", "", basename(path))
  req <- tryCatch(fromJSON(path, simplifyVector = TRUE), error = function(e) NULL)
  # Take the request out of the spool immediately: it is claimed, and a parse
  # failure must not leave it to be retried forever.
  unlink(path)
  if (is.null(req)) {
    write_status(spool, id, "rejected", message = "the request was not valid JSON")
    return(invisible(NULL))
  }

  say("job", id, "from", user, "-", req$kind)

  # No uid, no run. A reconstruction executed for a user the deployment does
  # not know would write root-owned files into a tree that user's own
  # container cannot read back.
  owner <- uid_for(user)
  if (is.null(owner)) {
    say("  refused: no uid configured for", user)
    write_status(spool, id, "rejected",
                 message = paste("this account is not configured on the server;",
                                 "ask the administrator to add it"))
    return(invisible(NULL))
  }

  write_status(spool, id, "running")

  kind <- req$kind
  if (!isTRUE(kind %in% c("point_cloud", "odm_project", "dji_mavic_3m"))) {
    write_status(spool, id, "rejected", message = "unknown kind of reconstruction")
    return(invisible(NULL))
  }

  # The DJI path builds five per-band reconstructions with their own dataset
  # directories, and threading the pinned mount through all of them has not
  # been verified against a real run. Refusing it is the honest position:
  # better a feature that is unavailable than one that is available and unsafe.
  if (identical(kind, "dji_mavic_3m")) {
    write_status(spool, id, "rejected",
                 message = paste("DJI Mavic 3M reconstruction is not yet available",
                                 "on the hosted site. Run it locally for now."))
    return(invisible(NULL))
  }

  resolved <- resolve_host_dir(user, req$project_dir)
  host_dir <- resolved[[1]]
  if (is.null(host_dir)) {
    say("  refused:", resolved[[2]])
    write_status(spool, id, "rejected", message = resolved[[2]])
    return(invisible(NULL))
  }
  say("  ", req$project_dir, "->", host_dir)

  relative <- resolved[[3]]
  user_root <- normalizePath(file.path(USERS_ROOT, user), mustWork = FALSE)

  args <- sanitise_args(req$args)
  logf <- file.path(spool, paste0(id, ".log"))

  # What is bound to /datasets, and where ODM works inside it. The mount is the
  # user's own root and nothing else: no field of the request appears in it, so
  # there is nothing for a symlink or a late swap to redirect. The project path
  # is user-derived but lives inside the container, where it cannot name a host
  # file.
  mount_dir    <- user_root
  project_path <- paste0("/datasets",
                         if (nzchar(relative)) paste0("/", relative) else "",
                         "/", DATASET_SUBDIR)

  # The layout is fixed by the deployment. Taking it from the request is what
  # let a symlink into the mount path; there is also no reason a user should be
  # choosing where inside their own project the engine writes.
  project <- dronebio_project(
    project_dir        = host_dir,
    odm_dataset_subdir = DATASET_SUBDIR,
    odm_project_name   = PROJECT_NAME
  )

  entry <- switch(kind,
    point_cloud  = DroneBioR::build_point_cloud_only,
    odm_project  = DroneBioR::run_odm_project,
    dji_mavic_3m = DroneBioR::run_odm_dji_mavic_3m
  )

  # Re-check immediately before the run. The user owns their whole subtree and
  # can replace a directory with a symlink at any moment, so the walk done
  # during validation is a snapshot. Repeating it here narrows the window to
  # the gap between these two lines rather than the whole validation phase.
  #
  # It does not close it. Closing it properly needs the file operations
  # themselves to refuse to traverse links (openat with RESOLVE_BENEATH), which
  # R does not expose. What makes the residual survivable is that the *mount* is
  # pinned and no longer depends on any of this: the worst a won race reaches is
  # another directory under USERS_ROOT, not the host.
  bad <- symlink_in_path(user_root,
                         c(strsplit(relative, "/", fixed = TRUE)[[1]],
                           strsplit(DATASET_SUBDIR, "/", fixed = TRUE)[[1]],
                           PROJECT_NAME))
  if (!is.null(bad)) {
    say("  refused at run time: symlink appeared at", bad)
    write_status(spool, id, "rejected",
                 message = "a symbolic link appeared in the project path")
    return(invisible(NULL))
  }

  con <- file(logf, open = "wt")
  ok <- tryCatch({
    sink(con, type = "output"); sink(con, type = "message")
    on.exit({ sink(type = "message"); sink(type = "output"); close(con) }, add = TRUE)
    do.call(entry, c(list(project), args,
                     list(mount_dir = mount_dir, project_path = project_path)))
    TRUE
  }, error = function(e) {
    tryCatch({ sink(type = "message"); sink(type = "output") }, error = function(...) NULL)
    say("  failed:", conditionMessage(e))
    write_status(spool, id, "failed", message = conditionMessage(e))
    FALSE
  })

  # ODM runs as root inside its own container and writes the products into the
  # mounted dataset directory, so everything it produces comes out root-owned.
  # The application runs as an unprivileged user and would be able to read
  # those files but never delete or replace them - a reconstruction could be
  # run once and then never cleaned up. Hand them back.
  # -h so the ownership of a symbolic link is changed rather than the thing it
  # points at: the walk refuses a link in the path, but a link may sit inside
  # the tree, and chown must not reach through it.
  if (nzchar(Sys.which("chown"))) {
    system2("chown", c("-R", "-h", paste0(owner, ":", owner), shQuote(host_dir)),
            stdout = FALSE, stderr = FALSE)
  }

  if (isTRUE(ok)) {
    say("  done")
    write_status(spool, id, "done")
  }
  invisible(NULL)
}

say("broker up | spool", SPOOL_ROOT, "| users", USERS_ROOT,
    "| worker ceiling", MAX_WORKERS)
if (!nzchar(Sys.which("docker"))) {
  say("WARNING: no docker on PATH - reconstructions will fail")
}

repeat {
  users <- list.dirs(SPOOL_ROOT, recursive = FALSE, full.names = FALSE)
  for (user in users) {
    spool <- file.path(SPOOL_ROOT, user)
    # Only *.json, and never the .part files a client is still writing.
    jobs <- list.files(spool, pattern = "^[^.].*[.]json$", full.names = TRUE)
    jobs <- jobs[!grepl("[.]status[.]json$", jobs)]
    for (j in sort(jobs)) {
      tryCatch(run_job(user, spool, j),
               error = function(e) say("  broker error:", conditionMessage(e)))
    }
  }
  Sys.sleep(POLL)
}
