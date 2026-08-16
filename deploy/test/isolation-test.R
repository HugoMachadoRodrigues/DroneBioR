#!/usr/bin/env Rscript
#
# Three users, and every way one of them can reach another.
#
# This runs the broker's real decision functions - sourced out of broker.R, not
# reimplemented - against a temporary tree laid out exactly like the deployment:
#
#   users/hugo    users/ana    users/carlos
#   spool/hugo    spool/ana    spool/carlos
#
# Each case states what an attacker does and what must happen. A case that
# PASSES means the attack was refused. A case that FAILS is a hole, and the
# output says which one.
#
# It does not need Docker, a VM, or the network: everything under test is the
# path handling and the argument handling, which is where every confirmed break
# so far has been.

`%||%` <- function(a, b) if (is.null(a)) b else a
PKG <- Sys.getenv("DRONEBIOR_PKG", getwd())
suppressWarnings(suppressMessages(devtools::load_all(PKG, quiet = TRUE)))

# ---- stand up the deployment layout ---------------------------------------
BASE       <- file.path(tempdir(), paste0("dbr-iso-", Sys.getpid()))
USERS_ROOT <- file.path(BASE, "users")
SPOOL_ROOT <- file.path(BASE, "spool")
USERS      <- c("hugo", "ana", "carlos")
CONTAINER_DATA_ROOT <- "/data"
DATASET_SUBDIR <- file.path("outputs", "odm_micasense_dataset")
PROJECT_NAME   <- "micasense"

for (u in USERS) {
  dir.create(file.path(USERS_ROOT, u), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(SPOOL_ROOT, u), recursive = TRUE, showWarnings = FALSE)
}
# Each user has one real project with a few images, so a request can get far
# enough to be interesting.
for (u in USERS) {
  img <- file.path(USERS_ROOT, u, "voo", "imagens", "micasense")
  dir.create(img, recursive = TRUE, showWarnings = FALSE)
  for (b in 1:5) file.create(file.path(img, sprintf("IMG_0001_%d.tif", b)))
}
# Something worth stealing.
writeLines("ana's unpublished biomass", file.path(USERS_ROOT, "ana", "secret.txt"))

# ---- the broker's own functions, not a copy of them -----------------------
src <- readLines(file.path(PKG, "deploy", "broker", "broker.R"))
for (fn in c("symlink_in_path", "resolve_host_dir", "sanitise_args")) {
  i <- grep(paste0("^", fn, " <- function"), src)
  j <- grep("^}", src); j <- min(j[j > i])
  eval(parse(text = paste(src[i:j], collapse = "\n")))
}
MAX_WORKERS <- 6L
say <- function(...) invisible(NULL)   # the broker logs; the test does not
ALLOWED <- eval(parse(text = paste(
  src[grep("^ALLOWED <- list\\(", src):(grep("^\\)$", src)[grep("^\\)$", src) >
      grep("^ALLOWED <- list\\(", src)][1])], collapse = "\n")))

# ---- the cases ------------------------------------------------------------
pass <- 0L; fail <- 0L
case <- function(what, expect_refused, run) {
  res <- tryCatch(run(), error = function(e) paste("error:", conditionMessage(e)))
  refused <- is.list(res) && is.null(res[[1]])
  ok <- identical(refused, expect_refused)
  if (ok) pass <<- pass + 1L else fail <<- fail + 1L
  cat(sprintf("  %s %-58s %s\n", if (ok) "PASS" else "FAIL", what,
              if (is.list(res) && is.null(res[[1]])) paste("refused:", res[[2]])
              else if (is.character(res)) res else "accepted"))
}

cat("\n== hugo attacking ana ==\n")

case("own project", FALSE, function() resolve_host_dir("hugo", "/data/voo"))

case("absolute path to ana", TRUE, function()
  resolve_host_dir("hugo", paste0("/data/../ana")))

case("traversal to ana", TRUE, function()
  resolve_host_dir("hugo", "/data/../../users/ana"))

case("symlink component -> ana", TRUE, function() {
  file.symlink(file.path(USERS_ROOT, "ana"), file.path(USERS_ROOT, "hugo", "peek"))
  on.exit(unlink(file.path(USERS_ROOT, "hugo", "peek")))
  resolve_host_dir("hugo", "/data/peek")
})

case("symlink at the appended outputs/ level", TRUE, function() {
  file.symlink(file.path(USERS_ROOT, "ana"), file.path(USERS_ROOT, "hugo", "outputs"))
  on.exit(unlink(file.path(USERS_ROOT, "hugo", "outputs")))
  resolve_host_dir("hugo", "/data")
})

case("symlink at the project-name level", TRUE, function() {
  d <- file.path(USERS_ROOT, "hugo", DATASET_SUBDIR)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  file.symlink(file.path(USERS_ROOT, "ana"), file.path(d, PROJECT_NAME))
  on.exit(unlink(file.path(USERS_ROOT, "hugo", "outputs"), recursive = TRUE))
  resolve_host_dir("hugo", "/data")
})

case("claiming to be ana in the request", TRUE, function()
  # The user argument comes from the spool directory, never the request, so
  # this is really a test that nothing in the request can reach it.
  resolve_host_dir("hugo", "/data/../../spool/ana"))

cat("\n== hugo attacking the host ==\n")

case("host absolute path", TRUE, function() resolve_host_dir("hugo", "/etc"))
case("under /data but escaping", TRUE, function() resolve_host_dir("hugo", "/data/../../.."))
case("symlink to /", TRUE, function() {
  file.symlink("/", file.path(USERS_ROOT, "hugo", "root"))
  on.exit(unlink(file.path(USERS_ROOT, "hugo", "root")))
  resolve_host_dir("hugo", "/data/root")
})
case("not a string", TRUE, function() resolve_host_dir("hugo", list(1, 2)))
case("vector of paths", TRUE, function() resolve_host_dir("hugo", c("/data/voo", "/etc")))

cat("\n== the mount that is actually built ==\n")
mount_of <- function(cmd) sub(".*-v. .([^']+):/datasets.*", "\\1", cmd)
for (u in c("hugo", "ana")) {
  root <- normalizePath(file.path(USERS_ROOT, u))
  pr <- dronebio_project(project_dir = file.path(root, "voo"))
  r <- run_odm_project(pr, run = FALSE, camera_type = "multispectral",
                       mount_dir = root,
                       project_path = "/datasets/voo/outputs/odm_micasense_dataset")
  m <- mount_of(r$command)
  ok <- identical(m, root)
  if (ok) pass <- pass + 1L else fail <- fail + 1L
  cat(sprintf("  %s %-58s %s\n", if (ok) "PASS" else "FAIL",
              paste(u, "mounts only their own root"), m))
}

cat("\n== a code path that forgets the pin ==\n")
Sys.setenv(DRONEBIOR_REQUIRE_PINNED_MOUNT = "1")
root <- normalizePath(file.path(USERS_ROOT, "hugo"))
pr <- dronebio_project(project_dir = file.path(root, "voo"))
r <- tryCatch(run_odm_project(pr, run = FALSE, camera_type = "multispectral"),
              error = function(e) conditionMessage(e))
refused <- is.character(r) && grepl("requires an explicit mount_dir", r)
if (refused) pass <- pass + 1L else fail <- fail + 1L
cat(sprintf("  %s %-58s %s\n", if (refused) "PASS" else "FAIL",
            "unpinned call fails closed",
            if (refused) "refused" else paste("MOUNTED", mount_of(r$command))))
Sys.unsetenv("DRONEBIOR_REQUIRE_PINNED_MOUNT")

cat("\n== parameters ==\n")
for (probe in list(
  list("unknown name dropped",        list(evil = "x"),                    0L),
  list("mount_dir cannot be injected", list(mount_dir = "/"),              0L),
  list("project_path cannot be injected", list(project_path = "/etc"),     0L),
  list("workers clamped",             list(max_concurrency = 9999L),       1L),
  list("quality from the fixed set",  list(pc_quality = "; rm -rf /"),     0L)
)) {
  got <- sanitise_args(probe[[2]])
  ok <- length(got) == probe[[3]]
  if (ok) pass <- pass + 1L else fail <- fail + 1L
  cat(sprintf("  %s %-58s %s\n", if (ok) "PASS" else "FAIL", probe[[1]],
              if (length(got)) paste(names(got), unlist(got), sep = "=", collapse = " ") else "dropped"))
}

cat("\n== per-user uids ==\n")

# The uid map the broker will actually parse, taken from the compose file so
# the test fails when the two drift apart rather than when someone remembers.
compose <- readLines(file.path(PKG, "deploy", "docker-compose.yml"))
raw <- sub('.*DRONEBIOR_UID_MAP: *"([^"]*)".*', "\\1",
           grep("DRONEBIOR_UID_MAP", compose, value = TRUE)[1])
uid_map <- local({
  out <- list()
  for (pair in strsplit(raw, ",", fixed = TRUE)[[1]]) {
    kv <- strsplit(trimws(pair), ":", fixed = TRUE)[[1]]
    if (length(kv) == 2L && nzchar(kv[1]) && grepl("^[0-9]+$", kv[2])) out[[kv[1]]] <- kv[2]
  }
  out
})
uid_for <- function(user) if (is.null(uid_map[[user]])) NULL else uid_map[[user]]

expect <- function(what, ok, detail = "") {
  if (isTRUE(ok)) pass <<- pass + 1L else fail <<- fail + 1L
  cat(sprintf("  %s %-58s %s\n", if (isTRUE(ok)) "PASS" else "FAIL", what, detail))
}

expect("every user has a uid", all(vapply(USERS, function(u) !is.null(uid_for(u)), logical(1))),
       paste(unlist(uid_map[USERS]), collapse = " "))
expect("uids are distinct", length(unique(unlist(uid_map[USERS]))) == length(USERS))
expect("no user runs as root", !any(unlist(uid_map) == "0"))
expect("an unknown account gets no run", is.null(uid_for("intruder")))

# The ShinyProxy spec and the broker's map must agree, or a reconstruction
# writes files the user's own container cannot read back.
appyml <- readLines(file.path(PKG, "deploy", "shinyproxy", "application.yml"))
spec_uid <- list()
cur <- NULL
for (l in appyml) {
  if (grepl("^    - id: studio-", l)) cur <- sub(".*studio-", "", trimws(l))
  if (grepl("^      docker-user:", l) && !is.null(cur)) {
    spec_uid[[cur]] <- gsub("[^0-9]", "", l); cur <- NULL
  }
}
agree <- all(vapply(USERS, function(u)
  identical(spec_uid[[u]], uid_for(u)), logical(1)))
expect("ShinyProxy docker-user agrees with the broker map", agree,
       paste(vapply(USERS, function(u) paste0(u, ":", spec_uid[[u]] %||% "?"), ""), collapse = " "))

# Each spec must mount only its own user. A one-character slip here is exactly
# what the uid barrier exists to catch, so the test looks for it directly.
own_only <- TRUE
for (u in USERS) {
  i <- grep(paste0("^    - id: studio-", u, "$"), appyml)
  block <- appyml[i:min(length(appyml), i + 14)]
  mounts <- grep("/srv/dronebior/", block, value = TRUE)
  others <- setdiff(USERS, u)
  if (any(vapply(others, function(o) any(grepl(paste0("/", o, ":"), mounts)), logical(1))) ||
      !all(grepl(paste0("/", u, ":"), mounts))) own_only <- FALSE
}
expect("each spec mounts only its own user", own_only)

cat(sprintf("\n%d passed, %d failed\n", pass, fail))
unlink(BASE, recursive = TRUE)
if (fail > 0) quit(status = 1)
