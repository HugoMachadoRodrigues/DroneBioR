# The bind mount that carries a reconstruction into its container.
#
# On one person's laptop the mount is the project directory and that is fine:
# they already own the machine. On a shared server it is the security boundary,
# because the project path is user-supplied and lands in `-v <path>:/datasets`
# against a container that runs as root. An adversarial review broke the first
# two versions of this:
#
#   * validating project_dir and then mounting project_dir + a second,
#     unvalidated field, so a symlink named in that field gave `-v /:/datasets`;
#   * and, after that was fixed, a code path that simply did not pass the pin -
#     build_point_cloud_only() branches on the *contents* of the images folder,
#     which the user controls, so naming one file DJI_0001_0001_D.JPG reached a
#     branch that fell back to the unpinned default.
#
# Hence these tests. The first two pin the behaviour; the third pins the thing
# that makes a future un-threaded call site safe, which is that a missing pin
# is an error rather than a default.

make_project <- function(root, name = "voo", dji = FALSE) {
  img <- file.path(root, name, "imagens", "micasense")
  dir.create(img, recursive = TRUE, showWarnings = FALSE)
  if (dji) {
    file.create(file.path(img, "DJI_20260501120000_0001_D.JPG"))
  } else {
    for (b in 1:5) file.create(file.path(img, sprintf("IMG_0001_%d.tif", b)))
  }
  dronebio_project(project_dir = file.path(root, name))
}

mount_of <- function(command) sub(".*-v. .([^']+):/datasets.*", "\\1", command)

test_that("mount_dir pins the bind mount away from the project path", {
  root <- withr::local_tempdir()
  project <- make_project(root)

  unpinned <- run_odm_project(project, run = FALSE, camera_type = "multispectral")
  expect_identical(mount_of(unpinned$command),
                   normalizePath(project$odm_dataset_dir, mustWork = FALSE))

  pinned <- run_odm_project(project, run = FALSE, camera_type = "multispectral",
                            mount_dir = root,
                            project_path = "/datasets/voo/outputs/odm_micasense_dataset")
  expect_identical(mount_of(pinned$command), normalizePath(root))
  expect_match(pinned$command, "--project-path' '/datasets/voo/outputs", fixed = TRUE)
})

test_that("a symbolic link under the project cannot redirect a pinned mount", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "voo", "outputs"), recursive = TRUE, showWarnings = FALSE)
  # What an attacker plants: a link inside their own tree whose target is the
  # host root. Unpinned, this is exactly what produced `-v /:/datasets`.
  file.symlink("/", file.path(root, "voo", "outputs", "odm_micasense_dataset"))
  dataset_dir <- file.path(root, "voo", "outputs", "odm_micasense_dataset")

  # build_odm_args() is the unit that decides the mount, so assert on it
  # directly. run_odm_project() would first try to copy images *through* the
  # link and fail on the way - which is the broker's second line of defence,
  # not the property under test here.
  unpinned <- build_odm_args(dataset_dir = dataset_dir, project_name = "micasense")
  expect_identical(unpinned[which(unpinned == "-v") + 1L], "/:/datasets")

  pinned <- build_odm_args(dataset_dir = dataset_dir, project_name = "micasense",
                           mount_dir = root,
                           project_path = "/datasets/voo/outputs/odm_micasense_dataset")
  expect_identical(pinned[which(pinned == "-v") + 1L],
                   paste0(normalizePath(root), ":/datasets"))
})

test_that("a deployment that requires a pin refuses a call that omits one", {
  root <- withr::local_tempdir()
  project <- make_project(root)
  withr::local_envvar(c(DRONEBIOR_REQUIRE_PINNED_MOUNT = "1"))

  expect_error(
    run_odm_project(project, run = FALSE, camera_type = "multispectral"),
    "requires an explicit mount_dir"
  )
  # And the DJI branch, which is reached from the *contents* of the folder
  # rather than from any argument, must fail the same way rather than quietly
  # mounting a user-derived directory.
  dji <- make_project(root, name = "dji", dji = TRUE)
  expect_error(
    build_point_cloud_only(dji, pc_quality = "low", run = FALSE),
    "requires an explicit mount_dir"
  )
})

test_that("without that variable the default is unchanged", {
  root <- withr::local_tempdir()
  project <- make_project(root)
  withr::local_envvar(c(DRONEBIOR_REQUIRE_PINNED_MOUNT = NA))
  res <- run_odm_project(project, run = FALSE, camera_type = "multispectral")
  expect_identical(mount_of(res$command),
                   normalizePath(project$odm_dataset_dir, mustWork = FALSE))
})
