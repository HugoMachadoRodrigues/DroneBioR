# A reconstruction outlives the browser tab: docker runs it detached. Without a
# name the app cannot find its own container, and closing the Studio looks like
# it stopped a run that is still holding 45 GB and every core.

test_that("the docker command names the container deterministically", {
  a <- build_odm_args(dataset_dir = "/tmp/proj/outputs/odm_micasense_dataset",
                      project_name = "micasense")
  i <- which(a == "--name")
  expect_length(i, 1L)
  expect_identical(a[i + 1L], "dronebior-odm_micasense_dataset-micasense")

  # same inputs, same name - otherwise a stop cannot find the run
  b <- build_odm_args(dataset_dir = "/tmp/proj/outputs/odm_micasense_dataset",
                      project_name = "micasense")
  expect_identical(a[i + 1L], b[which(b == "--name") + 1L])

  # different projects must not collide
  c2 <- build_odm_args(dataset_dir = "/tmp/proj/outputs/other_dataset",
                       project_name = "micasense")
  expect_false(identical(a[i + 1L], c2[which(c2 == "--name") + 1L]))
})

test_that("container names survive characters docker rejects", {
  nm <- DroneBioR:::odm_container_name("/tmp/my project (2)/déjà vu", "band ms")
  expect_match(nm, "^dronebior-")
  # docker accepts [a-zA-Z0-9][a-zA-Z0-9_.-]* only
  expect_match(nm, "^[A-Za-z0-9][A-Za-z0-9_.-]*$")
  expect_lte(nchar(nm), 90L)
})

test_that("max_concurrency reaches the docker command", {
  a <- build_odm_args(dataset_dir = tempdir(), project_name = "demo",
                      max_concurrency = 3)
  expect_identical(a[which(a == "--max-concurrency") + 1L], "3")

  # and through the engine entry point, which forwards via ...
  root <- tempfile("conc_"); dir.create(root)
  p <- dronebio_project(project_dir = root)
  dir.create(p$images_dir, recursive = TRUE)
  file.create(file.path(p$images_dir,
                        sprintf("IMG_%04d_%d.tif", rep(1:3, each = 5), 1:5)))
  # $command is the rendered command line, one string - not the arg vector
  cmd <- run_odm_project(p, run = FALSE, max_concurrency = 3)$command
  expect_length(cmd, 1L)
  expect_match(cmd, "'--max-concurrency' '3'", fixed = TRUE)
  expect_match(cmd, "'--name' 'dronebior-", fixed = TRUE)
})

test_that("stopping reports FALSE when nothing of ours is running", {
  # No container by this name exists, so the helper must say so rather than
  # claiming success or erroring when docker is absent.
  expect_false(DroneBioR:::stop_odm_container(tempfile("nope_"), "micasense"))
})
