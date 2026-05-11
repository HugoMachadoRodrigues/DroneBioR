test_that("with_error_toast returns the expression value on success", {
  expect_equal(with_error_toast("Demo", 42, session = NULL), 42)
})

test_that("with_error_toast catches errors and warns when no Shiny session", {
  expect_warning(
    result <- with_error_toast("Add", stop("boom"), session = NULL),
    "Add: boom"
  )
  expect_null(result)
})

test_that("with_error_toast re-throws shiny.silent.error so validate() still works", {
  cond <- structure(
    list(message = "Required input missing"),
    class = c("shiny.silent.error", "error", "condition")
  )
  expect_error(
    with_error_toast("Compute", stop(cond), session = NULL),
    "Required input missing"
  )
})

test_that("with_error_toast does not swallow non-error conditions", {
  # A warning thrown inside the body must propagate; we only catch errors.
  expect_warning(
    result <- with_error_toast("Compute", {
      warning("careful")
      "value"
    }, session = NULL),
    "careful"
  )
  expect_equal(result, "value")
})
