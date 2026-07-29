#' Run an expression and report errors as Shiny toasts
#'
#' Wraps a reactive body (or any side-effecting expression in a Shiny
#' session) so that uncaught errors surface as toast notifications instead
#' of the default red traceback. Useful inside `reactive()`,
#' `eventReactive()`, `observeEvent()` and render functions.
#'
#' Shiny's own `validate()` / `req()` / `need()` raise the
#' `shiny.silent.error` condition, which `with_error_toast()` re-throws so
#' those inline validations keep working unchanged.
#'
#' That re-throw only preserves a *visible* failure inside `render*()` and
#' `reactive()`, where Shiny displays the message in the dependent output. In
#' an `observeEvent()` there is no such output and the condition is discarded,
#' so a failed `validate()` produces nothing at all. Use [observer_need()]
#' there instead.
#'
#' Outside a Shiny session the error is re-emitted via `warning()` so the
#' helper is safe to call from package-level code too.
#'
#' @param label Short label for the operation; appears at the start of the
#'   toast.
#' @param expr Expression to evaluate. Lazy.
#' @param session Shiny session. Defaults to the current reactive domain.
#' @return The value of `expr`, or `NULL` on error.
#' @examples
#' \dontrun{
#' mosaic <- eventReactive(input$load_mosaic, {
#'   with_error_toast("Load orthomosaic", {
#'     read_multispectral_orthomosaic(input$orthomosaic_path)
#'   })
#' })
#' }
#'
#' # Outside Shiny, errors fall back to warning():
#' suppressWarnings(
#'   result <- with_error_toast("Demo", stop("nope"), session = NULL)
#' )
#' is.null(result)
#' @export
with_error_toast <- function(label, expr,
                             session = NULL) {
  if (is.null(session) && requireNamespace("shiny", quietly = TRUE)) {
    session <- shiny::getDefaultReactiveDomain()
  }
  tryCatch(
    expr,
    error = function(e) {
      # Let Shiny's silent validation errors keep their normal display.
      if (inherits(e, c("shiny.silent.error", "validation"))) {
        stop(e)
      }
      msg <- paste0(label, ": ", conditionMessage(e))
      if (is.null(session)) {
        warning(msg, call. = FALSE)
      } else {
        shiny::showNotification(
          ui       = msg,
          type     = "error",
          duration = 10,
          session  = session
        )
      }
      NULL
    }
  )
}

#' Guard an observer, showing the reason when it stops
#'
#' `validate(need(...))` is the right guard inside `render*()` and `reactive()`:
#' Shiny catches the `shiny.silent.error` it raises and displays the message in
#' the dependent output. Inside `observeEvent()` there is no output to display
#' it in, so Shiny swallows the condition and the click produces nothing at
#' all: no toast, no console message, just a button that looks broken.
#'
#' This raises the same abort, but shows the reason first. Use it in place of
#' `validate(need())` in any observer.
#'
#' @param cond Condition that must hold. Evaluated with [shiny::isTruthy()], so
#'   the semantics match [shiny::need()].
#' @param message Message shown when `cond` does not hold.
#' @param type Notification type, as in [shiny::showNotification()].
#' @param duration Seconds the notification stays up.
#' @param session Shiny session. Defaults to the current reactive domain.
#' @return `invisible(TRUE)` when `cond` holds; otherwise aborts the observer.
#' @examples
#' \dontrun{
#' observeEvent(input$train, {
#'   observer_need(nrow(samples()) > 0, "Extract field samples before training.")
#'   train_models(samples())
#' })
#' }
#' @export
observer_need <- function(cond, message, type = "warning", duration = 10,
                          session = NULL) {
  has_shiny <- requireNamespace("shiny", quietly = TRUE)
  ok <- tryCatch(
    if (has_shiny) shiny::isTruthy(cond) else isTRUE(cond),
    error = function(e) FALSE
  )
  if (isTRUE(ok)) return(invisible(TRUE))

  if (is.null(session) && has_shiny) {
    session <- shiny::getDefaultReactiveDomain()
  }
  if (is.null(session)) {
    # Outside a Shiny session, mirror with_error_toast(): warn and return
    # rather than abort, so package-level callers are not derailed.
    warning(message, call. = FALSE)
    return(invisible(FALSE))
  }
  shiny::showNotification(message, type = type, duration = duration,
                          session = session)
  shiny::req(FALSE)
}
