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
