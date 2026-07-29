# Run an expression and report errors as Shiny toasts

Wraps a reactive body (or any side-effecting expression in a Shiny
session) so that uncaught errors surface as toast notifications instead
of the default red traceback. Useful inside `reactive()`,
`eventReactive()`, `observeEvent()` and render functions.

## Usage

``` r
with_error_toast(label, expr, session = NULL)
```

## Arguments

- label:

  Short label for the operation; appears at the start of the toast.

- expr:

  Expression to evaluate. Lazy.

- session:

  Shiny session. Defaults to the current reactive domain.

## Value

The value of `expr`, or `NULL` on error.

## Details

Shiny's own `validate()` / `req()` / `need()` raise the
`shiny.silent.error` condition, which `with_error_toast()` re-throws so
those inline validations keep working unchanged.

That re-throw only preserves a *visible* failure inside `render*()` and
`reactive()`, where Shiny displays the message in the dependent output.
In an `observeEvent()` there is no such output and the condition is
discarded, so a failed `validate()` produces nothing at all. Use
[`observer_need()`](https://hugomachadorodrigues.github.io/DroneBioR/reference/observer_need.md)
there instead.

Outside a Shiny session the error is re-emitted via
[`warning()`](https://rdrr.io/r/base/warning.html) so the helper is safe
to call from package-level code too.

## Examples

``` r
if (FALSE) { # \dontrun{
mosaic <- eventReactive(input$load_mosaic, {
  with_error_toast("Load orthomosaic", {
    read_multispectral_orthomosaic(input$orthomosaic_path)
  })
})
} # }

# Outside Shiny, errors fall back to warning():
suppressWarnings(
  result <- with_error_toast("Demo", stop("nope"), session = NULL)
)
is.null(result)
#> [1] TRUE
```
