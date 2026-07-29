# Guard an observer, showing the reason when it stops

`validate(need(...))` is the right guard inside `render*()` and
`reactive()`: Shiny catches the `shiny.silent.error` it raises and
displays the message in the dependent output. Inside `observeEvent()`
there is no output to display it in, so Shiny swallows the condition and
the click produces nothing at all: no toast, no console message, just a
button that looks broken.

## Usage

``` r
observer_need(
  cond,
  message,
  type = "warning",
  duration = 10,
  session = NULL
)
```

## Arguments

- cond:

  Condition that must hold. Evaluated with
  [`shiny::isTruthy()`](https://rdrr.io/pkg/shiny/man/isTruthy.html), so
  the semantics match
  [`shiny::need()`](https://rdrr.io/pkg/shiny/man/validate.html).

- message:

  Message shown when `cond` does not hold.

- type:

  Notification type, as in
  [`shiny::showNotification()`](https://rdrr.io/pkg/shiny/man/showNotification.html).

- duration:

  Seconds the notification stays up.

- session:

  Shiny session. Defaults to the current reactive domain.

## Value

`invisible(TRUE)` when `cond` holds; otherwise aborts the observer.

## Details

This raises the same abort, but shows the reason first. Use it in place
of `validate(need())` in any observer.

## Examples

``` r
if (FALSE) { # \dontrun{
observeEvent(input$train, {
  observer_need(nrow(samples()) > 0, "Extract field samples before training.")
  train_models(samples())
})
} # }
```
