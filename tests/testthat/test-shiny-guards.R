# Walks a parsed expression, tracking the enclosing Shiny wrapper of each
# call. Defined first so every test below can use it.
.guard_walk <- function(e, ctx, visit) {
  if (!is.call(e)) return(invisible())
  nm <- if (is.name(e[[1L]])) as.character(e[[1L]]) else ""
  visit(nm, e, ctx)
  if (nm %in% c("observeEvent", "observe", "reactive", "eventReactive",
                "reactiveVal") || grepl("^render", nm)) {
    ctx <- c(ctx, nm)
  }
  for (el in as.list(e)) {
    if (missing(el)) next
    if (is.symbol(el) && !nzchar(as.character(el))) next
    .guard_walk(el, ctx, visit)
  }
  invisible()
}

test_that("observer_need lets a truthy condition through", {
  expect_true(observer_need(TRUE, "nope", session = NULL))
  expect_silent(observer_need(1 > 0, "nope", session = NULL))
})

test_that("the app never pumps the event loop from inside a reactive", {
  # httpuv::service() / later::run_now() run the libuv loop re-entrantly. Called
  # from inside a running reactive (as gis_task_send once did to flush a banner)
  # they execute Shiny's deferred flush/startCycle while .inFlush/busyCount are
  # non-zero, mis-sequencing the cycle: the active tab stops updating until a
  # fresh client event. Guard so this anti-pattern cannot come back.
  app <- system.file("shiny", "DroneBiomassStudio", "app.R",
                     package = "DroneBioR")
  skip_if(!nzchar(app) || !file.exists(app), "app.R not installed")

  banned <- character()
  # AST walk so comments mentioning httpuv::service() do not trip the guard.
  for (e in parse(app)) .guard_walk(e, character(), function(nm, ee, ctx) {
    fn <- if (is.call(ee) && is.call(ee[[1L]]) && length(ee[[1L]]) >= 3L)
            as.character(ee[[1L]][[3L]])          # pkg::fn -> "fn"
          else nm
    if (fn %in% c("service", "run_now")) {
      banned <<- c(banned, paste(deparse(ee), collapse = " "))
    }
  })
  expect_equal(banned, character())
})

test_that("leaflet maps are invalidated after content updates, not only on tab change", {
  # A leaflet map only re-measures its container on a resize event. When
  # overlays are added via leafletProxy while the map's tab was hidden or the
  # browser deferred layout, they stay invisible until invalidateSize() fires.
  # That nudge used to fire only from observeEvent(input$main_nav), so the user
  # had to switch tabs for freshly loaded GIS / biomass data to appear. It must
  # also fire from the render paths themselves (GIS overlays, field map).
  app <- system.file("shiny", "DroneBiomassStudio", "app.R",
                     package = "DroneBioR")
  skip_if(!nzchar(app) || !file.exists(app), "app.R not installed")
  src <- paste(readLines(app, warn = FALSE), collapse = "\n")
  n <- length(gregexpr('sendCustomMessage\\("dronebior_invalidate_maps"',
                       src)[[1L]])
  expect_gte(n, 3L)
})

test_that("observer_need matches need() truthiness", {
  for (falsy in list(FALSE, NULL, NA, "", character(0), integer(0))) {
    expect_warning(observer_need(falsy, "missing", session = NULL), "missing")
  }
  for (truthy in list(TRUE, 1L, "x", c(TRUE, TRUE))) {
    expect_true(observer_need(truthy, "missing", session = NULL))
  }
})

test_that("observer_need warns instead of aborting outside a Shiny session", {
  expect_warning(res <- observer_need(FALSE, "needs a mosaic", session = NULL),
                 "needs a mosaic")
  expect_false(res)
})

test_that("observer_need treats an erroring condition as unmet", {
  expect_warning(observer_need(stop("boom"), "guard message", session = NULL),
                 "guard message")
})

test_that("observer_need toasts, then aborts, inside a Shiny session", {
  skip_if_not_installed("shiny")
  shown <- NULL
  local_mocked_bindings(
    showNotification = function(ui, ...) {
      shown <<- ui
      invisible("id")
    },
    .package = "shiny"
  )
  expect_error(
    observer_need(FALSE, "Extract field samples before training.",
                  session = structure(list(), class = "ShinySession")),
    class = "shiny.silent.error"
  )
  expect_equal(shown, "Extract field samples before training.")
})

# Walks a parsed expression, tracking which Shiny wrapper encloses each call.

test_that("no observer guards its inputs with validate()", {
  # validate() raises shiny.silent.error, which Shiny displays in a render
  # output and swallows everywhere else. In an observeEvent() that means the
  # button does nothing at all -- no toast, no console message. observer_need()
  # is the observer-safe equivalent; this pins the invariant.
  app <- system.file("shiny", "DroneBiomassStudio", "app.R",
                     package = "DroneBioR")
  skip_if(!nzchar(app) || !file.exists(app), "app.R not installed")

  offenders <- character()
  visit <- function(nm, e, ctx) {
    if (identical(nm, "validate") && length(ctx) &&
        ctx[[length(ctx)]] %in% c("observeEvent", "observe")) {
      offenders <<- c(offenders, paste(deparse(e), collapse = " "))
    }
  }
  for (e in parse(app)) .guard_walk(e, character(), visit)
  expect_equal(offenders, character())
})

test_that("the point-cloud reconstruction controls are not duplicated", {
  # The outlier filter and ground-rectify used to exist twice: once on the
  # Point Cloud tab (pc_filter_stage0 / pc_rectify_stage0) and once on the
  # Processing Engine tab (pc_filter / pc_rectify), as independent inputs that
  # drifted apart. The Point Cloud tab is the single source of truth now.
  app <- system.file("shiny", "DroneBiomassStudio", "app.R",
                     package = "DroneBioR")
  skip_if(!nzchar(app) || !file.exists(app), "app.R not installed")
  src <- paste(readLines(app, warn = FALSE), collapse = "\n")

  # No widget defines the old duplicate ids.
  expect_false(grepl('(slider|checkbox|numeric|select)Input\\(\\s*"pc_filter"',
                     src))
  expect_false(grepl('(slider|checkbox|numeric|select)Input\\(\\s*"pc_rectify"',
                     src))
  # The _stage0 inputs still exist exactly once each.
  expect_equal(length(gregexpr('sliderInput\\("pc_filter_stage0"', src)[[1L]]), 1L)
  expect_equal(length(gregexpr('checkboxInput\\("pc_rectify_stage0"', src)[[1L]]), 1L)
  # Nothing reads the removed ids any more (word boundary excludes _stage0).
  expect_false(grepl("input\\$pc_filter\\b(?!_)", src, perl = TRUE))
  expect_false(grepl("input\\$pc_rectify\\b(?!_)", src, perl = TRUE))
})

test_that("literal sprintf formats match their argument count", {
  # Splitting a long format across string literals and forgetting paste0()
  # leaves sprintf() with the first fragment as the format and the rest as
  # arguments: the message comes out truncated and mangled, plus a runtime
  # warning. This shipped twice before being caught by a user's console.
  spec_re <- "%[-+ #0-9.*]*[diouxXeEfgGaAsp]"
  bad <- character()

  # The fix for this bug wraps the fragments in paste0(), so fold a literal
  # paste0() / paste() back into one string -- otherwise the guard would stop
  # covering exactly the code shape it exists to protect.
  literal_fmt <- function(x) {
    if (is.character(x) && length(x) == 1L) return(x)
    if (!is.call(x)) return(NULL)
    fn <- if (is.name(x[[1L]])) as.character(x[[1L]]) else ""
    if (!fn %in% c("paste0", "paste")) return(NULL)
    parts <- as.list(x)[-1L]
    named <- names(parts)
    if (!is.null(named) && any(nzchar(named))) return(NULL)  # sep=/collapse=
    if (!all(vapply(parts, function(p) is.character(p) && length(p) == 1L,
                    logical(1)))) {
      return(NULL)
    }
    sep <- if (identical(fn, "paste")) " " else ""
    paste(unlist(parts), collapse = sep)
  }

  check_call <- function(nm, e, ctx) {
    if (!identical(nm, "sprintf")) return(invisible())
    if (length(e) < 2L) return(invisible())
    fmt <- literal_fmt(e[[2L]])
    if (is.null(fmt)) return(invisible())
    stripped <- gsub("%%", "", fmt, fixed = TRUE)
    hits <- gregexpr(spec_re, stripped)[[1L]]
    n_spec <- if (hits[[1L]] == -1L) 0L else length(hits)
    n_arg <- length(as.list(e)) - 2L
    if (n_spec != n_arg) {
      bad <<- c(bad, sprintf("%d specs / %d args: %s", n_spec, n_arg,
                             substr(fmt, 1L, 60L)))
    }
  }

  # Package code, read from the namespace so this works installed or loaded.
  ns <- asNamespace("DroneBioR")
  for (obj in ls(ns, all.names = TRUE)) {
    f <- get(obj, envir = ns)
    if (!is.function(f)) next
    b <- body(f)
    if (is.null(b)) next
    .guard_walk(b, character(), check_call)
  }

  # The Shiny app is not part of the namespace, so parse it separately.
  app <- system.file("shiny", "DroneBiomassStudio", "app.R",
                     package = "DroneBioR")
  if (nzchar(app) && file.exists(app)) {
    for (e in parse(app)) .guard_walk(e, character(), check_call)
  }

  expect_equal(bad, character())
})
