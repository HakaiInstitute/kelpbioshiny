html_of <- function(value) paste(as.character(value$html), collapse = "")

# Fits the size model (no upstream models) with the example workbook loaded.
fit_size <- function(session, store) {
  store$queue_fits("size")
  session$flushReact()
  session$elapse(fit_ms("size") + 2 * TICK_MS)
}

test_that("predictions show the grouping control above Plot and Estimates pills", {
  shiny::testServer(store_app(mod_model_server, "size"), {
    fit_size(session, store)
    html <- html_of(output[["size-tab_predictions"]])
    expect_match(html, "size-grouping", fixed = TRUE)
    expect_match(html, "size-prediction_section", fixed = TRUE)
    expect_match(html, "nav-pills", fixed = TRUE)
    expect_match(html, ">Plot<", fixed = TRUE)
    expect_match(html, ">Estimates<", fixed = TRUE)
    expect_lt(regexpr("size-grouping", html), regexpr("size-prediction_section", html))
    expect_match(html, "size-predictions_table", fixed = TRUE)
  })
})

test_that("pre-fit models show predictions in the Plot and Estimates pills", {
  shiny::testServer(store_app(mod_model_server, "weight"), {
    session$flushReact()
    html <- html_of(output[["weight-tab_predictions"]])
    expect_match(html, "weight-prediction_section", fixed = TRUE)
    expect_match(html, ">Estimates<", fixed = TRUE)
  })
})

test_that("the size model header shows both warnings with dismiss buttons", {
  shiny::testServer(store_app(mod_model_server, "size"), {
    fit_size(session, store)
    html <- html_of(output[["size-notices"]])
    expect_match(html, "Convergence warning", fixed = TRUE)
    expect_match(html, "Prior sensitivity warning", fixed = TRUE)
    expect_match(html, "If this is unintended, try making the prior less informative.", fixed = TRUE)
    expect_match(html, "try reducing the rate (for example from 1 to 0.5)", fixed = TRUE)
    expect_match(html, "size-dismiss_convergence", fixed = TRUE)
    expect_match(html, "size-dismiss_sensitivity", fixed = TRUE)
    expect_no_match(html, "widen", fixed = TRUE)
  })
})

test_that("dismissing hides a warning for the current fit and a refit resets it", {
  shiny::testServer(store_app(mod_model_server, "size"), {
    fit_size(session, store)

    session$setInputs(`size-dismiss_convergence` = 1)
    expect_true(store$is_dismissed("size", "convergence"))
    html <- html_of(output[["size-notices"]])
    expect_no_match(html, "Convergence warning", fixed = TRUE)
    expect_match(html, "Prior sensitivity warning", fixed = TRUE)
    expect_match(html_of(output[["size-status"]]), "Fitted, warning dismissed", fixed = TRUE)

    session$setInputs(`size-dismiss_sensitivity` = 1)
    expect_null(output[["size-notices"]])
    sensitivity_html <- html_of(output[["size-sensitivity_notices"]])
    expect_no_match(sensitivity_html, "influencing", fixed = TRUE)

    session$setInputs(`size-fit` = 1)
    expect_false(store$is_dismissed("size", "convergence"))
    session$flushReact()
    session$elapse(fit_ms("size") + 2 * TICK_MS)
    expect_identical(store$fit_status()[["size"]], "fitted")
    html <- html_of(output[["size-notices"]])
    expect_match(html, "Convergence warning", fixed = TRUE)
    expect_match(html, "Prior sensitivity warning", fixed = TRUE)
    expect_match(html_of(output[["size-status"]]), "Fitted, convergence warning", fixed = TRUE)
  })
})

test_that("changing the source resets the dismissed warnings", {
  shiny::testServer(store_app(mod_model_server, "size"), {
    fit_size(session, store)
    store$dismiss("size", "convergence")
    store$set_source("size", "none")
    expect_length(store$dismissed(), 0)
  })
})

test_that("the hub convergence notice is hidden once dismissed", {
  shiny::testServer(store_app(mod_models_server, "models"), {
    store$queue_fits("size")
    session$flushReact()
    session$elapse(fit_ms("size") + 2 * TICK_MS)
    expect_match(html_of(output[["models-warnings"]]), "models-dismiss_size", fixed = TRUE)
    session$setInputs(`models-dismiss_size` = 1)
    expect_null(output[["models-warnings"]])
  })
})

test_that("tabs and diagnostics pills are marked while a warning is active", {
  shiny::testServer(store_app(mod_model_server, "size"), {
    marked <- function(id) grepl("Has warnings", html_of(output[[paste0("size-", id)]]), fixed = TRUE)
    expect_false(marked("mark_diagnostics"))

    fit_size(session, store)
    expect_true(marked("mark_diagnostics"))
    expect_true(marked("mark_settings"))
    expect_true(marked("mark_pill_sampler"))
    expect_true(marked("mark_pill_sensitivity"))
    expect_match(html_of(output[["size-tab_diagnostics"]]), "size-mark_pill_sampler", fixed = TRUE)

    session$setInputs(`size-dismiss_convergence` = 1)
    expect_false(marked("mark_pill_sampler"))
    expect_true(marked("mark_diagnostics"))

    session$setInputs(`size-dismiss_sensitivity` = 1)
    expect_false(marked("mark_diagnostics"))
    expect_false(marked("mark_settings"))
    expect_false(marked("mark_pill_sensitivity"))
  })
})
