fit_size <- function(session, store, runner) {
  store$queue_fits("size")
  finish_fits(session, runner)
}

test_that("predictions show the grouping control above Plot and Estimates pills", {
  local_stub_fits()
  shiny::testServer(store_app(mod_model_server, "size"), {
    fit_size(session, store, runner)
    html <- html_of(output[["size-tab_predictions"]])
    expect_match(html, "size-grouping", fixed = TRUE)
    expect_match(html, ">Plot<", fixed = TRUE)
    expect_match(html, ">Estimates<", fixed = TRUE)
    expect_lt(regexpr("size-grouping", html), regexpr("size-prediction_section", html))
  })
})

test_that("the size model header shows one notice per warning", {
  local_stub_fits()
  shiny::testServer(store_app(mod_model_server, "size"), {
    fit_size(session, store, runner)
    html <- html_of(output[["size-notices"]])
    expect_match(html, "Convergence warning", fixed = TRUE)
    expect_match(html, "Prior sensitivity warning", fixed = TRUE)
    expect_match(html, "try reducing the rate (for example from 1 to 0.5)", fixed = TRUE)
    expect_match(html_of(output[["size-status"]]), "Ready, convergence warning", fixed = TRUE)
    # The diagnostics tab holds its three sections, without a second prior warning.
    diagnostics <- html_of(output[["size-tab_diagnostics"]])
    for (section in c("Convergence", "Posterior predictive check", "Prior sensitivity")) expect_match(diagnostics, section, fixed = TRUE)
    expect_no_match(html_of(output[["size-sensitivity_note"]]), "influencing", fixed = TRUE)
  })
})

test_that("a failed fit shows its message with a Refit action", {
  local_stub_fits(fail = "size")
  shiny::testServer(store_app(mod_model_server, "size"), {
    fit_size(session, store, runner)
    html <- html_of(output[["size-notices"]])
    expect_match(html, "The fit failed", fixed = TRUE)
    expect_match(html, "Stub size fit failed.", fixed = TRUE)
    expect_match(html_of(output[["size-status"]]), "Failed", fixed = TRUE)
    expect_match(html_of(output[["size-fit_action"]]), "Refit", fixed = TRUE)
  })
})

test_that("an invalid prior shows kelpbio's message and disables Fit", {
  shiny::testServer(store_app(mod_model_server, "size"), {
    session$flushReact()
    expect_no_match(html_of(output[["size-fit_action"]]), "disabled", fixed = TRUE)
    session$setInputs(`size-intercept_b` = 0)
    expect_identical(output[["size-error_intercept"]], "`sd` must be greater than 0, not 0.")
    expect_match(html_of(output[["size-fit_action"]]), "disabled", fixed = TRUE)
    session$setInputs(`size-sampler_chains` = 0)
    expect_identical(output[["size-sampler_error_chains"]], "`chains` must be greater than 0, not 0.")
  })
})

test_that("the weight page switches the pre-fit reference", {
  shiny::testServer(store_app(mod_model_server, "weight"), {
    session$flushReact()
    expect_identical(store$sources()[["weight"]], "prefit_coastwide")
    expect_match(html_of(output[["weight-notices"]]), prefit_info$coastwide$data, fixed = TRUE)
    expect_identical(store$fit_of("weight")$meta$reference, "coastwide")

    session$setInputs(`weight-source` = "prefit_hakai")
    expect_match(html_of(output[["weight-notices"]]), prefit_info$hakai$data, fixed = TRUE)
    expect_identical(store$fit_of("weight")$meta$reference, "hakai")
    expect_identical(store$statuses()$weight$kind, "ready")
    expect_match(html_of(output[["weight-tab_predictions"]]), "weight-prediction_section", fixed = TRUE)
  })
})

test_that("the weight page shows its data error once Your data is chosen", {
  shiny::testServer(store_app(mod_model_server, "weight"), {
    session$flushReact()
    session$setInputs(`weight-source` = "user")
    html <- html_of(output[["weight-notices"]])
    expect_match(html, "The data check failed", fixed = TRUE)
    expect_match(html, "must not have any missing values", fixed = TRUE)
    expect_null(output[["weight-fit_action"]])
  })
})

test_that("wet:dry predictions are at the population level only", {
  shiny::testServer(store_app(mod_model_server, "wetdry"), {
    session$flushReact()
    html <- html_of(output[["wetdry-tab_predictions"]])
    expect_no_match(html, "wetdry-grouping", fixed = TRUE)
    expect_match(html, "Population level", fixed = TRUE)
  })
})
