test_that("Fit all fits the models in order and becomes Cancel while fitting", {
  local_stub_fits()
  shiny::testServer(store_app(mod_models_server, "models"), {
    session$flushReact()
    expect_match(html_of(output[["models-fit_all_ui"]]), "Fit all", fixed = TRUE)
    session$setInputs(`models-fit_all` = 1)
    expect_identical(store$fitting(), "density")
    expect_match(html_of(output[["models-fit_all_ui"]]), "models-cancel", fixed = TRUE)
    expect_match(html_of(output[["models-status_size"]]), "Queued", fixed = TRUE)

    finish_fits(session, runner)
    expect_true(store$biomass_ready())
    expect_true(store$totals()$available)
    expect_match(html_of(output[["models-status_density"]]), "Ready", fixed = TRUE)
    expect_match(html_of(output[["models-fit_all_ui"]]), "All models are fitted", fixed = TRUE)
  })
})

test_that("cancelling from the hub clears the queue", {
  local_stub_fits()
  shiny::testServer(store_app(mod_models_server, "models"), {
    session$setInputs(`models-fit_all` = 1)
    session$setInputs(`models-cancel` = 1)
    expect_null(store$fitting())
    expect_false(any(record_status(store$records()) %in% c("queued", "fitting")))
  })
})

test_that("a failed model shows its message and a Refit link in the hub", {
  log <- local_stub_fits(fail = "size")
  shiny::testServer(store_app(mod_models_server, "models"), {
    session$setInputs(`models-fit_all` = 1)
    finish_fits(session, runner)
    html <- html_of(output[["models-status_size"]])
    expect_match(html, "Failed", fixed = TRUE)
    expect_match(html, "Stub size fit failed.", fixed = TRUE)
    expect_match(html, "models-refit_size", fixed = TRUE)
  })
})

test_that("Fit all names the models it skips for invalid settings", {
  shiny::testServer(store_app(mod_models_server, "models"), {
    store$update_sampler("size", "nthin", 0)
    expect_match(html_of(output[["models-skipped"]]), "Size: a prior or sampler setting is invalid", fixed = TRUE)
  })
})
