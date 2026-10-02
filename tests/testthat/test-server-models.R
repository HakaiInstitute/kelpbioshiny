test_that("Fit all queues the fits in dependency order and they complete", {
  shiny::testServer(store_app(mod_models_server, "models"), {
    session$flushReact()
    expect_identical(unname(store$fittable()), c("density", "size", "cover"))

    session$setInputs(`models-fit_all` = 1)
    expect_identical(store$fitting(), "density")
    expect_identical(unname(store$fit_status()[c("size", "cover")]), c("queued", "queued"))
    expect_identical(store$statuses()$cover$kind, "waiting")

    session$elapse(fit_ms("density") + 2 * TICK_MS)
    expect_identical(store$fit_status()[["density"]], "fitted")
    expect_identical(store$fitting(), "size")

    session$elapse(fit_ms("size") + 2 * TICK_MS)
    expect_identical(store$fitting(), "cover")
    expect_true(store$biomass_ready())

    session$elapse(fit_ms("cover") + 2 * TICK_MS)
    expect_null(store$fitting())
    expect_identical(unname(store$fit_status()[c("density", "size", "cover")]), rep("fitted", 3))
    expect_true(store$totals()$available)
    expect_s3_class(store$fit_of("density"), c("kb_fit_density_nereo", "kb_fit_density", "kb_fit"))
  })
})

test_that("cancelling stops the running fit and clears the queue", {
  shiny::testServer(store_app(mod_models_server, "models"), {
    session$setInputs(`models-fit_all` = 1)
    session$setInputs(`models-cancel` = 1)
    expect_null(store$fitting())
    expect_length(store$queue(), 0)
    expect_false(any(store$fit_status() %in% c("queued", "fitting")))
  })
})
