# Two test types for R/state.R:
# - a unit test of a pure function (the status rule), on synthetic input;
# - a store test: shiny::testServer() on new_store() with the kb_fit_*() calls
#   stubbed (local_stub_fits()) and the fits run in the session (test_runner()).

test_that("a model fitted to your data with no sheet needs data", {
  status <- component_status("user", sheet = NULL, record = new_record())
  expect_identical(status$kind, "no-data")
  expect_match(as.character(status_badge(status)), "Needs data", fixed = TRUE)
})

test_that("Fit all fits each model with its sheet data and sampler settings", {
  log <- local_stub_fits()
  shiny::testServer(store_app(), {
    store$update_sampler("size", "nthin", 2)
    store$fit_all()
    finish_fits(session, runner)

    expect_identical(fitted_models(log), c("density", "size", "biomass_cover"))
    size <- log$calls[[2]]$args
    expect_identical(size$data, store$sheets()$size$rows)
    expect_identical(size$nthin, 2)
    expect_identical(store$statuses()$size$kind, "ready")
  })
})
