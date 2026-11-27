# Module test of each model's page once fitted: its parameter estimates,
# convergence, prior sensitivity and description are drawn from the fit.

test_that("each fitted model's page draws its estimates and diagnostics", {
  local_stub_fits()
  shiny::testServer(store_app(mod_models_server, "models"), {
    store$fit_all()
    finish_fits(session, runner)

    ready <- names(Filter(function(status) status$kind == "ready", store$statuses()))
    expect_length(ready, 6)
    for (cid in ready) {
      for (name in c("parameters", "convergence", "convergence_overall", "verdict", "sensitivity", "tab_description")) {
        value <- output[[sprintf("models-%s-%s", cid, name)]]
        # renderUI() gives list(html, deps); the tables, their JSON.
        if (is.list(value)) value <- value$html
        expect_true(nzchar(value), label = paste(cid, name))
      }
    }
  })
})
