test_that("export lists the warnings of the models in use", {
  local_stub_fits()
  shiny::testServer(store_app(mod_export_server, "export"), {
    store$queue_fits(c("density", "size"))
    finish_fits(session, runner)
    html <- html_of(output[["export-warnings"]])
    expect_match(html, "Size model: convergence warning", fixed = TRUE)
    expect_match(html, "Size model: prior sensitivity warning", fixed = TRUE)
    expect_no_match(html, "Density model", fixed = TRUE)
  })
})
