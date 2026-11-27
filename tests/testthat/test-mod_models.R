test_that("only the source select in the open view sets a model's source", {
  shiny::testServer(store_app(mod_models_server, "models"), {
    session$setInputs(`models-source_weight` = "user", `models-weight-source` = "user")
    session$setInputs(`models-source_weight` = "prefit_hakai")
    expect_identical(store$sources()[["weight"]], "prefit_hakai")

    # A value reported by the hidden page select, such as the echo of an
    # earlier update, is not a choice.
    session$setInputs(`models-weight-source` = "user")
    expect_identical(store$sources()[["weight"]], "prefit_hakai")

    store$open("weight")
    session$setInputs(`models-weight-source` = "combined_hakai")
    expect_identical(store$sources()[["weight"]], "combined_hakai")
    session$setInputs(`models-source_weight` = "user")
    expect_identical(store$sources()[["weight"]], "combined_hakai")
  })
})
