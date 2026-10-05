# Module test: shiny::testServer() on a step's server function, driven by
# session$setInputs(), reading the store directly.

test_that("Use example workbook loads the example sheets", {
  shiny::testServer(store_app(mod_data_server, "data", load_example = FALSE), {
    expect_length(store$sheets(), 0)

    session$setInputs(`data-example_density_size` = 1)

    expect_named(store$sheets(), c("density", "size"))
    expect_identical(store$workbook(), "example-density-size-nereo.xlsx")
  })
})
