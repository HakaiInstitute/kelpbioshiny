# Module test: shiny::testServer() on a step's server function, driven by
# session$setInputs(), reading the store directly.

test_that("Use example workbook loads the example sheets", {
  shiny::testServer(store_app(mod_data_server, "data", load_example = FALSE), {
    expect_length(store$sheets(), 0)

    session$setInputs(`data-example` = 1)

    expect_named(store$sheets(), c("weight", "size", "survey", "canopy", "wetdry", "carbon", "nitrogen"))
    expect_identical(store$workbook(), "kelpbio-1-full-nereo.xlsx")
    expect_identical(store$workbook_url(), example_url("nereo"))
  })
})

test_that("an invalid value fails its sheet's check", {
  rows <- example_rows("nereo")
  rows$weight$weight_kg[1] <- -1
  sheets <- check_sheets(rows, lapply(rows, function(x) "example.xlsx"), "nereo")

  expect_match(sheets$weight$error, "weight_kg")
  expect_identical(sheet_state(sheets$weight, "user"), "error")
  expect_identical(sheet_state(sheets$size, "user"), "ready")
})
