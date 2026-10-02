test_that("the example workbook loads its sheets and passes the data checks", {
  shiny::testServer(store_app(mod_data_server, "data", load_example = FALSE), {
    expect_length(store$sheets(), 0)
    expect_false(store$has_density())

    session$setInputs(`data-example` = 1)

    sheets <- store$sheets()
    expect_named(sheets, example_workbook_sheets)
    expect_identical(store$workbook(), example_workbook)
    expect_true(store$has_density())
    levels <- vapply(sheets, function(sheet) sheet$validation$level, character(1))
    expect_false(any(levels == "error"))
    expect_identical(sheets$density$validation, list(level = "ok", message = "All checks passed"))
    expect_identical(
      sheets$weight$validation,
      list(level = "warning", message = "2 rows with missing year will be dropped")
    )
    expect_identical(store$sources()[["density"]], "user")
    expect_identical(store$sources()[["weight"]], "prefit")
  })
})

test_that("clearing the data removes the sheets", {
  shiny::testServer(store_app(mod_data_server, "data"), {
    session$setInputs(`data-clear` = 1)
    expect_length(store$sheets(), 0)
    expect_null(store$workbook())
  })
})

test_that("coverage asks for a review of site names and proposes no rename", {
  shiny::testServer(store_app(mod_data_server, "data"), {
    html <- as.character(output[["data-sheets"]]$html)
    expect_match(html, "Check site names and years across sheets", fixed = TRUE)
    expect_match(html, "capitalisation, spacing or spelling", fixed = TRUE)
    expect_no_match(html, "Rename", fixed = TRUE)
    expect_no_match(html, "data-rename", fixed = TRUE)
    expect_null(store$rename_site)

    notes <- coverage(store$sheets(), store$sources())$rows$note
    expect_true(any(grepl("Site name differs across sheets", notes, fixed = TRUE)))
  })
})

test_that("the issues summary counts what the checks and coverage flag", {
  shiny::testServer(store_app(mod_data_server, "data"), {
    html <- as.character(output[["data-sheets"]]$html)
    text <- gsub("\\s+", " ", gsub("<[^>]+>", " ", html))
    expect_match(text, "4 sheets recognised · 1 sheet warning · 2 site-years with site names that differ across sheets", fixed = TRUE)
    expect_match(text, "2 site-years without size data · 5 site-years without plot cover", fixed = TRUE)
    expect_match(html, "href=\"#data-coverage_card\"", fixed = TRUE)
    expect_match(html, "id=\"data-coverage_card\"", fixed = TRUE)
    expect_match(html, "id=\"data-sheets_card\"", fixed = TRUE)
  })
})

test_that("a clean workbook gets a single success line", {
  sheets <- list(density = list(validation = list(level = "ok")), size = list(validation = list(level = "ok")))
  rows <- data.frame(issue = NA_character_, no_size = FALSE, no_weight = FALSE, no_plot = FALSE)
  text <- as.character(issues_summary(shiny::NS("data"), sheets, rows))
  expect_match(text, "All sheets passed their checks", fixed = TRUE)
  expect_match(text, "2 sheets recognised", fixed = TRUE)
  expect_no_match(text, "without", fixed = TRUE)
})

test_that("the welcome card shows until data are loaded and returns after clearing", {
  shiny::testServer(store_app(mod_data_server, "data", load_example = FALSE), {
    welcome <- function() as.character(output[["data-welcome"]]$html)
    expect_match(welcome(), "Welcome to kelpbio", fixed = TRUE)
    expect_match(welcome(), "What you need", fixed = TRUE)
    expect_match(welcome(), app_purpose, fixed = TRUE)
    expect_match(welcome(), "id=\"data-guide\"", fixed = TRUE)
    expect_match(welcome(), "id=\"data-welcome_example\"", fixed = TRUE)
    expect_no_match(welcome(), "Download template workbook", fixed = TRUE)
    expect_null(output[["data-sheets"]])

    session$setInputs(`data-welcome_example` = 1)
    expect_true(store$has_density())
    expect_no_match(welcome(), "Welcome to kelpbio", fixed = TRUE)

    session$setInputs(`data-clear` = 1)
    expect_match(welcome(), "Welcome to kelpbio", fixed = TRUE)

    session$setInputs(`data-example` = 1)
    expect_no_match(welcome(), "Welcome to kelpbio", fixed = TRUE)
  })
})

test_that("closing the welcome card hides it for the session, even after clearing", {
  shiny::testServer(store_app(mod_data_server, "data", load_example = FALSE), {
    welcome <- function() as.character(output[["data-welcome"]]$html)
    expect_match(welcome(), "id=\"data-welcome_close\"", fixed = TRUE)

    session$setInputs(`data-welcome_close` = 1)
    expect_no_match(welcome(), "Welcome to kelpbio", fixed = TRUE)

    session$setInputs(`data-example` = 1)
    session$setInputs(`data-clear` = 1)
    expect_no_match(welcome(), "Welcome to kelpbio", fixed = TRUE)
  })
})
