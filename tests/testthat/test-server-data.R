test_that("the example workbook loads its sheets and runs the data checks", {
  shiny::testServer(store_app(mod_data_server, "data", load_example = FALSE), {
    expect_length(store$sheets(), 0)
    expect_false(store$has_density())

    session$setInputs(`data-example` = 1)

    sheets <- store$sheets()
    expect_named(sheets, c("density", "size", "weight", "cover"))
    expect_identical(store$workbook(), "example-nereo.xlsx")
    expect_true(store$has_density())
    expect_null(sheets$density$error)
    expect_identical(sheets$weight$error, "Column `year` of `weight` must not have any missing values.")
    expect_match(sheets$cover$note, "5 site-years have canopy area but no plot cover", fixed = TRUE)
    expect_identical(store$sources()[["density"]], "user")
    expect_identical(store$sources()[["weight"]], "prefit_coastwide")
  })
})

test_that("clearing the data removes the sheets and brings back the welcome card", {
  shiny::testServer(store_app(mod_data_server, "data"), {
    welcome <- function() html_of(output[["data-welcome"]])
    expect_no_match(welcome(), "Welcome to kelpbio", fixed = TRUE)
    session$setInputs(`data-clear` = 1)
    expect_length(store$sheets(), 0)
    expect_null(store$workbook())
    expect_match(welcome(), "Welcome to kelpbio", fixed = TRUE)
  })
})

test_that("the issues summary counts what the checks and coverage flag", {
  shiny::testServer(store_app(mod_data_server, "data"), {
    text <- gsub("\\s+", " ", gsub("<[^>]+>", " ", html_of(output[["data-sheets"]])))
    expect_match(text, "4 sheets recognised · 1 sheet error · 2 site-years without size data · 5 site-years without plot cover", fixed = TRUE)
  })
})

test_that("a site name mismatch is flagged with the rename advice", {
  shiny::testServer(store_app(mod_data_server, "data"), {
    sheets <- store$sheets()
    sheets$density$rows$site[sheets$density$rows$site == "site1"] <- "Site 1"
    store$sheets(sheets)
    html <- html_of(output[["data-sheets"]])
    expect_match(html, "Site names differ across sheets", fixed = TRUE)
    expect_match(html, mismatch_advice, fixed = TRUE)
    expect_true(all(coverage(store$sheets(), store$sources())$rows$issue[coverage(store$sheets(), store$sources())$rows$site %in% c("Site 1", "site1")] == "mismatch"))
  })
})

test_that("a clean workbook gets a single success line", {
  sheets <- list(density = list(), size = list())
  rows <- data.frame(issue = NA_character_, no_size = FALSE, no_weight = FALSE, no_plot = FALSE)
  text <- as.character(issues_summary(shiny::NS("data"), sheets, rows))
  expect_match(text, "All sheets passed their checks", fixed = TRUE)
  expect_match(text, "2 sheets recognised", fixed = TRUE)
})

test_that("the CSV rows list the columns for the chosen species", {
  shiny::testServer(store_app(mod_data_server, "data"), {
    expect_identical(output[["data-csv_columns_density"]], "site, year, stipes, area_m2")
    session$setInputs(`data-species` = "macro")
    expect_identical(output[["data-csv_columns_density"]], "site, year, plants, area_m2")
    expect_identical(output[["data-csv_columns_wetdry"]], "wet_mass_g, dry_mass_g")
  })
})
