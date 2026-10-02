test_that("weight offers your data and two pre-fit models, defaulting to coastwide", {
  expect_identical(components$weight$sources, c("user", "prefit_coastwide", "prefit_hakai"))
  expect_identical(default_source("weight", has_sheet = TRUE), "prefit_coastwide")
  expect_identical(
    unname(source_choices("weight", has_sheet = TRUE)),
    c("user", "prefit_coastwide", "prefit_hakai")
  )
  expect_identical(
    names(source_choices("weight", has_sheet = FALSE)),
    c("Your data (no sheet)", "Pre-fit coastwide", "Pre-fit Hakai Institute")
  )
})

test_that("size, wet:dry and carbon offer the Hakai Institute pre-fit model only", {
  for (id in c("size", "wetdry", "carbon")) {
    sources <- components[[id]]$sources
    expect_identical(sources[is_prefit(sources)], "prefit_hakai", info = id)
  }
  expect_identical(default_source("size", has_sheet = FALSE), "prefit_hakai")
  expect_identical(default_source("size", has_sheet = TRUE), "user")
  expect_false(any(is_prefit(components$density$sources)))
})

test_that("every pre-fit source names a reference with a label", {
  sources <- unique(unlist(lapply(components, `[[`, "sources")))
  prefit <- sources[is_prefit(sources)]
  expect_setequal(prefit_reference(prefit), names(prefit_info))
  expect_identical(prefit_source(prefit_reference(prefit)), prefit)
})

test_that("source labels and notes name the pre-fit reference", {
  expect_identical(source_label("prefit_coastwide"), "Pre-fit coastwide")
  expect_identical(source_label("prefit_hakai"), "Pre-fit Hakai Institute")
  expect_identical(source_label("user"), "Your data")
  expect_identical(source_label("none"), "Not used")
  expect_identical(source_note("prefit_hakai"), "pre-fit Hakai Institute")
  expect_identical(
    source_labels(c(size = "prefit_hakai", weight = "prefit_coastwide")),
    c(size = "Pre-fit Hakai Institute", weight = "Pre-fit coastwide")
  )
})
