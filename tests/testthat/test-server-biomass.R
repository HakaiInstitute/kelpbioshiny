test_that("biomass shows the carried-through warning above Plot and Estimates pills", {
  local_stub_fits()
  shiny::testServer(store_app(mod_biomass_server, "biomass"), {
    store$queue_fits(c("density", "size"))
    finish_fits(session, runner)
    expect_true(store$biomass_ready())

    html <- html_of(output[["biomass-main"]])
    expect_match(html, "biomass-view", fixed = TRUE)
    expect_match(html, ">Plot<", fixed = TRUE)
    expect_match(html, ">Estimates<", fixed = TRUE)
    expect_match(html, "The size model has a convergence warning", fixed = TRUE)
    expect_lt(regexpr("convergence warning", html), regexpr("biomass-view", html))
  })
})

test_that("a site name mismatch locks biomass with the rename advice", {
  local_stub_fits()
  shiny::testServer(store_app(mod_biomass_server, "biomass"), {
    store$queue_fits(c("density", "size"))
    finish_fits(session, runner)
    sheets <- store$sheets()
    sheets$size$rows$site[sheets$size$rows$site == "site2"] <- "Site_2"
    store$sheets(sheets)
    session$flushReact()
    html <- html_of(output[["biomass-main"]])
    expect_match(html, "Biomass is locked", fixed = TRUE)
    expect_match(html, mismatch_advice, fixed = TRUE)
  })
})

test_that("the estimates flags name each pre-fit model's reference", {
  rows <- data.frame(site = "site1", year = "2020", estimate = 1, lower = 0.5, upper = 2, population_size = FALSE, population_weight = TRUE)
  sources <- default_sources(list())
  sources[["weight"]] <- "prefit_hakai"
  flags <- biomass_table(rows, sources, "wet")$flags
  expect_identical(flags, "Population-level weight|Size (pre-fit Hakai Institute)|Weight (pre-fit Hakai Institute)")

  sources[["weight"]] <- "prefit_coastwide"
  expect_match(biomass_table(rows, sources, "wet")$flags, "Weight (pre-fit coastwide)", fixed = TRUE)
})
