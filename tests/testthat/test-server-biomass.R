test_that("biomass shows the carried-through warning above Plot and Estimates pills", {
  shiny::testServer(store_app(mod_biomass_server, "biomass"), {
    store$queue_fits(c("density", "size"))
    session$flushReact()
    session$elapse(fit_ms("density") + 2 * TICK_MS)
    session$elapse(fit_ms("size") + 2 * TICK_MS)
    expect_true(store$biomass_ready())

    html <- as.character(output[["biomass-main"]]$html)
    expect_match(html, "biomass-view", fixed = TRUE)
    expect_match(html, ">Plot<", fixed = TRUE)
    expect_match(html, ">Estimates<", fixed = TRUE)
    expect_match(html, "The size model has a convergence warning", fixed = TRUE)
    expect_lt(regexpr("convergence warning", html), regexpr("biomass-view", html))

    session$setInputs(`biomass-dismiss_size` = 1)
    html <- as.character(output[["biomass-main"]]$html)
    expect_no_match(html, "convergence warning", fixed = TRUE)
    expect_match(html, "biomass-view", fixed = TRUE)
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
