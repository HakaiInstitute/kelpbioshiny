test_that("help: the Help tab opens the user guide and about pills", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- create_workflow_app("help")
  withr::defer(app$stop())

  app$set_inputs(step = "help")
  app$wait_for_idle()
  expect_identical(app$get_value(input = "help_page"), "guide")
  expect_match(app$get_text(".tab-pane.active[data-value='guide']"), "User guide")
  expect_match(app$get_text(".tab-pane.active[data-value='guide']"), "kelpbio documentation")

  app$set_inputs(help_page = "about")
  app$wait_for_idle()
  about <- app$get_text(".tab-pane.active[data-value='about']")
  expect_match(about, "Tula Foundation")
  expect_match(about, "kelpbioshiny: Shiny App for Bayesian Kelp Biomass", fixed = TRUE)
  expect_match(about, "kelpbio: Bayesian Kelp Biomass Estimation", fixed = TRUE)
})

test_that("help: the welcome card's guide link opens the user guide", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- create_workflow_app("help-link")
  withr::defer(app$stop())

  expect_true(app$get_js("!!document.getElementById('data-welcome_card')"))
  expect_false(app$get_js("document.getElementById('data-sheets').innerText.includes('No data loaded')"))
  # The link opens the guide even when About was the last Help page shown.
  app$set_inputs(help_page = "about", wait_ = FALSE)
  app$set_inputs(step = "data", wait_ = FALSE)
  app$click("data-guide")
  app$wait_for_idle()
  expect_identical(app$get_value(input = "step"), "help")
  expect_identical(app$get_value(input = "help_page"), "guide")
})
