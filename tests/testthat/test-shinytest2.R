# Browser test: shinytest2 drives the installed app (system.file("app")) in a
# headless browser, so install the package first. Never click Fit: with kelpbio
# in place of the mocks it would run real MCMC.

test_that("the app boots, loads the example and shows the Models step without JavaScript errors", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- shinytest2::AppDriver$new(
    app_dir = system.file("app", package = "kelpbioshiny"),
    name = "smoke",
    height = 1080,
    width = 1920,
    timeout = 60000
  )
  withr::defer(app$stop())

  app$click("data-example")
  app$wait_for_idle()
  expect_true(app$get_js("!!document.querySelector('#data-sheets .list-unstyled')"))

  app$set_inputs(step = "models")
  app$wait_for_idle()
  expect_match(app$get_text("#models-status_density"), "Not fitted")

  logs <- app$get_logs()
  js_errors <- logs$message[logs$location == "chromote" & logs$level %in% c("error", "throw")]
  expect_identical(js_errors, character())
})
