test_that("workflow: the example workbook loads and the models step shows the statuses", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- create_workflow_app("workflow")
  withr::defer(app$stop())

  app$click("data-example")
  wait_for_data(app)

  app$set_inputs(step = "models")
  app$wait_for_idle()

  expect_identical(app$get_value(input = "step"), "models")
  expect_match(app$get_text("#models-status_density"), "Not fitted")
  expect_match(app$get_text("#models-status_weight"), "Ready")
})
