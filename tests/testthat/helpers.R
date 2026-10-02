# testthat runs the tests with the package namespace as their parent
# environment, so the app's internal functions are available here without `:::`.

# A testServer() app function: the session store, run by a test_runner(), with
# the example workbook loaded, and the module under test. `runner` and `store`
# are available in the testServer() expression.
store_app <- function(module = NULL, id = NULL, load_example = TRUE) {
  function(input, output, session) {
    runner <- test_runner()
    store <- new_store(session, run_fit = function() runner)
    if (load_example) store$load_example()
    if (!is.null(module)) module(id, store)
  }
}

# Launches the installed app in a headless browser.
create_workflow_app <- function(name) {
  shinytest2::AppDriver$new(
    app_dir = system.file("app", package = "kelpbioshiny"),
    variant = shinytest2::platform_variant(),
    name = name,
    height = 1080,
    width = 1920,
    wait = TRUE,
    timeout = 60000
  )
}

# Waits until the app is idle and the example sheets are loaded.
wait_for_data <- function(app, timeout = 10000) {
  app$wait_for_idle(timeout = timeout)
  has_sheets <- app$get_js("!!document.querySelector('#data-sheets .list-unstyled')")
  if (!isTRUE(has_sheets)) {
    stop("Data did not load within the timeout period.")
  }
  invisible(app)
}

html_of <- function(value) paste(as.character(value$html), collapse = "")
