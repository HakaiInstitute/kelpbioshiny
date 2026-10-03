# testthat runs the tests with the package namespace as their parent
# environment, so the app's internal functions are available here without `:::`.

# A testServer() app function: the session store, run by a test_runner(),
# optionally with the example workbook loaded, and the module under test.
# `runner` and `store` are available in the testServer() expression.
store_app <- function(module = NULL, id = NULL, load_example = TRUE) {
  function(input, output, session) {
    runner <- test_runner()
    store <- new_store(session, run_fit = function() runner)
    if (load_example) store$load_example()
    if (!is.null(module)) module(id, store)
  }
}
