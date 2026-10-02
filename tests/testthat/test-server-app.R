test_that("the Help tab holds the user guide and about pills without a step marker", {
  html <- as.character(app_ui())
  expect_match(html, "data-value=\"help\"[^>]*>\\s*Help")
  expect_match(html, "id=\"help_page\"", fixed = TRUE)
  expect_match(html, "data-value=\"guide\"", fixed = TRUE)
  expect_match(html, "data-value=\"about\"", fixed = TRUE)
  markers <- regmatches(html, gregexpr("id=\"mark_[a-z]+\"", html))[[1]]
  expect_setequal(markers, c("id=\"mark_data\"", "id=\"mark_models\""))
})

test_that("the Data and Models markers show done once their step is", {
  local_stub_fits()
  shiny::testServer(app_server, {
    mark <- function(value) as.character(output[[paste0("mark_", value)]]$html)
    expect_match(mark("data"), "kb-step-todo", fixed = TRUE)
    session$setInputs(`data-example` = 1)
    expect_match(mark("data"), "complete", fixed = TRUE)
    expect_match(mark("models"), "kb-step-todo", fixed = TRUE)

    session$setInputs(step = "help", help_page = "about")
    expect_match(mark("data"), "complete", fixed = TRUE)
  })
})

test_that("step markers show the step number until the step is done", {
  expect_match(as.character(step_marker(2)), ">2</span>", fixed = TRUE)
  expect_match(as.character(step_marker(2, "done")), "aria-label=\"complete\"", fixed = TRUE)
  expect_match(as.character(step_marker(2, "warning")), "complete with warnings", fixed = TRUE)
})
