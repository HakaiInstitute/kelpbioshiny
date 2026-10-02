test_that("the Help tab holds the user guide and about pills without a step marker", {
  html <- as.character(app_ui())
  expect_match(html, "data-value=\"help\"[^>]*>\\s*Help")
  expect_no_match(html, "dropdown-toggle", fixed = TRUE)
  expect_match(html, "id=\"help_page\"", fixed = TRUE)
  expect_match(html, "nav-pills", fixed = TRUE)
  expect_match(html, "data-value=\"guide\"", fixed = TRUE)
  expect_match(html, "data-value=\"about\"", fixed = TRUE)
  markers <- regmatches(html, gregexpr("id=\"mark_[a-z]+\"", html))[[1]]
  expect_setequal(markers, paste0("id=\"mark_", names(steps), "\""))
})

test_that("opening the Help pages leaves the step markers unchanged", {
  shiny::testServer(app_server, {
    mark <- function(value) as.character(output[[paste0("mark_", value)]]$html)
    session$setInputs(`data-example` = 1)
    expect_match(mark("data"), "complete", fixed = TRUE)
    before <- lapply(names(steps), mark)

    session$setInputs(step = "help", help_page = "guide")
    expect_identical(lapply(names(steps), mark), before)
    session$setInputs(help_page = "about")
    expect_identical(lapply(names(steps), mark), before)
    expect_match(mark("models"), "kb-step-todo", fixed = TRUE)
  })
})

test_that("step markers show the step number until the step is done", {
  expect_match(as.character(step_marker(2)), ">2</span>", fixed = TRUE)
  expect_match(as.character(step_marker(2, "done")), "aria-label=\"complete\"", fixed = TRUE)
  expect_match(as.character(step_marker(2, "warning")), "complete with warnings", fixed = TRUE)
})
