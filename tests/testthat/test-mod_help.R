# The guide's text, one block per line, so a change to the steps, sheets,
# sources or help topics shows as a reviewable snapshot diff.
guide_text <- function() {
  html <- as.character(htmltools::renderTags(help_guide_ui())$html)
  html <- gsub("<script[^>]*>.*?</script>", "", html, perl = TRUE)
  html <- gsub("<a href=\"([^\"]+)\"[^>]*>(.*?)</a>", "\\2 (\\1)", html, perl = TRUE)
  lines <- strsplit(gsub("<[^>]+>", "\n", html), "\n", fixed = TRUE)[[1]]
  lines <- trimws(gsub("\\s+", " ", lines))
  lines[lines != ""]
}

test_that("the user guide text", {
  expect_snapshot(writeLines(guide_text()))
})

test_that("the user guide lists every step, sheet and help topic", {
  text <- paste(guide_text(), collapse = "\n")
  for (value in names(steps)) expect_match(text, step_description(value), fixed = TRUE)
  for (id in component_ids) expect_match(text, paste(components[[id]]$columns, collapse = ", "), fixed = TRUE)
  for (topic in help_topics) expect_match(text, topic$title, fixed = TRUE)
})

test_that("the about page links to the source code and kelpbio", {
  html <- as.character(help_about_ui())
  expect_match(html, source_url, fixed = TRUE)
  expect_match(html, kelpbio_url, fixed = TRUE)
  expect_match(html, as.character(utils::packageVersion("kelpbioshiny")), fixed = TRUE)
})

test_that("the CITATION file parses to the app's citation", {
  citation <- utils::readCitationFile(
    system.file("CITATION", package = "kelpbioshiny"),
    meta = list(Version = "0.0.0.9000")
  )
  expect_s3_class(citation, "citation")
  expect_identical(citation$title, "kelpbioshiny: Shiny App for Bayesian Kelp Biomass Estimation")
  expect_identical(citation$url, source_url)
})

test_that("the about page gives both citations, each with a copy button", {
  html <- as.character(help_about_ui())
  expect_match(html, "<em>kelpbioshiny: Shiny App for Bayesian Kelp Biomass Estimation</em>", fixed = TRUE)
  expect_match(html, "<em>kelpbio: Bayesian Kelp Biomass Estimation</em>", fixed = TRUE)
  expect_match(html, "https://github.com/HakaiInstitute/kelpbio</a>", fixed = TRUE)
  expect_no_match(html, "_kelpbio:", fixed = TRUE)
  expect_identical(lengths(regmatches(html, gregexpr("navigator.clipboard.writeText", html, fixed = TRUE))), 2L)
  expect_no_match(html, "Details to follow", fixed = TRUE)
})
