# Snapshot test: the User guide's text, so a change to the steps, sheets,
# sources or help topics shows as a reviewable diff in _snaps/mod_help.md.
# Also a check that an installed file (inst/CITATION) parses.

# The guide's text, one block per line.
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

test_that("the CITATION file parses to the app's citation", {
  citation <- utils::readCitationFile(
    system.file("CITATION", package = "kelpbioshiny"),
    meta = list(Version = "0.0.0.9000")
  )
  expect_s3_class(citation, "citation")
  expect_identical(citation$title, "kelpbioshiny: Shiny App for Bayesian Kelp Biomass Estimation")
  expect_identical(citation$url, source_url)
})
