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
  for (id in component_ids) {
    for (species in names(species_info)) expect_match(text, paste(sheet_columns(id, species), collapse = ", "), fixed = TRUE)
  }
  for (topic in help_topics) expect_match(text, topic$title, fixed = TRUE)
  expect_match(text, sprintf("A value above %s", default_arg(kb_convergence, "rhat")), fixed = TRUE)
})

test_that("the guide points to the kelpbio documentation above the glossary", {
  html <- as.character(help_guide_ui())
  expect_match(html, "Statistical details", fixed = TRUE)
  expect_match(html, "The models, priors and diagnostics are explained in the kelpbio documentation.", fixed = TRUE)
  expect_match(
    html,
    sprintf("<a href=\"%s\" target=\"_blank\" rel=\"noopener\" class=\"btn btn-primary\">", kelpbio_url),
    fixed = TRUE
  )
  expect_match(html, "Open the kelpbio documentation", fixed = TRUE)
  expect_lt(regexpr("Statistical details", html), regexpr("Glossary", html))
  expect_no_match(html, "Statistical details, including", fixed = TRUE)
})

test_that("the sources table lists the pre-fit references of each model", {
  text <- guide_text()
  expect_identical(text[which(text == "Weight")[2] + 1], "Your data, Pre-fit coastwide, Pre-fit Hakai Institute")
  expect_identical(text[which(text == "Size")[2] + 1], "Your data, Pre-fit Hakai Institute")
})

test_that("the about page links to the source code and kelpbio", {
  html <- as.character(help_about_ui())
  expect_match(html, source_url, fixed = TRUE)
  expect_match(html, kelpbio_url, fixed = TRUE)
})

test_that("the about page shows the kelpbioshiny and kelpbio versions", {
  local_mocked_bindings(package_version_text = function(package) {
    switch(package,
      kelpbioshiny = "0.1.0",
      kelpbio = "0.2.0"
    )
  })
  html <- as.character(help_about_ui())
  expect_match(html, "kelpbioshiny version</dt>\\s*<dd[^>]*>0.1.0</dd>")
  expect_match(html, "kelpbio version</dt>\\s*<dd[^>]*>0.2.0</dd>")

  local_mocked_bindings(package_version_text = function(package) if (package == "kelpbio") "Not installed" else "0.1.0")
  expect_match(as.character(help_about_ui()), "kelpbio version</dt>\\s*<dd[^>]*>Not installed</dd>")
})

test_that("a package version is the installed version or not installed", {
  expect_identical(package_version_text("kelpbioshiny"), as.character(utils::packageVersion("kelpbioshiny")))
  expect_identical(package_version_text("kelpbioshinynotapackage"), "Not installed")
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
