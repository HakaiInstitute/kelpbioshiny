# A check that an installed file (inst/CITATION) parses.

test_that("the CITATION file parses to the app's citation", {
  citation <- utils::readCitationFile(
    system.file("CITATION", package = "kelpbioshiny"),
    meta = list(Version = "0.0.0.9000")
  )
  expect_s3_class(citation, "citation")
  expect_identical(citation$title, "kelpbioshiny: Shiny App for Bayesian Kelp Biomass Estimation")
  expect_identical(citation$url, source_url)
})
