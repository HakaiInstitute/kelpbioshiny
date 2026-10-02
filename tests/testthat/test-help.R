test_that("every help topic links to a listed kelpbio article", {
  for (key in names(help_topics)) {
    expect_true(help_topics[[key]]$link %in% kelpbio_articles, info = key)
    expect_true(startsWith(help_url(key), "https://hakaiinstitute.github.io/kelpbio/"), info = key)
  }
})
