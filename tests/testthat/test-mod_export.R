script_for <- function(sources) {
  priors <- lapply(component_ids, default_priors)
  samplers <- lapply(component_ids, function(id) default_sampler())
  export_script("nereo", sources, list(), NULL, priors, samplers)
}

test_that("the R script calls the pre-fit accessor with the chosen reference", {
  sources <- default_sources(list())
  script <- script_for(sources)
  expect_match(script, "fit_weight <- kb_prefit_weight_nereo(reference = \"coastwide\")", fixed = TRUE)
  expect_match(script, "# Pre-fit coastwide model", fixed = TRUE)
  expect_match(script, "fit_size <- kb_prefit_size_nereo(reference = \"hakai\")", fixed = TRUE)

  sources[["weight"]] <- "prefit_hakai"
  script <- script_for(sources)
  expect_match(script, "fit_weight <- kb_prefit_weight_nereo(reference = \"hakai\")", fixed = TRUE)
  expect_no_match(script, "coastwide", fixed = TRUE)
})
