script_for <- function(sources, sheets = list()) {
  priors <- lapply(component_ids, default_priors)
  samplers <- lapply(component_ids, function(id) default_sampler())
  export_script("nereo", sources, sheets, NULL, priors, samplers)
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

test_that("the R script adds the stipe density to weight data fitted with density data", {
  sheets <- list(density = list(file = "survey.xlsx"), weight = list(file = "survey.xlsx"))
  sources <- default_sources(sheets)
  sources[["weight"]] <- "user"
  script <- script_for(sources, sheets)
  expect_match(script, "weight <- kb_add_stipes_m2(weight, density)", fixed = TRUE)
  expect_lt(regexpr("kb_add_stipes_m2", script), regexpr("kb_fit_weight_nereo", script))
})
