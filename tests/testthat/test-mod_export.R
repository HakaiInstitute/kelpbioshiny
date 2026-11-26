# Module test of the Export step's results download: the example run fitted
# with stubbed fits, and the ZIP's files read back.

test_that("the results download writes a folder per estimate", {
  local_stub_fits()
  shiny::testServer(store_app(mod_export_server, "export"), {
    store$fit_all()
    finish_fits(session, runner)
    store$set_estimate_choice("survey", "sum", "survey")

    files <- utils::unzip(output$`export-download_results`, list = TRUE)$Name
    folders <- unique(dirname(files[grepl("/", files)]))
    expect_setequal(folders, c(
      file.path(rep(c("in_situ_biomass", "canopy_biomass"), each = 4), c("wet", "dry", "carbon", "nitrogen")),
      "weight", "size", "cover_calibration", "wetdry", "carbon", "nitrogen"
    ))
    expect_true(all(c(
      "README.txt", "settings.csv", "weight/weight.csv", "weight/weight.png", "weight/weight.pdf", "canopy_biomass/carbon/canopy_biomass_carbon.png"
    ) %in% files))
    sites <- unique(store$sheets()$survey$rows$site)
    expect_true(all(sprintf("in_situ_biomass/wet/in_situ_biomass_wet_%s.png", file_name(sites)) %in% files))
    expect_true(store$exported())
  })
})

test_that("the R script uses the rows and scale chosen on the Estimates step", {
  rows <- example_rows("nereo")
  sheets <- check_sheets(rows, lapply(rows, function(x) "example.xlsx"), "nereo")
  sources <- default_sources(sheets)
  priors <- lapply(component_ids, default_priors)
  samplers <- lapply(component_ids, function(id) default_sampler(1L))

  default <- export_script("nereo", sources, sheets, "example.xlsx", priors, samplers)
  expect_match(default, 'kb_predict_biomass_in_situ(fit_weight, fit_size, survey, pool_by = c("site", "year"))', fixed = TRUE)
  expect_match(default, 'kb_predict_biomass_canopy(fit_cover_calibration, canopy, pool_by = c("site", "year"))', fixed = TRUE)

  chosen <- export_script(
    "nereo", sources, sheets, "example.xlsx", priors, samplers,
    choices = list(survey = list(sum = "survey", scale = "total"), canopy = list(sum = "year"))
  )
  expect_match(chosen, "kb_predict_biomass_in_situ(fit_weight, fit_size, survey, per_area = FALSE)", fixed = TRUE)
  expect_match(chosen, 'kb_predict_biomass_canopy(fit_cover_calibration, canopy, pool_by = "year")', fixed = TRUE)
  expect_match(chosen, "calibration_biomass <- kb_predict_biomass_in_situ(fit_weight, fit_size, survey)", fixed = TRUE)
})

test_that("canopy totals by map are offered only when a site-year has several maps", {
  rows <- example_rows("nereo")
  expect_false("map" %in% names(derived_sums("canopy", list(canopy = list(rows = rows$canopy)))))
  rows$canopy <- rbind(rows$canopy, rows$canopy[1, ])
  sums <- derived_sums("canopy", list(canopy = list(rows = rows$canopy)))
  expect_identical(names(sums)[1:3], c("site_year", "map", "year"))
  expect_null(sums$map$pool_by)
  expect_identical(sums$site_year$pool_by, c("site", "year"))
})
