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
