# Module test of the Export step's downloads: the example run fitted with
# stubbed fits, each download's file read back, and the fit bundle resumed.

test_that("each download writes its file, and the fit bundle resumes the run", {
  local_stub_fits()
  bundle <- withr::local_tempfile(fileext = ".rds")
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

    file.copy(output$`export-download_bundle`, bundle)
    expect_true(store$exported())
  })

  shiny::testServer(store_app(load_example = FALSE), {
    store$resume("kelpbio-fits-nereo.rds", bundle)
    session$flushReact()

    expect_named(store$sheets(), c("weight", "size", "survey", "canopy", "wetdry", "carbon", "nitrogen"))
    expect_identical(unname(record_status(store$records())), rep("fitted", 6))
    expect_true(all(vapply(store$statuses(), `[[`, "", "kind") == "ready"))
    expect_identical(store$estimate_choices()$survey$sum, "survey")
  })
})

test_that("a file that is not a fit bundle, or a damaged one, is not resumed", {
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(list(species = "nereo"), path)
  expect_error(read_bundle(path), "not a fit bundle")

  shiny::testServer(store_app(), {
    bundle <- run_bundle(store)
    bundle$sources <- bundle$sources[setdiff(names(bundle$sources), "cover")]
    saveRDS(bundle, path)
    expect_error(read_bundle(path), "damaged or from another version")
    store$resume("damaged.rds", path)
    expect_named(store$sheets(), c("weight", "size", "survey", "canopy", "wetdry", "carbon", "nitrogen"))
  })
})
