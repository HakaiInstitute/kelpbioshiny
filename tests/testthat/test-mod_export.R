# Module test of the Export step's downloads: the example run fitted with
# stubbed fits, each download's file read back, and the fit bundle resumed.

test_that("each download writes its file, and the fit bundle resumes the run", {
  local_stub_fits()
  bundle <- withr::local_tempfile(fileext = ".rds")
  shiny::testServer(store_app(mod_export_server, "export"), {
    store$fit_all()
    finish_fits(session, runner)
    store$set_estimate_choice("survey", "sum", "survey")

    results <- output$`export-download_results`
    expect_identical(readxl::excel_sheets(results), c(
      "contents", "survey_wet", "survey_dry", "survey_carbon", "canopy_wet", "canopy_dry", "canopy_carbon",
      "weight", "size", "cover_calibration", "wetdry", "carbon", "settings"
    ))
    contents <- readxl::read_excel(results, "contents")
    expect_identical(contents$rows[contents$sheet == "survey_wet"], "Each survey")

    figures <- utils::unzip(output$`export-download_figures`, list = TRUE)$Name
    expect_true(all(c("captions.txt", "weight.png", "weight.pdf", "canopy_carbon.png") %in% figures))
    sites <- unique(store$sheets()$survey$rows$site)
    expect_true(all(sprintf("survey_wet_%s.png", file_name(sites)) %in% figures))

    file.copy(output$`export-download_bundle`, bundle)
    expect_true(store$exported())
  })

  shiny::testServer(store_app(load_example = FALSE), {
    store$resume("kelpbio-fits-nereo.rds", bundle)
    session$flushReact()

    expect_named(store$sheets(), c("weight", "size", "survey", "canopy", "wetdry", "carbon"))
    expect_identical(unname(record_status(store$records())), rep("fitted", 5))
    expect_true(all(vapply(store$statuses(), `[[`, "", "kind") == "ready"))
    expect_identical(store$estimate_choices()$survey$sum, "survey")
  })
})

test_that("a file that is not a fit bundle is not resumed", {
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(list(species = "nereo"), path)
  expect_error(read_bundle(path), "not a fit bundle")
})
