# Two test types for R/state.R:
# - a unit test of a pure function (the status rule), on synthetic input;
# - a store test: shiny::testServer() on new_store() with the kb_fit_*() calls
#   stubbed (local_stub_fits()) and the fits run in the session (test_runner()).

test_that("a model fitted to your data with no sheet needs data", {
  status <- component_status("user", sheet = NULL, record = new_record())
  expect_identical(status$kind, "no-data")
  expect_match(as.character(status_badge(status)), "Needs data", fixed = TRUE)
})

test_that("a run is a census when chosen, and always for Macrocystis", {
  expect_false(is_census("nereo", "sample"))
  expect_true(is_census("nereo", "census"))
  expect_true(is_census("macro", "sample"))
})

test_that("Fit all fits each model with its sheet data and sampler settings", {
  log <- local_stub_fits()
  shiny::testServer(store_app(), {
    store$update_sampler("size", "nthin", 2)
    store$update_sampler("size", "adapt_delta", 0.99)
    store$fit_all()
    finish_fits(session, runner)

    expect_identical(fitted_models(log), c("weight", "size", "wetdry", "carbon", "nitrogen", "cover"))
    size <- log$calls[[2]]$args
    expect_identical(size$data, store$sheets()$size$rows)
    expect_identical(size$nthin, 2)
    expect_identical(size$control, list(adapt_delta = 0.99))
    expect_identical(c(size$chains, size$niters), c(4L, 1000L))
    expect_identical(size$seed, as.integer(store$samplers()$size$seed))
    expect_identical(store$statuses()$size$kind, "ready")
  })
})

test_that("a fit keeps its settings and is out of date once they change", {
  local_stub_fits()
  shiny::testServer(store_app(), {
    store$fit_all()
    finish_fits(session, runner)

    store$update_sampler("size", "nthin", 2)
    session$flushReact()
    expect_true(store$statuses()$size$outdated)
    expect_identical(store$run_settings()$size$sampler$nthin, 1)
    expect_true("size" %in% store$fit_plan()$ids)

    store$update_sampler("size", "nthin", 1)
    session$flushReact()
    expect_false(store$statuses()$size$outdated)
  })
})

test_that("a Macrocystis run is a census: it fits every model but size and gives both biomass estimates", {
  log <- local_stub_fits()
  shiny::testServer(store_app(load_example = FALSE), {
    store$set_species("macro")
    store$load_example()
    store$fit_all()
    finish_fits(session, runner)

    expect_true(store$census())
    expect_identical(fitted_models(log), c("weight", "wetdry", "carbon", "nitrogen", "cover"))
    expect_true(all(vapply(store$statuses()[fitted_models(log)], `[[`, "", "kind") == "ready"))
    expect_gt(nrow(store$survey_biomass("wet", FALSE, c("site", "year"))), 0)
    expect_gt(nrow(store$canopy_biomass("wet", "year")), 0)
  })
})

test_that("an idle session ends, unless a fit is running or an input changes", {
  withr::local_options(kelpbioshiny.idle_minutes = 10)
  busy <- shiny::reactiveVal(FALSE)
  idle_app <- function(input, output, session) watch_idle(input, session, busy)
  minute <- 60 * 1000

  shiny::testServer(idle_app, {
    session$elapse(9 * minute)
    session$setInputs(x = 1)
    session$elapse(9 * minute)
    expect_false(session$isClosed())
    busy(TRUE)
    session$elapse(20 * minute)
    expect_false(session$isClosed())
    busy(FALSE)
    session$elapse(10 * minute)
    expect_true(session$isClosed())
  })
})

test_that("a combined source names the site-years your data share with the reference data", {
  reference <- kelpbiodata::data_weight_hakai_nereo
  rows <- data.frame(site = c(as.character(reference$site[1]), "new_site"), year = c(as.character(reference$year[1]), "2024"))

  shared <- reference_overlap("weight", "nereo", rows, "combined_hakai")
  expect_identical(shared, paste(reference$site[1], reference$year[1]))
  expect_identical(reference_overlap("weight", "nereo", rows, "user"), character())
})

test_that("a kelpbio warning from a biomass estimate is kept with the estimate", {
  canopy <- kelpbio::data_survey_canopy_sim_nereo
  canopy$canopy_tide_height_m[1:3] <- 2.5

  rows <- with_kb_warnings(kb_predict_biomass_canopy(kelpbio::fit_cover_calibration_sim_nereo, canopy))
  expect_match(attr(rows, "kb_warnings"), "canopy_tide_height_m above 2 m")
})
