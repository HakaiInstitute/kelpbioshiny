# Transitions -------------------------------------------------------------------

test_that("fit records move through queued, fitting and fitted or failed", {
  records <- queue_records(idle_records(), c("size", "density"))
  expect_identical(unname(record_status(records)[c("density", "size", "weight")]), c("queued", "queued", "idle"))
  expect_identical(next_queued(records), "density")

  records <- start_record(records, "density")
  expect_identical(fitting_id(records), "density")
  records <- finish_record(records, "density", "fit")
  expect_identical(records$density, new_record("fitted", fit = "fit"))

  records <- fail_record(start_record(records, "size"), "size", "Sampling failed.")
  expect_identical(records$size, new_record("failed", error = "Sampling failed."))
  expect_null(next_queued(records))
})

test_that("a result for a record that is no longer fitting is discarded", {
  records <- cancel_records(start_record(idle_records(), "size"))
  expect_identical(records$size$status, "idle")
  expect_identical(finish_record(records, "size", "fit")$size$status, "idle")
  expect_identical(fail_record(records, "size", "late")$size$status, "idle")
})

test_that("cancelling returns queued and fitting records to idle and keeps fitted ones", {
  records <- finish_record(start_record(idle_records(), "density"), "density", "fit")
  records <- start_record(queue_records(records, c("size", "cover")), "size")
  records <- cancel_records(records)
  expect_identical(unname(record_status(records)), c("fitted", rep("idle", 6)))
})

test_that("a change to a biomass model resets a fitted or running cover fit but not a queued one", {
  fitted_cover <- finish_record(start_record(idle_records(), "cover"), "cover", "fit")
  expect_identical(reset_records(fitted_cover, "size")$cover$status, "idle")
  expect_identical(queue_records(fitted_cover, "size")$cover$status, "idle")
  expect_identical(reset_records(start_record(idle_records(), "cover"), "weight")$cover$status, "idle")
  expect_identical(queue_records(queue_records(idle_records(), "cover"), "size")$cover$status, "queued")
  expect_identical(queue_records(fitted_cover, "cover")$cover$status, "queued")
})

# Statuses ------------------------------------------------------------------------

sheets_of <- function(species = "nereo") {
  rows <- kb_example_data(species)
  lapply(stats::setNames(nm = names(rows)), function(id) new_sheet(id, rows[[id]], "example.xlsx", species))
}

test_that("statuses follow the source, the data check and the fit record", {
  sheets <- sheets_of()
  sources <- default_sources(sheets)
  sources[["weight"]] <- "user"
  statuses <- component_statuses(sources, sheets, idle_records(), list(wetdry = list(fit = "fit"), carbon = list(error = "Not found.")))
  kinds <- vapply(statuses, `[[`, "", "kind")
  expect_identical(
    kinds,
    c(density = "not-fitted", size = "not-fitted", weight = "data-error", blade = "not-used", wetdry = "ready", carbon = "failed", cover = "not-fitted")
  )
  expect_match(statuses$weight$message, "Column `year` of `weight` must not have any missing values.", fixed = TRUE)
  expect_identical(statuses$carbon$message, "Not found.")
  expect_true(statuses$cover$blocked)
  expect_identical(component_status("user", NULL, new_record())$kind, "no-data")
})

test_that("a fit that did not converge is ready with a warning", {
  sheet <- sheets_of()$size
  fit <- .mock_new_fit("size", "nereo", sheet$rows, NULL)
  status <- component_status("user", sheet, new_record("fitted", fit = fit))
  expect_identical(status$kind, "ready")
  expect_true(has_warning(status))
  expect_false(has_warning(component_status("user", sheet, new_record("fitted", fit = .mock_new_fit("size", "nereo", sheet$rows, NULL, nthin = 10L)))))
})

test_that("Fit all skips models with invalid settings and adds cover once the biomass models are on track", {
  sheets <- sheets_of()
  sources <- default_sources(sheets)
  statuses <- component_statuses(sources, sheets, idle_records(), list())
  expect_identical(fit_all_plan(statuses), list(ids = c("density", "size", "cover"), skipped = character()))
  expect_identical(fit_all_plan(statuses, invalid = "size"), list(ids = "density", skipped = "size"))

  sources[["weight"]] <- "user"
  statuses <- component_statuses(sources, sheets, idle_records(), list())
  expect_identical(fit_all_plan(statuses)$ids, c("density", "size"))
})

# The store -----------------------------------------------------------------------

test_that("Fit all fits density, size and then cover with the data, priors and sampler settings", {
  log <- local_stub_fits()
  shiny::testServer(store_app(), {
    store$update_sampler("size", "nthin", 2)
    store$fit_all()
    session$flushReact()
    expect_identical(store$fitting(), "density")
    expect_identical(unname(record_status(store$records())[c("size", "cover")]), c("queued", "queued"))
    expect_identical(store$statuses()$size$kind, "queued")

    finish_fits(session, runner)
    expect_identical(fitted_models(log), c("density", "size", "biomass_cover"))
    expect_identical(unname(record_status(store$records())[c("density", "size", "cover")]), rep("fitted", 3))
    expect_null(store$fitting())

    density <- log$calls[[1]]$args
    expect_identical(density$data, store$sheets()$density$rows)
    expect_identical(names(density$priors), names(kb_priors_density_nereo()))
    expect_s3_class(density$priors$intercept, "kb_prior_normal")
    expect_identical(density[c("chains", "niters", "nthin", "progress")], list(chains = 4, niters = 1000, nthin = 1, progress = "none"))
    expect_type(density$progress_dir, "character")
    expect_false(dir.exists(density$progress_dir))
    expect_identical(log$calls[[2]]$args$nthin, 2)
    expect_s3_class(log$calls[[3]]$args$biomass, "kb_biomass")
    expect_true(store$totals()$available)
  })
})

test_that("weight fitted to your data takes stipes_m2 from the density data", {
  log <- local_stub_fits()
  shiny::testServer(store_app(), {
    sheets <- store$sheets()
    rows <- sheets$weight$rows
    sheets$weight <- new_sheet("weight", rows[!is.na(rows$year), ], "weight.csv", "nereo")
    store$sheets(sheets)
    store$set_source("weight", "user")
    store$queue_fits("weight")
    finish_fits(session, runner)
    data <- log$calls[[1]]$args$data
    expect_true("stipes_m2" %in% names(data))
    expect_identical(data$stipes_m2, kb_add_stipes_m2(sheets$weight$rows, sheets$density$rows)$stipes_m2)
    expect_identical(store$statuses()$weight$kind, "ready")

    # Without a density sheet, weight is fitted without the density effect.
    sheets$density <- NULL
    store$sheets(sheets)
    store$queue_fits("weight")
    finish_fits(session, runner)
    expect_false("stipes_m2" %in% names(log$calls[[2]]$args$data))
  })
})

test_that("a failed fit is marked failed with its message and the queue carries on", {
  log <- local_stub_fits(fail = "density")
  shiny::testServer(store_app(), {
    store$fit_all()
    finish_fits(session, runner)
    expect_identical(store$statuses()$density$kind, "failed")
    expect_identical(store$statuses()$density$message, "Stub density fit failed.")
    expect_identical(store$statuses()$size$kind, "ready")
    # The cover fit needs biomass per unit area, which waits on density.
    expect_identical(store$statuses()$cover$kind, "failed")
    expect_match(store$statuses()$cover$message, "Biomass per unit area is not available", fixed = TRUE)
    expect_identical(fitted_models(log), c("density", "size"))
    expect_false(store$biomass_ready())
  })
})

test_that("cancelling stops the running fit and clears the queue", {
  log <- local_stub_fits()
  shiny::testServer(store_app(), {
    store$fit_all()
    session$flushReact()
    dir <- environment(store$cancel_fits)$running$dir
    expect_true(dir.exists(dir))
    store$cancel_fits()
    finish_fits(session, runner)
    expect_null(store$fitting())
    expect_false(any(record_status(store$records()) %in% c("queued", "fitting")))
    expect_false(dir.exists(dir))
    expect_length(log$calls, 0)
  })
})

test_that("clearing, a species change, a source change and a CSV reload reset fits", {
  local_stub_fits()
  shiny::testServer(store_app(), {
    fit_density_size <- function() {
      store$queue_fits(c("density", "size"))
      finish_fits(session, runner)
      expect_identical(unname(record_status(store$records())[c("density", "size")]), c("fitted", "fitted"))
    }

    fit_density_size()
    store$set_source("size", "prefit_hakai")
    expect_identical(store$records()$size$status, "idle")
    expect_identical(store$records()$density$status, "fitted")

    store$set_source("size", "user")
    fit_density_size()
    store$load_csv("size", "size.csv")
    expect_identical(store$records()$size$status, "idle")

    fit_density_size()
    store$set_species("macro")
    expect_true(all(record_status(store$records()) == "idle"))
    # The sheets are checked again for the new species.
    expect_match(store$sheets()$density$error, "plants", fixed = TRUE)

    store$load_example()
    fit_density_size()
    store$clear_data()
    expect_true(all(record_status(store$records()) == "idle"))
    expect_length(store$sheets(), 0)
  })
})

test_that("refitting a biomass model resets the fitted cover model", {
  local_stub_fits()
  shiny::testServer(store_app(), {
    store$fit_all()
    finish_fits(session, runner)
    expect_identical(store$statuses()$cover$kind, "ready")
    store$queue_fits("size")
    expect_identical(store$records()$cover$status, "idle")
    finish_fits(session, runner)
    expect_identical(store$statuses()$cover$kind, "not-fitted")
  })
})

test_that("invalid settings show kelpbio's message and keep the model out of the fits", {
  log <- local_stub_fits()
  shiny::testServer(store_app(), {
    store$update_prior("size", "intercept", "b", 0)
    store$update_sampler("density", "niters", 1.5)
    errors <- store$setting_errors()
    expect_identical(errors$size$priors[["intercept"]], "`sd` must be greater than 0, not 0.")
    expect_match(errors$density$sampler[["niters"]], "`niters` must be a whole number", fixed = TRUE)
    expect_identical(store$invalid(), c(density = "density", size = "size"))
    expect_identical(store$fit_plan()$skipped, c("density", "size"))

    store$queue_fits("size")
    store$fit_all()
    finish_fits(session, runner)
    expect_length(log$calls, 0)

    store$reset_priors("size")
    store$queue_fits("size")
    finish_fits(session, runner)
    expect_identical(fitted_models(log), "size")
  })
})

test_that("site names that differ only in case or spacing block biomass", {
  local_stub_fits()
  shiny::testServer(store_app(), {
    store$fit_all()
    finish_fits(session, runner)
    expect_true(store$biomass_ready())

    sheets <- store$sheets()
    sheets$density$rows$site[sheets$density$rows$site == "site1"] <- "Site 1"
    store$sheets(sheets)
    expect_identical(store$mismatches(), c("Site 1", "site1"))
    expect_false(store$biomass_ready())
  })
})

test_that("a change that would discard fits waits for confirmation", {
  local_stub_fits()
  shiny::testServer(store_app(), {
    expect_identical(reset_count(store$records(), component_ids), 0L)
    store$queue_fits(c("density", "size"))
    finish_fits(session, runner)
    expect_identical(reset_count(store$records(), "size"), 1L)

    store$confirm_reset("size", function() store$set_source("size", "prefit_hakai"))
    session$setInputs(reset_cancel = 1)
    expect_identical(store$sources()[["size"]], "user")

    store$confirm_reset(component_ids, store$clear_data)
    expect_length(store$sheets(), 4)
    session$setInputs(reset_continue = 1)
    expect_length(store$sheets(), 0)
  })
})
