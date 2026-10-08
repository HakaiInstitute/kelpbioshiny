# App state and actions.
#
# One store per session. Steps and modules read its reactive values and call its
# actions; nothing else mutates state.

TICK_MS <- 200

# Fit records -------------------------------------------------------------------
# Each model's fit to your data is a record: its status ("idle", "queued",
# "fitting", "fitted" or "failed"), the kelpbio fit once fitted, and the error
# message once failed. The transitions below are pure, so they can be tested
# without a session.

new_record <- function(status = "idle", fit = NULL, error = NULL) list(status = status, fit = fit, error = error)

idle_records <- function() lapply(component_ids, function(id) new_record())

record_status <- function(records) vapply(records, `[[`, "", "status")

# A change to a model in plot biomass makes the cover biomass fit, which is
# fitted to the plot biomass predictions, out of date. A queued cover fit stays queued:
# it runs after the biomass models.
invalidate_cover <- function(records, ids) {
  if (any(ids %in% biomass_ids) && records$cover$status %in% c("fitting", "fitted", "failed")) {
    records$cover <- new_record()
  }
  records
}

queue_records <- function(records, ids) {
  records <- invalidate_cover(records, ids)
  records[ids] <- list(new_record("queued"))
  records
}

reset_records <- function(records, ids) {
  records <- invalidate_cover(records, ids)
  records[ids] <- list(new_record())
  records
}

# The fitted and running models a reset of `ids` discards: the ids and, for a
# biomass model, the cover model.
reset_ids <- function(records, ids) {
  affected <- unique(c(ids, if (any(ids %in% biomass_ids)) "cover"))
  affected[record_status(records[affected]) %in% c("fitted", "fitting")]
}

# Whether any model's priors differ from the defaults for `species`.
priors_edited <- function(priors, species) {
  any(vapply(component_ids, function(id) {
    any(format_prior(priors[[id]]) != format_prior(default_priors(id, species)))
  }, logical(1)))
}

cancel_records <- function(records) {
  pending <- names(records)[record_status(records) %in% c("queued", "fitting")]
  records[pending] <- list(new_record())
  records
}

# Cancels one model's queued or running fit, and a queued cover fit, which waits
# on the biomass models.
cancel_record <- function(records, id) {
  ids <- c(id, if (id %in% biomass_ids) "cover")
  pending <- ids[record_status(records[ids]) %in% c("queued", "fitting")]
  records[pending] <- list(new_record())
  records
}

start_record <- function(records, id) {
  records[[id]] <- new_record("fitting")
  records
}

# A result applies only while its record is still fitting: a fit that was
# cancelled or reset in the meantime is discarded.
finish_record <- function(records, id, fit) {
  if (records[[id]]$status == "fitting") records[[id]] <- new_record("fitted", fit = fit)
  records
}

fail_record <- function(records, id, message) {
  if (records[[id]]$status %in% c("queued", "fitting")) records[[id]] <- new_record("failed", error = message)
  records
}

# Fits run one at a time in model order, so the cover fit comes last.
next_queued <- function(records) {
  queued <- names(records)[record_status(records) == "queued"]
  if (length(queued) > 0) queued[[1]]
}

fitting_id <- function(records) {
  fitting <- names(records)[record_status(records) == "fitting"]
  if (length(fitting) > 0) fitting[[1]]
}

# Statuses ----------------------------------------------------------------------
# What the app shows for each model, from its source, sheet, fit record and, for
# a pre-fit source, the pre-fit model (list(fit, error)). Kinds: "not-used",
# "no-data", "data-error", "not-fitted", "queued", "fitting", "ready" (pre-fit,
# or fitted) and "failed". A fitted model carries two warnings: `convergence`
# when the fit did not converge, and `prior` (prior_warning, from its prior
# sensitivity) when a prior is influencing an estimate.
component_status <- function(source, sheet, record, prefit = NULL, cover_blocked = FALSE, prior_warning = FALSE) {
  if (source == "none") {
    return(list(kind = "not-used", source = source))
  }
  if (is_prefit(source)) {
    if (!is.null(prefit$error)) {
      return(list(kind = "failed", source = source, message = prefit$error))
    }
    return(list(kind = "ready", source = source))
  }
  if (is.null(sheet)) {
    return(list(kind = "no-data", source = source))
  }
  if (!is.null(sheet$error)) {
    return(list(kind = "data-error", source = source, message = sheet$error))
  }
  switch(record$status,
    queued = list(kind = "queued", source = source),
    fitting = list(kind = "fitting", source = source),
    fitted = list(kind = "ready", source = source, convergence = !kb_converged(record$fit), prior = prior_warning),
    failed = list(kind = "failed", source = source, message = record$error),
    list(kind = "not-fitted", source = source, blocked = cover_blocked)
  )
}

is_ready_status <- function(status) status$kind %in% c("ready", "not-used")
is_pending_status <- function(status) status$kind %in% c("queued", "fitting")
has_warning <- function(status) isTRUE(status$convergence) || isTRUE(status$prior)

# kb_sensitivity() rows flag a prior warning when any prior is not weak.
prior_flagged <- function(rows) !is.null(rows) && !all(rows$weak_prior)

# The biomass models first; the cover model can be fitted once they are ready
# and biomass per unit area can be estimated from them. prior_warnings flags, by
# model, a fit whose priors influence its estimates.
component_statuses <- function(sources, sheets, records, prefits = list(), prior_warnings = list()) {
  status <- function(id, ...) {
    component_status(sources[[id]], sheets[[id]], records[[id]], ..., prior_warning = isTRUE(prior_warnings[[id]]))
  }
  statuses <- lapply(biomass_ids, function(id) status(id, prefits[[id]]))
  ready <- all(vapply(statuses, is_ready_status, logical(1)))
  c(statuses, list(cover = status("cover", cover_blocked = !ready || !biomass_possible(sources))))
}

# A model fitted to your data can be fitted now: it has checked data and, for the
# cover model, the biomass models are ready.
can_fit <- function(status) {
  status$source == "user" && (status$kind %in% c("failed", "ready") || (status$kind == "not-fitted" && !status$blocked))
}

# The models Fit all queues: those not yet fitted or failed, less those with an
# invalid setting (`skipped`). The cover model joins when the density, size and
# weight models are in use and every biomass model is ready, already pending,
# or in the same batch.
fit_all_plan <- function(statuses, invalid = character()) {
  candidates <- Filter(function(id) {
    status <- statuses[[id]]
    status$source == "user" && status$kind %in% c("not-fitted", "failed")
  }, component_ids)
  skipped <- intersect(candidates, invalid)
  ids <- setdiff(candidates, invalid)
  if ("cover" %in% ids) {
    on_track <- function(id) is_ready_status(statuses[[id]]) || is_pending_status(statuses[[id]]) || id %in% ids
    in_use <- all(vapply(biomass_core_ids, function(id) statuses[[id]]$source != "none", logical(1)))
    if (!in_use || !all(vapply(biomass_ids, on_track, logical(1)))) ids <- setdiff(ids, "cover")
  }
  list(ids = unname(ids), skipped = unname(skipped))
}

# Whether an estimate on the Estimates step can be shown, with the reason when
# not: a model's predictions need the model ready (fitted to your data or
# pre-fit); plot biomass needs biomass_ready; total site biomass also needs a
# fitted cover biomass model and site surveys (totals, from total_availability()).
estimate_availability <- function(id, statuses, sources, blocking, mismatches, biomass_ready, totals) {
  unavailable <- function(reason) list(available = FALSE, reason = reason)
  if (id %in% component_ids) {
    status <- statuses[[id]]
    model <- lower_label(id)
    return(switch(status$kind,
      "ready" = list(available = TRUE),
      "not-used" = unavailable(sprintf("The %s model is not used in this run.", model)),
      "no-data" = unavailable(sprintf("The %s model has no data. Add a %s sheet on the Data step.", model, components[[id]]$sheet)),
      "data-error" = unavailable(sprintf("The %s sheet failed its data check.", components[[id]]$sheet)),
      "queued" = ,
      "fitting" = unavailable(sprintf("The %s model is fitting.", model)),
      "failed" = unavailable(sprintf("The %s model failed to fit.", model)),
      unavailable(sprintf("The %s model is not fitted yet.", model))
    ))
  }
  if (!biomass_possible(sources)) {
    return(unavailable("Plot biomass needs the density, size and weight models, each fitted to your data or pre-fit."))
  }
  if (length(mismatches) > 0) {
    return(unavailable(sprintf("Site names differ across sheets: %s. %s", and_list(sprintf("\"%s\"", mismatches)), mismatch_advice)))
  }
  if (!biomass_ready) {
    return(unavailable(sprintf(
      "Every model in use needs to be ready first. %s.",
      paste(sprintf("%s: %s", vapply(blocking, label_of, ""), vapply(statuses[blocking], block_reason, "")), collapse = "; ")
    )))
  }
  if (id == "total" && !totals$available) {
    return(unavailable(totals$reason))
  }
  list(available = TRUE)
}

# Total site biomass needs a fitted cover biomass model and drone surveys of
# whole sites in the cover sheet; the reason is shown when it is unavailable.
total_availability <- function(sources, sheets, status) {
  cover <- sheets$cover
  reason <- if (is.null(cover)) {
    "Total site biomass unavailable: add a cover sheet of drone surveys to estimate it."
  } else if (sources[["cover"]] == "none") {
    "Total site biomass unavailable: the cover biomass model is not used. Choose Your data for it on the Models step."
  } else if (is.null(cover$error) && nrow(site_surveys(cover$rows)) == 0) {
    "Total site biomass unavailable: the cover sheet has no drone surveys of whole sites. Add their site_canopy_area_m2."
  } else if (status$kind != "ready") {
    "Total site biomass unavailable until the cover biomass model is fitted."
  }
  if (is.null(reason)) list(available = TRUE) else list(available = FALSE, reason = reason)
}

output_availability <- function(sources) {
  available <- function() list(available = TRUE)
  unavailable <- function(reason) list(available = FALSE, reason = reason)
  list(
    wet = available(),
    dry = if (sources[["wetdry"]] == "none") {
      unavailable("Dry biomass unavailable: no wet:dry model")
    } else {
      available()
    },
    carbon = if (sources[["carbon"]] == "none") {
      unavailable("Carbon unavailable: no carbon model")
    } else if (sources[["wetdry"]] == "none") {
      unavailable("Carbon unavailable: carbon needs the wet:dry model")
    } else {
      available()
    }
  )
}

# The kb_fit_*() call for a model fitted to your data, as list(fn, args). The
# Nereocystis weight model takes the observed stipe density of each site-year
# from the density data, when there are any; the cover biomass model takes the
# drone surveys of plots and the wet plot biomass of their site-years.
fit_call <- function(id, species, sheets, priors, sampler, progress_dir, biomass = NULL) {
  data <- sheets[[id]]$rows
  if (id == "weight" && species == "nereo" && !is.null(sheets$density)) {
    data <- kb_add_stipes_m2(data, sheets$density$rows)
  }
  if (id == "cover") data <- plot_surveys(data)
  args <- c(
    list(data = data),
    if (id == "cover") list(biomass = biomass),
    list(priors = prior_list(priors),
    chains = sampler$chains, niters = sampler$niters, nthin = sampler$nthin,
    progress = "none", progress_dir = progress_dir)
  )
  list(fn = kelpbio_fn("fit", id, species), args = args)
}

# Fit runners -------------------------------------------------------------------
# A runner runs one kb_fit_*() call at a time in the background: invoke(fn, args)
# starts it, status() is "initial", "running", "success" or "error", result() is
# list(fit) or list(error), and cancel() stops it. The default runs each fit on a
# mirai daemon (started in inst/app/global.R) through an ExtendedTask; the daemon
# loads the installed kelpbioshiny namespace to run the mock fits. Tests pass a
# runner that runs in the session.
mirai_fit_runner <- function() {
  current <- NULL
  task <- ExtendedTask$new(function(fn, args) {
    current <<- mirai::mirai(
      tryCatch(list(fit = do.call(fn, args)), error = function(e) list(error = conditionMessage(e))),
      .args = list(fn = fn, args = args)
    )
    current
  })
  list(
    invoke = function(fn, args) task$invoke(fn, args),
    status = function() task$status(),
    result = function() task$result(),
    cancel = function() if (!is.null(current)) mirai::stop_mirai(current)
  )
}

# The store ---------------------------------------------------------------------
# A reactiveVal only invalidates when its value changes, so routing a reactive
# through one stops downstream outputs re-rendering on unrelated state changes.
dedupe <- function(r) {
  value <- reactiveVal()
  observe(value(r()), priority = 100)
  value
}


new_store <- function(session, run_fit = mirai_fit_runner) {
  s <- new.env()
  runner <- run_fit()

  s$session <- session
  s$species <- reactiveVal("nereo")
  s$workbook <- reactiveVal(NULL)
  s$sheets <- reactiveVal(list())
  s$sources <- reactiveVal(default_sources(list()))
  s$priors <- reactiveVal(lapply(component_ids, default_priors, species = "nereo"))
  s$samplers <- reactiveVal(lapply(component_ids, function(id) default_sampler()))
  s$records <- reactiveVal(idle_records())
  # The completed share of the running fit, in percent.
  s$progress <- reactiveVal(0)
  s$open <- reactiveVal("hub")
  s$tab_request <- reactiveVal(NULL)

  # Pre-fit models, loaded once per model, species and reference; a load that
  # fails gives its error message.
  prefits <- new.env()
  s$prefit <- function(id) {
    source <- s$sources()[[id]]
    key <- paste(id, s$species(), source)
    if (is.null(prefits[[key]])) {
      prefits[[key]] <- tryCatch(
        list(fit = kelpbio_fn("prefit", id, s$species())(reference = prefit_reference(source))),
        error = function(e) list(error = conditionMessage(e))
      )
    }
    prefits[[key]]
  }

  # Derived state --------------------------------------------------------------

  s$statuses <- reactive({
    sources <- s$sources()
    prefit_ids <- Filter(function(id) is_prefit(sources[[id]]), component_ids)
    prior_warnings <- lapply(component_ids, function(id) prior_flagged(s$sensitivity[[id]]()))
    component_statuses(sources, s$sheets(), s$records(), lapply(prefit_ids, s$prefit), prior_warnings)
  })
  s$fitting <- reactive(fitting_id(s$records()))

  # The fit a model contributes: its pre-fit model, or its fit to your data once
  # fitted; NULL otherwise.
  s$fit_of <- function(id) {
    source <- s$sources()[[id]]
    if (is_prefit(source)) {
      return(s$prefit(id)$fit)
    }
    if (source == "user") s$records()[[id]]$fit
  }

  # Each kelpbio result below is a reactive on the fits it uses, routed through
  # dedupe(), so it is computed once per change of those fits and shared by
  # every output that shows it.

  # Prior sensitivity of each model's fit to your data; NULL until fitted. The
  # statuses read it, so it is computed as soon as a model is fitted.
  s$sensitivity <- lapply(component_ids, function(id) {
    fit <- dedupe(reactive(s$records()[[id]]$fit))
    reactive(if (!is.null(fit())) kb_sensitivity(fit()))
  })

  # Plot biomass by site-year, by measure, and total biomass of each drone
  # survey of a whole site from the cover biomass model.
  biomass_fits <- dedupe(reactive(lapply(biomass_ids, s$fit_of)))
  cover_fit <- dedupe(reactive(s$fit_of("cover")))
  sites <- dedupe(reactive({
    cover <- s$sheets()$cover
    if (!is.null(cover) && is.null(cover$error)) site_surveys(cover$rows)
  }))
  biomass <- lapply(names(output_info), function(measure) {
    reactive({
      fits <- biomass_fits()
      kb_predict_plot_biomass(
        fits$weight, fits$size, fits$density, fits$wetdry, fits$carbon,
        measure = measure, progress = "none"
      )
    })
  })
  names(biomass) <- names(output_info)
  biomass_total <- lapply(names(output_info), function(measure) {
    reactive({
      fits <- biomass_fits()
      kb_predict_site_biomass(cover_fit(), sites(), fits$wetdry, fits$carbon, measure = measure)
    })
  })
  names(biomass_total) <- names(output_info)
  s$biomass <- function(measure) biomass[[measure]]()
  s$biomass_total <- function(measure) biomass_total[[measure]]()

  # Invalid prior and sampler settings, by model: list(priors, sampler), each a
  # named character vector of error messages.
  s$setting_errors <- reactive({
    priors <- s$priors()
    samplers <- s$samplers()
    lapply(component_ids, function(id) list(priors = prior_errors(priors[[id]]), sampler = sampler_errors(samplers[[id]])))
  })
  s$invalid <- reactive({
    errors <- s$setting_errors()
    component_ids[vapply(errors, function(e) length(e$priors) + length(e$sampler) > 0, logical(1))]
  })
  s$fit_plan <- reactive(fit_all_plan(s$statuses(), s$invalid()))

  s$has_data <- reactive(length(s$sheets()) > 0)
  s$mismatches <- reactive(site_mismatches(s$sheets()))
  # The cover biomass model is optional: plot biomass does not wait for it.
  s$blocking <- reactive(biomass_ids[!vapply(s$statuses()[biomass_ids], is_ready_status, logical(1))])
  s$biomass_ready <- reactive(biomass_possible(s$sources()) && length(s$blocking()) == 0 && length(s$mismatches()) == 0)
  s$outputs <- reactive(output_availability(s$sources()))
  s$totals <- reactive(total_availability(s$sources(), s$sheets(), s$statuses()$cover))
  # Each estimate on the Estimates step: list(available, reason).
  s$estimates <- reactive({
    lapply(estimate_ids, estimate_availability,
      statuses = s$statuses(), sources = s$sources(), blocking = s$blocking(), mismatches = s$mismatches(),
      biomass_ready = s$biomass_ready(), totals = s$totals()
    )
  })
  # Pre-fit models are ready before any data are loaded, so a run has an
  # estimate to save only once it has data.
  s$any_estimate <- reactive(s$has_data() && any(vapply(s$estimates(), `[[`, logical(1), "available")))

  # Actions ----------------------------------------------------------------------

  # An error is announced at once (role alert); other notices wait for the
  # screen reader to finish (role status).
  s$notify <- function(text, type = "message", title = NULL, duration = 4) {
    ui <- div(role = if (type == "error") "alert" else "status", if (is.null(title)) text else tagList(div(class = "fw-semibold", title), div(text)))
    showNotification(ui, type = type, duration = duration, session = session)
  }

  # Runs `action`, a change that resets the fits of `ids` and, with
  # `resets_priors` (a species change), every model's priors, after asking first
  # when it would discard a fit or an edited prior. Cancel runs `cancel`, which
  # puts the input that asked for the change back.
  pending_reset <- NULL
  s$confirm_reset <- function(ids, action, cancel = function() NULL, resets_priors = FALSE) {
    fitted <- reset_ids(isolate(s$records()), ids)
    edited <- resets_priors && priors_edited(isolate(s$priors()), isolate(s$species()))
    if (length(fitted) == 0 && !edited) {
      return(action())
    }
    pending_reset <<- list(action = action, cancel = cancel)
    showModal(
      modalDialog(
        title = span(id = "reset_title", if (length(fitted) > 0) "Discard fitted models?" else "Reset priors?"),
        div(
          class = "d-flex flex-column gap-2",
          if (length(fitted) > 0) {
            div(sprintf(
              "This discards the %s of %s. %s to be fitted again.",
              if (length(fitted) == 1) "fit" else "fits", and_list(vapply(fitted, label_of, "")),
              if (length(fitted) == 1) "It needs" else "They need"
            ))
          },
          if (edited) div("Edited priors return to their defaults for the new species.")
        ),
        footer = tagList(
          button("reset_cancel", "Cancel", variant = "outline"),
          button("reset_continue", if (length(fitted) > 0) "Discard fits" else "Reset priors")
        ),
        size = "s"
      ) |>
        tagAppendAttributes(`aria-labelledby` = "reset_title"),
      session = session
    )
  }
  resolve_reset <- function(choice) {
    removeModal(session)
    pending <- pending_reset
    pending_reset <<- NULL
    if (!is.null(pending)) pending[[choice]]()
  }
  observeEvent(session$input$reset_continue, resolve_reset("action"))
  observeEvent(session$input$reset_cancel, resolve_reset("cancel"))

  # The fit that is running: its model and progress directory.
  running <- NULL
  stop_running <- function(cancel = FALSE) {
    if (!is.null(running)) {
      if (cancel) runner$cancel()
      unlink(running$dir, recursive = TRUE)
    }
    running <<- NULL
    s$progress(0)
  }

  # Every change of fit records goes through here, so a reset of the running
  # model also stops its fit.
  set_records <- function(records) {
    s$records(records)
    if (!is.null(running) && records[[running$id]]$status != "fitting") stop_running(cancel = TRUE)
  }
  update_records <- function(transition, ...) set_records(transition(isolate(s$records()), ...))

  s$reset_fit <- function(id) update_records(reset_records, id)

  # New data start a new run: fits are discarded and the Estimates step opens
  # on its default estimate again.
  apply_sheets <- function(sheets, file) {
    update_records(function(records) idle_records())
    s$estimate(NULL)
    s$sheets(sheets)
    s$workbook(file)
    s$sources(default_sources(sheets))
  }

  checked_sheets <- function(rows, file) {
    species <- isolate(s$species())
    lapply(stats::setNames(nm = names(rows)), function(id) new_sheet(id, rows[[id]], file, species))
  }

  # One of the example workbooks (example_workbooks).
  s$load_example <- function(example = "density_size") {
    species <- isolate(s$species())
    file <- sprintf("example-%s-%s.xlsx", gsub("_", "-", example), species)
    apply_sheets(checked_sheets(kb_example_data(species, example), file), file)
  }

  # A workbook that cannot be read, or has no sheet named after a model, leaves
  # the data as they were.
  s$load_workbook <- function(file, path) {
    rows <- tryCatch(read_workbook(path), error = function(e) e)
    if (inherits(rows, "error")) {
      s$notify(conditionMessage(rows), type = "error", title = sprintf("%s could not be read", file), duration = 10)
      return()
    }
    if (length(rows) == 0) {
      s$notify(
        sprintf("Name each sheet after its model: %s.", and_list(vapply(components, `[[`, "", "sheet"))),
        type = "error", title = sprintf("%s has no sheet named after a model", file), duration = 10
      )
      return()
    }
    apply_sheets(checked_sheets(rows, file), file)
  }

  s$load_csv <- function(id, file, path) {
    rows <- tryCatch(read_csv_rows(path), error = function(e) e)
    if (inherits(rows, "error")) {
      s$notify(conditionMessage(rows), type = "error", title = sprintf("%s could not be read", file), duration = 10)
      return()
    }
    s$reset_fit(id)
    sheets <- isolate(s$sheets())
    sheets[[id]] <- new_sheet(id, rows, file, isolate(s$species()))
    s$sheets(sheets)
    sources <- isolate(s$sources())
    sources[[id]] <- "user"
    s$sources(sources)
  }

  s$clear_data <- function() apply_sheets(list(), NULL)

  # The data checks depend on the species, so a species change checks the sheets again.
  s$set_species <- function(value) {
    if (identical(value, isolate(s$species()))) {
      return()
    }
    s$species(value)
    s$priors(lapply(component_ids, default_priors, species = value))
    update_records(function(records) idle_records())
    s$estimate(NULL)
    s$sheets(lapply(isolate(s$sheets()), function(sheet) new_sheet(sheet$component, sheet$rows, sheet$file, value)))
  }

  s$set_source <- function(id, value) {
    sources <- isolate(s$sources())
    if (identical(sources[[id]], value)) {
      return()
    }
    s$reset_fit(id)
    sources[[id]] <- value
    s$sources(sources)
  }

  # Edits are stored as typed, invalid ones included, so the editor can show
  # kelpbio's message for them; an invalid setting blocks fitting.
  s$update_prior <- function(id, name, field, value) {
    priors <- isolate(s$priors())
    row <- priors[[id]]$name == name
    if (is.null(value) || identical(priors[[id]][row, field], value)) {
      return()
    }
    priors[[id]][row, field] <- value
    s$priors(priors)
  }

  s$reset_priors <- function(id) {
    priors <- isolate(s$priors())
    priors[[id]] <- default_priors(id, isolate(s$species()))
    s$priors(priors)
  }

  s$update_sampler <- function(id, field, value) {
    samplers <- isolate(s$samplers())
    if (is.null(value) || identical(samplers[[id]][[field]], value)) {
      return()
    }
    samplers[[id]][[field]] <- value
    s$samplers(samplers)
  }

  s$queue_fits <- function(ids) {
    ids <- setdiff(ids, isolate(s$invalid()))
    if (length(ids) > 0) update_records(queue_records, ids)
  }

  s$fit_all <- function() s$queue_fits(isolate(s$fit_plan())$ids)

  s$cancel_fits <- function() {
    update_records(cancel_records)
    s$notify("Fits cancelled.")
  }

  s$cancel_fit <- function(id) update_records(cancel_record, id)

  # Navigation: the Models step shows the hub unless a model page was asked for.
  pending_model <- NULL
  s$go_to <- function(step, model = NULL) {
    if (step == "models") pending_model <<- model %||% "hub"
    if (identical(isolate(session$input$step), step)) {
      if (step == "models") s$open(pending_model)
      return()
    }
    nav_select("step", step, session = session)
  }

  # The estimate the Estimates step shows (an estimate_ids value), or NULL for
  # the first one available; reset when the data are replaced or the species
  # changes.
  s$estimate <- reactiveVal(NULL)
  s$open_estimate <- function(id) {
    s$estimate(id)
    s$go_to("estimates")
  }

  # Opens one page of the Help tab: "guide" or "about".
  s$open_help <- function(page) {
    s$go_to("help")
    nav_select("help_page", page, session = session)
  }

  # Opens one tab of a model's detail page, e.g. its diagnostics from a warning.
  tab_requests <- 0
  s$open_tab <- function(id, tab) {
    tab_requests <<- tab_requests + 1
    s$go_to("models", id)
    s$tab_request(list(id = id, tab = tab, n = tab_requests))
  }

  observeEvent(session$input$step, {
    if (session$input$step == "models") {
      s$open(pending_model %||% "hub")
      pending_model <<- NULL
    }
  })

  # Whether the current results have been exported (a download, or the R script
  # copied), for the navbar marker. A change of fits or sources changes the
  # results, so it resets.
  s$exported <- reactiveVal(FALSE)
  observeEvent(list(s$records(), s$sources()), s$exported(FALSE), ignoreInit = TRUE)

  # Fit queue ------------------------------------------------------------------
  # Fits run one at a time in the background, so the session stays responsive
  # and fits continue whichever step is shown. The next queued fit starts once
  # the runner is free; a fit whose call cannot be built (e.g. the cover model
  # without biomass) fails at once.
  #
  # TODO: once the real kelpbio predictions replace the mocks, run the slow
  # ones in this queue too, so all waiting stays on the Models step:
  # - Order: fit the models in plot biomass -> predict plot biomass (the cover
  #   biomass model is fitted to it) -> fit cover biomass -> predict total
  #   site biomass, automatically. Wet, dry and carbon in one job, as they
  #   share draws.
  # - Each model's own predictions stay on demand on the Estimates step, cached
  #   with bindCache(), unless timing with kelpbio shows them slow; then run
  #   them in the same daemon call as the fit.
  # - Statuses gain "predicting"; the navbar badge and Models banner show it,
  #   and Continue to estimates waits for the predictions.
  # - A refit, source change or new data marks the dependent predictions stale
  #   and queues them again.
  # - kb_predict_plot_biomass() records progress in a progress_dir for
  #   kb_progress(), as the fits do. Needs from kelpbio: pre-fit models shipped
  #   with their predictions.

  start_next <- function(id) {
    progress_dir <- tempfile("kb-fit-")
    dir.create(progress_dir)
    call <- tryCatch(
      {
        if (id == "cover" && !s$biomass_ready()) {
          stop("Plot biomass is not available: every other model in use must be ready first.", call. = FALSE)
        }
        fit_call(
          id, s$species(), s$sheets(), s$priors()[[id]], s$samplers()[[id]], progress_dir,
          biomass = if (id == "cover") s$biomass("wet")
        )
      },
      error = function(e) e
    )
    set_records(start_record(s$records(), id))
    if (inherits(call, "error")) {
      unlink(progress_dir, recursive = TRUE)
      fit_failed(id, conditionMessage(call))
      return()
    }
    running <<- list(id = id, dir = progress_dir)
    s$progress(0)
    runner$invoke(call$fn, call$args)
  }

  # A failure is the one fit result shown as a notification, as it can happen
  # while another step is open. A finished fit shows only in the statuses and
  # the model page notices.
  fit_failed <- function(id, message) {
    set_records(fail_record(s$records(), id, message))
    s$notify(message, type = "error", title = sprintf("%s model failed", label_of(id)), duration = 10)
  }

  fit_done <- function(id, fit) set_records(finish_record(s$records(), id, fit))

  observe({
    records <- s$records()
    if (!is.null(running) || runner$status() == "running") {
      return()
    }
    id <- next_queued(records)
    if (!is.null(id)) isolate(start_next(id))
  })

  # A cancelled fit settles as an error after running was cleared, so only the
  # running fit's result is used.
  observe({
    status <- runner$status()
    if (is.null(running) || !status %in% c("success", "error")) {
      return()
    }
    isolate({
      id <- running$id
      result <- tryCatch(runner$result(), error = function(e) list(error = conditionMessage(e)))
      stop_running()
      if (is.null(result$error)) fit_done(id, result$fit) else fit_failed(id, result$error)
    })
  })

  observe({
    if (is.null(s$fitting())) {
      return()
    }
    invalidateLater(TICK_MS)
    if (!is.null(running)) s$progress(100 * kb_progress(running$dir))
  })

  session$onSessionEnded(function() stop_running(cancel = TRUE))

  s
}
