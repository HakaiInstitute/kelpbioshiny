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

# A change to a model in biomass per unit area makes the cover fit, which is
# fitted to the biomass predictions, out of date. A queued cover fit stays queued:
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
reset_count <- function(records, ids) {
  affected <- unique(c(ids, if (any(ids %in% biomass_ids)) "cover"))
  sum(record_status(records[affected]) %in% c("fitted", "fitting"))
}

cancel_records <- function(records) {
  pending <- names(records)[record_status(records) %in% c("queued", "fitting")]
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
# or fitted; `warning` when the fit did not converge) and "failed".
component_status <- function(source, sheet, record, prefit = NULL, cover_blocked = FALSE) {
  if (source == "none") {
    return(list(kind = "not-used", source = source))
  }
  if (is_prefit(source)) {
    if (!is.null(prefit$error)) {
      return(list(kind = "failed", source = source, message = prefit$error))
    }
    return(list(kind = "ready", source = source, warning = FALSE))
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
    fitted = list(kind = "ready", source = source, warning = !converged(record$fit)),
    failed = list(kind = "failed", source = source, message = record$error),
    list(kind = "not-fitted", source = source, blocked = cover_blocked)
  )
}

is_ready_status <- function(status) status$kind %in% c("ready", "not-used")
is_pending_status <- function(status) status$kind %in% c("queued", "fitting")
has_warning <- function(status) isTRUE(status$warning)

# The biomass models first; the cover model can be fitted once they are ready.
component_statuses <- function(sources, sheets, records, prefits = list()) {
  statuses <- lapply(biomass_ids, function(id) component_status(sources[[id]], sheets[[id]], records[[id]], prefits[[id]]))
  ready <- all(vapply(statuses, is_ready_status, logical(1)))
  c(statuses, list(cover = component_status(sources[["cover"]], sheets$cover, records$cover, cover_blocked = !ready)))
}

# A model fitted to your data can be fitted now: it has checked data and, for the
# cover model, the biomass models are ready.
can_fit <- function(status) {
  status$source == "user" && (status$kind %in% c("failed", "ready") || (status$kind == "not-fitted" && !status$blocked))
}

# The models Fit all queues: those not yet fitted or failed, less those with an
# invalid setting (`skipped`). The cover model joins when every biomass model is
# ready, already pending, or in the same batch.
fit_all_plan <- function(statuses, invalid = character()) {
  candidates <- Filter(function(id) {
    status <- statuses[[id]]
    status$source == "user" && status$kind %in% c("not-fitted", "failed")
  }, component_ids)
  skipped <- intersect(candidates, invalid)
  ids <- setdiff(candidates, invalid)
  if ("cover" %in% ids) {
    on_track <- function(id) is_ready_status(statuses[[id]]) || is_pending_status(statuses[[id]]) || id %in% ids
    if (!all(vapply(biomass_ids, on_track, logical(1)))) ids <- setdiff(ids, "cover")
  }
  list(ids = unname(ids), skipped = unname(skipped))
}

# Total biomass needs a fitted cover model; the reason is shown when it is unavailable.
total_availability <- function(sources, sheets, status) {
  if (status$kind == "ready") {
    return(list(available = TRUE))
  }
  reason <- if (is.null(sheets$cover)) {
    "Total biomass unavailable: add a cover sheet with canopy area to estimate total biomass per site-year."
  } else if (sources[["cover"]] == "none") {
    "Total biomass unavailable: the cover model is not used. Choose Your data for it on the Models step."
  } else {
    "Total biomass unavailable until the cover model is fitted."
  }
  list(available = FALSE, reason = reason)
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
# from the density data, when there are any; cover takes biomass per unit area.
fit_call <- function(id, species, sheets, priors, sampler, progress_dir, biomass = NULL) {
  data <- sheets[[id]]$rows
  if (id == "weight" && species == "nereo" && !is.null(sheets$density)) {
    data <- kb_add_stipes_m2(data, sheets$density$rows)
  }
  args <- list(
    data = data, priors = prior_list(priors),
    chains = sampler$chains, niters = sampler$niters, nthin = sampler$nthin,
    progress = "none", progress_dir = progress_dir
  )
  if (id == "cover") args$biomass <- biomass
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
    component_statuses(sources, s$sheets(), s$records(), lapply(prefit_ids, s$prefit))
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

  # Prior sensitivity of each model's fit to your data; NULL until fitted.
  s$sensitivity <- lapply(component_ids, function(id) {
    fit <- dedupe(reactive(s$records()[[id]]$fit))
    reactive(if (!is.null(fit())) kb_sensitivity(fit()))
  })

  # Biomass per unit area by site-year, by output type, and total biomass from
  # the cover model.
  biomass_fits <- dedupe(reactive(lapply(biomass_ids, s$fit_of)))
  cover_fit <- dedupe(reactive(s$fit_of("cover")))
  biomass <- lapply(names(output_info), function(type) {
    reactive({
      fits <- biomass_fits()
      kb_predict_biomass(
        density = fits$density, size = fits$size, weight = fits$weight,
        blade = fits$blade, wetdry = fits$wetdry, carbon = fits$carbon,
        type = type
      )
    })
  })
  names(biomass) <- names(output_info)
  biomass_total <- lapply(names(output_info), function(type) {
    reactive(kb_predict_biomass_total(cover_fit(), biomass[[type]]()))
  })
  names(biomass_total) <- names(output_info)
  s$biomass <- function(type) biomass[[type]]()
  s$biomass_total <- function(type) biomass_total[[type]]()

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

  s$has_density <- reactive(!is.null(s$sheets()$density))
  s$mismatches <- reactive(site_mismatches(s$sheets()))
  # The cover model is optional: biomass per unit area does not wait for it.
  s$blocking <- reactive(biomass_ids[!vapply(s$statuses()[biomass_ids], is_ready_status, logical(1))])
  s$biomass_ready <- reactive(length(s$blocking()) == 0 && length(s$mismatches()) == 0)
  s$outputs <- reactive(output_availability(s$sources()))
  s$totals <- reactive(total_availability(s$sources(), s$sheets(), s$statuses()$cover))

  # Actions ----------------------------------------------------------------------

  s$notify <- function(text, type = "message", title = NULL, action = NULL, duration = 4, id = NULL) {
    ui <- if (is.null(title)) text else tagList(div(class = "fw-semibold", title), div(text))
    showNotification(ui, action = action, type = type, duration = duration, id = id, session = session)
  }

  # Runs `action`, a change that resets the fits of `ids`, after asking first
  # when any of them is fitted or fitting. Cancel runs `cancel`, which puts the
  # input that asked for the change back.
  pending_reset <- NULL
  s$confirm_reset <- function(ids, action, cancel = function() NULL) {
    n <- reset_count(isolate(s$records()), ids)
    if (n == 0) {
      return(action())
    }
    pending_reset <<- list(action = action, cancel = cancel)
    showModal(
      modalDialog(
        sprintf("This resets %d fitted %s. Continue?", n, if (n == 1) "model" else "models"),
        footer = tagList(
          button("reset_cancel", "Cancel", variant = "outline"),
          button("reset_continue", "Continue")
        ),
        size = "s"
      ),
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

  apply_sheets <- function(sheets, file) {
    update_records(function(records) idle_records())
    s$sheets(sheets)
    s$workbook(file)
    s$sources(default_sources(sheets))
  }

  # Prototype: every upload loads the example workbook, from kb_example_data().
  example_sheets <- function(file) {
    species <- isolate(s$species())
    rows <- kb_example_data(species)
    lapply(stats::setNames(nm = names(rows)), function(id) new_sheet(id, rows[[id]], file, species))
  }

  s$load_example <- function() {
    file <- sprintf("example-%s.xlsx", isolate(s$species()))
    apply_sheets(example_sheets(file), file)
  }

  s$load_workbook <- function(file) {
    apply_sheets(example_sheets(file), file)
    s$notify("Prototype: the example workbook was loaded in place of your file.")
  }

  # Prototype: a CSV loads the example sheet for its model, where there is one.
  s$load_csv <- function(id, file) {
    rows <- kb_example_data(isolate(s$species()))[[id]]
    if (is.null(rows)) {
      s$notify(sprintf("Prototype: there are no example %s rows to load.", lower_label(id)))
      return()
    }
    s$reset_fit(id)
    sheets <- isolate(s$sheets())
    sheets[[id]] <- new_sheet(id, rows, file, isolate(s$species()))
    s$sheets(sheets)
    sources <- isolate(s$sources())
    sources[[id]] <- "user"
    s$sources(sources)
    s$notify(sprintf("Prototype: example %s rows were loaded from %s.", lower_label(id), file))
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

  # Opens one page of the Help tab: "guide" or "about".
  s$open_help <- function(page) {
    s$go_to("help")
    nav_select("help_page", page, session = session)
  }

  # Opens one tab of a model's detail page, e.g. its settings after a convergence warning.
  tab_requests <- 0
  s$open_tab <- function(id, tab) {
    tab_requests <<- tab_requests + 1
    s$go_to("models", id)
    s$tab_request(list(id = id, tab = tab, n = tab_requests))
  }
  s$open_settings <- function(id) s$open_tab(id, "settings")

  lapply(component_ids, function(id) {
    observeEvent(session$input[[paste0("toast_settings_", id)]], {
      removeNotification(paste0("convergence_", id), session = session)
      s$open_settings(id)
    })
  })

  observeEvent(session$input$step, {
    if (session$input$step == "models") {
      s$open(pending_model %||% "hub")
      pending_model <<- NULL
    }
  })

  # Fit queue ------------------------------------------------------------------
  # Fits run one at a time in the background, so the session stays responsive
  # and fits continue whichever step is shown. The next queued fit starts once
  # the runner is free; a fit whose call cannot be built (e.g. the cover model
  # without biomass) fails at once.

  start_next <- function(id) {
    progress_dir <- tempfile("kb-fit-")
    dir.create(progress_dir)
    call <- tryCatch(
      {
        if (id == "cover" && !s$biomass_ready()) {
          stop("Biomass per unit area is not available: every other model in use must be ready first.", call. = FALSE)
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

  fit_failed <- function(id, message) {
    set_records(fail_record(s$records(), id, message))
    s$notify(message, type = "error", title = sprintf("%s model failed", label_of(id)), duration = 10)
  }

  fit_done <- function(id, fit) {
    set_records(finish_record(s$records(), id, fit))
    if (converged(fit)) {
      s$notify("All parameters converged.", title = sprintf("%s model fitted", label_of(id)))
      return()
    }
    s$notify(
      warning_help$convergence$advice,
      type = "warning",
      title = sprintf("%s model fitted with a convergence warning", label_of(id)),
      action = div(
        class = "mt-2",
        actionButton(paste0("toast_settings_", id), "Open sampler settings", class = "btn-primary btn-sm")
      ),
      duration = 10,
      id = paste0("convergence_", id)
    )
  }

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
    if (!is.null(running)) s$progress(100 * kb_fit_progress(running$dir))
  })

  session$onSessionEnded(function() stop_running(cancel = TRUE))

  s
}
