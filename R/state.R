# App state and actions.
#
# One store per session. Steps and modules read its reactive values and call its
# actions; nothing else mutates state.

TICK_MS <- 200

# Pure status logic, kept outside the store so it can be tested without a session.
# waiting_on: the upstream models (upstream_of()) that are not ready yet; fit:
# the model's kelpbio fit once it is fitted; dismissed: the warnings dismissed
# for that fit ("convergence", "sensitivity").
component_status <- function(id, source, sheet, fit_status, waiting_on = character(), fit = NULL, dismissed = character()) {
  if (source == "none") {
    return(list(kind = "not-used"))
  }
  if (source == "prefit") {
    return(list(kind = "ready"))
  }
  if (is.null(sheet)) {
    return(list(kind = "no-data"))
  }
  if (fit_status %in% c("idle", "queued") && length(waiting_on) > 0) {
    return(list(kind = "waiting", on = waiting_on, queued = fit_status == "queued"))
  }
  switch(fit_status,
    queued = list(kind = "queued"),
    fitting = list(kind = "fitting"),
    fitted = list(kind = "fitted", converged = is_converged(fit), dismissed = dismissed),
    list(kind = "data-ok", warning = sheet$validation$level == "warning")
  )
}

is_ready_status <- function(status) status$kind %in% c("ready", "fitted", "not-used")

# Statuses in component order; upstream models come first, so each model's
# waiting list can be read from the statuses already worked out.
component_statuses <- function(sources, sheets, fit_status, fits = list(), dismissed = list()) {
  statuses <- list()
  for (id in component_ids) {
    upstream <- upstream_of(id, sources)
    waiting_on <- upstream[!vapply(statuses[upstream], is_ready_status, logical(1))]
    statuses[[id]] <- component_status(
      id, sources[[id]], sheets[[id]], fit_status[[id]], waiting_on, fits[[id]], dismissed[[id]] %||% character()
    )
  }
  statuses
}

is_pending <- function(status) {
  status$kind %in% c("queued", "fitting") || (status$kind == "waiting" && status$queued)
}

# A model can be queued when it has data and everything it waits on can be
# fitted or is already queued or fitting.
is_queueable <- function(id, statuses) {
  status <- statuses[[id]]
  if (status$kind == "data-ok") {
    return(TRUE)
  }
  if (status$kind != "waiting" || status$queued) {
    return(FALSE)
  }
  all(vapply(status$on, function(up) is_pending(statuses[[up]]) || is_queueable(up, statuses), logical(1)))
}

# The models to queue for ids: each with the upstream models it still needs.
with_upstream <- function(ids, statuses) {
  expand <- function(id) {
    status <- statuses[[id]]
    upstream <- if (status$kind == "waiting") Filter(function(up) is_queueable(up, statuses), status$on) else character()
    c(unlist(lapply(upstream, expand)), id)
  }
  needed <- unlist(lapply(ids, expand))
  unname(component_ids[component_ids %in% needed])
}

# Models whose fit uses id's fit, directly or through another model.
downstream_of <- function(id, sources) {
  direct <- component_ids[vapply(component_ids, function(other) id %in% upstream_of(other, sources), logical(1))]
  unique(c(direct, unlist(lapply(direct, downstream_of, sources = sources))))
}

# Totals need a fitted biomass:cover model; the reason is shown when they are unavailable.
total_availability <- function(sources, sheets, status) {
  if (status$kind == "fitted") {
    return(list(available = TRUE))
  }
  reason <- if (is.null(sheets$cover)) {
    "Totals unavailable: add a cover sheet with canopy area to estimate total biomass per site-year."
  } else if (sources[["cover"]] == "none") {
    "Totals unavailable: the biomass:cover model is not used. Choose Your data for it on the Models step."
  } else {
    "Totals unavailable until the biomass:cover model is fitted."
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

new_store <- function(session) {
  s <- new.env()
  idle <- stats::setNames(rep("idle", length(component_ids)), component_ids)

  s$session <- session
  s$species <- reactiveVal("nereo")
  s$workbook <- reactiveVal(NULL)
  s$sheets <- reactiveVal(list())
  s$sources <- reactiveVal(default_sources(list()))
  s$priors <- reactiveVal(lapply(component_ids, default_priors, species = "nereo"))
  s$samplers <- reactiveVal(lapply(component_ids, function(id) default_sampler()))
  s$fit_status <- reactiveVal(idle)
  # The kelpbio fit objects of the models fitted to your data, by model id.
  s$fits <- reactiveVal(list())
  # The warnings dismissed for each model's current fit, by model id; cleared
  # whenever the model's fit changes.
  s$dismissed <- reactiveVal(list())
  s$progress <- reactiveVal(0)
  s$queue <- reactiveVal(character())
  s$fitting <- reactiveVal(NULL)
  s$biomass_viewed <- reactiveVal(FALSE)
  s$exported <- reactiveVal(FALSE)
  s$open <- reactiveVal("hub")
  s$tab_request <- reactiveVal(NULL)

  # Derived state --------------------------------------------------------------

  s$statuses <- reactive(component_statuses(s$sources(), s$sheets(), s$fit_status(), s$fits(), s$dismissed()))
  s$is_dismissed <- function(id, type) type %in% s$dismissed()[[id]]

  # The fit a model contributes: its pre-fit model, or its fit to your data once
  # fitted; NULL otherwise.
  prefits <- new.env()
  prefit <- function(id, species) {
    key <- paste(id, species)
    if (is.null(prefits[[key]])) prefits[[key]] <- kelpbio_fn("prefit", id, species)()
    prefits[[key]]
  }
  s$fit_of <- function(id) {
    source <- s$sources()[[id]]
    if (source == "prefit") {
      return(prefit(id, s$species()))
    }
    if (source == "user" && s$fit_status()[[id]] == "fitted") {
      return(s$fits()[[id]])
    }
    NULL
  }

  # Biomass per unit area by site-year, and totals from the biomass:cover model.
  s$biomass <- function(type) {
    kb_predict_biomass(
      density = s$fit_of("density"), size = s$fit_of("size"), weight = s$fit_of("weight"),
      blade = s$fit_of("blade"), wetdry = s$fit_of("wetdry"), carbon = s$fit_of("carbon"),
      type = type
    )
  }
  s$biomass_total <- function(type) kb_predict_biomass_total(s$fit_of("cover"), s$biomass(type))
  s$fittable <- reactive({
    statuses <- s$statuses()
    component_ids[vapply(component_ids, is_queueable, logical(1), statuses = statuses)]
  })
  # The cover model is optional: biomass per unit area does not wait for it.
  s$blocking <- reactive(biomass_ids[!vapply(s$statuses()[biomass_ids], is_ready_status, logical(1))])
  s$biomass_ready <- reactive(length(s$blocking()) == 0)
  s$outputs <- reactive(output_availability(s$sources()))
  s$totals <- reactive(total_availability(s$sources(), s$sheets(), s$statuses()$cover))
  s$has_density <- reactive(!is.null(s$sheets()$density))

  # Actions ----------------------------------------------------------------------

  s$notify <- function(text, type = "message", title = NULL, action = NULL, duration = 4, id = NULL) {
    ui <- if (is.null(title)) text else tagList(div(class = "fw-semibold", title), div(text))
    showNotification(ui, action = action, type = type, duration = duration, id = id, session = session)
  }

  # A model's dismissals apply to one fit, so any change of fit state clears them.
  clear_dismissed <- function(ids) {
    dismissed <- isolate(s$dismissed())
    if (any(ids %in% names(dismissed))) s$dismissed(dismissed[setdiff(names(dismissed), ids)])
  }

  patch_status <- function(ids, value) {
    clear_dismissed(ids)
    current <- isolate(s$fit_status())
    current[ids] <- value
    s$fit_status(current)
  }

  s$dismiss <- function(id, type) {
    dismissed <- isolate(s$dismissed())
    dismissed[[id]] <- union(dismissed[[id]], type)
    s$dismissed(dismissed)
  }

  # The fit that is sampling: its model, progress directory and kelpbio fit.
  running <- NULL
  stop_running <- function() {
    if (!is.null(running)) unlink(running$dir, recursive = TRUE)
    running <<- NULL
    s$fitting(NULL)
  }

  reset_all_fits <- function() {
    s$queue(character())
    stop_running()
    s$fit_status(idle)
    s$dismissed(list())
    s$biomass_viewed(FALSE)
  }

  # A refit makes the fits built on it out of date.
  stale_downstream <- function(ids) {
    fits <- isolate(s$fit_status())
    stale <- unique(unlist(lapply(ids, downstream_of, sources = isolate(s$sources()))))
    stale[fits[stale] == "fitted"]
  }

  reset_fit <- function(id) {
    s$queue(setdiff(isolate(s$queue()), id))
    if (identical(isolate(s$fitting()), id)) stop_running()
    patch_status(c(id, stale_downstream(id)), "idle")
    s$biomass_viewed(FALSE)
  }

  apply_sheets <- function(sheets, file) {
    reset_all_fits()
    s$sheets(sheets)
    s$workbook(file)
    s$sources(default_sources(sheets))
  }

  s$load_example <- function() {
    sheets <- lapply(
      stats::setNames(nm = example_workbook_sheets), example_sheet,
      file = example_workbook, species = isolate(s$species())
    )
    apply_sheets(sheets, example_workbook)
  }

  s$load_workbook <- function(file) {
    s$load_example()
    s$workbook(file)
    s$notify("Prototype: the example workbook was loaded in place of your file.")
  }

  s$load_csv <- function(id, file) {
    reset_fit(id)
    sheets <- isolate(s$sheets())
    sheets[[id]] <- example_sheet(id, file, isolate(s$species()))
    s$sheets(sheets)
    sources <- isolate(s$sources())
    sources[[id]] <- "user"
    s$sources(sources)
    s$notify(sprintf("Prototype: example %s rows were loaded from %s.", lower_label(id), file))
  }

  s$clear_data <- function() apply_sheets(list(), NULL)

  s$set_species <- function(value) {
    if (identical(value, isolate(s$species()))) {
      return()
    }
    s$species(value)
    s$priors(lapply(component_ids, default_priors, species = value))
    reset_all_fits()
  }

  s$set_source <- function(id, value) {
    sources <- isolate(s$sources())
    if (identical(sources[[id]], value)) {
      return()
    }
    reset_fit(id)
    sources[[id]] <- value
    s$sources(sources)
  }

  s$update_prior <- function(id, name, field, value) {
    priors <- isolate(s$priors())
    row <- priors[[id]]$name == name
    if (is.null(value) || is.na(value) || identical(priors[[id]][row, field], value)) {
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
    if (is.null(value) || is.na(value) || identical(samplers[[id]][[field]], value)) {
      return()
    }
    samplers[[id]][[field]] <- value
    s$samplers(samplers)
  }

  # Queues ids together with the upstream models they still need, and refits
  # the fitted models built on them.
  s$queue_fits <- function(ids) {
    if (length(ids) == 0) {
      return()
    }
    ids <- with_upstream(ids, isolate(s$statuses()))
    ids <- component_ids[component_ids %in% c(ids, stale_downstream(ids))]
    patch_status(ids, "queued")
    s$queue(union(isolate(s$queue()), ids))
  }

  s$cancel_fits <- function() {
    fits <- isolate(s$fit_status())
    patch_status(names(fits)[fits %in% c("queued", "fitting")], "idle")
    s$queue(character())
    stop_running()
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

  observe({
    if (identical(session$input$step, "biomass") && s$biomass_ready()) s$biomass_viewed(TRUE)
  })

  # Fit queue ------------------------------------------------------------------
  # Fits run one at a time, polled by a reactive timer that lives in the session,
  # so they continue whichever step is shown. Each fit calls its kb_fit_*() with a
  # progress_dir that kb_fit_progress() reads on every tick. The mock fits return
  # at once and simulate sampling time through kb_fit_progress(); the real app
  # would run kb_fit_*() as an ExtendedTask backed by mirai and take the fit when
  # the task completes.

  start_fit <- function(id) {
    species <- s$species()
    sampler <- s$samplers()[[id]]
    progress_dir <- tempfile("kb-fit-")
    dir.create(progress_dir)
    args <- list(
      s$sheets()[[id]]$rows, prior_list(s$priors()[[id]]),
      chains = sampler$chains, niters = sampler$iterations, nthin = sampler$thin,
      progress = "none", progress_dir = progress_dir
    )
    # The biomass:cover model is fitted to the plots' biomass per unit area.
    if (id == "cover") args$biomass <- s$biomass("wet")
    list(id = id, dir = progress_dir, fit = do.call(kelpbio_fn("fit", id, species), args))
  }

  skip_tick <- FALSE

  observe({
    if (!is.null(s$fitting()) || length(s$queue()) == 0) {
      return()
    }
    # The next fit is the first queued model that is not waiting on another.
    isolate({
      queue <- s$queue()
      statuses <- s$statuses()
      runnable <- queue[vapply(statuses[queue], function(x) x$kind == "queued", logical(1))]
      if (length(runnable) == 0) {
        # What is left waits on models that are no longer queued, e.g. after a source change.
        patch_status(queue, "idle")
        s$queue(character())
        s$notify(sprintf("Not fitted: %s. The models it needs are no longer queued.", paste(vapply(queue, lower_label, ""), collapse = ", ")))
        return()
      }
      s$queue(setdiff(queue, runnable[1]))
      running <<- start_fit(runnable[1])
      s$fitting(runnable[1])
      patch_status(runnable[1], "fitting")
      s$progress(0)
      skip_tick <<- TRUE
    })
  })

  observe({
    id <- s$fitting()
    if (is.null(id)) {
      return()
    }
    invalidateLater(TICK_MS)
    isolate({
      if (skip_tick) {
        skip_tick <<- FALSE
        return()
      }
      progress <- kb_fit_progress(running$dir)
      s$progress(100 * progress)
      if (progress < 1) {
        return()
      }
      fit <- running$fit
      fits <- s$fits()
      fits[[id]] <- fit
      s$fits(fits)
      patch_status(id, "fitted")
      stop_running()
      if (is_converged(fit)) {
        s$notify("All parameters converged.", title = sprintf("%s model fitted", label_of(id)))
      } else {
        s$notify(
          convergence_advice,
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
    })
  })

  s
}
