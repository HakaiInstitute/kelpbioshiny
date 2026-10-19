# One model's detail page: a header (name, source, status, fit button and
# progress) above tabs for settings, diagnostics, parameter estimates and the
# model description; its
# data are on the Data step and its predictions on the Estimates step. A single
# module, instantiated per model. A pre-fit model has no settings of its own,
# so it shows only the diagnostics and description of the model as fitted in
# advance; a census fits no model, so the size page shows only its notice.

# The labels of each prior family's hyperparameter inputs (a, then b).
prior_inputs <- list(normal = c("Mean", "SD"), lognormal = c("Mean (log)", "SD (log)"), exponential = "Rate")

model_tabs <- c(settings = "Settings", diagnostics = "Diagnostics", parameters = "Parameters", description = "Description")
own_fit_tabs <- "settings"
fit_tabs <- c("settings", "diagnostics", "parameters", "description")

mod_model_ui <- function(id, cid) {
  ns <- NS(id)
  def <- components[[cid]]
  tabs <- lapply(names(model_tabs), function(tab) {
    nav_panel(model_tabs[[tab]], value = tab, div(class = "pt-4", uiOutput(ns(paste0("tab_", tab)))))
  })
  tagList(
    actionLink(
      ns("back"), span(class = "d-inline-flex align-items-center gap-1", lucide("arrow-left"), "All models"),
      class = "small text-body-secondary d-inline-block mb-3"
    ),
    # As a step's header, with the model's status and source on a line under
    # its description.
    page_header(
      def$label,
      tagList(
        def$detail,
        div(
          class = "d-flex flex-wrap align-items-center gap-2 mt-2",
          uiOutput(ns("status"), inline = TRUE),
          source_select(ns("source"), cid)
        )
      ),
      div(
        class = "d-flex flex-wrap align-items-center gap-2",
        uiOutput(ns("fit_action"), inline = TRUE),
        uiOutput(ns("view_estimates_ui"), inline = TRUE)
      )
    ),
    uiOutput(ns("fit_progress")),
    uiOutput(ns("notices")),
    do.call(navset_underline, c(list(id = ns("tab")), tabs))
  )
}

mod_model_server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    cid <- id
    def <- components[[cid]]
    ns <- session$ns

    # Routed through dedupe(), so the page re-renders only when this model's
    # status or fit changes, not when another model's does.
    status <- dedupe(reactive(store$statuses()[[cid]]))
    kind <- reactive(status()$kind)
    sheet <- reactive(store$sheets()[[def$sheet]])
    page <- dedupe(reactive({
      source <- store$sources()[[cid]]
      sheet <- sheet()
      mode <- if (is_prefit(source)) "prefit" else if (source == "none") "none" else if (is.null(sheet)) "no-sheet" else source
      reference <- if (is_prefit(source)) prefit_reference(source)
      list(mode = mode, reference = reference, name = sheet$name, file = sheet$file, error = !is.null(sheet$error), warnings = sheet$warnings)
    }))
    # The kelpbio fit behind every number and figure: the pre-fit model, or the
    # fit to your data once fitted.
    fit <- dedupe(reactive(store$fit_of(cid)))
    fitted_fit <- reactive(req(fit()))
    # Prior sensitivity: of the fit to your data, shared with the other steps,
    # or of the pre-fit model.
    sensitivity <- reactive({
      if (page()$mode == "prefit") kb_sensitivity(fitted_fit()) else req(store$sensitivity[[cid]]())
    })
    # Influential observations of the fit to your data, shared with the other steps.
    influence <- reactive(req(store$influence[[cid]]()))
    errors <- reactive(store$setting_errors()[[cid]])

    # A pre-fit model shows only its diagnostics and description, and a census
    # no tabs; the page opens on the first tab shown.
    tab_mode <- dedupe(reactive(if (page()$mode %in% c("prefit", "census")) page()$mode else "fit"))
    observeEvent(tab_mode(), {
      hidden <- switch(tab_mode(), prefit = own_fit_tabs, census = fit_tabs, character())
      for (tab in names(model_tabs)) {
        if (tab %in% hidden) nav_hide("tab", tab, session = session) else nav_show("tab", tab, session = session)
      }
      nav_select("tab", if (tab_mode() == "prefit") "diagnostics" else "settings", session = session)
    })
    # Once a fit to your data is ready, its page shows the diagnostics.
    ready <- dedupe(reactive(kind() == "ready"))
    observeEvent(ready(), {
      if (ready() && page()$mode == "user") nav_select("tab", "diagnostics", session = session)
    }, ignoreInit = TRUE)

    sync_source_select(input, session, "source", cid, store)

    observeEvent(input$back, store$open("hub"))
    # Opens the model's sheet on the Data step, or the step without a sheet.
    observeEvent(input$to_data, if (is.null(sheet())) store$go_to("data") else store$view_sheet(def$sheet))
    observeEvent(input$fit, store$queue_fits(cid))
    observeEvent(input$cancel, store$cancel_fit(cid))
    observeEvent(input$view_estimates, store$open_estimate(cid))
    observeEvent(input$settings_convergence, nav_select("tab", "settings", session = session))
    observeEvent(input$settings_prior, {
      nav_select("tab", "settings", session = session)
      accordion_panel_open("priors_open", "priors", session = session)
    })
    observeEvent(input$diagnostics_influence, nav_select("tab", "diagnostics", session = session))
    observeEvent(store$tab_request(), {
      request <- store$tab_request()
      if (identical(request$id, cid)) nav_select("tab", request$tab, session = session)
    })

    output$status <- renderUI(status_badge(status()))

    # The next step once the model is ready; until then Fit model is.
    estimates_available <- dedupe(reactive(isTRUE(store$estimates()[[cid]]$available)))
    output$view_estimates_ui <- renderUI({
      if (estimates_available()) button(ns("view_estimates"), span("View estimates ", lucide("arrow-right")))
    })

    output$fit_action <- renderUI(fit_control(ns("fit"), ns("cancel"), status(), cid %in% store$invalid(), fit_label = "Fit model"))

    # The banner of the Models list, while this model is queued or fitting.
    # Rendered when the fit starts; only the bar and the percentage then update,
    # so the rest does not re-render as the fit moves.
    output$fit_progress <- renderUI({
      req(kind() %in% c("queued", "fitting"))
      if (kind() == "queued") {
        return(fit_progress("Queued", "Fits once the fits ahead of it finish", icon = lucide("clock", "text-primary")))
      }
      fit_progress(
        sprintf("Fitting the %s model", lower_label(cid)),
        textOutput(ns("fit_percent"), inline = TRUE),
        uiOutput(ns("fit_bar"))
      )
    })
    output$fit_bar <- renderUI(progress_bar(store$progress()))
    output$fit_percent <- renderText(percent_text(store$progress()))

    # One notice per warning, whichever tab is open.
    output$notices <- renderUI({
      status <- status()
      mode <- page()$mode
      notice_ui <- if (mode == "prefit" && status$kind != "failed") {
        notice(
          "layout-list", with_help(sprintf("Fitted in advance to %s", prefit_info[[page()$reference]]$data), "prefit")
        )
      } else if (mode == "census" && status$kind == "ready") {
        notice(
          "list-checks", with_help("Census of every counted plant", "census"),
          "All plants measured, so survey biomass weighs each plant.",
          action = button(ns("to_data"), "View data", variant = "outline", size = "sm")
        )
      } else if (mode == "none") {
        notice(
          "minus-circle", sprintf("%s is not used in this run", def$label),
          if (cid == "cover") {
            "Add drone flights to the surveys and a canopy sheet of drone maps to estimate canopy biomass."
          } else {
            sprintf("Estimates that need %s are unavailable.", tolower(def$label))
          },
          tone = "muted"
        )
      } else if (cid == "cover" && status$kind == "no-data" && mode == "user") {
        notice(
          "minus-circle", "No surveys with a drone flight",
          "Add plot_area_m2, canopy_area_m2 and tide_height_m to the surveys flown by drone to fit the cover calibration.",
          tone = "muted", action = button(ns("to_data"), "View data", variant = "outline", size = "sm")
        )
      } else if (status$kind == "data-error") {
        notice(
          "x-circle", warning_help$data_check$title, status$message,
          paste(
            warning_help$data_check$advice,
            # A pre-fit model stands in until the sheet is fixed.
            if (any(is_prefit(def$sources))) sprintf("Meanwhile, choose the %s model as its source.", source_note(Filter(is_prefit, def$sources)[[1]]))
          ),
          tone = "warning", action = button(ns("to_data"), "View data", variant = "outline", size = "sm")
        )
      } else if (status$kind == "failed") {
        notice("x-circle", warning_help$failed$title, status$message, tone = "warning")
      } else if (status$kind == "not-fitted" && status$blocked) {
        if (survey_possible(store$sources())) {
          notice("clock", "Fitted once survey biomass is ready", "The weight and size models must be ready first.", tone = "muted")
        } else {
          notice("minus-circle", "Needs survey biomass", "Survey biomass needs the weight and size models, each fitted to your data or pre-fit.", tone = "muted")
        }
      } else if (mode == "user" && status$kind == "ready") {
        tagList(
          if (isTRUE(status$outdated)) notice("refresh-cw", warning_help$outdated$title, warning_help$outdated$advice, tone = "warning"),
          if (isTRUE(status$convergence)) convergence_notice(ns("settings_convergence")),
          prior_notice(store$sensitivity[[cid]](), fit()$meta$priors, ns("settings_prior")),
          influence_notice(store$influence[[cid]](), ns("diagnostics_influence"))
        )
      }
      if (length(notice_ui) > 0) div(class = "d-flex flex-column gap-2 mb-3", notice_ui)
    })

    # Tabs -----------------------------------------------------------------------

    not_used <- function() empty_state("minus-circle", "Not used", "Choose a source above to use this model.")
    no_sheet <- function(what) {
      empty_state(
        "file-spreadsheet", "No data uploaded", what,
        div(class = "mt-2", button(ns("to_data"), "Go to data", variant = "outline"))
      )
    }

    output$tab_settings <- renderUI({
      switch(page()$mode,
        user = tagList(
          panel(
            "Sampler settings",
            uiOutput(ns("sampler"))
          ),
          # Priors are an advanced setting, so they are collapsed unless the
          # fit flagged one.
          panel(
            with_help("Priors", "priors"),
            description = "Advanced: the defaults suit most datasets.",
            accordion(
              id = ns("priors_open"),
              open = if (isTRUE(isolate(status()$prior))) "priors" else FALSE,
              accordion_panel(
                "Show priors",
                value = "priors",
                div(class = "d-flex justify-content-end mb-2", button(ns("reset"), "Reset to defaults", "rotate-ccw", "ghost", "sm")),
                uiOutput(ns("priors"))
              )
            )
          )
        ),
        "no-sheet" = no_sheet("Settings can be changed once there are data to fit."),
        none = not_used(),
        NULL
      )
    })

    output$tab_diagnostics <- renderUI({
      mode <- page()$mode
      if (mode == "user" && kind() == "ready") {
        return(diagnostics_panel(ns, "your data", influence = TRUE))
      }
      if (mode == "prefit" && !is.null(fit())) {
        return(diagnostics_panel(ns, "the reference data"))
      }
      switch(mode,
        user = fit_pending_state(kind(), "Fit the model to check its convergence, prior sensitivity, influential observations and posterior predictive check."),
        "no-sheet" = no_sheet("Diagnostics appear once the model is fitted to your data."),
        none = not_used(),
        NULL
      )
    })

    # The fitted parameters, from kelpbio's tidy().
    output$tab_parameters <- renderUI({
      if (!is.null(fit())) {
        return(panel(
          "Parameter estimates",
          description = with_help("With 95% compatibility intervals, to 3 significant figures.", "interval"),
          reactable::reactableOutput(ns("parameters"))
        ))
      }
      switch(page()$mode,
        user = fit_pending_state(kind(), "Fit the model to see its parameter estimates."),
        "no-sheet" = no_sheet("Parameter estimates appear once the model is fitted to your data."),
        none = not_used(),
        NULL
      )
    })

    # kb_model_describe() describes a fit, with its priors and the effects it fitted.
    output$tab_description <- renderUI({
      if (!is.null(fit())) {
        return(description_panel(fit()))
      }
      switch(page()$mode,
        user = fit_pending_state(kind(), "Fit the model to see its description, with its fitted effects."),
        "no-sheet" = no_sheet("The description appears once the model is fitted to your data."),
        none = not_used(),
        NULL
      )
    })

    output$parameters <- reactable::renderReactable({
      rows <- tidy(fitted_fit())[c("term", "estimate", "lower", "upper")]
      number <- function(name) reactable::colDef(name = name, cell = number_text, align = "right", class = "kb-tabular")
      app_table(
        rows,
        pagination = FALSE,
        columns = list(
          term = reactable::colDef(name = "Parameter", style = list(fontFamily = "var(--bs-font-monospace)")),
          estimate = number("Estimate"),
          lower = number("Lower"),
          upper = number("Upper")
        )
      )
    })

    # Priors and sampler -----------------------------------------------------------
    # The editors re-render only when the fit starts or stops (to disable them);
    # values are read once and edits flow back into the store. kelpbio's message
    # for an invalid value shows under its inputs.

    pending <- reactive(is_pending_status(status()))
    # Prior entries differ by species, so inputs are wired for every entry either species has.
    prior_names <- unique(unlist(lapply(names(species_info), function(sp) default_priors(cid, sp)$name)))
    number_input <- function(input_id, label, value, step = "any") {
      div(
        class = "flex-fill",
        numericInput(ns(input_id), span(class = "small text-body-secondary fw-normal", label), value, step = step, width = "100%") |>
          tagAppendAttributes(class = "mb-0")
      )
    }
    error_text <- function(errors, name) if (name %in% names(errors)) errors[[name]] else ""
    field_error <- function(output_id) textOutput(ns(output_id)) |> tagAppendAttributes(class = "small text-danger-emphasis")

    output$priors <- renderUI({
      store$species()
      current <- isolate(store$priors()[[cid]])
      # After a fit, mark the priors the sensitivity check found influential.
      rows <- if (kind() == "ready") isolate(store$sensitivity[[cid]]())
      influential <- if (!is.null(rows)) rows$term[!rows$weak_prior]
      tags$fieldset(
        disabled = if (pending()) NA,
        div(
          class = "row g-2",
          lapply(seq_len(nrow(current)), function(i) {
            p <- current[i, ]
            div(
              class = "col-sm-6",
              div(
                class = paste(
                  "border rounded-3 p-3 h-100 d-flex flex-column gap-1",
                  if (p$name %in% influential) "border-warning"
                ),
                # The parameter, as the diagnostics and warnings name it, with its prior.
                div(
                  class = "d-flex flex-wrap align-items-center justify-content-between gap-2",
                  tags$code(textOutput(ns(paste0("label_", p$name)), inline = TRUE)),
                  if (p$name %in% influential) warning_badge("Influencing the estimate")
                ),
                div(class = "small text-body-secondary mb-1", p$label),
                div(
                  class = "d-flex gap-2",
                  number_input(paste0(p$name, "_a"), prior_inputs[[p$family]][1], p$a),
                  if (p$family != "exponential") number_input(paste0(p$name, "_b"), prior_inputs[[p$family]][2], p$b)
                ),
                field_error(paste0("error_", p$name))
              )
            )
          })
        )
      )
    })

    lapply(prior_names, function(name) {
      output[[paste0("label_", name)]] <- renderText({
        current <- store$priors()[[cid]]
        req(name %in% current$name)
        row <- current[current$name == name, ]
        paste(row$name, "~", format_prior(row))
      })
      output[[paste0("error_", name)]] <- renderText(error_text(errors()$priors, name))
      observeEvent(input[[paste0(name, "_a")]], store$update_prior(cid, name, "a", input[[paste0(name, "_a")]]))
      observeEvent(input[[paste0(name, "_b")]], store$update_prior(cid, name, "b", input[[paste0(name, "_b")]]))
    })

    observeEvent(input$reset, {
      store$reset_priors(cid)
      priors <- store$priors()[[cid]]
      for (i in seq_len(nrow(priors))) {
        updateNumericInput(session, paste0(priors$name[i], "_a"), value = priors$a[i])
        if (priors$family[i] != "exponential") updateNumericInput(session, paste0(priors$name[i], "_b"), value = priors$b[i])
      }
    })

    sampler_labels <- list(
      chains = with_help("Chains", "chains"),
      niters = with_help("Iterations (niters)", "niters"),
      nthin = with_help("Thinning (nthin)", "nthin")
    )
    sampler_steps <- c(chains = 1, niters = 100, nthin = 1)

    output$sampler <- renderUI({
      current <- isolate(store$samplers()[[cid]])
      tags$fieldset(
        disabled = if (pending()) NA,
        div(
          class = "d-flex gap-3", style = "max-width: 32rem",
          lapply(names(sampler_labels), function(field) {
            div(
              class = "flex-fill",
              number_input(paste0("sampler_", field), sampler_labels[[field]], current[[field]], sampler_steps[[field]]),
              field_error(paste0("sampler_error_", field))
            )
          })
        )
      )
    })

    lapply(names(sampler_labels), function(field) {
      output[[paste0("sampler_error_", field)]] <- renderText(error_text(errors()$sampler, field))
      observeEvent(input[[paste0("sampler_", field)]], store$update_sampler(cid, field, input[[paste0("sampler_", field)]]))
    })

    # Diagnostics -----------------------------------------------------------------

    output$convergence <- reactable::renderReactable({
      rows <- kb_convergence(fitted_fit())[c("term", "rhat", "ess_bulk", "ess_tail", "converged")]
      app_table(
        rows,
        pagination = FALSE,
        row_style = function(index) if (!rows$converged[index]) app_row_colour$warning,
        columns = list(
          term = reactable::colDef(name = "Parameter", style = list(fontFamily = "var(--bs-font-monospace)")),
          rhat = reactable::colDef(name = "R-hat", class = "kb-tabular"),
          ess_bulk = reactable::colDef(name = "Bulk ESS", class = "kb-tabular"),
          ess_tail = reactable::colDef(name = "Tail ESS", class = "kb-tabular"),
          converged = reactable::colDef(
            name = "Status", minWidth = 130,
            cell = function(value) if (value) "Converged" else warning_badge("Not converged", NULL)
          )
        )
      )
    })

    output$trace <- render_figure(
      function() kb_plot_trace(fitted_fit()), "trace",
      height = function() figure_px(70 + 150 * ceiling(nrow(tidy(fitted_fit())) / 2)),
      alt = sprintf("Trace plots of the %s model parameters by chain", lower_label(cid))
    )
    output$ppc_dens <- render_figure(
      function() kb_ppc_dens(fitted_fit()), "ppc_dens",
      aspect = 4 / 8,
      alt = sprintf("Density of the observed data over densities of simulated data for the %s model", lower_label(cid))
    )
    output$ppc_resid <- render_figure(
      function() kb_ppc_resid(fitted_fit()), "ppc_resid",
      aspect = 4 / 8,
      alt = sprintf("Density of the observed deviance residuals over those of simulated data for the %s model", lower_label(cid))
    )

    output$sensitivity_note <- renderUI(data_strength_notice(sensitivity()))

    # One line per check, from the same kelpbio results as the panels below,
    # each linking to its panel.
    output$verdict <- renderUI({
      line <- function(ok, text, target, tone = if (ok) "ok" else "warning") {
        icon <- switch(tone, ok = lucide("check-circle-2", "text-success"), warning = lucide("alert-triangle", "text-warning"), info = lucide("info", "text-body-secondary"))
        tags$li(class = "d-flex align-items-center gap-2", icon, tags$a(href = paste0("#", ns(target)), class = "link-body-emphasis", text))
      }
      convergence <- kb_convergence(fitted_fit())
      unconverged <- convergence$term[!convergence$converged]
      rows <- sensitivity()
      influencing <- rows$term[!rows$weak_prior]
      weak_data <- rows$term[rows$weak_prior & !rows$strong_data]
      influential <- if (page()$mode == "user") sum(influence()$influential)
      tags$ul(
        class = "list-unstyled d-flex flex-column gap-2 mb-0 small",
        line(length(unconverged) == 0, if (length(unconverged) == 0) "All parameters converged" else sprintf("Not converged: %s", and_list(unconverged)), "diag_convergence"),
        if (length(influencing) > 0) {
          line(FALSE, sprintf("Prior influencing the estimate: %s", and_list(influencing)), "diag_sensitivity")
        } else if (length(weak_data) > 0) {
          line(FALSE, sprintf("Not strongly informed by the data: %s", and_list(weak_data)), "diag_sensitivity")
        } else {
          line(TRUE, "No prior sensitivity", "diag_sensitivity")
        },
        if (!is.null(influential)) {
          line(influential == 0, if (influential == 0) "No influential observations" else sprintf("%d influential observations to check", influential), "diag_influence")
        },
        line(TRUE, "Posterior predictive check: compare the data with the simulated datasets", "diag_ppc", tone = "info")
      )
    })

    output$sensitivity <- reactable::renderReactable({
      rows <- sensitivity()[c("term", "weak_prior", "strong_data")]
      flag <- function(value) if (value) "Yes" else warning_badge("No", NULL)
      app_table(
        rows,
        pagination = FALSE,
        columns = list(
          term = reactable::colDef(name = "Parameter", style = list(fontFamily = "var(--bs-font-monospace)")),
          weak_prior = reactable::colDef(name = "Weak prior", cell = flag),
          strong_data = reactable::colDef(name = "Strong data", cell = flag)
        )
      )
    })

    output$influence_note <- renderUI({
      if (!influence_flagged(influence())) notice("check-circle-2", "No influential observations", tone = "muted")
    })

    # The influential observations, most influential first, as fitted.
    output$influence <- reactable::renderReactable({
      rows <- influence()
      req(influence_flagged(rows))
      rows <- rows[rows$influential, setdiff(names(rows), c("elpd_loo", "influential"))]
      rows <- rows[order(rows$pareto_k, decreasing = TRUE), ]
      app_table(
        rows,
        page_size = 8,
        columns = list(pareto_k = reactable::colDef(name = "Pareto k", class = "kb-tabular"))
      )
    })
  })
}

fit_pending_state <- function(kind, description) {
  if (kind %in% c("queued", "fitting")) {
    return(empty_state("loader-2", "Fitting", "Results appear here when the fit finishes."))
  }
  if (kind == "failed") {
    return(empty_state("x-circle", warning_help$failed$title, warning_help$failed$advice))
  }
  if (kind == "data-error") {
    return(empty_state("x-circle", warning_help$data_check$title, warning_help$data_check$advice))
  }
  empty_state("play", "Not fitted yet", description)
}

# The diagnostics of a fit, in one scrolling tab: convergence and trace plots,
# prior sensitivity, influential observations (`influence`, for a fit to your
# data, whose sheet can be corrected) and posterior predictive checks. `data`
# names the data the model was fitted to: "your data", or "the reference data"
# of a pre-fit model.
diagnostics_panel <- function(ns, data, influence = FALSE) {
  div(
    class = "d-flex flex-column gap-3",
    panel("Summary", uiOutput(ns("verdict"))),
    convergence_panel(ns), sensitivity_panel(ns), if (influence) influence_panel(ns), ppc_panel(ns, data)
  )
}

sensitivity_panel <- function(ns) {
  panel(
    with_help("Prior sensitivity", "sensitivity"),
    id = ns("diag_sensitivity"),
    description = "Weak prior: the prior is not driving the estimate. Strong data: the data inform it.",
    uiOutput(ns("sensitivity_note")),
    reactable::reactableOutput(ns("sensitivity"))
  )
}

influence_panel <- function(ns) {
  panel(
    with_help("Influential observations", "influence"),
    id = ns("diag_influence"),
    description = sprintf("Pareto k above %s: check these for recording errors.", default_arg(kb_influence, "threshold")),
    uiOutput(ns("influence_note")),
    reactable::reactableOutput(ns("influence"))
  )
}

# The captions name the legend labels.
ppc_panel <- function(ns, data) {
  panel(
    with_help("Posterior predictive check", "ppc"),
    id = ns("diag_ppc"),
    h3(class = "fs-6 fw-medium mb-0", "Density overlay"),
    figure_plot(
      ns("ppc_dens"),
      HTML(sprintf(
        "%s (<em>y</em>) and 50 simulated datasets (<em>y</em><sub>rep</sub>): the dark line should sit within the light ones.",
        sentence_case(data)
      ))
    ),
    h3(class = "fs-6 fw-medium mb-0", "Deviance residuals overlay"),
    figure_plot(
      ns("ppc_resid"),
      "The same check on deviance residuals."
    )
  )
}

convergence_panel <- function(ns) {
  panel(
    "Convergence",
    id = ns("diag_convergence"),
    description = tagList(
      "Parameters with ", with_help("R-hat", "rhat"), sprintf(" above %s or a bulk or tail ", default_arg(kb_converged, "rhat")),
      with_help("effective sample size (ESS)", "ess"),
      sprintf(" below %s per chain are flagged.", default_arg(kb_converged, "ess"))
    ),
    reactable::reactableOutput(ns("convergence")),
    figure_plot(ns("trace"), "Draws for each chain by iteration. Well-mixed chains overlap.")
  )
}

# kb_model_describe() prints the description and returns its lines invisibly.
description_panel <- function(fit) {
  lines <- withr::with_output_sink(nullfile(), kb_model_describe(fit))
  panel(
    "Likelihood, random effects and priors",
    tags$pre(class = "bg-body-tertiary rounded-3 p-3 small mb-0", paste(lines, collapse = "\n"))
  )
}
