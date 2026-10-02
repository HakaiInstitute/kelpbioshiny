# One model's detail page: a header (name, source, status, fit button and
# progress) above tabs for data, settings, diagnostics, predictions and the
# model description. A single module, instantiated per model.

model_tabs <- c(data = "Data", settings = "Settings", diagnostics = "Diagnostics", predictions = "Predictions", description = "Description")

# The tabs and diagnostics pills marked while a warning they hold is active:
# the warnings ("convergence", "sensitivity") each one shows.
marked_tabs <- list(settings = "sensitivity", diagnostics = c("convergence", "sensitivity"))
marked_pills <- list(sampler = "convergence", sensitivity = "sensitivity")

mod_model_ui <- function(id, cid) {
  ns <- NS(id)
  def <- components[[cid]]
  tabs <- lapply(names(model_tabs), function(tab) {
    title <- if (tab %in% names(marked_tabs)) marked_title(model_tabs[[tab]], ns(paste0("mark_", tab))) else model_tabs[[tab]]
    nav_panel(title, value = tab, div(class = "pt-4", uiOutput(ns(paste0("tab_", tab)))))
  })
  tagList(
    actionLink(
      ns("back"), span(class = "d-inline-flex align-items-center gap-1", lucide("arrow-left"), "All models"),
      class = "small text-body-secondary d-inline-block mb-3"
    ),
    div(
      class = "d-flex flex-wrap align-items-start justify-content-between gap-3 mb-3",
      div(
        h2(class = "kb-page-title", def$label),
        div(class = "kb-lead text-body-secondary", def$detail),
        if (cid == "weight") {
          div(class = "small text-body-secondary mt-1", with_help("Uses the density estimates", "weight_density"))
        }
      ),
      div(
        class = "d-flex flex-wrap align-items-center gap-2",
        uiOutput(ns("status"), inline = TRUE),
        source_select(ns("source"), cid),
        uiOutput(ns("fit_action"), inline = TRUE)
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

    status <- reactive(store$statuses()[[cid]])
    kind <- dedupe(reactive(status()$kind))
    busy <- dedupe(reactive(is_pending(status())))
    waiting <- dedupe(reactive(if (status()$kind == "waiting") status()))
    sheet <- reactive(store$sheets()[[cid]])
    page <- dedupe(reactive({
      source <- store$sources()[[cid]]
      sheet <- sheet()
      mode <- if (is_prefit(source)) "prefit" else if (source == "none") "none" else if (is.null(sheet)) "no-sheet" else "user"
      reference <- if (is_prefit(source)) prefit_reference(source)
      list(mode = mode, reference = reference, name = sheet$name, file = sheet$file, validation = sheet$validation)
    }))
    # What the diagnostics and predictions tabs can show: mode plus fit state.
    view <- dedupe(reactive(list(mode = page()$mode, kind = kind())))
    # The kelpbio fit behind every number and figure: the pre-fit model, or the
    # fit to your data once fitted.
    fit <- reactive(store$fit_of(cid))
    fitted_fit <- reactive({
      req(kind() == "fitted")
      req(fit())
    })

    # The warnings shown for the current fit to your data, less those dismissed.
    active_warnings <- reactive({
      if (kind() != "fitted") {
        return(character())
      }
      c(
        if (shows_convergence_warning(status())) "convergence",
        if (!all(sensitivity(fit())$weak_prior) && !store$is_dismissed(cid, "sensitivity")) "sensitivity"
      )
    })
    lapply(names(marked_tabs), function(tab) {
      output[[paste0("mark_", tab)]] <- renderUI(warning_marker(any(marked_tabs[[tab]] %in% active_warnings())))
    })
    lapply(names(marked_pills), function(pill) {
      output[[paste0("mark_pill_", pill)]] <- renderUI(warning_marker(any(marked_pills[[pill]] %in% active_warnings())))
    })

    sync_source_select(input, session, "source", cid, store)

    observeEvent(input$back, store$open("hub"))
    observeEvent(input$to_data, store$go_to("data"))
    observeEvent(input$fit, store$queue_fits(cid))
    observeEvent(input$cancel, store$cancel_fits())
    observeEvent(input$open_settings, store$open_settings(cid))
    observeEvent(input$open_priors, store$open_settings(cid))
    observeEvent(input$open_priors_header, store$open_settings(cid))
    observeEvent(input$dismiss_convergence, store$dismiss(cid, "convergence"))
    observeEvent(input$dismiss_sensitivity, store$dismiss(cid, "sensitivity"))
    observeEvent(input$dismiss_sensitivity_section, store$dismiss(cid, "sensitivity"))
    observeEvent(store$tab_request(), {
      request <- store$tab_request()
      if (identical(request$id, cid)) nav_select("tab", request$tab, session = session)
    })

    output$status <- renderUI({
      status <- status()
      status_badge(status, if (status$kind == "fitting") store$progress() else 0)
    })

    output$fit_action <- renderUI({
      if (page()$mode != "user") {
        return(NULL)
      }
      if (busy()) {
        return(button(ns("cancel"), "Cancel", "x", "outline"))
      }
      fitted <- kind() == "fitted"
      button(
        ns("fit"), if (fitted) "Refit" else "Fit model", "play", if (fitted) "outline" else "primary",
        disabled = kind() == "waiting" && !cid %in% store$fittable()
      )
    })

    output$fit_progress <- renderUI({
      req(busy())
      progress <- if (kind() == "fitting") store$progress() else 0
      label <- switch(kind(),
        queued = "Waiting for the current fit to finish",
        waiting = paste(waiting_text(waiting()), "to finish"),
        "Sampling"
      )
      div(
        class = "d-flex flex-column gap-2 mb-3",
        progress_bar(progress),
        div(
          class = "d-flex justify-content-between small text-body-secondary kb-tabular",
          span(label),
          span(sprintf("%d%%", floor(progress)))
        )
      )
    })

    # Notices that apply whichever tab is open.
    output$notices <- renderUI({
      mode <- page()$mode
      notice_ui <- if (mode == "prefit") {
        prefit <- prefit_info[[page()$reference]]
        notice(
          "layout-list", with_help(paste(prefit$label, "model"), "prefit"),
          sprintf("Fitted in advance to %s. No fitting is needed: biomass uses the stored draws.", prefit$data)
        )
      } else if (mode == "none") {
        notice(
          "minus-circle", sprintf("%s is not used in this run", def$label),
          if (cid == "blade") {
            "Upload a blade sheet to estimate blade fraction."
          } else if (cid == "cover") {
            "Add a cover sheet with canopy area to estimate total biomass per site-year."
          } else {
            sprintf("Outputs that need %s are unavailable on the Biomass step.", tolower(def$label))
          },
          tone = "muted"
        )
      } else if (!is.null(waiting()) && !waiting()$queued) {
        on <- waiting()$on
        notice(
          "clock", waiting_text(waiting()),
          sprintf(
            "This model is fitted after the %s %s. %s",
            and_list(vapply(on, lower_label, "")), if (length(on) > 1) "models" else "model",
            if (cid %in% store$fittable()) {
              sprintf("Fit model queues %s first.", if (length(on) > 1) "them" else "it")
            } else {
              "Add their data to fit it."
            }
          ),
          tone = "muted"
        )
      } else if (kind() == "fitted") {
        Filter(Negate(is.null), list(
          if ("convergence" %in% active_warnings()) {
            convergence_notice(
              "Convergence warning", ns("open_settings"),
              "The warning is shown with the biomass estimates until the model converges.",
              dismiss_id = ns("dismiss_convergence")
            )
          },
          if ("sensitivity" %in% active_warnings()) {
            prior_notice(
              sensitivity(fit()), fit()$meta$priors, ns("open_priors_header"),
              dismiss_id = ns("dismiss_sensitivity"), compact = TRUE
            )
          }
        ))
      }
      if (length(notice_ui) > 0) div(class = "d-flex flex-column gap-2 mb-3", notice_ui)
    })

    # Tabs -----------------------------------------------------------------------

    output$tab_data <- renderUI({
      page <- page()
      switch(page$mode,
        user = panel(
          "Data",
          description = tagList(
            "Sheet ", tags$code(page$name), paste0(" from ", page$file, "."),
            if (cid == "cover") {
              span(
                class = "d-inline-flex flex-wrap align-items-center gap-1 ms-1",
                "One row per plot, with the site-year's", with_help("canopy area", "canopy_area"),
                "and the plot's", with_help("percent cover.", "percent_cover")
              )
            }
          ),
          if (page$validation$level == "warning") {
            notice(
              "alert-triangle", page$validation$message,
              "Fix them in the workbook and upload it again to keep these rows.",
              tone = "warning"
            )
          },
          reactable::reactableOutput(ns("data"))
        ),
        "no-sheet" = empty_state(
          "file-spreadsheet", "No data uploaded",
          sprintf("Add a %s sheet to the workbook, or upload a CSV, to fit this model.", def$sheet),
          div(class = "mt-2", button(ns("to_data"), "Go to data", variant = "outline"))
        ),
        prefit = empty_state(
          "database", "No data needed",
          sprintf("This model was fitted to %s. Choose Your data above to fit it to your own data.", prefit_info[[page$reference]]$data)
        ),
        none = empty_state("minus-circle", "Not used", "Choose a source above to use this model.")
      )
    })

    output$tab_settings <- renderUI({
      switch(page()$mode,
        user = tagList(
          panel(
            "Sampler settings",
            description = "More iterations or more thinning give more reliable estimates, but fitting takes longer.",
            uiOutput(ns("sampler"))
          ),
          panel(
            with_help("Priors", "priors"),
            description = "The defaults suit most data sets.",
            action = button(ns("reset"), "Reset to defaults", "rotate-ccw", "ghost", "sm"),
            uiOutput(ns("priors"))
          )
        ),
        "no-sheet" = empty_state("sliders-horizontal", "No data uploaded", "Settings can be changed once there are data to fit."),
        prefit = empty_state("lock", "Settings are fixed", "Priors and sampler settings were set when the pre-fit model was built."),
        none = empty_state("minus-circle", "Not used", "Choose a source above to use this model.")
      )
    })

    output$tab_diagnostics <- renderUI({
      view <- view()
      if (view$mode == "user" && view$kind == "fitted") {
        return(diagnostics_panel(ns, cid, isolate(fit())))
      }
      switch(view$mode,
        user = fit_pending_state(view$kind, "Fit the model to check R-hat, ESS and the trace plots.", waiting()),
        "no-sheet" = empty_state("file-spreadsheet", "No data uploaded", "Diagnostics appear once the model is fitted to your data."),
        prefit = empty_state("check-circle-2", "Checked when built", "The pre-fit model was checked for convergence when it was built."),
        none = empty_state("minus-circle", "Not used", "Choose a source above to use this model.")
      )
    })

    output$tab_predictions <- renderUI({
      view <- view()
      if (view$mode == "prefit" || (view$mode == "user" && view$kind == "fitted")) {
        return(predictions_panel(ns, cid, view$mode == "prefit", isolate(store$species())))
      }
      switch(view$mode,
        user = fit_pending_state(view$kind, "Fit the model to see its predictions.", waiting()),
        "no-sheet" = empty_state("file-spreadsheet", "No data uploaded", "Predictions appear once the model is fitted to your data."),
        none = empty_state("minus-circle", "Not used", "Choose a source above to use this model.")
      )
    })

    # kb_model_describe() describes a fit, with its priors and the effects it fitted.
    output$tab_description <- renderUI({
      view <- view()
      if (view$mode == "prefit" || (view$mode == "user" && view$kind == "fitted")) {
        return(description_panel(fit()))
      }
      switch(view$mode,
        user = fit_pending_state(view$kind, "Fit the model to see its description, with its priors and fitted effects.", waiting()),
        "no-sheet" = empty_state("file-spreadsheet", "No data uploaded", "The description appears once the model is fitted to your data."),
        none = empty_state("minus-circle", "Not used", "Choose a source above to use this model.")
      )
    })

    output$data <- reactable::renderReactable({
      req(sheet())
      app_table(
        sheet()$rows,
        page_size = 8,
        defaultColDef = reactable::colDef(
          na = "missing",
          style = function(value, ...) if (is.na(value)) list(color = "var(--bs-warning-text-emphasis)")
        )
      )
    })

    # Priors and sampler -----------------------------------------------------------
    # The editors re-render only when the fit starts or stops (to disable them);
    # values are read once and edits flow back into the store.

    # Prior entries differ by species, so inputs are wired for every entry either species has.
    prior_names <- unique(unlist(lapply(names(species_info), function(sp) default_priors(cid, sp)$name)))
    number_input <- function(input_id, label, value, step = "any") {
      div(
        class = "flex-fill",
        numericInput(ns(input_id), span(class = "small text-body-secondary fw-normal", label), value, step = step, width = "100%") |>
          tagAppendAttributes(class = "mb-0")
      )
    }

    output$priors <- renderUI({
      store$species()
      current <- isolate(store$priors()[[cid]])
      # After a fit, mark the priors the sensitivity check found influential.
      influential <- if (kind() == "fitted") {
        rows <- sensitivity(isolate(fit()))
        rows$prior[!rows$weak_prior]
      }
      tags$fieldset(
        disabled = if (busy()) NA,
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
                div(
                  class = "d-flex flex-wrap align-items-center justify-content-between gap-2",
                  tags$code(textOutput(ns(paste0("label_", p$name)), inline = TRUE)),
                  if (p$name %in% influential) warning_badge("Influencing the estimate")
                ),
                div(class = "small text-body-secondary mb-1", p$label),
                div(
                  class = "d-flex gap-2",
                  number_input(paste0(p$name, "_a"), if (p$family == "normal") "Mean" else "Rate", p$a),
                  if (p$family == "normal") number_input(paste0(p$name, "_b"), "SD", p$b)
                )
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
        format_prior(current[current$name == name, ])
      })
      observeEvent(input[[paste0(name, "_a")]], store$update_prior(cid, name, "a", input[[paste0(name, "_a")]]))
      observeEvent(input[[paste0(name, "_b")]], store$update_prior(cid, name, "b", input[[paste0(name, "_b")]]))
    })

    observeEvent(input$reset, {
      store$reset_priors(cid)
      priors <- store$priors()[[cid]]
      for (i in seq_len(nrow(priors))) {
        updateNumericInput(session, paste0(priors$name[i], "_a"), value = priors$a[i])
        if (priors$family[i] == "normal") updateNumericInput(session, paste0(priors$name[i], "_b"), value = priors$b[i])
      }
    })

    sampler_labels <- list(
      chains = help_term("Chains", "Independent runs of the sampler, compared with each other to check convergence."),
      iterations = help_term("Iterations", "Draws kept from each chain (niters)."),
      thin = with_help("Thinning (nthin)", "nthin")
    )
    sampler_steps <- c(chains = 1, iterations = 100, thin = 1)

    output$sampler <- renderUI({
      current <- isolate(store$samplers()[[cid]])
      tags$fieldset(
        disabled = if (busy()) NA,
        div(
          class = "d-flex gap-3", style = "max-width: 32rem",
          lapply(names(sampler_labels), function(field) {
            number_input(paste0("sampler_", field), sampler_labels[[field]], current[[field]], sampler_steps[[field]])
          })
        )
      )
    })

    lapply(names(sampler_labels), function(field) {
      observeEvent(input[[paste0("sampler_", field)]], store$update_sampler(cid, field, input[[paste0("sampler_", field)]]))
    })

    # Diagnostics -----------------------------------------------------------------

    output$convergence <- reactable::renderReactable({
      fit <- fitted_fit()
      conv <- summary(fit)$coefficients[c("term", "rhat", "ess_bulk")]
      flagged <- unname(rhat(fit)[conv$term] > RHAT_MAX | esr(fit)[conv$term] < ESR_MIN)
      conv$status <- ifelse(flagged, "Check", "OK")
      app_table(
        conv,
        pagination = FALSE,
        row_style = function(index) if (flagged[index]) app_row_colour$warning,
        columns = list(
          term = reactable::colDef(name = "Parameter", style = list(fontFamily = "var(--bs-font-monospace)")),
          rhat = reactable::colDef(name = "R-hat", format = reactable::colFormat(digits = 3)),
          ess_bulk = reactable::colDef(name = "ESS", format = reactable::colFormat(digits = 0)),
          status = reactable::colDef(
            name = "Status",
            cell = function(value) if (value == "OK") success_badge("OK", NULL) else warning_badge("Check", NULL)
          )
        )
      )
    })

    output$trace <- render_figure(
      function() kb_plot_trace(fitted_fit()), "trace",
      height = function() 70 + 150 * ceiling(nrow(tidy(fitted_fit())) / 2),
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

    # Predictions: one figure and table per grouping of the predictions.
    prediction_grouping <- reactive({
      choices <- prediction_choices(cid, page()$mode == "prefit", store$species())
      grouping <- input$grouping
      if (is.null(grouping) || !grouping %in% names(choices)) default_grouping(choices) else grouping
    })

    output$prediction_figure <- renderUI({
      grouping <- prediction_grouping()
      panel(
        prediction_groupings[[grouping]],
        figure_plot(
          ns("prediction_plot"), prediction_caption(cid, grouping),
          class = if (grouping == "population") "kb-figure-narrow"
        )
      )
    })

    output$prediction_plot <- render_figure(
      function() {
        grouping <- prediction_grouping()
        plot <- kb_plot_predictions(model_predictions(cid, req(fit()), grouping))
        # The plots the biomass:cover model was fitted to, over its curves.
        if (cid == "cover" && grouping != "site_year") {
          plot <- plot + ggplot2::geom_point(
            ggplot2::aes(.data$plot_percent_cover, .data$biomass_kg_m2),
            data = augment(fit()), alpha = 0.6, inherit.aes = FALSE
          )
        }
        plot
      },
      "prediction_plot",
      aspect = function() prediction_aspect(cid, prediction_grouping()),
      alt = function() sprintf("Predicted %s for the %s model", prediction_info[[cid]]$response, lower_label(cid))
    )

    output$predictions_table <- reactable::renderReactable({
      rows <- model_predictions(cid, req(fit()), prediction_grouping(), at = TRUE)
      number <- function(name) reactable::colDef(name = name, cell = number_text, align = "right")
      app_table(rows[intersect(c("site", "year", "estimate", "lower", "upper"), names(rows))], columns = Filter(Negate(is.null), list(
        site = if ("site" %in% names(rows)) reactable::colDef(name = "Site"),
        year = if ("year" %in% names(rows)) reactable::colDef(name = "Year"),
        estimate = number("Estimate"),
        lower = number("Lower"),
        upper = number("Upper")
      )))
    })

    output$sensitivity_notices <- renderUI({
      fit <- fitted_fit()
      div(
        class = "d-flex flex-column gap-2",
        sensitivity_notices(
          sensitivity(fit), fit$meta$priors, ns("open_priors"), ns("dismiss_sensitivity_section"),
          dismissed = store$is_dismissed(cid, "sensitivity")
        )
      )
    })

    output$sensitivity <- reactable::renderReactable({
      rows <- sensitivity(fitted_fit())[c("parameter", "prior_cjs", "lik_cjs", "weak_prior", "strong_data")]
      flag <- function(value) if (value) success_badge("Yes", NULL) else warning_badge("No", NULL)
      app_table(
        rows,
        pagination = FALSE,
        columns = list(
          parameter = reactable::colDef(name = "Parameter", style = list(fontFamily = "var(--bs-font-monospace)")),
          prior_cjs = reactable::colDef(name = "Prior CJS", format = reactable::colFormat(digits = 3)),
          lik_cjs = reactable::colDef(name = "Likelihood CJS", format = reactable::colFormat(digits = 3)),
          weak_prior = reactable::colDef(name = "Weak prior", cell = flag),
          strong_data = reactable::colDef(name = "Strong data", cell = flag)
        )
      )
    })
  })
}

fit_pending_state <- function(kind, description, waiting = NULL) {
  if (kind %in% c("queued", "fitting") || isTRUE(waiting$queued)) {
    return(empty_state("loader-2", "Fitting", "Results appear here when the fit finishes."))
  }
  if (kind == "waiting") {
    return(empty_state("clock", waiting_text(waiting), "Results appear here once those models are ready and this one is fitted."))
  }
  empty_state("play", "Not fitted yet", description)
}

# The diagnostics of a fit to your data: convergence and trace plots, posterior
# predictive checks and prior sensitivity.
diagnostics_panel <- function(ns, cid, fit) {
  section <- function(title, value, content) {
    if (value %in% names(marked_pills)) title <- marked_title(title, ns(paste0("mark_pill_", value)))
    nav_panel(title, value = value, div(class = "pt-3", content))
  }
  navset_pill(
    id = ns("diagnostics_section"),
    section("Sampler diagnostics", "sampler", convergence_panel(ns, fit)),
    section("PPC", "ppc", ppc_panel(ns)),
    section("Prior sensitivity", "sensitivity", sensitivity_panel(ns, fit))
  )
}

sensitivity_panel <- function(ns, fit) {
  panel(
    with_help("Prior sensitivity", "sensitivity"),
    description = sprintf(
      "A prior CJS above %s means the prior is informative; a likelihood CJS below %s means the data say little about the parameter.",
      PRIOR_CJS_MAX, LIK_CJS_MIN
    ),
    uiOutput(ns("sensitivity_notices")),
    reactable::reactableOutput(ns("sensitivity"))
  )
}

# The captions name the legend labels.
ppc_panel <- function(ns) {
  div(
    class = "d-flex flex-column gap-3",
    div(
      div(class = "kb-card-title", with_help("Posterior predictive check", "ppc")),
      div(class = "small text-body-secondary mt-1", "Datasets simulated from the fitted model, compared with your data.")
    ),
    panel(
      "Density overlay",
      description = "The dark line for your data should sit within the light lines for the simulated data.",
      figure_plot(
        ns("ppc_dens"), HTML("Densities of your data (<em>y</em>) and of 50 datasets simulated from the fitted model (<em>y</em><sub>rep</sub>).")
      )
    ),
    panel(
      "Deviance residuals overlay",
      description = "The same check on the residual scale: the shapes should match.",
      figure_plot(
        ns("ppc_resid"), HTML("Densities of the deviance residuals of your data (<em>y</em>) and of the 50 simulated datasets (<em>y</em><sub>rep</sub>).")
      )
    )
  )
}

# ESS is judged against the draws kept: ESR_MIN of the default 4 chains of 1000 draws is 400.
convergence_panel <- function(ns, fit) {
  panel(
    "Convergence",
    description = tagList(
      "Parameters with ", with_help("R-hat", "rhat"), sprintf(" above %s or ", RHAT_MAX),
      with_help("effective sample size (ESS)", "ess"), sprintf(" below %s%% of the draws are flagged.", ESR_MIN * 100)
    ),
    if (is_converged(fit)) notice("check-circle-2", "All parameters converged", tone = "muted"),
    reactable::reactableOutput(ns("convergence")),
    figure_plot(ns("trace"), "Draws for each chain by iteration. Well-mixed chains overlap.")
  )
}

# By site where the model has site effects, as that is what most users want
# to see; pre-fit models show the population level only, as their sites are
# the reference data's rather than the user's.
default_grouping <- function(choices) if ("site" %in% names(choices)) "site" else "population"

predictions_panel <- function(ns, cid, prefit, species) {
  choices <- prediction_choices(cid, prefit, species)
  note <- if (prefit) {
    "Predictions here are at the population level. Biomass estimates use site-level estimates for sites in the reference data."
  } else if (!has_year_effect(cid, species)) {
    "This model has a site effect but no year effect, so there are no predictions by year."
  }
  tagList(
    div(
      class = "d-flex flex-wrap align-items-center column-gap-3 row-gap-1 mb-3",
      span(class = "small fw-medium", with_help(if (prefit) "Population level" else "Group by", "prediction_groups")),
      if (length(choices) > 1) {
        radioButtons(
          ns("grouping"), NULL,
          choices = stats::setNames(names(choices), choices), selected = default_grouping(choices), inline = TRUE
        ) |>
          tagAppendAttributes(class = "mb-0")
      },
      if (!is.null(note)) div(class = "small text-body-secondary w-100", note)
    ),
    plot_estimates_nav(
      ns("prediction_section"),
      uiOutput(ns("prediction_figure")),
      panel(
        "Estimates",
        description = with_help(
          sprintf("%s with 95%% compatibility intervals, to 3 significant figures.", prediction_info[[cid]]$table), "interval"
        ),
        reactable::reactableOutput(ns("predictions_table"))
      )
    )
  )
}

# kb_model_describe() prints the description and returns its lines invisibly.
description_panel <- function(fit) {
  lines <- withr::with_output_sink(nullfile(), kb_model_describe(fit))
  panel(
    "Model description",
    description = "Likelihood, random effects and priors",
    tags$pre(class = "bg-body-tertiary rounded-3 p-3 small mb-0", paste(lines, collapse = "\n"))
  )
}

# A reactiveVal only invalidates when its value changes, so routing a reactive
# through one stops downstream outputs re-rendering on unrelated state changes.
dedupe <- function(r) {
  value <- reactiveVal()
  observe(value(r()), priority = 100)
  value
}
