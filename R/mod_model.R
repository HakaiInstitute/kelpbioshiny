# One model's detail page: a header (name, source, status, fit button and
# progress) above tabs for data, settings, diagnostics, predictions and the
# model description. A single module, instantiated per model. A pre-fit model
# has no data, settings or diagnostics of its own, so it shows only the
# predictions and description tabs.

model_tabs <- c(data = "Data", settings = "Settings", diagnostics = "Diagnostics", predictions = "Predictions", description = "Description")
own_fit_tabs <- c("data", "settings", "diagnostics")

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
    div(
      class = "d-flex flex-wrap align-items-start justify-content-between gap-3 mb-3",
      div(
        h2(class = "kb-page-title", def$label),
        div(class = "kb-lead text-body-secondary", def$detail),
        if (cid == "weight") uiOutput(ns("density_note"))
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

    # Routed through dedupe(), so the page re-renders only when this model's
    # status or fit changes, not when another model's does.
    status <- dedupe(reactive(store$statuses()[[cid]]))
    kind <- reactive(status()$kind)
    sheet <- reactive(store$sheets()[[cid]])
    page <- dedupe(reactive({
      source <- store$sources()[[cid]]
      sheet <- sheet()
      mode <- if (is_prefit(source)) "prefit" else if (source == "none") "none" else if (is.null(sheet)) "no-sheet" else "user"
      reference <- if (is_prefit(source)) prefit_reference(source)
      list(mode = mode, reference = reference, name = sheet$name, file = sheet$file)
    }))
    # The kelpbio fit behind every number and figure: the pre-fit model, or the
    # fit to your data once fitted.
    fit <- dedupe(reactive(store$fit_of(cid)))
    fitted_fit <- reactive(req(fit(), page()$mode == "user"))
    errors <- reactive(store$setting_errors()[[cid]])

    # A pre-fit model shows only its predictions and description; the page opens
    # on the first tab shown.
    prefit_mode <- dedupe(reactive(page()$mode == "prefit"))
    observeEvent(prefit_mode(), {
      prefit <- prefit_mode()
      for (tab in own_fit_tabs) {
        if (prefit) nav_hide("tab", tab, session = session) else nav_show("tab", tab, session = session)
      }
      nav_select("tab", if (prefit) "predictions" else "data", session = session)
    })

    sync_source_select(input, session, "source", cid, store)

    observeEvent(input$back, store$open("hub"))
    observeEvent(input$to_data, store$go_to("data"))
    observeEvent(input$fit, store$queue_fits(cid))
    observeEvent(input$cancel, store$cancel_fits())
    observeEvent(input$open_settings, store$open_settings(cid))
    observeEvent(input$open_priors, store$open_settings(cid))
    observeEvent(store$tab_request(), {
      request <- store$tab_request()
      if (identical(request$id, cid)) nav_select("tab", request$tab, session = session)
    })

    output$density_note <- renderUI({
      if (store$species() == "nereo") {
        div(class = "small text-body-secondary mt-1", with_help("Uses the stipe density in the density data", "weight_density"))
      }
    })

    output$status <- renderUI({
      status <- status()
      status_badge(status, if (status$kind == "fitting") store$progress() else 0)
    })

    output$fit_action <- renderUI({
      status <- status()
      if (status$source != "user" || status$kind %in% c("no-data", "data-error")) {
        return(NULL)
      }
      if (is_pending_status(status)) {
        return(button(ns("cancel"), "Cancel", "x", "outline"))
      }
      refit <- status$kind %in% c("ready", "failed")
      button(
        ns("fit"), if (refit) "Refit" else "Fit model", "play", if (refit) "outline" else "primary",
        disabled = !can_fit(status) || cid %in% store$invalid()
      )
    })

    output$fit_progress <- renderUI({
      req(kind() == "fitting")
      progress <- store$progress()
      div(
        class = "d-flex flex-column gap-2 mb-3",
        progress_bar(progress),
        div(
          class = "d-flex justify-content-between small text-body-secondary kb-tabular",
          span("Sampling"),
          span(sprintf("%d%%", floor(progress)))
        )
      )
    })

    # One notice per warning, whichever tab is open.
    output$notices <- renderUI({
      status <- status()
      mode <- page()$mode
      notice_ui <- if (mode == "prefit" && status$kind != "failed") {
        notice(
          "layout-list", with_help(sprintf("Fitted in advance to %s", prefit_info[[page()$reference]]$data), "prefit"),
          "No fitting is needed: biomass uses the stored draws."
        )
      } else if (mode == "none") {
        notice(
          "minus-circle", sprintf("%s is not used in this run", def$label),
          if (cid == "cover") {
            "Add a cover sheet with canopy area to estimate total biomass per site-year."
          } else {
            sprintf("Outputs that need %s are unavailable on the Biomass step.", tolower(def$label))
          },
          tone = "muted"
        )
      } else if (status$kind == "data-error") {
        notice(
          "x-circle", "The data check failed", status$message, "Correct the sheet and upload it again.",
          tone = "warning", action = button(ns("to_data"), "Go to data", variant = "outline", size = "sm")
        )
      } else if (status$kind == "failed") {
        notice("x-circle", "The fit failed", status$message, tone = "warning")
      } else if (status$kind == "not-fitted" && status$blocked) {
        notice("clock", "Fitted once biomass per unit area is ready", "Every other model in use must be ready first.", tone = "muted")
      } else if (mode == "user" && status$kind == "ready") {
        tagList(
          if (has_warning(status)) convergence_notice("Convergence warning", ns("open_settings")),
          prior_notice(kb_sensitivity(fit()), fit()$meta$priors, ns("open_priors"))
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
          reactable::reactableOutput(ns("data"))
        ),
        "no-sheet" = no_sheet(sprintf("Add a %s sheet to the workbook, or upload a CSV, to fit this model.", def$sheet)),
        none = not_used(),
        NULL
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
        "no-sheet" = no_sheet("Settings can be changed once there are data to fit."),
        none = not_used(),
        NULL
      )
    })

    output$tab_diagnostics <- renderUI({
      mode <- page()$mode
      if (mode == "user" && kind() == "ready") {
        return(diagnostics_panel(ns))
      }
      switch(mode,
        user = fit_pending_state(kind(), "Fit the model to check its convergence, posterior predictive check and prior sensitivity."),
        "no-sheet" = no_sheet("Diagnostics appear once the model is fitted to your data."),
        none = not_used(),
        NULL
      )
    })

    output$tab_predictions <- renderUI({
      mode <- page()$mode
      if (!is.null(fit())) {
        return(predictions_panel(ns, cid, mode == "prefit", isolate(store$species())))
      }
      switch(mode,
        user = fit_pending_state(kind(), "Fit the model to see its predictions."),
        "no-sheet" = no_sheet("Predictions appear once the model is fitted to your data."),
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
        user = fit_pending_state(kind(), "Fit the model to see its description, with its priors and fitted effects."),
        "no-sheet" = no_sheet("The description appears once the model is fitted to your data."),
        none = not_used(),
        NULL
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
      influential <- if (kind() == "ready" && !is.null(isolate(fit()))) {
        rows <- kb_sensitivity(isolate(fit()))
        rows$prior[!rows$weak_prior]
      }
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
        format_prior(current[current$name == name, ])
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
        if (priors$family[i] == "normal") updateNumericInput(session, paste0(priors$name[i], "_b"), value = priors$b[i])
      }
    })

    sampler_labels <- list(
      chains = help_term("Chains", "Independent runs of the sampler, compared with each other to check convergence."),
      niters = help_term("Iterations (niters)", "Draws kept from each chain."),
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
      rows <- kb_convergence(fitted_fit())[c("term", "rhat", "ess_bulk", "converged")]
      app_table(
        rows,
        pagination = FALSE,
        row_style = function(index) if (!rows$converged[index]) app_row_colour$warning,
        columns = list(
          term = reactable::colDef(name = "Parameter", style = list(fontFamily = "var(--bs-font-monospace)")),
          rhat = reactable::colDef(name = "R-hat"),
          ess_bulk = reactable::colDef(name = "ESS"),
          converged = reactable::colDef(
            name = "Status",
            cell = function(value) if (value) success_badge("OK", NULL) else warning_badge("Check", NULL)
          )
        )
      )
    })

    output$convergence_summary <- renderUI({
      if (converged(fitted_fit())) notice("check-circle-2", "All parameters converged", tone = "muted")
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

    output$sensitivity_note <- renderUI(data_strength_notice(kb_sensitivity(fitted_fit())))

    output$sensitivity <- reactable::renderReactable({
      rows <- kb_sensitivity(fitted_fit())[c("parameter", "prior_cjs", "lik_cjs", "weak_prior", "strong_data")]
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

fit_pending_state <- function(kind, description) {
  if (kind %in% c("queued", "fitting")) {
    return(empty_state("loader-2", "Fitting", "Results appear here when the fit finishes."))
  }
  if (kind == "failed") {
    return(empty_state("x-circle", "The fit failed", "Refit the model to see its results."))
  }
  if (kind == "data-error") {
    return(empty_state("x-circle", "The data check failed", "Correct the sheet and upload it again to fit this model."))
  }
  empty_state("play", "Not fitted yet", description)
}

# The diagnostics of a fit to your data, in one scrolling tab: convergence and
# trace plots, posterior predictive checks and prior sensitivity.
diagnostics_panel <- function(ns) {
  div(class = "d-flex flex-column gap-3", convergence_panel(ns), ppc_panel(ns), sensitivity_panel(ns))
}

sensitivity_panel <- function(ns) {
  panel(
    with_help("Prior sensitivity", "sensitivity"),
    description = sprintf(
      "A prior CJS above %s means the prior is informative; a likelihood CJS below %s means the data say little about the parameter.",
      default_arg(kb_sensitivity, "prior_cjs"), default_arg(kb_sensitivity, "lik_cjs")
    ),
    uiOutput(ns("sensitivity_note")),
    reactable::reactableOutput(ns("sensitivity"))
  )
}

# The captions name the legend labels.
ppc_panel <- function(ns) {
  panel(
    with_help("Posterior predictive check", "ppc"),
    description = "Datasets simulated from the fitted model, compared with your data.",
    div(class = "fw-medium", "Density overlay"),
    figure_plot(
      ns("ppc_dens"),
      HTML("Densities of your data (<em>y</em>) and of 50 datasets simulated from the fitted model (<em>y</em><sub>rep</sub>). The dark line for your data should sit within the light lines.")
    ),
    div(class = "fw-medium", "Deviance residuals overlay"),
    figure_plot(
      ns("ppc_resid"),
      HTML("Densities of the deviance residuals of your data (<em>y</em>) and of the 50 simulated datasets (<em>y</em><sub>rep</sub>). The shapes should match.")
    )
  )
}

convergence_panel <- function(ns) {
  panel(
    "Convergence",
    description = tagList(
      "Parameters with ", with_help("R-hat", "rhat"), sprintf(" above %s or ", default_arg(kb_convergence, "rhat")),
      with_help("effective sample size (ESS)", "ess"),
      sprintf(" below %s%% of the draws are flagged.", 100 * default_arg(kb_convergence, "esr"))
    ),
    uiOutput(ns("convergence_summary")),
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
  } else if (!has_effect(cid, "site", species)) {
    "This model has no site or year effects, so its predictions are at the population level."
  } else if (!has_effect(cid, "year", species)) {
    "This model has a site effect but no year effect, so there are no predictions by year."
  }
  tagList(
    div(
      class = "d-flex flex-wrap align-items-center column-gap-3 row-gap-1 mb-3",
      span(class = "small fw-medium", with_help(if (length(choices) > 1) "Group by" else "Population level", "prediction_groups")),
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
