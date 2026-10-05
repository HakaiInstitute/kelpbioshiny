# Models step: a sub-navigation, the hub (sources, statuses, fit queue) and one
# detail page per model (mod_model.R), switched with a hidden navset.

source_choices <- function(id, has_sheet) {
  options <- components[[id]]$sources
  labels <- vapply(options, source_label, character(1))
  if (!has_sheet) labels[options == "user"] <- "Your data (no sheet)"
  stats::setNames(options, labels)
}

source_select <- function(input_id, id) {
  if (length(components[[id]]$sources) == 1) {
    return(span(source_label(components[[id]]$sources)))
  }
  initial <- default_source(id, FALSE)
  selectInput(input_id, NULL, choices = source_choices(id, FALSE), selected = initial, selectize = FALSE, width = "12rem") |>
    tagAppendAttributes(class = "mb-0") |>
    tagAppendAttributes(.cssSelector = "select", `aria-label` = sprintf("Source of the %s model", lower_label(id)))
}

# Keeps a source select and the store in step, in both directions. A change that
# would discard fits asks first; Cancel puts the select back.
sync_source_select <- function(input, session, input_id, id, store) {
  if (length(components[[id]]$sources) == 1) {
    return()
  }
  observeEvent(input[[input_id]], {
    value <- input[[input_id]]
    if (!identical(value, store$sources()[[id]])) {
      store$confirm_reset(
        id,
        function() store$set_source(id, value),
        function() updateSelectInput(session, input_id, selected = store$sources()[[id]])
      )
    }
  }, ignoreInit = TRUE)
  observe({
    has_sheet <- !is.null(store$sheets()[[id]])
    updateSelectInput(session, input_id, choices = source_choices(id, has_sheet), selected = store$sources()[[id]])
  })
}

mod_models_ui <- function(id) {
  ns <- NS(id)

  rows <- lapply(component_ids, function(cid) {
    tags$tr(
      tags$td(
        class = "ps-4",
        actionLink(ns(paste0("open_", cid)), label_of(cid), class = "fw-medium link-body-emphasis link-underline-opacity-0 link-underline-opacity-100-hover"),
        div(class = "small text-body-secondary", components[[cid]]$detail)
      ),
      # Phones show only the model and its status; the model page has the
      # source and the fit control.
      tags$td(class = "d-none d-sm-table-cell", source_select(ns(paste0("source_", cid)), cid)),
      tags$td(uiOutput(ns(paste0("status_", cid)))),
      # The fit control, then a chevron that opens the model's page.
      tags$td(
        class = "pe-4 text-end text-nowrap d-none d-sm-table-cell",
        uiOutput(ns(paste0("fit_", cid)), inline = TRUE),
        actionLink(
          ns(paste0("chevron_", cid)), lucide("chevron-right"),
          class = "text-body-secondary ms-2 align-middle", `aria-label` = sprintf("Open the %s model", lower_label(cid))
        )
      )
    )
  })
  # The cover model is optional and builds on the others, so it sits in its own group.
  group_row <- tags$tr(tags$td(
    colspan = 4, class = "ps-4 py-2 bg-body-tertiary small text-body-secondary",
    span(class = "fw-medium text-body", "Total site biomass"), " (optional): fitted after the models above"
  ))
  rows <- append(rows, list(group_row), after = length(biomass_ids))
  head_cell <- function(...) tags$th(class = "bg-body-tertiary text-body-secondary small", ...)

  hub <- tagList(
    page_header("Models", step_description("models"), uiOutput(ns("fit_all_ui"), inline = TRUE)),
    # While fits run: the model fitting, a full-width bar and the fits queued.
    # The banner is part of the page; only its text and bar update.
    conditionalPanel(
      "output.fitting_active",
      fit_progress(
        textOutput(ns("progress_title"), inline = TRUE),
        textOutput(ns("progress_detail"), inline = TRUE),
        uiOutput(ns("progress_bar"))
      )
    ),
    conditionalPanel(
      "!output.has_data",
      ns = ns,
      empty_state(
        "file-spreadsheet", "Add data first",
        "Upload a workbook or use the example workbook.",
        div(class = "mt-2", button(ns("to_data"), "Go to data", variant = "outline"))
      )
    ),
    conditionalPanel(
      "output.has_data",
      ns = ns,
      uiOutput(ns("skipped")),
      card(
        class = "overflow-hidden",
        card_body(
          padding = 0,
          div(
            class = "table-responsive",
            tags$table(
              class = "table table-hover align-middle mb-0",
              # Fixed widths, so the columns stay put as the status badges change.
              tags$thead(tags$tr(
                head_cell(class = "ps-4", "Model"),
                head_cell(class = "d-none d-sm-table-cell", style = "width: 14rem", with_help("Source", "prefit")),
                head_cell(style = "width: 13rem", "Status"),
                head_cell(class = "pe-4 d-none d-sm-table-cell", style = "width: 9rem", span(class = "visually-hidden", "Fit"))
              )),
              tags$tbody(rows)
            )
          )
        )
      ),
      uiOutput(ns("readiness"))
    )
  )

  pages <- unname(lapply(component_ids, function(cid) nav_panel_hidden(cid, mod_model_ui(ns(cid), cid))))

  step_layout(
    card(card_body(padding = "0.5rem", uiOutput(ns("subnav")))),
    do.call(navset_hidden, c(list(id = ns("view"), nav_panel_hidden("hub", hub)), pages))
  )
}

mod_models_server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    for (cid in component_ids) mod_model_server(cid, store)

    observe(nav_select("view", store$open(), session = session))

    output$has_data <- reactive(store$has_data())
    outputOptions(output, "has_data", suspendWhenHidden = FALSE)

    observeEvent(input$hub, store$open("hub"))
    observeEvent(input$to_data, store$go_to("data"))
    observeEvent(input$fit_all, store$fit_all())
    observeEvent(input$cancel, store$cancel_fits())
    observeEvent(input$continue, store$go_to("estimates"))

    lapply(component_ids, function(cid) {
      observeEvent(input[[paste0("open_", cid)]], store$open(cid))
      observeEvent(input[[paste0("chevron_", cid)]], store$open(cid))
      observeEvent(input[[paste0("nav_", cid)]], store$open(cid))
      observeEvent(input[[paste0("fit_model_", cid)]], store$queue_fits(cid))
      observeEvent(input[[paste0("cancel_", cid)]], store$cancel_fit(cid))
      sync_source_select(input, session, paste0("source_", cid), cid, store)
      # The badge, then a link only when the model needs action (status_next()).
      # An error is in full on the Data step and the model's page, so a long
      # message does not crowd the column. A badge with warnings opens the
      # diagnostics.
      output[[paste0("status_", cid)]] <- renderUI({
        status <- store$statuses()[[cid]]
        badge <- status_badge(status)
        if (has_warning(status)) {
          badge <- actionLink(
            session$ns(paste0("badge_", cid)), badge,
            class = "text-decoration-none", `aria-label` = sprintf("%s: open the diagnostics", status_label(status))
          )
        }
        step <- status_next(status)
        tagList(badge, if (!is.null(step)) div(class = "small mt-1", actionLink(session$ns(paste0("next_", cid)), step$label)))
      })
      observeEvent(input[[paste0("badge_", cid)]], store$open_tab(cid, "diagnostics"))
      observeEvent(input[[paste0("next_", cid)]], {
        step <- status_next(store$statuses()[[cid]])
        if (identical(step$target, "data")) store$go_to("data") else store$open_tab(cid, step$target)
      })
      output[[paste0("fit_", cid)]] <- renderUI({
        fit_control(
          session$ns(paste0("fit_model_", cid)), session$ns(paste0("cancel_", cid)), store$statuses()[[cid]],
          cid %in% store$invalid(), variant = "soft", size = "sm", model = cid
        )
      })
    })

    output$fit_all_ui <- renderUI({
      if (!store$has_data()) {
        return(NULL)
      }
      if (!is.null(store$fitting())) {
        return(button(session$ns("cancel"), "Cancel", "x", "outline"))
      }
      plan <- store$fit_plan()
      if (length(plan$ids) > 0) {
        return(button(session$ns("fit_all"), "Fit all", "play"))
      }
      # Nothing left to fit: the next step is the estimates, as on the Data step.
      # With no estimate yet, say why Fit all has nothing to fit.
      if (store$any_estimate()) {
        return(button(session$ns("continue"), span("Continue to estimates ", lucide("arrow-right"))))
      }
      span(class = "small text-body-secondary", fit_all_reason(store$statuses(), plan))
    })

    output$progress_title <- renderText(sprintf("Fitting the %s model", lower_label(req(store$fitting()))))
    output$progress_detail <- renderText({
      req(store$fitting())
      queued <- sum(record_status(store$records()) == "queued")
      paste0(percent_text(store$progress()), if (queued > 0) sprintf(" \u00b7 %d queued", queued))
    })
    output$progress_bar <- renderUI(progress_bar(store$progress()))

    output$skipped <- renderUI({
      skipped <- store$fit_plan()$skipped
      if (length(skipped) > 0) {
        notice(
          "sliders-horizontal", "Fit all skips some models",
          sprintf("%s: a prior or sampler setting is invalid. Correct it on the model's Settings tab.", and_list(vapply(skipped, label_of, ""))),
          tone = "muted"
        ) |>
          tagAppendAttributes(class = "mb-3")
      }
    })

    # The Status column says what each model needs, so only a problem across
    # models shows here.
    output$readiness <- renderUI({
      mismatches <- store$mismatches()
      if (length(mismatches) > 0) {
        notice(
          "alert-triangle", warning_help$mismatch$title,
          sprintf("%s. %s", and_list(sprintf("\"%s\"", mismatches)), mismatch_advice),
          tone = "warning"
        )
      }
    })

    output$subnav <- renderUI({
      open <- store$open()
      statuses <- store$statuses()
      tags$nav(
        class = "nav nav-pills flex-column gap-1",
        `aria-label` = "Models",
        subnav_link(session$ns("hub"), open == "hub", lucide("layout-list", "text-body-secondary"), "All models"),
        tags$hr(class = "my-1"),
        lapply(component_ids, function(cid) {
          tagList(
            if (cid == "cover") tags$hr(class = "my-1"),
            subnav_link(session$ns(paste0("nav_", cid)), open == cid, status_icon(statuses[[cid]]), label_of(cid), status_hidden(statuses[[cid]]))
          )
        })
      )
    })
  })
}

# The action a model in the Models list needs, by its status: data to correct
# or a failed fit to look at. Other statuses need none: the defaults suit most
# runs, and the model's page (its name, the chevron) holds its settings and
# diagnostics for those who want them.
status_next <- function(status) {
  switch(status$kind,
    "data-error" = ,
    "no-data" = list(label = "Go to data", target = "data"),
    "failed" = list(label = "Details", target = "diagnostics"),
    NULL
  )
}

# Why Fit all has nothing to fit.
fit_all_reason <- function(statuses, plan) {
  if (length(plan$skipped) > 0) {
    return("Fix the settings marked in red")
  }
  own <- Filter(function(status) status$source == "user", statuses)
  if (all(vapply(own, function(status) status$kind == "ready", logical(1)))) {
    return("All models are fitted")
  }
  "No model can be fitted yet"
}
