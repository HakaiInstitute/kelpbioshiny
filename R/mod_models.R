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
        actionLink(ns(paste0("open_", cid)), label_of(cid), class = "fw-medium text-body"),
        div(class = "small text-body-secondary", components[[cid]]$detail)
      ),
      # Phones show only the model and its status; the model page has the source.
      tags$td(class = "d-none d-sm-table-cell", source_select(ns(paste0("source_", cid)), cid)),
      tags$td(class = "pe-4", uiOutput(ns(paste0("status_", cid))))
    )
  })
  # The cover model is optional and builds on the others, so it sits in its own group.
  group_row <- tags$tr(tags$td(
    colspan = 3, class = "ps-4 py-2 bg-body-tertiary small text-body-secondary",
    span(class = "fw-medium text-body", "Total biomass"), " (optional): fitted after the models above"
  ))
  rows <- append(rows, list(group_row), after = length(biomass_ids))
  head_cell <- function(...) tags$th(class = "bg-body-tertiary text-body-secondary small", ...)

  hub <- tagList(
    page_header("Models", uiOutput(ns("description"), inline = TRUE), uiOutput(ns("fit_all_ui"), inline = TRUE)),
    conditionalPanel(
      "!output.has_density",
      ns = ns,
      empty_state(
        "file-spreadsheet", "Add data first",
        "Density data are required. Upload a workbook or use the example workbook.",
        div(class = "mt-2", button(ns("to_data"), "Go to data", variant = "outline"))
      )
    ),
    conditionalPanel(
      "output.has_density",
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
                head_cell(class = "pe-4", style = "width: 13rem", "Status")
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

    output$has_density <- reactive(store$has_density())
    outputOptions(output, "has_density", suspendWhenHidden = FALSE)

    observeEvent(input$hub, store$open("hub"))
    observeEvent(input$to_data, store$go_to("data"))
    observeEvent(input$fit_all, store$fit_all())
    observeEvent(input$cancel, store$cancel_fits())
    observeEvent(input$view_biomass, store$go_to("biomass"))

    lapply(component_ids, function(cid) {
      observeEvent(input[[paste0("open_", cid)]], store$open(cid))
      observeEvent(input[[paste0("nav_", cid)]], store$open(cid))
      observeEvent(input[[paste0("refit_", cid)]], store$queue_fits(cid))
      sync_source_select(input, session, paste0("source_", cid), cid, store)
      output[[paste0("status_", cid)]] <- renderUI({
        status <- store$statuses()[[cid]]
        tagList(
          status_badge(status, if (status$kind == "fitting") store$progress() else 0),
          if (!is.null(status$message)) div(class = "small text-danger-emphasis mt-1", status$message),
          if (status$kind == "failed" && status$source == "user") {
            actionLink(session$ns(paste0("refit_", cid)), "Refit", class = "small")
          }
        )
      })
    })

    output$description <- renderUI({
      sp <- species_info[[store$species()]]
      tagList(step_description("models"), " Species: ", species_phrase(sp, "."))
    })

    output$fit_all_ui <- renderUI({
      if (!store$has_density()) {
        return(NULL)
      }
      if (!is.null(store$fitting())) {
        return(button(session$ns("cancel"), "Cancel", "x", "outline"))
      }
      plan <- store$fit_plan()
      if (length(plan$ids) > 0) {
        return(button(session$ns("fit_all"), "Fit all", "play"))
      }
      span(class = "small text-body-secondary", fit_all_reason(store$statuses(), plan))
    })

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

    output$readiness <- renderUI({
      mismatches <- store$mismatches()
      if (length(mismatches) > 0) {
        return(notice(
          "alert-triangle", warning_help$mismatch$title,
          sprintf("%s. %s", and_list(sprintf("\"%s\"", mismatches)), mismatch_advice),
          tone = "warning"
        ))
      }
      if (store$biomass_ready()) {
        cover <- store$statuses()$cover$kind
        return(notice(
          "arrow-right", "All models are ready",
          if (cover %in% c("not-fitted", "queued", "fitting")) {
            "Biomass per unit area can now be estimated. Total biomass follows once the cover model is fitted."
          } else {
            "Biomass can now be estimated."
          },
          action = button(session$ns("view_biomass"), "View biomass", size = "sm")
        ))
      }
      statuses <- store$statuses()
      blocking <- store$blocking()
      notice(
        "layout-list", "Before biomass can be estimated",
        paste(sprintf("%s: %s", vapply(blocking, label_of, ""), vapply(statuses[blocking], block_reason, "")), collapse = "; "),
        tone = "muted"
      )
    })

    output$subnav <- renderUI({
      open <- store$open()
      statuses <- store$statuses()
      item <- function(input_id, active, ...) {
        actionLink(session$ns(input_id), div(class = "d-flex align-items-center gap-2", ...),
          class = paste("nav-link py-2 px-2", if (active) "active fw-medium")
        )
      }
      tags$nav(
        class = "nav nav-pills flex-column gap-1",
        `aria-label` = "Models",
        item("hub", open == "hub", lucide("layout-list", "text-body-secondary"), "All models"),
        tags$hr(class = "my-1"),
        lapply(component_ids, function(cid) {
          tagList(
            if (cid == "cover") tags$hr(class = "my-1"),
            item(paste0("nav_", cid), open == cid, status_icon(statuses[[cid]]), span(class = "flex-grow-1", label_of(cid), status_hidden(statuses[[cid]])))
          )
        })
      )
    })
  })
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
