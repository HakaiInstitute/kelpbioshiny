# Models step: a sub-navigation, the hub (sources, statuses, fit queue) and one
# detail page per model (mod_model.R), switched with a hidden navset.

source_choices <- function(id, has_sheet) {
  options <- components[[id]]$sources
  labels <- vapply(options, function(option) source_label(id, option), character(1))
  if (!has_sheet) labels[options == "user"] <- "Your data (no sheet)"
  stats::setNames(options, labels)
}

source_select <- function(input_id, id) {
  if (length(components[[id]]$sources) == 1) {
    return(span(source_label(id, components[[id]]$sources)))
  }
  initial <- default_source(id, FALSE)
  selectInput(input_id, NULL, choices = source_choices(id, FALSE), selected = initial, selectize = FALSE, width = "12rem") |>
    tagAppendAttributes(class = "mb-0")
}

# Keeps a source select and the store in step, in both directions.
sync_source_select <- function(input, session, input_id, id, store) {
  if (length(components[[id]]$sources) == 1) {
    return()
  }
  observeEvent(input[[input_id]], store$set_source(id, input[[input_id]]), ignoreInit = TRUE)
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
      tags$td(source_select(ns(paste0("source_", cid)), cid)),
      tags$td(uiOutput(ns(paste0("status_", cid)))),
      tags$td(
        class = "pe-4 text-end",
        actionLink(ns(paste0("go_", cid)), lucide("chevron-right"), class = "text-body-secondary", `aria-label` = paste("Open", label_of(cid)))
      )
    )
  })
  # The cover model is optional and builds on the others, so it sits in its own group.
  group_row <- tags$tr(tags$td(
    colspan = 4, class = "ps-4 py-2 bg-body-tertiary small text-body-secondary",
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
      conditionalPanel(
        "output.fitting_active",
        ns = ns,
        panel(
          textOutput(ns("fitting_title"), inline = TRUE),
          description = textOutput(ns("fitting_detail"), inline = TRUE),
          action = button(ns("cancel"), "Cancel", "x", "outline", "sm"),
          uiOutput(ns("fitting_progress"))
        )
      ),
      card(
        class = "overflow-hidden",
        card_body(
          padding = 0,
          tags$table(
            class = "table table-hover align-middle mb-0",
            tags$thead(tags$tr(head_cell(class = "ps-4", "Model"), head_cell(with_help("Source", "prefit")), head_cell("Status"), head_cell())),
            tags$tbody(rows)
          )
        )
      ),
      uiOutput(ns("warnings")),
      uiOutput(ns("readiness"))
    )
  )

  pages <- unname(lapply(component_ids, function(cid) nav_panel_hidden(cid, mod_model_ui(ns(cid), cid))))

  layout_columns(
    col_widths = c(3, 9),
    div(class = "kb-aside", card(card_body(padding = "0.5rem", uiOutput(ns("subnav"))))),
    do.call(navset_hidden, c(list(id = ns("view"), nav_panel_hidden("hub", hub)), pages))
  )
}

mod_models_server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    for (cid in component_ids) mod_model_server(cid, store)

    observe(nav_select("view", store$open(), session = session))

    output$has_density <- reactive(store$has_density())
    output$fitting_active <- reactive(!is.null(store$fitting()))
    outputOptions(output, "has_density", suspendWhenHidden = FALSE)
    outputOptions(output, "fitting_active", suspendWhenHidden = FALSE)

    observeEvent(input$hub, store$open("hub"))
    observeEvent(input$to_data, store$go_to("data"))
    observeEvent(input$fit_all, store$queue_fits(store$fittable()))
    observeEvent(input$cancel, store$cancel_fits())
    observeEvent(input$view_biomass, store$go_to("biomass"))

    lapply(component_ids, function(cid) {
      observeEvent(input[[paste0("open_", cid)]], store$open(cid))
      observeEvent(input[[paste0("go_", cid)]], store$open(cid))
      observeEvent(input[[paste0("nav_", cid)]], store$open(cid))
      observeEvent(input[[paste0("settings_", cid)]], store$open_settings(cid))
      observeEvent(input[[paste0("dismiss_", cid)]], store$dismiss(cid, "convergence"))
      sync_source_select(input, session, paste0("source_", cid), cid, store)
      output[[paste0("status_", cid)]] <- renderUI({
        status <- store$statuses()[[cid]]
        status_badge(status, if (status$kind == "fitting") store$progress() else 0)
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
      busy <- !is.null(store$fitting())
      button(
        session$ns("fit_all"), "Fit all", if (busy) lucide("loader-2", "kb-spin") else "play",
        disabled = length(store$fittable()) == 0
      )
    })

    output$fitting_title <- renderText({
      req(store$fitting())
      paste("Fitting", lower_label(store$fitting()))
    })
    output$fitting_detail <- renderText({
      queued <- component_ids[store$fit_status() == "queued"]
      then <- if (length(queued) > 0) paste0("Then ", paste(tolower(vapply(queued, label_of, "")), collapse = ", "), ". ")
      paste0(then, "Fits keep running while you move between steps.")
    })
    output$fitting_progress <- renderUI({
      progress <- store$progress()
      div(
        class = "d-flex flex-column gap-2",
        progress_bar(progress),
        div(class = "small text-body-secondary text-end kb-tabular", sprintf("%d%%", floor(progress)))
      )
    })

    output$warnings <- renderUI({
      statuses <- store$statuses()
      flagged <- Filter(function(cid) shows_convergence_warning(statuses[[cid]]), component_ids)
      if (length(flagged) == 0) {
        return(NULL)
      }
      div(
        class = "d-flex flex-column gap-2 mb-3",
        lapply(flagged, function(cid) {
          convergence_notice(
            sprintf("The %s model has a convergence warning", lower_label(cid)), session$ns(paste0("settings_", cid)),
            dismiss_id = session$ns(paste0("dismiss_", cid))
          )
        })
      )
    })

    output$readiness <- renderUI({
      if (store$biomass_ready()) {
        cover <- store$statuses()$cover$kind
        return(notice(
          "arrow-right", "All models are ready",
          if (cover %in% c("data-ok", "queued", "fitting")) {
            "Biomass per unit area can now be estimated. Totals follow once the biomass:cover model is fitted."
          } else {
            "Biomass can now be estimated."
          },
          action = button(session$ns("view_biomass"), "View biomass", size = "sm")
        ))
      }
      statuses <- store$statuses()
      blocking <- store$blocking()
      notice(
        "layout-list", "Still to do before biomass",
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
            item(
              paste0("nav_", cid), open == cid,
              status_icon(statuses[[cid]]),
              span(class = "flex-grow-1", label_of(cid)),
              if (statuses[[cid]]$kind == "fitting") {
                textOutput(session$ns("subnav_progress"), inline = TRUE) |>
                  tagAppendAttributes(class = "small text-body-secondary kb-tabular")
              }
            )
          )
        })
      )
    })
    output$subnav_progress <- renderText(sprintf("%d%%", floor(store$progress())))
  })
}
