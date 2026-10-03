app_server <- function(input, output, session) {
  store <- new_store(session)

  mod_data_server("data", store)
  mod_models_server("models", store)
  mod_biomass_server("biomass", store)
  mod_export_server("export", store)

  # Step markers in the navbar for Data and Models: number, done, warning or busy.
  output$mark_data <- renderUI(step_marker(1, if (store$has_density()) "done" else "todo"))
  output$mark_models <- renderUI({
    state <- if (!is.null(store$fitting())) {
      "busy"
    } else if (store$has_density() && store$biomass_ready()) {
      if (any(vapply(store$statuses(), has_warning, logical(1)))) "warning" else "done"
    } else {
      "todo"
    }
    step_marker(2, state)
  })

  output$fitting_active <- reactive(!is.null(store$fitting()))
  outputOptions(output, "fitting_active", suspendWhenHidden = FALSE)

  output$activity_label <- renderText({
    req(store$fitting())
    queued <- sum(record_status(store$records()) == "queued")
    paste0(
      sprintf("Fitting %s %d%%", lower_label(store$fitting()), floor(store$progress())),
      if (queued > 0) sprintf(" \u00b7 %d queued", queued)
    )
  })

  observeEvent(input$activity, store$go_to("models"))
  # Set by a Copy button (copy_button()) once the text is on the clipboard.
  observeEvent(input$copied, store$notify(copied_messages[[input$copied]]))
}
