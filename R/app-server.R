# jarl-ignore unused_function: called from inst/app/server.R.
app_server <- function(input, output, session) {
  store <- new_store(session)

  mod_data_server("data", store)
  mod_models_server("models", store)
  mod_biomass_server("biomass", store)
  mod_export_server("export", store)

  # The states of the step markers in the navbar (step_marker_switch()): "todo",
  # "done", "warning" or "busy". Biomass is done once its estimates have been
  # opened, and Export once they have been downloaded or the R script copied.
  output$mark_data <- reactive(if (store$has_density()) "done" else "todo")
  output$mark_models <- reactive({
    if (!is.null(store$fitting())) {
      "busy"
    } else if (store$has_density() && store$biomass_ready()) {
      if (any(vapply(store$statuses(), has_warning, logical(1)))) "warning" else "done"
    } else {
      "todo"
    }
  })
  output$mark_biomass <- reactive(if (store$reviewed() && store$biomass_ready()) "done" else "todo")
  output$mark_export <- reactive(if (store$exported() && store$biomass_ready()) "done" else "todo")
  for (step in names(steps)) outputOptions(output, paste0("mark_", step), suspendWhenHidden = FALSE)

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
  observeEvent(input$copied, {
    if (input$copied == "script") store$exported(TRUE)
    store$notify(copied_messages[[input$copied]])
  })
}
