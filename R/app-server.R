app_server <- function(input, output, session) {
  store <- new_store(session)

  mod_data_server("data", store)
  mod_models_server("models", store)
  mod_biomass_server("biomass", store)
  mod_export_server("export", store)

  theme <- reactive(input$theme %||% app_default_theme)
  observeEvent(input$theme, session$setCurrentTheme(app_theme(input$theme)), ignoreInit = TRUE)
  output$brand <- renderUI(brand(theme(), store$species()))

  # Step markers in the navbar: number, done, warning or busy.
  marker <- reactive({
    has_data <- store$has_density()
    ready <- has_data && store$biomass_ready()
    warning <- any(vapply(store$statuses(), shows_convergence_warning, logical(1)))
    list(
      data = if (has_data) "done" else "todo",
      models = if (!is.null(store$fitting())) "busy" else if (ready) (if (warning) "warning" else "done") else "todo",
      biomass = if (ready && store$biomass_viewed()) "done" else "todo",
      export = if (store$exported()) "done" else "todo"
    )
  })

  lapply(seq_along(steps), function(i) {
    value <- names(steps)[i]
    output[[paste0("mark_", value)]] <- renderUI(step_marker(i, marker()[[value]]))
  })

  output$fitting_active <- reactive(!is.null(store$fitting()))
  outputOptions(output, "fitting_active", suspendWhenHidden = FALSE)

  output$activity_label <- renderText({
    req(store$fitting())
    queued <- sum(store$fit_status() == "queued")
    paste0(
      sprintf("Fitting %s %d%%", lower_label(store$fitting()), floor(store$progress())),
      if (queued > 0) sprintf(" \u00b7 %d queued", queued)
    )
  })

  observeEvent(input$activity, store$go_to("models"))
  # Set by a citation's Copy button once the text is on the clipboard.
  observeEvent(input$citation_copied, store$notify("Citation copied to the clipboard."))
}
