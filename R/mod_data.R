# Data step: species, template, workbook or per-model CSVs, sheet checks and coverage.

# The sheets of the models fitted to your data whose estimates differ by site
# or year (weight, and size unless it is a census), so a surveyed site-year's
# estimate depends on which sites and years their sheets hold.
coverage_models <- function(sheets, sources, species) {
  Filter(function(id) {
    !is.null(sheets[[id]]) && sources[[id]] == "user" && any(has_effect(id, c("site", "year"), species))
  }, survey_ids)
}

# The models in use that the coverage card leaves out: pre-fit, a census, or
# with no site or year effects.
coverage_omitted <- function(sources, shown) {
  ids <- setdiff(c(survey_ids, "wetdry", "carbon"), shown)
  ids[vapply(ids, function(id) sources[[id]] != "none", NA)]
}

# The site-years of the survey sheet, of the sheets of coverage_models() and of
# the canopy sheet: for each, the sites, years and site-years its rows hold and,
# for a model, its site, year and site-year effects. `gaps` are the surveyed
# site-years that a model has no data for, so they borrow its estimate from
# other sites and years; `unsurveyed` are the other site-years of the models,
# which get no survey biomass.
coverage <- function(sheets, sources, species) {
  ids <- c(if (!is.null(sheets$survey)) "survey", coverage_models(sheets, sources, species), if (!is.null(sheets$canopy)) "canopy")
  models <- lapply(stats::setNames(nm = ids), function(id) {
    rows <- sheets[[id]]$rows
    list(
      effects = if (id %in% survey_ids) c("site", "year", "site_year")[has_effect(id, c("site", "year", "site_year"), species)],
      sites = unique(rows$site), years = unique(row_years(rows)), keys = site_years(rows)
    )
  })
  keys <- unique(unlist(lapply(models, `[[`, "keys"), use.names = FALSE))
  if (length(keys) == 0) {
    return(NULL)
  }
  parts <- strsplit(keys, "|", fixed = TRUE)
  site <- vapply(parts, `[`, "", 1)
  year <- vapply(parts, `[`, "", 2)
  site_number <- suppressWarnings(as.numeric(gsub("\\D", "", site)))
  year_number <- suppressWarnings(as.numeric(year))
  surveyed <- models$survey$keys
  borrowing <- intersect(ids, survey_ids)
  gaps <- if (is.null(surveyed)) character() else {
    keys[keys %in% surveyed & !vapply(keys, function(k) all(vapply(borrowing, function(id) k %in% models[[id]]$keys, NA)), NA)]
  }
  model_keys <- unique(unlist(lapply(models[borrowing], `[[`, "keys"), use.names = FALSE))
  mismatched <- site_mismatches(sheets)
  list(
    ids = ids, models = models, keys = keys, surveyed = surveyed,
    sites = unique(site[order(site_number, site)]), years = unique(year[order(year_number, year)]),
    gaps = gaps, unsurveyed = if (is.null(surveyed)) character() else setdiff(model_keys, surveyed),
    mismatched = mismatched, mismatched_keys = keys[site %in% mismatched]
  )
}

# A cell of a sheet's site-by-year grid: "data", "gap" (no data, so the
# estimate is borrowed from other sites and years), "none" (not surveyed, so
# no survey biomass) or "blank", with the text shown on hover.
coverage_cell <- function(cov, id, site, year) {
  key <- paste(site, year, sep = "|")
  model <- cov$models[[id]]
  surveyed <- if (is.null(cov$surveyed)) NA else key %in% cov$surveyed
  if (id == "survey") {
    return(if (isTRUE(surveyed)) list(state = "data", text = "Surveyed") else list(state = "blank", text = "Not surveyed"))
  }
  if (id == "canopy") {
    return(if (key %in% model$keys) list(state = "data", text = "Drone map of the site") else list(state = "blank", text = "No drone map, so no canopy biomass"))
  }
  if (key %in% model$keys) {
    text <- paste(sheet_label(id), "data")
    if (isFALSE(surveyed)) text <- paste0(text, "; not surveyed, so no survey biomass")
    return(list(state = "data", text = text))
  }
  if (isTRUE(surveyed)) {
    return(list(state = "gap", text = coverage_borrowed(model, lower_label(id), site, year)))
  }
  if (isFALSE(surveyed)) {
    return(list(state = "none", text = "Not surveyed, so no survey biomass"))
  }
  list(state = "blank", text = "No data")
}

# Where a model's estimate for a site-year without its data comes from.
coverage_borrowed <- function(model, label, site, year) {
  from <- c(
    if ("site" %in% model$effects && site %in% model$sites) "the site's estimate from other years",
    if ("year" %in% model$effects && year %in% model$years) "the year's estimate from other sites"
  )
  if (length(from) == 0) from <- "the estimate for a typical site and year"
  sprintf("No %s data for this site-year: uses %s", label, and_list(from))
}

coverage_states <- list(
  data = "Data",
  gap = "No data: estimate borrowed from other sites and years",
  none = "Not surveyed: no survey biomass"
)

# Names for a fileInput's two fields: the file picker, which would otherwise be
# named by its button (so every CSV picker would read "Choose CSV"), and the
# read-only box that shows the chosen file.
file_names <- function(input, picker, chosen) {
  input |>
    tagAppendAttributes(.cssSelector = ".shiny-input-file", `aria-label` = picker) |>
    tagAppendAttributes(.cssSelector = "input.form-control", `aria-label` = chosen)
}

mod_data_ui <- function(id) {
  ns <- NS(id)
  species_choices <- lapply(species_info, function(x) {
    span(class = "d-inline-flex flex-column align-top", em(x$latin), span(class = "small text-body-secondary", sentence_case(x$common)))
  })

  sidebar <- card(card_body(
    sidebar_section(
      "Species",
      radioButtons(ns("species"), NULL, choiceNames = unname(species_choices), choiceValues = names(species_info)) |>
        tagAppendAttributes(class = "mb-0", `aria-labelledby` = ns("species_title")),
      div(class = "small text-body-secondary", "Applies to every model in this run."),
      id = ns("species_title")
    ),
    # Macrocystis surveys are always a census, so only Nereocystis has the choice.
    conditionalPanel(
      "input.species == 'nereo'",
      ns = ns,
      sidebar_section(
        with_help("Size data", "census"),
        radioButtons(
          ns("size_data"), NULL,
          choiceNames = unname(lapply(size_data_info, function(x) {
            span(class = "d-inline-flex flex-column align-top", style = "max-width: 100%", x$label, span(class = "small text-body-secondary", x$detail))
          })),
          choiceValues = names(size_data_info)
        ) |>
          tagAppendAttributes(class = "mb-0", `aria-labelledby` = ns("size_data_title")),
        id = ns("size_data_title")
      )
    ),
    sidebar_section("Template", button(ns("template"), "Download template workbook", "download", "outline", download = TRUE)),
    div(
      class = "d-flex flex-column gap-2",
      div(class = "kb-eyebrow text-body-secondary", "Resume a run"),
      div(class = "small text-body-secondary", "Upload a fit bundle exported from an earlier run."),
      # TODO: read a fit bundle (the fits, sources and settings of a run) and restore the run from it.
      fileInput(ns("bundle"), NULL, accept = ".rds", buttonLabel = "Fit bundle (.rds)", placeholder = "No file", width = "100%") |>
        tagAppendAttributes(class = "mb-0") |>
        file_names("Upload a fit bundle", "Chosen fit bundle")
    )
  ))

  csv_rows <- lapply(sheet_ids, function(cid) {
    div(
      class = "d-flex flex-wrap align-items-center gap-3 py-2 border-bottom",
      div(
        class = "flex-grow-1",
        div(class = "fw-medium", sheet_label(cid)),
        textOutput(ns(paste0("csv_columns_", cid))) |> tagAppendAttributes(class = "small font-monospace text-body-secondary")
      ),
      textOutput(ns(paste0("csv_file_", cid)), inline = TRUE) |> tagAppendAttributes(class = "small text-body-secondary"),
      fileInput(ns(paste0("csv_", cid)), NULL, accept = ".csv", buttonLabel = "Choose CSV", width = "15rem") |>
        tagAppendAttributes(class = "mb-0") |>
        file_names(sprintf("Upload a CSV for the %s sheet", cid), sprintf("Chosen %s CSV", cid))
    )
  })

  main <- tagList(
    # In the page from the start, so it shows before the server's first
    # response; hidden in the browser once data are loaded.
    conditionalPanel("!output.has_data", welcome_card(ns), ns = ns),
    page_header(
      "Data",
      step_description("data"),
      uiOutput(ns("continue_ui"), inline = TRUE)
    ),
    panel(
      "Workbook",
      description = sheets_description,
      action = uiOutput(ns("clear_ui"), inline = TRUE),
      div(
        class = "d-flex flex-column gap-2",
        div(
          class = "d-flex flex-wrap align-items-start gap-2",
          div(
            class = "kb-file-input",
            fileInput(ns("workbook"), NULL, accept = c(".xlsx", ".xls"), buttonLabel = "Browse files", width = "100%") |>
              tagAppendAttributes(class = "mb-0") |>
              file_names("Upload a workbook", "Chosen workbook")
          ),
          example_menu(ns)
        ),
        # In conditional panels rather than outputs that render nothing, which
        # would hold the space of a busy spinner until the server first responds.
        conditionalPanel(
          "output.has_workbook",
          ns = ns,
          div(
            class = "small text-body-secondary",
            "Loaded ",
            span(class = "font-monospace text-body", textOutput(ns("workbook_name"), inline = TRUE), .noWS = c("after", "after-begin", "before-end")),
            ". Upload another workbook to replace it."
          )
        )
      ),
      accordion(
        open = FALSE,
        accordion_panel(
          title = div(
            div(class = "fw-medium", "Upload individual CSV files instead"),
            div(class = "small text-body-secondary", "One file per sheet, for example to add a sheet later")
          ),
          value = "csv",
          csv_rows
        )
      )
    ),
    conditionalPanel("output.has_data", uiOutput(ns("sheets")), ns = ns)
  )

  # The sidebar holds the run's setup, so on a phone it comes first.
  step_layout(sidebar, main, aside_first = TRUE)
}

mod_data_server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    loaded <- reactive(length(store$sheets()) > 0)
    cov <- reactive(coverage(store$sheets(), store$sources(), store$species()))

    # Each of these replaces the data, so it resets every fit.
    replace_data <- function(action) store$confirm_reset(component_ids, action)

    observeEvent(input$species, {
      value <- input$species
      if (!identical(value, store$species())) {
        store$confirm_reset(
          component_ids,
          function() store$set_species(value),
          function() updateRadioButtons(session, "species", selected = store$species()),
          resets_priors = TRUE
        )
      }
    }, ignoreInit = TRUE)
    observeEvent(store$species(), {
      if (!identical(input$species, store$species())) updateRadioButtons(session, "species", selected = store$species())
    })
    observeEvent(input$size_data, {
      value <- input$size_data
      if (!identical(value, store$size_data())) {
        store$confirm_reset(
          "size",
          function() store$set_size_data(value),
          function() updateRadioButtons(session, "size_data", selected = store$size_data())
        )
      }
    }, ignoreInit = TRUE)
    observeEvent(store$size_data(), {
      if (!identical(input$size_data, store$size_data())) updateRadioButtons(session, "size_data", selected = store$size_data())
    })

    # The template follows the species and the kind of size data.
    output$template <- downloadHandler(
      filename = function() template_file(store$species(), store$census()),
      content = function(file) writexl::write_xlsx(template_sheets(store$species(), store$census()), file)
    )
    observeEvent(input$guide, store$open_help("guide"))
    # TODO: replace the notice with the fit bundle once it is built.
    observeEvent(input$bundle, store$notify("Prototype: resuming from a fit bundle is not simulated."))
    observeEvent(input$workbook, {
      upload <- input$workbook
      replace_data(function() store$load_workbook(upload$name, upload$datapath))
    })
    lapply(names(example_workbooks), function(key) {
      observeEvent(input[[paste0("example_", key)]], replace_data(function() store$load_example(key)))
    })
    observeEvent(input$clear, replace_data(store$clear_data))
    observeEvent(input$continue, store$go_to("models"))

    lapply(sheet_ids, function(sid) {
      model <- sheet_component(sid)
      observeEvent(input[[paste0("csv_", sid)]], {
        upload <- input[[paste0("csv_", sid)]]
        store$confirm_reset(if (is.null(model)) character() else model, function() store$load_csv(sid, upload$name, upload$datapath))
      })
      output[[paste0("csv_file_", sid)]] <- renderText(store$sheets()[[sid]]$file)
      output[[paste0("csv_columns_", sid)]] <- renderText(paste(sheet_columns(sid, store$species(), store$census()), collapse = ", "))

      # The sheet's figure, in one panel or a panel per site or year, and its
      # rows, searchable, with the missing values marked.
      sheet <- reactive(req(store$sheets()[[sid]]))
      facet <- reactive({
        value <- input[[paste0("facet_", sid)]]
        if (is.null(value) || value == "none" || !value %in% names(sheet_facets(sid, sheet()$rows))) NULL else value
      })
      output[[paste0("plot_", sid)]] <- render_figure(
        function() sheet_plot(sid, sheet()$rows, store$species(), facet()),
        paste0("plot_", sid),
        height = function() {
          rows <- sheet()$rows
          if (is.null(facet())) {
            width <- session$clientData[[paste0("output_", session$ns(paste0("plot_", sid)), "_width")]]
            return(if (is.null(width) || width == 0) 400 else round(width * data_plot_info[[sid]]$aspect))
          }
          levels <- if (facet() == "site") unique(rows$site) else unique(row_years(rows))
          figure_px(60 + 170 * ggplot2::wrap_dims(length(levels))[1])
        },
        alt = function() data_plot_caption(sid, store$species())
      )
      output[[paste0("table_", sid)]] <- reactable::renderReactable({
        app_table(
          sheet()$rows,
          page_size = 8,
          searchable = TRUE,
          filterable = TRUE,
          defaultColDef = reactable::colDef(
            na = "missing",
            style = function(value, ...) if (is.na(value)) list(color = "var(--bs-warning-text-emphasis)")
          )
        )
      })
    })

    output$has_data <- reactive(loaded())
    outputOptions(output, "has_data", suspendWhenHidden = FALSE)
    output$has_workbook <- reactive(!is.null(store$workbook()))
    outputOptions(output, "has_workbook", suspendWhenHidden = FALSE)

    output$continue_ui <- renderUI({
      if (loaded()) button(session$ns("continue"), span("Continue to models ", lucide("arrow-right")))
    })

    output$clear_ui <- renderUI({
      if (loaded()) button(session$ns("clear"), "Clear", variant = "ghost", size = "sm")
    })

    output$workbook_name <- renderText(store$workbook())

    output$sheets <- renderUI({
      sheets <- store$sheets()
      # With no data loaded, the welcome card above stands in for an empty state.
      if (length(sheets) == 0) {
        return(NULL)
      }
      sources <- store$sources()
      present <- sheet_ids[sheet_ids %in% names(sheets)]
      absent <- setdiff(sheet_ids, present)
      tagList(
        issues_summary(session$ns, sheets, cov(), sources),
        panel(
          "Recognised sheets",
          id = session$ns("sheets_card"),
          description = "Each sheet is checked before fitting. Open a sheet to see its data.",
          accordion(
            id = session$ns("sheet_list"),
            open = isolate(input$sheet_list) %||% FALSE,
            multiple = TRUE,
            !!!lapply(unname(present), function(id) sheet_panel(session$ns, sheets[[id]], sheet_source(id, sources), store$species(), isolate(input[[paste0("facet_", id)]])))
          ),
          if (length(absent) > 0) {
            notice(
              "file-spreadsheet", "Some optional sheets are missing",
              tone = "muted",
              paste0(paste(sprintf("%s: %s", vapply(absent, sheet_label, ""), vapply(absent, absent_note, "", sources = sources)), collapse = "; "), ".")
            )
          }
        ),
        coverage_panel(session$ns, sources, store$species(), cov(), isolate(input$coverage_view), store$census())
      )
    })

    output$coverage_legend <- renderUI({
      cov <- req(cov())
      view <- req(input$coverage_view)
      if (view != "all") coverage_legend(cov, view)
    })

    output$coverage <- reactable::renderReactable({
      cov <- req(cov())
      view <- req(input$coverage_view)
      if (view == "all") coverage_list(cov) else coverage_grid(cov, view)
    })
  })
}

# A dropdown of the example workbooks, each with a line on what it holds.
# Bootstrap's own dropdown opens it, positioned as fixed so the card around it
# does not clip it.
example_menu <- function(ns) {
  div(
    class = "dropdown",
    tags$button(
      type = "button", class = "btn btn-light border dropdown-toggle",
      `data-bs-toggle` = "dropdown", `aria-expanded` = "false",
      `data-bs-config` = '{"popperConfig": {"strategy": "fixed"}}', "Use example workbook"
    ),
    tags$ul(
      class = "dropdown-menu", style = "width: 24rem",
      lapply(names(example_workbooks), function(key) {
        example <- example_workbooks[[key]]
        tags$li(actionLink(
          ns(paste0("example_", key)),
          div(div(class = "fw-medium", example$label), div(class = "small text-body-secondary text-wrap", example$detail)),
          class = "dropdown-item py-2"
        ))
      })
    )
  )
}

# Shown on the Data step until data are loaded: what the app does and its steps.
welcome_card <- function(ns) {
  card(
    id = ns("welcome_card"),
    card_body(
      gap = "1.25rem",
      div(
        h2(class = "kb-card-title mb-0", "Welcome to kelpbio"),
        div(class = "text-body-secondary mt-1", app_purpose)
      ),
      layout_column_wrap(
        width = "12rem",
        gap = "1rem",
        !!!lapply(names(steps), step_item)
      ),
      div(actionLink(ns("guide"), "Read the user guide"))
    )
  )
}

# The one description of the workbook's sheets; the columns are in the guide and
# under the CSV uploads.
sheets_description <- paste(
  "An Excel workbook with one sheet per kind of data, named weight, size, survey, canopy, wetdry and carbon; leave out the sheets you do not have.",
  "Survey biomass needs a survey sheet with weight and size, from your data or pre-fit models; drone flights over the surveyed plots and a canopy sheet of drone maps add canopy biomass."
)

# What a missing sheet means for the run.
absent_note <- function(id, sources) {
  switch(id,
    survey = "no survey biomass or cover calibration",
    canopy = "no canopy biomass",
    source_note(sources[[id]])
  )
}

# A sheet as an accordion panel. Its title holds the sheet's name, the result
# of its check and the first error or warning; a sheet whose model uses a
# pre-fit model or none shows its check muted, as it blocks nothing until its
# source is Your data. Opened, it shows every warning of the check, then the
# sheet's figure and table; a sheet that fails its check, or has no figure,
# shows only its table, where missing values are marked.
sheet_panel <- function(ns, sheet, source, species, facet = NULL) {
  id <- sheet$component
  used <- source %in% c("user", "census")
  model <- sheet_component(id)
  unused_note <- if (!used) {
    sprintf(
      "Not used: %s. To fit it to this sheet, set its source to Your data on the Models step.",
      if (is_prefit(source)) sprintf("the %s model uses the %s model", lower_label(model), source_note(source)) else sprintf("the %s model is not used", lower_label(model))
    )
  }
  badge_ui <- if (!used) {
    badge("Not used", "bg-secondary-subtle text-secondary-emphasis", "minus-circle")
  } else if (!is.null(sheet$error)) {
    badge("Data error", "text-bg-danger", "x-circle")
  } else if (!is.null(sheet$warnings)) {
    warning_badge("Passed with warnings")
  } else {
    success_badge("Checks passed")
  }
  warning_line <- function(text) {
    div(
      class = paste("small d-flex align-items-center gap-2", if (used) "text-warning-emphasis" else "text-body-secondary"),
      lucide("alert-triangle", if (used) "text-warning"), span(text)
    )
  }
  more <- length(sheet$warnings) - 1
  # The badge follows the name, which has a fixed width so the badges line up
  # down the list; the messages wrap below.
  title <- div(
    class = "d-flex flex-column gap-2",
    div(
      class = "d-flex flex-wrap align-items-center gap-2",
      lucide("file-spreadsheet", "text-body-secondary"),
      tags$code(style = "min-width: 4.5rem", sheet$name),
      badge_ui,
      if (endsWith(sheet$file, ".csv")) span(class = "small text-body-secondary fw-normal", "from ", sheet$file),
      # Text styled as a link: the whole title opens the panel, and a link
      # inside the accordion's button would not be valid HTML.
      span(class = "small link-primary fw-normal ms-1", "View data")
    ),
    if (!used) div(class = "small text-body-secondary fw-normal", unused_note),
    if (!is.null(sheet$error)) div(class = paste("small fw-normal", if (used) "text-danger-emphasis" else "text-body-secondary"), sheet$error),
    if (!is.null(sheet$warnings)) warning_line(paste0(sheet$warnings[[1]], if (more > 0) sprintf(" (and %d more)", more))),
    if (!is.null(sheet$note)) {
      div(class = "small d-flex align-items-center gap-2 text-info-emphasis fw-normal", lucide("info", "text-info"), sheet$note)
    }
  )
  table <- reactable::reactableOutput(ns(paste0("table_", id)))
  facets <- sheet_facets(id, sheet$rows)
  plot <- tagList(
    if (length(facets) > 0) {
      div(
        class = "d-flex align-items-center gap-2 mb-2",
        span(class = "small fw-medium", id = ns(paste0("facet_", id, "_title")), "Facet"),
        selectInput(
          ns(paste0("facet_", id)), NULL,
          choices = c(None = "none", stats::setNames(names(facets), facets)), selected = facet %||% "none",
          selectize = FALSE, width = "10rem"
        ) |>
          tagAppendAttributes(class = "mb-0", `aria-labelledby` = ns(paste0("facet_", id, "_title")))
      )
    },
    figure_plot(ns(paste0("plot_", id)), data_plot_caption(id, species))
  )
  accordion_panel(
    title = title,
    value = id,
    div(
      class = "d-flex flex-column gap-3",
      if (more > 0) div(class = "d-flex flex-column gap-1", lapply(sheet$warnings[-1], warning_line)),
      if (!is.null(sheet$error) || !sheet_has_plot(id, sheet$rows)) table else plot_table_nav(ns(paste0("view_", id)), plot, table)
    )
  )
}

# One line above the sheets: counts of what the checks and the coverage table
# flag, each linking to its section. Only sheets used by their model count as
# errors or warnings. Site-years that borrow estimates from other sites or years
# are listed but are not a warning.
issues_summary <- function(ns, sheets, cov, sources) {
  present <- sheets[sheet_ids[sheet_ids %in% names(sheets)]]
  used <- function(field) {
    sum(vapply(names(present), function(id) !is.null(present[[id]][[field]]) && sheet_source(id, sources) %in% c("user", "census"), logical(1)))
  }
  count <- function(n, one, many = paste0(one, "s")) sprintf("%d %s", n, if (n == 1) one else many)
  site_years <- function(n, what) paste(count(n, "site-year"), what)
  link <- function(text, target) tags$a(href = paste0("#", ns(target)), class = "link-body-emphasis", text)
  warnings <- c(
    "sheet errors" = used("error"),
    "sheet warnings" = used("warnings"),
    "with site names that differ across sheets" = length(cov$mismatched_keys)
  )
  warnings <- warnings[warnings > 0]
  gaps <- length(cov$gaps)
  issues <- c(
    lapply(names(warnings), function(what) {
      switch(what,
        "sheet errors" = link(count(warnings[[what]], "sheet error"), "sheets_card"),
        "sheet warnings" = link(count(warnings[[what]], "sheet warning"), "sheets_card"),
        link(site_years(warnings[[what]], what), "coverage_card")
      )
    }),
    if (gaps > 0) list(link(site_years(gaps, "with data gaps"), "coverage_card"))
  )
  recognised <- link(count(length(present), "sheet recognised", "sheets recognised"), "sheets_card")
  if (length(issues) == 0) {
    return(div(
      class = "d-flex flex-wrap align-items-center gap-2 small border rounded-3 px-3 py-2 mb-3 bg-success-subtle border-success-subtle",
      lucide("check-circle-2", "text-success"), span(class = "fw-medium", "All sheets passed their checks"),
      span(class = "text-body-secondary", "\u00b7"), recognised
    ))
  }
  items <- c(list(recognised), issues)
  separated <- lapply(seq_along(items), function(i) {
    tagList(if (i > 1) span(class = "text-body-secondary", `aria-hidden` = "true", "\u00b7"), items[[i]])
  })
  tone <- if (length(warnings) > 0) {
    list(box = "bg-warning-subtle border-warning-subtle", icon = lucide("alert-triangle", "text-warning"))
  } else {
    list(box = "bg-body-tertiary", icon = lucide("info", "text-body-secondary"))
  }
  div(class = paste("d-flex flex-wrap align-items-center gap-2 small border rounded-3 px-3 py-2 mb-3", tone$box), tone$icon, separated)
}

# The coverage card: a site-by-year grid for each model, or all site-years in
# one table ("all"), picked by `view`, which keeps the reader's choice when the
# card is redrawn.
coverage_panel <- function(ns, sources, species, cov, view, census = FALSE) {
  if (is.null(cov)) {
    return(NULL)
  }
  choices <- c(vapply(cov$ids, sheet_label, ""), all = "All")
  if (is.null(view) || !view %in% names(choices)) view <- cov$ids[[1]]
  topic <- help_topics$coverage
  panel(
    "Coverage",
    id = ns("coverage_card"),
    description = "The site-years each sheet has data for. Survey biomass is estimated for every survey, and canopy biomass for every drone map of a site.",
    if (length(cov$mismatched) > 0) {
      notice(
        "alert-triangle", warning_help$mismatch$title,
        sprintf("%s. Biomass cannot be estimated until they match. %s", and_list(sprintf("\"%s\"", cov$mismatched)), mismatch_advice),
        tone = "warning"
      )
    },
    radioButtons(ns("coverage_view"), NULL, choices = stats::setNames(names(choices), choices), selected = view, inline = TRUE) |>
      tagAppendAttributes(class = "mb-0", `aria-label` = "Coverage view"),
    uiOutput(ns("coverage_legend")),
    reactable::reactableOutput(ns("coverage")),
    div(
      class = "d-flex flex-column gap-1",
      div(class = "small text-body-secondary", topic_body(topic), " ", tags$a(href = help_url("coverage"), target = "_blank", rel = "noopener", "Learn more")),
      coverage_omitted_note(coverage_omitted(sources, cov$ids), species, census)
    )
  )
}

# Why the models in use are missing from the coverage table: no site or year
# effects, or pre-fit to other data.
coverage_omitted_note <- function(ids, species, census = FALSE) {
  if (census) ids <- setdiff(ids, "size")
  if (length(ids) == 0 && !census) {
    return(NULL)
  }
  grouped <- vapply(ids, function(id) any(has_effect(id, c("site", "year"), species)), NA)
  labels <- function(x) sentence_case(and_list(vapply(x, lower_label, "")))
  no_effects <- ids[!grouped]
  prefit <- ids[grouped]
  div(
    class = "small text-body-secondary",
    if (census) "The surveys are a census: survey biomass weighs each counted plant in the size sheet, so no size model is fitted.",
    if (length(no_effects) > 0) {
      sprintf(
        "%s %s no site or year effects, so every site-year uses the same estimate.",
        labels(no_effects), if (length(no_effects) == 1) "has" else "have"
      )
    },
    if (length(prefit) > 0) {
      with_help(sprintf("%s %s pre-fit.", labels(prefit), if (length(prefit) == 1) "is" else "are"), "prefit")
    }
  )
}

# The swatches of the states in a model's grid.
coverage_legend <- function(cov, id) {
  states <- unique(unlist(lapply(cov$sites, function(site) {
    vapply(cov$years, function(year) coverage_cell(cov, id, site, year)$state, "")
  })))
  states <- intersect(names(coverage_states), states)
  labels <- coverage_states
  if (id == "survey") labels$data <- "Surveyed"
  if (id == "canopy") labels$data <- "Drone map of the site"
  div(
    class = "d-flex flex-wrap column-gap-3 row-gap-1 small",
    lapply(states, function(state) {
      span(
        class = "d-inline-flex align-items-center gap-2",
        span(class = paste("kb-cov", paste0("kb-cov-", state)), `aria-hidden` = "true"), labels[[state]]
      )
    })
  )
}

# A model's site-by-year grid: a row per site, a column per year, each cell
# coloured by coverage_cell(), with its text on hover and for screen readers.
coverage_grid <- function(cov, id) {
  cells <- lapply(cov$years, function(year) lapply(cov$sites, function(site) coverage_cell(cov, id, site, year)))
  names(cells) <- cov$years
  data <- data.frame(site = cov$sites)
  for (year in cov$years) data[[year]] <- vapply(cells[[year]], `[[`, "", "state")
  year_col <- function(year) {
    reactable::colDef(
      name = year, align = "center", minWidth = 52, sortable = FALSE, html = TRUE,
      cell = function(value, index) {
        text <- cells[[year]][[index]]$text
        as.character(span(class = paste("kb-cov", paste0("kb-cov-", value)), title = text, role = "img", `aria-label` = text))
      }
    )
  }
  app_table(
    data,
    pagination = FALSE,
    compact = TRUE,
    height = if (length(cov$sites) > 15) 450 else "auto",
    columns = c(
      list(site = reactable::colDef(name = "Site", sticky = "left", minWidth = 90, style = list(fontWeight = 500))),
      lapply(stats::setNames(nm = cov$years), year_col)
    )
  )
}

# Every site-year with a column per sheet marking whether it has data, and notes
# on site names that differ across sheets and site-years not surveyed.
# Searchable, to find a site's rows across the sheets.
coverage_list <- function(cov) {
  parts <- strsplit(cov$keys, "|", fixed = TRUE)
  data <- data.frame(site = vapply(parts, `[`, "", 1), year = vapply(parts, `[`, "", 2))
  for (id in cov$ids) data[[id]] <- cov$keys %in% cov$models[[id]]$keys
  data$note <- ifelse(
    cov$keys %in% cov$mismatched_keys, "Site name differs across sheets only in case or spacing",
    ifelse(cov$keys %in% cov$unsurveyed, "Not surveyed: no survey biomass", NA_character_)
  )
  data <- data[order(match(data$site, cov$sites), match(data$year, cov$years)), ]
  warning_rows <- !is.na(data$note)
  presence <- function(value) {
    as.character(if (value) {
      tagList(lucide("check", "text-success"), span(class = "visually-hidden", "Yes"))
    } else {
      tagList(lucide("minus", "text-body-secondary"), span(class = "visually-hidden", "No"))
    })
  }
  app_table(
    data,
    pagination = FALSE,
    searchable = TRUE,
    height = if (nrow(data) > 12) 450 else "auto",
    row_style = function(index) if (warning_rows[index]) app_row_colour$warning,
    columns = c(
      list(
        site = reactable::colDef(name = "Site", style = list(fontWeight = 500), minWidth = 90),
        year = reactable::colDef(name = "Year", minWidth = 70)
      ),
      lapply(stats::setNames(nm = cov$ids), function(id) {
        reactable::colDef(name = sheet_label(id), align = "center", minWidth = 90, cell = presence, html = TRUE, sortable = FALSE)
      }),
      list(note = reactable::colDef(name = "Note", na = "", show = any(warning_rows), style = list(color = "var(--bs-secondary-color)"), minWidth = 240))
    )
  )
}
