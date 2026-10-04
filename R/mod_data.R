# Data step: species, template, workbook or per-model CSVs, sheet checks and coverage.

# The models fitted to your data whose estimates differ by site or year, so a
# site-year's estimate depends on which sites and years their sheets hold.
# Density is left out: biomass is estimated for the site-years in its sheet.
coverage_ids <- function(sheets, sources, species) {
  ids <- setdiff(component_ids, "density")
  ids[vapply(ids, function(id) {
    !is.null(sheets[[id]]) && sources[[id]] == "user" && any(has_effect(id, c("site", "year"), species))
  }, NA)]
}

# The models in use that the coverage card leaves out: pre-fit, or with no
# site or year effects.
coverage_omitted <- function(sources, species, shown) {
  ids <- setdiff(component_ids, c("density", shown))
  ids[vapply(ids, function(id) sources[[id]] != "none", NA)]
}

# The site-years in the density sheet and in the sheets of coverage_ids(): for
# each model, the sites, years and site-years its data hold and its site, year
# and site-year effects. `gaps` are the site-years with density data that some
# model other than cover has no data for, so they borrow its estimate from other
# sites and years; a site-year without a drone survey gets no total biomass
# instead. `no_density` are the other site-years without density data, which
# get no biomass.
coverage <- function(sheets, sources, species) {
  ids <- c(if (!is.null(sheets$density)) "density", coverage_ids(sheets, sources, species))
  models <- lapply(stats::setNames(nm = ids), function(id) {
    rows <- sheets[[id]]$rows
    list(
      effects = c("site", "year", "site_year")[has_effect(id, c("site", "year", "site_year"), species)],
      sites = unique(rows$site), years = unique(as.character(rows$year)), keys = site_years(rows)
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
  density <- models$density$keys
  borrowing <- setdiff(ids, c("density", "cover"))
  gaps <- if (is.null(density)) character() else {
    keys[keys %in% density & !vapply(keys, function(k) all(vapply(borrowing, function(id) k %in% models[[id]]$keys, NA)), NA)]
  }
  mismatched <- site_mismatches(sheets)
  list(
    ids = ids, models = models, keys = keys, density = density,
    sites = unique(site[order(site_number, site)]), years = unique(year[order(year_number, year)]),
    gaps = gaps, no_density = if (is.null(density)) character() else setdiff(keys, density),
    mismatched = mismatched, mismatched_keys = keys[site %in% mismatched]
  )
}

# A cell of a model's site-by-year grid: "data", "gap" (no data, so the
# estimate is borrowed from other sites and years), "none" (no density data, so
# no biomass) or "blank", with the text shown on hover.
coverage_cell <- function(cov, id, site, year) {
  key <- paste(site, year, sep = "|")
  model <- cov$models[[id]]
  label <- lower_label(id)
  density <- if (is.null(cov$density)) NA else key %in% cov$density
  if (key %in% model$keys) {
    text <- if (id == "cover") "Drone survey" else paste(label_of(id), "data")
    if (isFALSE(density) && id != "density") text <- paste0(text, "; no density data, so biomass is not estimated")
    return(list(state = "data", text = text))
  }
  if (isTRUE(density) && id == "cover") {
    return(list(state = "blank", text = "No drone survey, so no total biomass"))
  }
  if (isTRUE(density)) {
    return(list(state = "gap", text = coverage_borrowed(model, label, site, year)))
  }
  if (isFALSE(density)) {
    return(list(state = "none", text = "No density data, so biomass is not estimated"))
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
  none = "No density data: biomass not estimated"
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
    # TODO: generate the template workbook (one sheet per model with its columns) for download.
    sidebar_section("Template", button(ns("template"), "Download template workbook", "download", "outline")),
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

  csv_rows <- lapply(component_ids, function(cid) {
    div(
      class = "d-flex flex-wrap align-items-center gap-3 py-2 border-bottom",
      div(
        class = "flex-grow-1",
        div(class = "fw-medium", label_of(cid)),
        textOutput(ns(paste0("csv_columns_", cid))) |> tagAppendAttributes(class = "small font-monospace text-body-secondary")
      ),
      textOutput(ns(paste0("csv_file_", cid)), inline = TRUE) |> tagAppendAttributes(class = "small text-body-secondary"),
      fileInput(ns(paste0("csv_", cid)), NULL, accept = ".csv", buttonLabel = "Choose CSV", width = "15rem") |>
        tagAppendAttributes(class = "mb-0") |>
        file_names(sprintf("Upload a CSV for the %s model", lower_label(cid)), sprintf("Chosen %s CSV", lower_label(cid)))
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
            div(class = "small text-body-secondary", "One file per model, for example to add a sheet later")
          ),
          value = "csv",
          csv_rows
        )
      )
    ),
    conditionalPanel("output.has_data", uiOutput(ns("sheets")), ns = ns)
  )

  step_layout(sidebar, main)
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

    # TODO: replace the notices below with the template and fit bundle once they are built.
    observeEvent(input$template, {
      store$notify(sprintf("Prototype: kelpbio-template-%s.xlsx is not generated.", species_info[[store$species()]]$suffix))
    })
    observeEvent(input$guide, store$open_help("guide"))
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

    lapply(component_ids, function(cid) {
      observeEvent(input[[paste0("csv_", cid)]], {
        upload <- input[[paste0("csv_", cid)]]
        store$confirm_reset(cid, function() store$load_csv(cid, upload$name, upload$datapath))
      })
      output[[paste0("csv_file_", cid)]] <- renderText(store$sheets()[[cid]]$file)
      output[[paste0("csv_columns_", cid)]] <- renderText(paste(sheet_columns(cid, store$species()), collapse = ", "))
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
      present <- component_ids[component_ids %in% names(sheets)]
      absent <- setdiff(component_ids, present)
      tagList(
        issues_summary(session$ns, sheets, cov(), sources),
        panel(
          "Recognised sheets",
          id = session$ns("sheets_card"),
          description = "Each sheet is matched to a model and checked before fitting.",
          tags$ul(class = "list-unstyled d-flex flex-column gap-2 mb-0", lapply(present, function(id) sheet_row(sheets[[id]], sources[[id]]))),
          if (length(absent) > 0) {
            notice(
              "file-spreadsheet", "No sheet for some models",
              tone = "muted",
              paste0(
                paste(sprintf("%s: %s", vapply(absent, label_of, ""), vapply(absent, function(id) source_note(sources[[id]]), "")), collapse = "; "),
                ". You can change sources on the Models step."
              )
            )
          }
        ),
        coverage_panel(session$ns, sources, store$species(), cov(), isolate(input$coverage_view))
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
  "An Excel workbook with one sheet per model, named after the model; leave out the sheets you do not have.",
  "Biomass needs density, size and weight, from your data or pre-fit models, and a cover sheet adds total biomass per site-year."
)

# A sheet whose model uses a pre-fit model or none shows its check muted, as it
# blocks nothing until its source is Your data.
sheet_row <- function(sheet, source) {
  used <- source == "user"
  model <- lower_label(sheet$component)
  unused_note <- sprintf(
    "Not used: %s. To fit it to this sheet, set its source to Your data on the Models step.",
    if (is_prefit(source)) sprintf("the %s model uses the %s model", model, source_note(source)) else sprintf("the %s model is not used", model)
  )
  badge_ui <- if (!used) {
    badge("Not used", "bg-secondary-subtle text-secondary-emphasis", "minus-circle")
  } else if (is.null(sheet$error)) {
    success_badge("Checks passed")
  } else {
    badge("Data error", "text-bg-danger", "x-circle")
  }
  tags$li(
    class = "d-flex flex-wrap align-items-center gap-3 border rounded-3 p-3",
    div(
      class = "flex-grow-1 d-flex flex-column gap-2",
      div(
        class = "d-flex flex-wrap align-items-center gap-2",
        lucide("file-spreadsheet", "text-body-secondary"),
        tags$code(sheet$name),
        lucide("arrow-right", "text-body-secondary"),
        span(class = "fw-medium", label_of(sheet$component)),
        if (endsWith(sheet$file, ".csv")) span(class = "small text-body-secondary", "from ", sheet$file)
      ),
      if (!used) div(class = "small text-body-secondary", unused_note),
      div(
        class = "d-flex flex-wrap gap-1",
        lapply(names(sheet$rows), function(column) {
          tags$code(class = "small bg-body-tertiary text-body-secondary rounded px-2 py-1", column)
        })
      ),
      if (!is.null(sheet$error)) div(class = paste("small", if (used) "text-danger-emphasis" else "text-body-secondary"), sheet$error),
      if (!is.null(sheet$note)) {
        div(class = "small d-flex align-items-center gap-2 text-info-emphasis", lucide("info", "text-info"), sheet$note)
      }
    ),
    badge_ui
  )
}

# One line above the sheets: counts of what the checks and the coverage table
# flag, each linking to its section. Only sheets used by their model count as
# errors. Site-years that borrow estimates from other sites or years are listed
# but are not a warning.
issues_summary <- function(ns, sheets, cov, sources) {
  present <- sheets[component_ids[component_ids %in% names(sheets)]]
  errors <- sum(vapply(names(present), function(id) !is.null(present[[id]]$error) && sources[[id]] == "user", logical(1)))
  count <- function(n, one, many = paste0(one, "s")) sprintf("%d %s", n, if (n == 1) one else many)
  site_years <- function(n, what) paste(count(n, "site-year"), what)
  link <- function(text, target) tags$a(href = paste0("#", ns(target)), class = "link-body-emphasis", text)
  warnings <- c(
    "sheet errors" = errors,
    "with site names that differ across sheets" = length(cov$mismatched_keys),
    "without density data" = length(cov$no_density)
  )
  warnings <- warnings[warnings > 0]
  gaps <- length(cov$gaps)
  issues <- c(
    lapply(names(warnings), function(what) {
      if (what == "sheet errors") link(count(warnings[[what]], "sheet error"), "sheets_card") else link(site_years(warnings[[what]], what), "coverage_card")
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
coverage_panel <- function(ns, sources, species, cov, view) {
  if (is.null(cov)) {
    return(NULL)
  }
  choices <- c(vapply(cov$ids, label_of, ""), all = "All")
  if (is.null(view) || !view %in% names(choices)) view <- cov$ids[[1]]
  topic <- help_topics$coverage
  panel(
    "Coverage",
    id = ns("coverage_card"),
    description = "The site-years each model has data for. Biomass is estimated for every site-year with density data.",
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
      coverage_omitted_note(coverage_omitted(sources, species, cov$ids), species)
    )
  )
}

# Why the models in use are missing from the coverage table: no site or year
# effects, or pre-fit to other data.
coverage_omitted_note <- function(ids, species) {
  if (length(ids) == 0) {
    return(NULL)
  }
  grouped <- vapply(ids, function(id) any(has_effect(id, c("site", "year"), species)), NA)
  labels <- function(x) sentence_case(and_list(vapply(x, lower_label, "")))
  no_effects <- ids[!grouped]
  prefit <- ids[grouped]
  div(
    class = "small text-body-secondary",
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
  if (id == "cover") labels$data <- "Drone survey"
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

# Every site-year with a column per model marking whether it has data, and notes
# on site names that differ across sheets and site-years without density data.
# Searchable, to find a site's rows across the sheets.
coverage_list <- function(cov) {
  parts <- strsplit(cov$keys, "|", fixed = TRUE)
  data <- data.frame(site = vapply(parts, `[`, "", 1), year = vapply(parts, `[`, "", 2))
  for (id in cov$ids) data[[id]] <- cov$keys %in% cov$models[[id]]$keys
  data$note <- ifelse(
    cov$keys %in% cov$mismatched_keys, "Site name differs across sheets only in case or spacing",
    ifelse(cov$keys %in% cov$no_density, "No density data: biomass not estimated", NA_character_)
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
        reactable::colDef(name = label_of(id), align = "center", minWidth = 90, cell = presence, html = TRUE, sortable = FALSE)
      }),
      list(note = reactable::colDef(name = "Note", na = "", show = any(warning_rows), style = list(color = "var(--bs-secondary-color)"), minWidth = 240))
    )
  )
}
