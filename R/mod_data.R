# Data step: species, template, workbook or per-model CSVs, sheet checks and coverage.

coverage_ids <- c("density", "size", "weight")

# Site-year coverage across the density, size and weight sheets, plus the
# site-years in the cover sheet, with notes on gaps and on site names that
# differ across sheets only in case or spacing. The notes on gaps are about
# biomass, which needs density data, so they apply only with a density sheet.
coverage <- function(sheets, sources) {
  sets <- lapply(stats::setNames(nm = coverage_ids), function(id) {
    if (is.null(sheets[[id]])) NULL else site_years(sheets[[id]]$rows)
  })
  cover <- if (is.null(sheets$cover)) NULL else site_years(sheets$cover$rows)
  no_plot <- if (is.null(sheets$cover)) character() else no_plot_cover(sheets$cover$rows)
  mismatched <- site_mismatches(sheets)
  keys <- unique(unlist(sets, use.names = FALSE))
  if (length(keys) == 0) {
    return(list(rows = NULL))
  }
  parts <- strsplit(keys, "|", fixed = TRUE)
  site <- vapply(parts, `[`, "", 1)
  year <- vapply(parts, `[`, "", 2)

  rows <- lapply(seq_along(keys), function(i) {
    present <- vapply(coverage_ids, function(id) if (is.null(sets[[id]])) NA else keys[i] %in% sets[[id]], NA)
    note <- NA_character_
    tone <- NA_character_
    issue <- NA_character_
    missing <- character()
    if (site[i] %in% mismatched) {
      tone <- "warning"
      issue <- "mismatch"
      note <- "Site name differs across sheets only in case or spacing"
    } else if (is.na(present[["density"]])) {
      # No density sheet, so no biomass to note gaps for.
    } else if (!present[["density"]]) {
      note <- "No density data: biomass not estimated"
      tone <- "warning"
      issue <- "no_density"
    } else {
      missing <- c("size", "weight")[present[c("size", "weight")] %in% FALSE & sources[c("size", "weight")] == "user"]
      if (length(missing) > 0) {
        note <- sprintf(
          "Population-level %s %s used",
          paste(tolower(vapply(missing, label_of, "")), collapse = " and "),
          if (length(missing) > 1) "estimates" else "estimate"
        )
        tone <- "info"
      }
    }
    plot_missing <- keys[i] %in% no_plot && isTRUE(present[["density"]])
    if (plot_missing) {
      cover_note <- "No plot cover: total biomass uses the population-level cover relationship"
      note <- if (is.na(note)) cover_note else paste0(note, "; ", tolower(cover_note))
      tone <- tone %|NA|% "info"
    }
    data.frame(
      site = site[i], year = year[i],
      density = present[["density"]], size = present[["size"]], weight = present[["weight"]],
      cover = if (is.null(cover)) NA else keys[i] %in% cover,
      note = note, tone = tone,
      # For the issues summary, not shown in the table.
      issue = issue, no_size = "size" %in% missing, no_weight = "weight" %in% missing, no_plot = plot_missing
    )
  })
  rows <- do.call(rbind, rows)
  site_number <- suppressWarnings(as.numeric(gsub("\\D", "", rows$site)))
  rows <- rows[order(rows$year, site_number, rows$site), ]
  rownames(rows) <- NULL
  list(rows = rows)
}

# Site-years with a canopy area but no plot cover.
no_plot_cover <- function(rows) {
  has_cover <- tapply(!is.na(rows$plot_percent_cover), paste(rows$site, rows$year, sep = "|"), any)
  names(has_cover)[!has_cover]
}

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
      uiOutput(ns("description"), inline = TRUE),
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
        uiOutput(ns("loaded"))
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
    uiOutput(ns("sheets"))
  )

  step_layout(sidebar, main)
}

mod_data_server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    show_all <- reactiveVal(FALSE)
    loaded <- reactive(length(store$sheets()) > 0)
    cov <- reactive(coverage(store$sheets(), store$sources()))

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
    observeEvent(input$toggle_all, show_all(!show_all()))

    lapply(component_ids, function(cid) {
      observeEvent(input[[paste0("csv_", cid)]], {
        upload <- input[[paste0("csv_", cid)]]
        store$confirm_reset(cid, function() store$load_csv(cid, upload$name, upload$datapath))
      })
      output[[paste0("csv_file_", cid)]] <- renderText(store$sheets()[[cid]]$file)
      output[[paste0("csv_columns_", cid)]] <- renderText(paste(sheet_columns(cid, store$species()), collapse = ", "))
    })

    output$description <- renderUI({
      sp <- species_info[[store$species()]]
      tagList(step_description("data"), " Species: ", species_phrase(sp, "."))
    })

    output$has_data <- reactive(loaded())
    outputOptions(output, "has_data", suspendWhenHidden = FALSE)

    output$continue_ui <- renderUI({
      if (loaded()) button(session$ns("continue"), span("Continue to models ", lucide("arrow-right")))
    })

    output$clear_ui <- renderUI({
      if (loaded()) button(session$ns("clear"), "Clear", variant = "ghost", size = "sm")
    })

    output$loaded <- renderUI({
      workbook <- req(store$workbook())
      div(
        class = "small text-body-secondary",
        "Loaded ", span(class = "font-monospace text-body", workbook, .noWS = "after"), ". Upload another workbook to replace it."
      )
    })

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
        issues_summary(session$ns, sheets, cov()$rows, sources),
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
        coverage_panel(session$ns, sheets, cov(), show_all())
      )
    })

    output$coverage <- reactable::renderReactable({
      rows <- req(cov()$rows)
      if (!show_all()) rows <- rows[!is.na(rows$note), ]
      req(nrow(rows) > 0)
      coverage_table(rows, store$sheets(), store$sources(), show_all())
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
# errors.
issues_summary <- function(ns, sheets, rows, sources) {
  present <- sheets[component_ids[component_ids %in% names(sheets)]]
  errors <- sum(vapply(names(present), function(id) !is.null(present[[id]]$error) && sources[[id]] == "user", logical(1)))
  count <- function(n, one, many = paste0(one, "s")) sprintf("%d %s", n, if (n == 1) one else many)
  site_years <- function(n, what) paste(count(n, "site-year"), what)
  link <- function(text, target) tags$a(href = paste0("#", ns(target)), class = "link-body-emphasis", text)
  sheet_issues <- list(
    if (errors > 0) count(errors, "sheet error")
  )
  coverage_counts <- c(
    "with site names that differ across sheets" = sum(rows$issue %in% "mismatch"),
    "without density data" = sum(rows$issue %in% "no_density"),
    "without size data" = sum(rows$no_size),
    "without weight data" = sum(rows$no_weight),
    "without plot cover" = sum(rows$no_plot)
  )
  coverage_counts <- coverage_counts[coverage_counts > 0]
  issues <- c(
    lapply(Filter(Negate(is.null), sheet_issues), link, target = "sheets_card"),
    lapply(names(coverage_counts), function(what) link(site_years(coverage_counts[[what]], what), "coverage_card"))
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
  div(
    class = "d-flex flex-wrap align-items-center gap-2 small border rounded-3 px-3 py-2 mb-3 bg-warning-subtle border-warning-subtle",
    lucide("alert-triangle", "text-warning"), separated
  )
}

coverage_panel <- function(ns, sheets, cov, show_all) {
  flagged <- sum(!is.na(cov$rows$note))
  visible <- if (show_all) nrow(cov$rows) else flagged
  mismatches <- site_mismatches(sheets)
  panel(
    "Coverage",
    id = ns("coverage_card"),
    description = tagList(
      "Site-years in each dataset. Biomass is estimated for every site-year with density data;",
      "gaps in size or weight use a", with_help("population-level estimate.", "population"),
      "Cover marks site-years with a", with_help("canopy area", "canopy_area"), "where total biomass can be estimated."
    ),
    action = button(ns("toggle_all"), if (show_all) "Show gaps only" else "Show all", variant = "ghost", size = "sm"),
    if (length(mismatches) > 0) {
      notice(
        "alert-triangle", warning_help$mismatch$title,
        sprintf("%s. Biomass cannot be estimated until they match. %s", and_list(sprintf("\"%s\"", mismatches)), mismatch_advice),
        tone = "warning"
      )
    },
    if (visible == 0) {
      notice("check-circle-2", "Every site-year with density has size and weight data", tone = "muted")
    } else {
      reactable::reactableOutput(ns("coverage"))
    },
    if (!show_all && flagged > 0) {
      div(
        class = "small text-body-secondary",
        "Showing site-years with gaps or mismatches. All other site-years have density, size and weight data."
      )
    }
  )
}

coverage_table <- function(rows, sheets, sources, show_all) {
  presence <- function(value) {
    if (is.na(value)) {
      return(as.character(span(class = "small text-body-secondary", "n/a")))
    }
    as.character(if (value) {
      tagList(lucide("check", "text-success"), span(class = "visually-hidden", "Yes"))
    } else {
      tagList(lucide("minus", "text-body-secondary"), span(class = "visually-hidden", "No"))
    })
  }
  presence_col <- function(id, label = label_of(id), width = 100) {
    header <- if (is.null(sheets[[id]])) {
      div(label, div(class = "small fw-normal", source_note(sources[[id]])))
    } else {
      label
    }
    reactable::colDef(header = header, align = "center", width = width, cell = presence, html = TRUE, sortable = FALSE)
  }
  tones <- rows$tone
  app_table(
    rows,
    pagination = FALSE,
    height = if (show_all) 450 else "auto",
    row_style = function(index) {
      switch(tones[index] %|NA|% "none",
        warning = app_row_colour$warning,
        info = app_row_colour$info,
        NULL
      )
    },
    columns = list(
      site = reactable::colDef(name = "Site", style = list(fontWeight = 500), width = 110),
      year = reactable::colDef(name = "Year", width = 80),
      density = presence_col("density"),
      size = presence_col("size"),
      weight = presence_col("weight"),
      cover = presence_col("cover"),
      note = reactable::colDef(name = "Note", na = "", style = list(color = "var(--bs-secondary-color)"), minWidth = 260),
      tone = reactable::colDef(show = FALSE),
      issue = reactable::colDef(show = FALSE),
      no_size = reactable::colDef(show = FALSE),
      no_weight = reactable::colDef(show = FALSE),
      no_plot = reactable::colDef(show = FALSE)
    )
  )
}

`%|NA|%` <- function(x, y) if (is.na(x)) y else x
