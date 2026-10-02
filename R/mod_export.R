# Export step: downloads (simulated) and an R script that reproduces the run.

export_script <- function(species, sources, sheets, workbook, priors, samplers) {
  sp <- species_info[[species]]$suffix
  used <- component_ids[sources != "none"]
  from_data <- Filter(function(id) sources[[id]] == "user" && !is.null(sheets[[id]]), used)
  from_csv <- Filter(function(id) endsWith(sheets[[id]]$file, ".csv"), from_data)
  from_book <- setdiff(from_data, from_csv)

  read <- c(
    if (length(from_book) > 0) sprintf("path <- \"%s\"", workbook %||% "kelp-survey.xlsx"),
    sprintf("%s <- readxl::read_excel(path, sheet = \"%s\")", from_book, vapply(from_book, function(id) components[[id]]$sheet, "")),
    sprintf("%s <- readr::read_csv(\"%s\")", from_csv, vapply(from_csv, function(id) sheets[[id]]$file, ""))
  )

  fit_lines <- function(id) {
    if (sources[[id]] == "prefit") {
      prefit <- prefit_info[[components[[id]]$prefit]]
      return(c(
        sprintf("# %s model", prefit$label),
        sprintf("fit_%s <- kelpbiodata::fit_%s_%s_%s", id, id, sp, prefit$suffix)
      ))
    }
    changed <- priors[[id]][format_prior(priors[[id]]) != format_prior(default_priors(id, species)), ]
    sampler <- samplers[[id]]
    args <- c(
      id,
      if (nrow(changed) > 0) sprintf("priors = priors_%s", id),
      if (id == "cover") "biomass = biomass",
      if (sampler$chains != 4) sprintf("chains = %s", sampler$chains),
      if (sampler$iterations != 1000) sprintf("niters = %s", sampler$iterations),
      if (sampler$thin != 1) sprintf("nthin = %s", sampler$thin)
    )
    prior_lines <- if (nrow(changed) > 0) {
      c(
        sprintf("priors_%s <- kb_priors_%s_%s()", id, fn_of(id), sp),
        ifelse(
          changed$family == "normal",
          sprintf("priors_%s$%s <- kb_prior_normal(%s, %s)", id, changed$name, changed$a, changed$b),
          sprintf("priors_%s$%s <- kb_prior_exponential(%s)", id, changed$name, changed$a)
        )
      )
    }
    c(prior_lines, sprintf("fit_%s <- kb_fit_%s_%s(%s)", id, fn_of(id), sp, paste(args, collapse = ", ")))
  }
  in_biomass <- setdiff(used, "cover")

  paste(
    c(
      "# Illustrative script: function names follow the planned kelpbio API.",
      "library(kelpbio)",
      "",
      read,
      "",
      sprintf("kb_check_data_%s_%s(%s)", vapply(from_data, fn_of, ""), sp, from_data),
      "",
      unlist(lapply(in_biomass, fit_lines)),
      "",
      "biomass <- kb_predict_biomass(",
      sprintf("  %s = fit_%s,", in_biomass, in_biomass),
      "  by = c(\"site\", \"year\")",
      ")",
      "kb_plot_biomass(biomass)",
      if ("cover" %in% used) {
        c(
          "",
          "# Total biomass per site-year from drone canopy area",
          fit_lines("cover"),
          "totals <- kb_predict_biomass_total(fit_cover, biomass)",
          "kb_plot_biomass(totals)"
        )
      }
    ),
    collapse = "\n"
  )
}

download_items <- list(
  results = list(label = "Results workbook", detail = "Excel: biomass per unit area, totals when the cover model is used, settings and sources", file = "kelpbio-results-%s.xlsx", icon = "file-spreadsheet"),
  figures = list(label = "Figures", detail = "ZIP of PNG and PDF figures", file = "kelpbio-figures-%s.zip", icon = "file-image"),
  bundle = list(label = "Fit bundle", detail = "RDS: add it on the Data step to resume this run", file = "kelpbio-fits-%s.rds", icon = "database"),
  report = list(label = "Report", detail = "PDF with methods, diagnostics and results", file = "kelpbio-report-%s.pdf", icon = "file-text")
)

mod_export_ui <- function(id) {
  ns <- NS(id)
  sidebar <- card(card_body(
    div(class = "kb-eyebrow text-body-secondary", "This run"),
    uiOutput(ns("run"))
  ))

  downloads <- lapply(names(download_items), function(key) {
    item <- download_items[[key]]
    div(
      class = "col-sm-6",
      div(
        class = "d-flex align-items-center gap-3 border rounded-3 p-3 h-100",
        div(class = "kb-tile-icon bg-primary-subtle text-primary-emphasis", lucide(item$icon)),
        div(class = "flex-grow-1", div(class = "fw-medium", item$label), div(class = "small text-body-secondary", item$detail)),
        actionButton(
          ns(paste0("download_", key)), lucide("download"),
          class = "btn-light border btn-sm", disabled = TRUE, `aria-label` = paste("Download", tolower(item$label))
        )
      )
    )
  })

  main <- tagList(
    page_header("Export", step_description("export")),
    panel("Downloads", description = textOutput(ns("downloads_note"), inline = TRUE), div(class = "row g-3", downloads)),
    panel(
      "R script",
      description = "Reproduces this run, with the same sources, priors and sampler settings.",
      div(
        class = "border rounded-3 overflow-hidden",
        div(
          class = "d-flex align-items-center justify-content-between border-bottom bg-body-tertiary px-3 py-1",
          span(class = "font-monospace small text-body-secondary", "analysis.R"),
          actionButton(
            ns("copy"), span(class = "d-inline-flex align-items-center gap-2", lucide("copy"), "Copy"),
            class = "btn-light btn-sm",
            onclick = sprintf("navigator.clipboard.writeText(document.getElementById('%s').innerText)", ns("script"))
          )
        ),
        uiOutput(ns("script_block"))
      )
    )
  )
  layout_columns(col_widths = c(3, 9), div(class = "kb-aside", sidebar), div(main))
}

mod_export_server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    any_fitted <- reactive(any(store$fit_status() == "fitted"))
    suffix <- reactive(species_info[[store$species()]]$suffix)

    observe({
      enabled <- c(results = store$biomass_ready(), figures = store$biomass_ready(), bundle = any_fitted(), report = store$biomass_ready())
      for (key in names(enabled)) updateActionButton(session, paste0("download_", key), disabled = !enabled[[key]])
    })

    lapply(names(download_items), function(key) {
      observeEvent(input[[paste0("download_", key)]], {
        store$exported(TRUE)
        store$notify(sprintf("Prototype: %s is not generated.", sprintf(download_items[[key]]$file, suffix())))
      })
    })

    observeEvent(input$copy, store$notify("R code copied to clipboard."))

    output$downloads_note <- renderText({
      if (store$biomass_ready()) {
        "Files from the current run."
      } else {
        "Results, figures and the report become available once biomass can be estimated."
      }
    })

    output$run <- renderUI({
      sources <- store$sources()
      tagList(
        div(class = "small text-body-secondary", em(species_info[[store$species()]]$latin)),
        status_list(store$statuses(), as.list(source_labels(sources)))
      )
    })

    output$script_block <- renderUI({
      code <- export_script(
        store$species(), store$sources(), store$sheets(), store$workbook(), store$priors(), store$samplers()
      )
      tags$pre(id = session$ns("script"), class = "m-0 px-3 py-3 small bg-white border-0", .noWS = "inside", tags$code(.noWS = "inside", code))
    })
  })
}
