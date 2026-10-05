# Export step: downloads (simulated) and an R script that reproduces the run.

export_script <- function(species, sources, sheets, workbook, priors, samplers) {
  sp <- species_info[[species]]$suffix
  used <- component_ids[sources != "none"]
  from_data <- Filter(function(id) sources[[id]] == "user" && !is.null(sheets[[id]]), used)
  # The weight of each plant in the size sheet, read even when the size model is
  # not fitted to it.
  plants <- "weight" %in% used && !is.null(sheets$size) && is.null(sheets$size$error)
  read_ids <- union(from_data, if (plants) "size")
  from_csv <- Filter(function(id) endsWith(sheets[[id]]$file, ".csv"), read_ids)
  from_book <- setdiff(read_ids, from_csv)

  read <- c(
    if (length(from_book) > 0) sprintf("path <- \"%s\"", workbook %||% "kelp-survey.xlsx"),
    sprintf("%s <- readxl::read_excel(path, sheet = \"%s\")", from_book, vapply(from_book, function(id) components[[id]]$sheet, "")),
    sprintf("%s <- readr::read_csv(\"%s\")", from_csv, vapply(from_csv, function(id) sheets[[id]]$file, ""))
  )

  fit_lines <- function(id) {
    if (is_prefit(sources[[id]])) {
      reference <- prefit_reference(sources[[id]])
      return(c(
        sprintf("# %s model", prefit_info[[reference]]$label),
        sprintf("fit_%s <- kb_prefit_%s_%s(reference = \"%s\")", id, id, sp, reference)
      ))
    }
    changed <- priors[[id]][format_prior(priors[[id]]) != format_prior(default_priors(id, species)), ]
    sampler <- samplers[[id]]
    args <- c(
      id,
      if (nrow(changed) > 0) sprintf("priors = priors_%s", id),
      if (sampler$chains != 4) sprintf("chains = %s", sampler$chains),
      if (sampler$niters != 1000) sprintf("niters = %s", sampler$niters),
      if (sampler$nthin != 1) sprintf("nthin = %s", sampler$nthin)
    )
    prior_lines <- if (nrow(changed) > 0) {
      c(
        sprintf("priors_%s <- kb_priors_%s_%s()", id, id, sp),
        ifelse(
          changed$family == "normal",
          sprintf("priors_%s$%s <- kb_prior_normal(%s, %s)", id, changed$name, changed$a, changed$b),
          sprintf("priors_%s$%s <- kb_prior_exponential(%s)", id, changed$name, changed$a)
        )
      )
    }
    c(prior_lines, sprintf("fit_%s <- kb_fit_%s_%s(%s)", id, id, sp, paste(args, collapse = ", ")))
  }
  in_biomass <- setdiff(used, "cover")
  biomass <- biomass_possible(sources)
  plant_data <- if (sp == "nereo" && "density" %in% from_data) "kb_add_stipes_m2(size, density)" else "size"

  paste(
    c(
      "# Illustrative script: function names follow the planned kelpbio API.",
      "library(kelpbio)",
      "",
      read,
      "",
      sprintf("kb_check_data_%s_%s(%s)", vapply(from_data, check_name, ""), sp, from_data),
      if ("weight" %in% from_data && sp == "nereo" && !is.null(sheets$density)) {
        c("", "# The observed stipe density of each site-year, for the weight model", "weight <- kb_add_stipes_m2(weight, density)")
      },
      "",
      unlist(lapply(in_biomass, fit_lines)),
      if (plants) {
        c("", "# The predicted weight of each plant in the size sheet", sprintf("plant_weights <- kb_predict_weight(fit_weight, new_data = %s)", plant_data))
      },
      if (biomass) {
        c(
          "",
          "biomass <- kb_predict_biomass(",
          sprintf("  %s = fit_%s,", in_biomass, in_biomass),
          "  by = c(\"site\", \"year\")",
          ")",
          "kb_plot_biomass(biomass)"
        )
      },
      if (biomass && "cover" %in% used) {
        c(
          "",
          "# Total site biomass from the cover model, fitted to the drone surveys",
          "# with the plot biomass of their site-years added",
          "cover <- kb_add_biomass(cover, biomass)",
          fit_lines("cover"),
          "totals <- kb_predict_biomass_total(fit_cover, biomass)",
          "kb_plot_biomass(totals)"
        )
      }
    ),
    collapse = "\n"
  )
}

# TODO: build each download (results workbook, figures, fit bundle and report);
# the buttons only show a notice for now.
download_items <- list(
  results = list(label = "Results workbook", detail = "Excel: every available estimate, with the settings and sources", file = "kelpbio-results-%s.xlsx", icon = "file-spreadsheet"),
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
    uiOutput(ns("warnings")),
    panel("Downloads", description = textOutput(ns("downloads_note"), inline = TRUE), div(class = "row g-3", downloads)),
    panel(
      "R script",
      description = "Reproduces this run, with the same sources, priors and sampler settings.",
      div(
        class = "border rounded-3 overflow-hidden",
        div(
          class = "d-flex align-items-center justify-content-between border-bottom bg-body-tertiary px-3 py-1",
          span(class = "font-monospace small text-body-secondary", "analysis.R"),
          copy_button(ns("script"), "script", "Copy the R script")
        ),
        uiOutput(ns("script_block"))
      )
    )
  )
  step_layout(sidebar, main)
}

mod_export_server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    any_fitted <- reactive(any(record_status(store$records()) == "fitted"))
    suffix <- reactive(species_info[[store$species()]]$suffix)

    observe({
      enabled <- c(results = store$any_estimate(), figures = store$any_estimate(), bundle = any_fitted(), report = store$any_estimate())
      for (key in names(enabled)) updateActionButton(session, paste0("download_", key), disabled = !enabled[[key]])
    })

    lapply(names(download_items), function(key) {
      observeEvent(input[[paste0("download_", key)]], {
        store$exported(TRUE)
        store$notify(sprintf("Prototype: %s is not generated.", sprintf(download_items[[key]]$file, suffix())))
      })
    })

    observe_warning_links(input, store)

    output$warnings <- renderUI({
      summary <- warnings_summary(session$ns, store$statuses())
      if (!is.null(summary)) div(class = "mb-3", summary)
    })

    output$downloads_note <- renderText({
      if (store$any_estimate()) {
        "Files from the current run."
      } else {
        "Results, figures and the report become available once there is an estimate to save."
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
