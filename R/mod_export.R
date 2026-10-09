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
  biomass <- biomass_possible(sources)
  cover <- biomass && "cover" %in% from_data
  stipes <- sp == "nereo" && !is.null(sheets$density) && ("weight" %in% from_data || (plants && "density" %in% from_data))

  # kelpbio takes site and year as text; spreadsheets store years as numbers.
  as_text <- function(id) if ("year" %in% sheet_columns(id, species)) " |>\n  dplyr::mutate(year = as.character(year))" else ""
  read <- c(
    if (length(from_book) > 0) sprintf("path <- \"%s\"", workbook %||% "kelp-survey.xlsx"),
    sprintf("%s <- readxl::read_excel(path, sheet = \"%s\")%s", from_book, vapply(from_book, function(id) components[[id]]$sheet, ""), vapply(from_book, as_text, "")),
    sprintf("%s <- readr::read_csv(\"%s\")%s", from_csv, vapply(from_csv, function(id) sheets[[id]]$file, ""), vapply(from_csv, as_text, ""))
  )

  fit_name <- function(ids) paste0("fit_", vapply(ids, model_name, ""))
  fit_lines <- function(id, data = id) {
    model <- model_name(id)
    if (is_prefit(sources[[id]])) {
      reference <- prefit_reference(sources[[id]])
      return(c(
        sprintf("# %s model", prefit_info[[reference]]$label),
        sprintf("%s <- kb_prefit_%s_%s(reference = \"%s\")", fit_name(id), model, sp, reference)
      ))
    }
    changed <- priors[[id]][format_prior(priors[[id]]) != format_prior(default_priors(id, species)), ]
    sampler <- samplers[[id]]
    args <- c(
      data,
      if (nrow(changed) > 0) sprintf("priors = priors_%s", model),
      if (sampler$chains != 4) sprintf("chains = %s", sampler$chains),
      if (sampler$niters != 1000) sprintf("niters = %s", sampler$niters),
      if (sampler$nthin != 1) sprintf("nthin = %s", sampler$nthin)
    )
    prior_lines <- if (nrow(changed) > 0) {
      constructor <- c(normal = "kb_prior_normal(%s, %s)", lognormal = "kb_prior_lognormal(%s, %s)", exponential = "kb_prior_exponential(%s)")
      c(
        sprintf("priors_%s <- kb_priors_%s_%s()", model, model, sp),
        sprintf("priors_%s$%s <- %s", model, changed$name, ifelse(
          changed$family == "exponential",
          sprintf(constructor[changed$family], changed$a),
          sprintf(constructor[changed$family], changed$a, changed$b)
        ))
      )
    }
    c(prior_lines, sprintf("%s <- kb_fit_%s_%s(%s)", fit_name(id), model, sp, paste(args, collapse = ", ")))
  }
  in_biomass <- setdiff(used, "cover")
  plant_data <- if (stipes && "density" %in% from_data) "kb_add_stipes_m2(size, density)" else "size"
  biomass_call <- function(measure, fits) {
    sprintf(
      "kb_predict_plot_biomass(%s%s)", paste(fit_name(fits), collapse = ", "),
      if (measure == "wet") "" else sprintf(", measure = \"%s\"", measure)
    )
  }
  measures <- names(output_info)[vapply(output_availability(sources), `[[`, logical(1), "available")]
  fits <- list(wet = c("weight", "size", "density"), dry = c("weight", "size", "density", "wetdry"))
  fits$carbon <- c(fits$dry, "carbon")
  prefix <- c(wet = "", dry = "dry_", carbon = "carbon_")
  planned <- c(
    if (any(vapply(sources[used], is_prefit, logical(1)))) "kb_prefit_*()",
    if (stipes && ("weight" %in% from_data || "density" %in% from_data)) "kb_add_stipes_m2()"
  )

  paste(
    c(
      if (length(planned) > 0) sprintf("# %s %s planned, not yet in kelpbio.", and_list(planned), if (length(planned) == 1) "is" else "are"),
      "library(kelpbio)",
      "",
      read,
      if ("cover" %in% from_data) {
        c(
          "",
          "# The drone surveys of plots, and of whole sites, in kelpbio's columns",
          "plots <- cover |>",
          "  dplyr::filter(!is.na(plot_canopy_area_m2)) |>",
          "  dplyr::select(site, year, canopy_area_m2 = plot_canopy_area_m2, plot_area_m2 = plot_boundary_area_m2, tide_height_m)",
          "sites <- cover |>",
          "  dplyr::filter(!is.na(site_canopy_area_m2)) |>",
          "  dplyr::select(site, year, canopy_area_m2 = site_canopy_area_m2, tide_height_m) |>",
          "  dplyr::distinct()"
        )
      },
      "",
      sprintf("kb_check_data_%s_%s(%s)", vapply(from_data, model_name, ""), sp, ifelse(from_data == "cover", "plots", from_data)),
      if ("weight" %in% from_data && stipes) {
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
          sprintf("%sbiomass <- %s", prefix[measures], vapply(measures, function(m) biomass_call(m, fits[[m]]), "")),
          "kb_plot_predictions(biomass)"
        )
      },
      if (cover) {
        c(
          "",
          "# Total site biomass from the cover biomass model, fitted to the drone",
          "# surveys of plots with the wet plot biomass of their site-years",
          fit_lines("cover", c("plots", "biomass")),
          sprintf(
            "%stotals <- kb_predict_site_biomass(%s)", prefix[measures],
            vapply(measures, function(m) {
              paste(c("fit_cover_biomass", "sites", if (m != "wet") fit_name(setdiff(fits[[m]], fits$wet)), if (m != "wet") sprintf("measure = \"%s\"", m)), collapse = ", ")
            }, "")
          ),
          "kb_plot_predictions(totals)"
        )
      }
    ),
    collapse = "\n"
  )
}

# TODO: build each download (results workbook, figures, fit bundle and report);
# the buttons only show a notice for now. Build each file once per set of fits
# and reuse it for every download of it (a preview, the file itself, a ZIP), as
# rendering the report or figures again on each click would hold up the R
# process that serves every session.
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
      # A fitted model's settings are those it was fitted with, which may differ
      # from the settings now shown on its page.
      settings <- store$run_settings()
      code <- export_script(
        store$species(), store$sources(), store$sheets(), store$workbook(),
        lapply(settings, `[[`, "priors"), lapply(settings, `[[`, "sampler")
      )
      tags$pre(id = session$ns("script"), class = "m-0 px-3 py-3 small bg-white border-0", .noWS = "inside", tags$code(.noWS = "inside", code))
    })
  })
}
