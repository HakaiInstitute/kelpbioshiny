# Export step: downloads (simulated) and an R script that reproduces the run.

export_script <- function(species, sources, sheets, workbook, priors, samplers, census = is_census(species)) {
  sp <- species_info[[species]]$suffix
  used <- component_ids[sources != "none"]
  from_data <- Filter(function(id) sources[[id]] == "user" && !is.null(sheets[[components[[id]]$sheet]]), used)
  valid <- function(id) !is.null(sheets[[id]]) && is.null(sheets[[id]]$error)
  # The weight of each plant in the size sheet, read even when the size model is
  # not fitted to it.
  plants <- "weight" %in% used && valid("size")
  survey <- survey_possible(sources) && valid("survey") && (!census || valid("size"))
  cover <- survey && "cover" %in% from_data && nrow(drone_surveys(sheets$survey$rows)) > 0
  canopy <- cover && valid("canopy")
  fitted <- setdiff(from_data, "cover")
  read_ids <- intersect(sheet_ids, c(vapply(fitted, function(id) components[[id]]$sheet, ""), if (plants) "size", if (survey) "survey", if (canopy) "canopy"))
  from_csv <- Filter(function(id) endsWith(sheets[[id]]$file, ".csv"), read_ids)
  from_book <- setdiff(read_ids, from_csv)

  # kelpbio takes site, plot and year as text and dates as Dates; spreadsheets
  # store years and plot numbers as numbers and dates as date-times.
  keyed <- function(id) any(c("site", "plot_id", "year", "date") %in% sheet_columns(id, species))
  read_call <- function(id, call) if (keyed(id)) sprintf("%s <- as_kelpbio(%s)", id, call) else sprintf("%s <- %s", id, call)
  read <- c(
    if (any(vapply(read_ids, keyed, logical(1)))) {
      c(
        "as_kelpbio <- function(x) {",
        "  dplyr::mutate(x, dplyr::across(dplyr::any_of(c(\"site\", \"plot_id\", \"year\")), as.character), dplyr::across(dplyr::any_of(\"date\"), as.Date))",
        "}"
      )
    },
    # File names are the user's, so they are quoted as R strings.
    if (length(from_book) > 0) sprintf("path <- %s", encodeString(workbook %||% "kelp-survey.xlsx", quote = "\"")),
    vapply(from_book, function(id) read_call(id, sprintf("readxl::read_excel(path, sheet = \"%s\")", id)), ""),
    vapply(from_csv, function(id) read_call(id, sprintf("readr::read_csv(%s)", encodeString(sheets[[id]]$file, quote = "\""))), ""),
    if (survey && census && "plants" %in% names(sheets$survey$rows)) {
      c("# A census: the counts come from the plant rows of the size sheet", "survey <- dplyr::select(survey, -plants)")
    }
  )

  check <- function(id) {
    switch(id,
      size = if (census) sprintf("kb_check_data_plants_%s(size)", sp) else "kb_check_data_size_nereo(size)",
      survey = if (census) "kb_check_data_survey(survey, size)" else "kb_check_data_survey(survey)",
      canopy = "kb_check_data_canopy(canopy)",
      sprintf("kb_check_data_%s_%s(%s)", id, sp, id)
    )
  }

  fit_name <- function(id) paste0("fit_", model_name(id))
  fit_lines <- function(id, data = components[[id]]$sheet) {
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

  measures <- names(output_info)[vapply(output_availability(sources), `[[`, logical(1), "available")]
  prefix <- c(wet = "", dry = "dry_", carbon = "carbon_")
  measure_args <- function(m) {
    c(
      if (m != "wet") "fit_wetdry", if (m == "carbon") "fit_carbon",
      if (m != "wet") sprintf("measure = \"%s\"", m)
    )
  }
  size_arg <- if (census) "size" else "fit_size"
  survey_call <- function(m) paste(c("fit_weight", size_arg, "survey", measure_args(m)), collapse = ", ")
  canopy_call <- function(m) paste(c("fit_cover_calibration", "canopy", measure_args(m)), collapse = ", ")
  planned <- c(
    if (any(vapply(sources[used], is_prefit, logical(1)))) "kb_prefit_*()",
    if (canopy) "kb_check_data_canopy()"
  )
  in_models <- setdiff(used, c("cover", if (census) "size"))

  paste(
    c(
      # TODO: name the kelpbio version too once kelpbio is in Imports.
      sprintf(
        "# Generated by kelpbioshiny %s with %s on %s",
        utils::packageVersion("kelpbioshiny"), R.version.string, format(Sys.Date())
      ),
      if (length(planned) > 0) sprintf("# %s %s planned, not yet in kelpbio.", and_list(planned), if (length(planned) == 1) "is" else "are"),
      "library(kelpbio)",
      "",
      read,
      "",
      vapply(read_ids, check, ""),
      "",
      unlist(lapply(in_models, fit_lines)),
      if (plants) {
        c("", "# The predicted weight of each plant in the size sheet", "plant_weights <- kb_predict_weight(fit_weight, new_data = size)")
      },
      if (survey) {
        c(
          "",
          if (census) "# Survey biomass per m\u00b2 surveyed, weighing each counted plant" else "# Survey biomass per m\u00b2 surveyed",
          sprintf("%sbiomass <- kb_predict_biomass_survey(%s)", prefix[measures], vapply(measures, survey_call, "")),
          "kb_plot_predictions(biomass)"
        )
      },
      if (cover) {
        c(
          "",
          "# The cover calibration, fitted to the wet survey biomass of the surveys",
          "# with a drone flight",
          fit_lines("cover", "biomass")
        )
      },
      if (canopy) {
        c(
          "",
          "# Biomass over the canopy mapped in each drone map of a site",
          sprintf("%stotals <- kb_predict_biomass_canopy(%s)", prefix[measures], vapply(measures, canopy_call, "")),
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
        ""
      } else {
        "Results, figures and the report become available once there is an estimate to save."
      }
    })

    output$run <- renderUI({
      sources <- store$sources()
      tagList(
        div(
          class = "small text-body-secondary",
          em(species_info[[store$species()]]$latin),
          if (store$census()) paste0(", ", tolower(size_data_info$census$label))
        ),
        status_list(store$statuses(), as.list(source_labels(sources)), intersect(model_order, store$shown_models()))
      )
    })

    output$script_block <- renderUI({
      # A fitted model's settings are those it was fitted with, which may differ
      # from the settings now shown on its page.
      settings <- store$run_settings()
      code <- export_script(
        store$species(), store$sources(), store$sheets(), store$workbook(),
        lapply(settings, `[[`, "priors"), lapply(settings, `[[`, "sampler"), store$census()
      )
      tags$pre(id = session$ns("script"), class = "m-0 px-3 py-3 small bg-white border-0", .noWS = "inside", tags$code(.noWS = "inside", code))
    })
  })
}
