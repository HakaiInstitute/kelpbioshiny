# Export step: downloads of the results, figures, fits and report, and an R
# script that reproduces the run.

# `stand_ins` are the sites standing in for sites a model has no data for, by
# model (store$stand_ins()).
export_script <- function(species, sources, sheets, workbook, priors, samplers, census = is_census(species),
                          stand_ins = list()) {
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

  read_call <- function(id, call) sprintf("%s <- %s", id, call)
  read <- c(
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
      size = if (census) sprintf("kb_check_data_size_census_%s(size)", sp) else "kb_check_data_size_sample_nereo(size)",
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
        sprintf("%s <- kelpbiodata::%s", fit_name(id), prefit_name(id, species, reference))
      ))
    }
    changed <- priors[[id]][format_prior(priors[[id]]) != format_prior(default_priors(id, species)), ]
    sampler <- samplers[[id]]
    args <- c(
      data,
      if (nrow(changed) > 0) sprintf("priors = priors_%s", model),
      if (sampler$nthin != default_sampler()$nthin) sprintf("nthin = %s", sampler$nthin),
      if (sampler$adapt_delta != default_sampler()$adapt_delta) sprintf("control = list(adapt_delta = %s)", sampler$adapt_delta)
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
  # Site names are the user's, so they are quoted as R strings.
  quoted <- function(sites) paste(encodeString(sites, quote = "\""), collapse = ", ")
  vector_code <- function(sites) if (length(sites) == 1) quoted(sites) else sprintf("c(%s)", quoted(sites))
  survey_stand_ins <- stand_ins[intersect(survey_ids, names(stand_ins))]
  survey_site <- if (length(survey_stand_ins) > 0) {
    sprintf("representative_site = list(%s)", paste(sprintf("%s = %s", names(survey_stand_ins), vapply(survey_stand_ins, vector_code, "")), collapse = ", "))
  }
  canopy_site <- if (length(stand_ins$cover) > 0) sprintf("representative_site = %s", vector_code(stand_ins$cover))
  survey_call <- function(m) paste(c("fit_weight", size_arg, "survey", measure_args(m), survey_site), collapse = ", ")
  canopy_call <- function(m) paste(c("fit_cover_calibration", "canopy", measure_args(m), canopy_site), collapse = ", ")
  in_models <- setdiff(used, c("cover", if (census) "size"))

  paste(
    c(
      sprintf(
        "# Generated by kelpbioshiny %s with kelpbio %s and %s on %s",
        utils::packageVersion("kelpbioshiny"), utils::packageVersion("kelpbio"), R.version.string, format(Sys.Date())
      ),
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
          # The app fits it to survey biomass without stand-in sites.
          if (!is.null(survey_site)) sprintf("calibration_biomass <- kb_predict_biomass_survey(fit_weight, %s, survey)", size_arg),
          fit_lines("cover", if (is.null(survey_site)) "biomass" else "calibration_biomass")
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

# Downloads ---------------------------------------------------------------------
# The results workbook, the figures and the report hold each estimate the run
# shows that is available, at the choices made on its page on the Estimates
# step: each model's predictions, and survey and canopy biomass in every
# measure available. The fit bundle holds what the Data step needs to resume
# the run. The report (R/report.R) needs the Quarto command-line tool.

download_items <- list(
  results = list(label = "Results workbook", detail = "Excel: every available estimate, with the settings and sources", file = "kelpbio-results-%s.xlsx", icon = "file-spreadsheet"),
  figures = list(label = "Figures", detail = "ZIP of PNG and PDF figures", file = "kelpbio-figures-%s.zip", icon = "file-image"),
  bundle = list(label = "Fit bundle", detail = "RDS: add it on the Data step to resume this run", file = "kelpbio-fits-%s.rds", icon = "database"),
  report = list(label = "Report", detail = "PDF with methods, diagnostics and results", file = "kelpbio-report-%s.pdf", icon = "file-text")
)

# The width of an exported figure, in inches, and half that for an overall
# prediction, as on the Estimates step; its height follows the figure there.
export_width <- 8

# Each estimate to export: list(name, title, rows, note, table, figures) or,
# where kelpbio gave an error, list(name, title, error). `name` names its sheet
# and figure files, `rows` says what its rows are, and each figure is
# list(name, plot, width, height, caption), in inches.
export_estimates <- function(store) {
  estimates <- store$estimates()
  ids <- intersect(c(unname(derived_ids), model_order), store$shown_estimates())
  ids <- ids[vapply(estimates[ids], `[[`, logical(1), "available")]
  choices <- store$estimate_choices()
  measures <- names(Filter(function(x) x$available, store$outputs()))
  items <- lapply(ids, function(id) {
    if (id %in% derived_ids) {
      lapply(measures, function(measure) {
        name <- paste(id, measure, sep = "_")
        tryCatch(derived_export(store, id, measure, choices[[id]]), error = function(e) export_error(name, estimate_label(id), e))
      })
    } else {
      list(tryCatch(model_export(store, id, choices[[id]]), error = function(e) export_error(model_name(id), label_of(id), e)))
    }
  })
  unlist(items, recursive = FALSE)
}

export_error <- function(name, title, error) list(name = name, title = title, error = condition_text(error))

model_export <- function(store, cid, choice) {
  species <- store$species()
  fit <- store$fit_of(cid)
  plants <- if (cid == "weight") plant_rows(store$sheets())
  choices <- prediction_choices(cid, is_prefit(store$sources()[[cid]]), species, !is.null(plants))
  grouping <- chosen_grouping(choice$grouping, choices)
  predictions <- model_predictions(cid, fit, grouping, species, plants = plants)
  plant <- grouping == "plant"
  info <- prediction_info[[cid]]
  caption <- prediction_caption(cid, grouping, species)
  width <- if (grouping == "population") export_width / 2 else export_width
  list(
    name = model_name(cid),
    title = if (plant) info$plant_table else info$table,
    rows = prediction_groupings[[grouping]],
    note = "Estimates with their lower and upper 95% compatibility limits.",
    table = prediction_table(table_predictions(cid, fit, grouping, species, predictions), plant),
    figures = list(list(
      name = model_name(cid), plot = prediction_figure(cid, predictions, grouping, species),
      width = width, height = width * prediction_aspect(cid, grouping), caption = caption
    ))
  )
}

# Each survey's figure is drawn one site at a time, as on the Estimates step.
derived_export <- function(store, eid, measure, choice) {
  per_area <- chosen_per_area(eid, choice$scale, store$census())
  sums <- derived_sums(eid, store$sheets())
  sum_key <- chosen_sum(choice$sum, sums)
  pool_by <- sums[[sum_key]]$pool_by
  rows <- switch(eid,
    survey = store$survey_biomass(measure, per_area, pool_by),
    canopy = store$canopy_biomass(measure, pool_by)
  )
  labels <- derived_labels(eid, measure, per_area)
  name <- paste(eid, measure, sep = "_")
  figure <- function(site = NULL) {
    list(
      name = if (is.null(site)) name else paste(name, file_name(site), sep = "_"),
      plot = derived_figure(rows, site), width = export_width, height = derived_height(rows, site) / 96,
      caption = derived_caption(eid, labels, sum_key, site)
    )
  }
  by_survey <- eid == "survey" && sum_key == "survey"
  list(
    name = name,
    title = sprintf("%s: %s (%s)", estimate_label(eid), labels$label, labels$unit),
    rows = sums[[sum_key]]$label,
    note = "Estimates with their lower and upper 95% compatibility limits. Flags name the models without data for a row's site-year.",
    table = biomass_table(rows, store$stand_ins()),
    figures = if (by_survey) lapply(unique(as.character(rows$site)), figure) else list(figure())
  )
}

# A user's site name as part of a file name.
file_name <- function(x) gsub("[^A-Za-z0-9_-]+", "-", x)

# The sources, prior and sampler settings and stand-in sites of the run, one
# setting per row: those each model was fitted with (store$run_settings()).
settings_rows <- function(store) {
  sources <- store$sources()
  settings <- store$run_settings()
  stand_ins <- store$stand_ins()
  row <- function(model, setting, value) data.frame(model = model, setting = setting, value = value)
  run <- row("Run", c("Species", "Size data", "Draws per fit", "kelpbioshiny version", "kelpbio version", "Created"), c(
    species_info[[store$species()]]$latin,
    size_data_info[[if (store$census()) "census" else "sample"]]$label,
    sprintf("%d chains of %d", fit_draws$chains, fit_draws$niters),
    format(utils::packageVersion("kelpbioshiny")), format(utils::packageVersion("kelpbio")),
    format(Sys.time(), "%Y-%m-%d %H:%M %Z")
  ))
  models <- lapply(intersect(model_order, store$shown_models()), function(id) {
    label <- label_of(id)
    source <- sources[[id]]
    priors <- settings[[id]]$priors
    sampler <- settings[[id]]$sampler
    rbind(
      row(label, "Source", source_label(source)),
      if (source == "user") {
        row(
          label, c(paste("Prior:", priors$label), "Thinning (nthin)", "adapt_delta"),
          c(format_prior(priors), format(sampler$nthin), format(sampler$adapt_delta))
        )
      },
      if (length(stand_ins[[id]]) > 0) row(label, "Stand-in sites", paste(stand_ins[[id]], collapse = ", "))
    )
  })
  do.call(rbind, c(list(run), models))
}

# The results workbook: a contents sheet, a sheet per estimate and the settings.
write_results <- function(estimates, settings, path) {
  done <- Filter(function(x) is.null(x$error), estimates)
  field <- function(name) vapply(estimates, function(x) x[[name]] %||% "", "")
  notes <- ifelse(field("error") == "", field("note"), paste("Could not be estimated:", field("error")))
  contents <- data.frame(
    sheet = c(ifelse(field("error") == "", field("name"), ""), "settings"),
    estimate = c(field("title"), "Settings"),
    rows = c(field("rows"), "One per setting"),
    notes = c(notes, "The sources, the prior and sampler settings each model was fitted with, and the stand-in sites of the run.")
  )
  tables <- stats::setNames(lapply(done, `[[`, "table"), vapply(done, `[[`, "", "name"))
  writexl::write_xlsx(c(list(contents = contents), tables, list(settings = settings)), path)
}

# The figures as a ZIP of PNG and PDF files, with their captions.
write_figures <- function(estimates, path) {
  dir <- tempfile("kb-figures-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  figures <- unlist(lapply(Filter(function(x) is.null(x$error), estimates), `[[`, "figures"), recursive = FALSE)
  files <- unlist(lapply(figures, function(figure) vapply(c("png", "pdf"), function(ext) save_figure(figure, dir, ext), "")))
  captions <- file.path(dir, "captions.txt")
  writeLines(sprintf("%s: %s", vapply(figures, `[[`, "", "name"), vapply(figures, `[[`, "", "caption")), captions)
  if (utils::zip(path, c(captions, files), flags = "-qj") != 0) stop("The figures could not be zipped.", call. = FALSE)
}

# An export_estimates() figure saved in `dir` in the app's figure theme, as a
# PNG or PDF; returns its path.
save_figure <- function(figure, dir, ext) {
  file <- file.path(dir, paste0(figure$name, ".", ext))
  ggplot2::ggsave(file, figure$plot + figure_theme(), width = figure$width, height = figure$height, units = "in", dpi = 300)
  file
}

# A fit bundle: the run's data, sources, settings, fits to your data (with the
# settings each was fitted with), stand-in sites and estimate choices. Pre-fit
# models are kelpbio's, so they are not saved.
run_bundle <- function(store) {
  records <- store$records()
  fitted <- Filter(function(id) records[[id]]$status == "fitted", component_ids)
  structure(
    list(
      kelpbioshiny = format(utils::packageVersion("kelpbioshiny")), kelpbio = format(utils::packageVersion("kelpbio")),
      species = store$species(), size_data = store$size_data(), workbook = store$workbook(),
      sheets = lapply(store$sheets(), function(sheet) list(rows = sheet$rows, file = sheet$file)),
      sources = store$sources(), priors = store$priors(), samplers = store$samplers(),
      fits = lapply(records[fitted], function(record) list(fit = record$fit, settings = record$settings)),
      stand_ins = store$stand_ins_chosen(), estimate_choices = store$estimate_choices()
    ),
    class = "kelpbioshiny_bundle"
  )
}

# A fit bundle read from `path`, checked to be one: it errors otherwise.
read_bundle <- function(path) {
  bundle <- tryCatch(readRDS(path), error = function(e) NULL)
  if (!inherits(bundle, "kelpbioshiny_bundle")) {
    stop("The file is not a fit bundle. Download one from the Export step.", call. = FALSE)
  }
  fits <- lapply(bundle$fits, `[[`, "fit")
  valid <- isTRUE(bundle$species %in% names(species_info)) &&
    all(names(bundle$fits) %in% component_ids) &&
    all(vapply(fits, inherits, logical(1), "kb_fit")) &&
    all(names(bundle$sheets) %in% sheet_ids)
  if (!valid) stop("The fit bundle is damaged or from another version of the app.", call. = FALSE)
  bundle
}

# A download's file, built on its first download by `write(path)`, which reads
# the reactive values it needs, and reused until any of them changes, as
# building the figures again on each click would hold up the R process that
# serves every session.
cached_file <- function(write, ext, session = getDefaultReactiveDomain()) {
  last <- NULL
  session$onSessionEnded(function() unlink(last))
  reactive({
    unlink(last)
    path <- tempfile("kb-export-", fileext = ext)
    write(path)
    last <<- path
    path
  })
}

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
        uiOutput(ns(paste0("button_", key)), inline = TRUE)
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
    quarto <- report_available()
    enabled <- reactive(c(results = store$any_estimate(), figures = store$any_estimate(), bundle = any_fitted(), report = store$any_estimate() && quarto))

    # The estimates are computed once for the workbook, figures and report.
    estimates <- reactive(export_estimates(store))
    files <- list(
      results = cached_file(function(path) write_results(estimates(), settings_rows(store), path), ".xlsx"),
      figures = cached_file(function(path) write_figures(estimates(), path), ".zip"),
      bundle = cached_file(function(path) saveRDS(run_bundle(store), path), ".rds"),
      report = cached_file(function(path) write_report(report_content(store), estimates(), path), ".pdf")
    )

    # A download button while there is something to download.
    lapply(names(download_items), function(key) {
      label <- paste("Download", tolower(download_items[[key]]$label))
      output[[paste0("button_", key)]] <- renderUI({
        if (!enabled()[[key]]) {
          return(tags$button(type = "button", class = "btn btn-light border btn-sm", disabled = NA, `aria-label` = label, lucide("download")))
        }
        button(session$ns(paste0("download_", key)), NULL, "download", "outline", size = "sm", download = TRUE, `aria-label` = label)
      })
    })

    lapply(names(files), function(key) {
      item <- download_items[[key]]
      output[[paste0("download_", key)]] <- downloadHandler(
        filename = function() sprintf(item$file, suffix()),
        content = function(file) {
          built <- tryCatch(files[[key]](), error = function(e) e)
          if (inherits(built, "error")) {
            store$notify(condition_text(built), type = "error", title = sprintf("%s could not be built", item$label), duration = 10)
            stop(built)
          }
          file.copy(built, file, overwrite = TRUE)
          store$exported(TRUE)
        }
      )
    })

    observe_warning_links(input, store)

    output$warnings <- renderUI({
      summary <- warnings_summary(session$ns, store$statuses())
      if (!is.null(summary)) div(class = "mb-3", summary)
    })

    output$downloads_note <- renderText({
      if (store$any_estimate()) {
        if (quarto) "" else "The report needs Quarto installed where the app runs."
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
        lapply(settings, `[[`, "priors"), lapply(settings, `[[`, "sampler"), store$census(), store$stand_ins()
      )
      tags$pre(id = session$ns("script"), class = "m-0 px-3 py-3 small bg-white border-0", .noWS = "inside", tags$code(.noWS = "inside", code))
    })
  })
}
