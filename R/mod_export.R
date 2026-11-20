# Export step: downloads of the results and fits, and an R
# script that reproduces the run.

# `stand_ins` are the sites standing in for sites a model has no data for, by
# model (store$stand_ins()).
export_script <- function(species, sources, sheets, workbook, priors, samplers, census = is_census(species),
                          stand_ins = list()) {
  sp <- species_info[[species]]$suffix
  used <- component_ids[sources != "none"]
  from_data <- Filter(function(id) fits_here(sources[[id]]) && !is.null(sheets[[components[[id]]$sheet]]), used)
  valid <- function(id) !is.null(sheets[[id]]) && is.null(sheets[[id]]$error)
  # The weight of each plant in the size sheet, read even when the size model is
  # not fitted to it.
  plants <- "weight" %in% used && valid("size")
  survey <- survey_possible(sources) && valid("survey") && (!census || valid("size"))
  cover <- survey && "cover" %in% from_data && nrow(drone_surveys(sheets$survey$rows)) > 0
  # A pre-fit cover calibration gives canopy biomass from the canopy sheet alone.
  cover_prefit <- is_prefit(sources[["cover"]])
  canopy <- (cover || cover_prefit) && valid("canopy")
  fitted <- setdiff(from_data, "cover")
  read_ids <- intersect(sheet_ids, c(vapply(fitted, function(id) components[[id]]$sheet, ""), if (plants) "size", if (survey) "survey", if (canopy) "canopy"))
  from_csv <- Filter(function(id) endsWith(sheets[[id]]$file, ".csv"), read_ids)
  from_book <- setdiff(read_ids, from_csv)

  read_call <- function(id, call) sprintf("%s <- %s", id, call)
  read <- c(
    # File names are the user's, so they are quoted as R strings.
    if (length(from_book) > 0) sprintf("path <- %s", encodeString(workbook %||% "kelp-data.xlsx", quote = "\"")),
    vapply(from_book, function(id) read_call(id, sprintf("readxl::read_excel(path, sheet = \"%s\")", sheet_name(id, census))), ""),
    vapply(from_csv, function(id) read_call(id, sprintf("readr::read_csv(%s)", encodeString(sheets[[id]]$file, quote = "\""))), ""),
    if (survey && census && "plants" %in% names(sheets$survey$rows)) {
      c("# A census: the counts come from the plant rows of the size sheet", "survey <- dplyr::select(survey, -plants)")
    }
  )

  check <- function(id) {
    switch(id,
      size = if (census) sprintf("kb_check_data_size_census_%s(size)", sp) else "kb_check_data_size_sample_nereo(size)",
      survey = if (census) "kb_check_data_survey_in_situ(survey, size)" else "kb_check_data_survey_in_situ(survey)",
      canopy = "kb_check_data_survey_canopy(canopy)",
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
      if (is_combined(sources[[id]])) paste0(data, "_combined") else data,
      if (nrow(changed) > 0) sprintf("priors = priors_%s", model),
      if (sampler$nthin != default_sampler()$nthin) sprintf("nthin = %s", sampler$nthin),
      if (sampler$adapt_delta != default_sampler()$adapt_delta) sprintf("control = list(adapt_delta = %s)", sampler$adapt_delta),
      sprintf("seed = %s", format(sampler$seed, scientific = FALSE))
    )
    prior_lines <- if (nrow(changed) > 0) {
      constructor <- c(normal = "kb_prior_normal(%s, %s)", lognormal = "kb_prior_lognormal(%s, %s)", exponential = "kb_prior_exponential(%s)")
      # One prior at a time, as an exponential prior takes one argument.
      calls <- vapply(seq_len(nrow(changed)), function(i) {
        family <- changed$family[i]
        if (family == "exponential") sprintf(constructor[[family]], changed$a[i]) else sprintf(constructor[[family]], changed$a[i], changed$b[i])
      }, "")
      c(
        sprintf("priors_%s <- kb_priors_%s_%s()", model, model, sp),
        sprintf("priors_%s$%s <- %s", model, changed$name, calls)
      )
    }
    combined <- if (is_combined(sources[[id]])) {
      c(
        sprintf("# Your %s data with the %s reference data", data, prefit_info[[prefit_reference(sources[[id]])]]$name),
        # readxl reads years as numbers, the reference data hold them as a factor.
        sprintf(
          "%s_combined <- dplyr::bind_rows(dplyr::mutate(%s, year = factor(year)), kelpbiodata::%s)",
          data, data, reference_data_name(id, species, prefit_reference(sources[[id]]))
        )
      )
    }
    c(combined, prior_lines, sprintf("%s <- kb_fit_%s_%s(%s)", fit_name(id), model, sp, paste(args, collapse = ", ")))
  }

  measures <- names(output_info)[vapply(output_availability(sources), `[[`, logical(1), "available")]
  prefix <- c(wet = "", dry = "dry_", carbon = "carbon_", nitrogen = "nitrogen_")
  measure_args <- function(m) {
    c(
      if (m != "wet") "fit_wetdry", if (m == "carbon") "fit_carbon", if (m == "nitrogen") "nitrogen = fit_nitrogen",
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
  # The weight and size models are read only for in situ survey biomass or plant weights.
  in_models <- setdiff(used, c("cover", if (census) "size", if (!survey && !plants) survey_ids))

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
      drop_repeated_comments(lapply(in_models, fit_lines)),
      if (plants) {
        c("", "# The predicted weight of each plant in the size sheet", "plant_weights <- kb_predict_weight(fit_weight, new_data = size)")
      },
      if (survey) {
        c(
          "",
          if (census) "# In situ survey biomass per m\u00b2 surveyed, weighing each counted plant" else "# In situ survey biomass per m\u00b2 surveyed",
          sprintf("%sbiomass <- kb_predict_biomass_in_situ(%s)", prefix[measures], vapply(measures, survey_call, "")),
          "kb_plot_predictions(biomass, geom = \"bar\")"
        )
      },
      if (cover) {
        c(
          "",
          "# The cover calibration, fitted to the wet in situ survey biomass of the in situ surveys",
          "# with canopy imagery",
          # The app fits it to in situ survey biomass without stand-in sites.
          if (!is.null(survey_site)) sprintf("calibration_biomass <- kb_predict_biomass_in_situ(fit_weight, %s, survey)", size_arg),
          fit_lines("cover", if (is.null(survey_site)) "biomass" else "calibration_biomass")
        )
      },
      if (canopy) {
        c(
          "",
          if (cover_prefit) c(fit_lines("cover"), ""),
          "# Biomass over the canopy mapped in each canopy map of a site",
          sprintf("%stotals <- kb_predict_biomass_canopy(%s)", prefix[measures], vapply(measures, canopy_call, "")),
          "kb_plot_predictions(totals, geom = \"bar\")"
        )
      }
    ),
    collapse = "\n"
  )
}

# Downloads ---------------------------------------------------------------------
# The results hold each estimate the run shows that is available, at the
# choices made on its page on the Estimates step: each model's predictions, and
# survey and canopy biomass in every measure available. The fit bundle holds
# what the Data step needs to resume the run.

download_items <- list(
  results = list(label = "Results", detail = "ZIP: tables (CSV) and figures (PNG and PDF), a folder per estimate", file = "kelpbio-results-%s.zip", icon = "folder-open"),
  bundle = list(label = "Fit bundle", detail = "RDS: add it on the Data step to resume this run", file = "kelpbio-fits-%s.rds", icon = "database")
)

# The width of an exported figure, in inches, and half that for an overall
# prediction, as on the Estimates step; its height follows the figure there.
export_width <- 8

# Each estimate to export: list(name, title, rows, note, table, figures) or,
# where kelpbio gave an error, list(name, title, error). `name` names its
# files, `folder` (derived_folder()) the folder that holds them, `rows` says
# what its rows are, and each figure is list(name, plot, width, height,
# caption), in inches.
export_estimates <- function(store) {
  estimates <- store$estimates()
  ids <- intersect(c(unname(derived_ids), model_order), store$shown_estimates())
  ids <- ids[vapply(estimates[ids], `[[`, logical(1), "available")]
  choices <- store$estimate_choices()
  measures <- names(Filter(function(x) x$available, store$outputs()))
  steps <- sum(ifelse(ids %in% derived_ids, length(measures), 1))
  items <- lapply(ids, function(id) {
    incProgress(0, detail = if (id %in% derived_ids) estimate_label(id) else label_of(id))
    on.exit(incProgress(0.5 * if (id %in% derived_ids) length(measures) / steps else 1 / steps))
    if (id %in% derived_ids) {
      lapply(measures, function(measure) {
        name <- derived_name(id, measure)
        tryCatch(derived_export(store, id, measure, choices[[id]]), error = function(e) export_error(name, estimate_label(id), e, derived_folder(id, measure)))
      })
    } else {
      list(tryCatch(model_export(store, id, choices[[id]]), error = function(e) export_error(model_name(id), label_of(id), e)))
    }
  })
  unlist(items, recursive = FALSE)
}

export_error <- function(name, title, error, folder = name) list(name = name, folder = folder, title = title, error = condition_text(error))

# A derived estimate's files are named for it and its measure
# (in_situ_biomass_wet), in a folder per measure within one per estimate
# (in_situ_biomass/wet).
derived_names <- c(survey = "in_situ_biomass", canopy = "canopy_biomass")
derived_name <- function(eid, measure) paste(derived_names[[eid]], measure, sep = "_")
derived_folder <- function(eid, measure) file.path(derived_names[[eid]], measure)

model_export <- function(store, cid, choice) {
  species <- store$species()
  fit <- store$fit_of(cid)
  plants <- if (cid == "weight") plant_rows(store$sheets())
  choices <- prediction_choices(cid, is_prefit(store$sources()[[cid]]), species, !is.null(plants))
  grouping <- chosen_grouping(cid, choice$grouping, choices)
  own <- own_rows(cid, store$sources()[[cid]], store$sheets())
  predictions <- model_predictions(cid, fit, grouping, species, plants = plants, own = own)
  caption <- prediction_caption(cid, grouping, species)
  width <- if (grouping == "population") export_width / 2 else export_width
  table <- prediction_table(table_predictions(cid, fit, grouping, species, predictions, own), cid, grouping)
  list(
    name = model_name(cid),
    folder = model_name(cid),
    title = prediction_title(cid, grouping),
    rows = prediction_groupings[[grouping]],
    note = paste(c(
      "Estimates with their lower and upper 95% compatibility limits.",
      if (!is.null(table$data_support)) support_export_note
    ), collapse = " "),
    table = table,
    figures = paged_figures(
      predictions, model_name(cid), caption, if (grouping == "plant") character() else panel_vars(predictions),
      function(rows) {
        list(plot = prediction_figure(cid, rows, grouping, species), width = width, height = width * prediction_aspect(cid, grouping))
      }
    )
  )
}

# A figure per page of panels (panel_pages()), numbered where there are several
# and captioned with the panels each shows; `figure(rows)` gives a page's plot,
# width and height.
paged_figures <- function(rows, name, caption, vars, figure) {
  pages <- panel_pages(rows, vars)
  lapply(seq_along(pages), function(i) {
    paged <- length(pages) > 1
    c(
      list(
        name = if (paged) paste(name, i, sep = "_") else name,
        caption = if (paged) sprintf("%s %s: %s.", caption, panel_label(pages), names(pages)[[i]]) else caption
      ),
      figure(page_rows(rows, pages, pages[[i]]))
    )
  })
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
  name <- derived_name(eid, measure)
  figure <- function(site) {
    list(
      name = paste(name, file_name(site), sep = "_"),
      plot = derived_figure(rows, site), width = export_width, height = derived_height(rows, site) / 96,
      caption = derived_caption(eid, labels, sum_key, site)
    )
  }
  by_survey <- eid == "survey" && sum_key == "survey"
  pages <- function() {
    paged_figures(rows, name, derived_caption(eid, labels, sum_key), panel_vars(rows), function(page) {
      list(plot = derived_figure(page), width = export_width, height = derived_height(page) / 96)
    })
  }
  list(
    name = name,
    folder = derived_folder(eid, measure),
    title = sprintf("%s: %s (%s)", estimate_label(eid), labels$label, labels$unit),
    rows = sums[[sum_key]]$label,
    note = paste("Estimates with their lower and upper 95% compatibility limits.", support_export_note),
    table = biomass_table(rows, store$stand_ins()),
    figures = if (by_survey) lapply(unique(as.character(rows$site)), figure) else pages()
  )
}

support_export_note <- "data_support names the models without data for a row's site-year and how their estimates were made."

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
      if (fits_here(source)) {
        row(
          label, c(paste("Prior:", priors$label), "Thinning (nthin)", "adapt_delta", "Seed"),
          c(format_prior(priors), format(sampler$nthin), format(sampler$adapt_delta), format(sampler$seed, scientific = FALSE))
        )
      },
      if (length(stand_ins[[id]]) > 0) row(label, "Representative sites", paste(stand_ins[[id]], collapse = ", "))
    )
  })
  do.call(rbind, c(list(run), models))
}

# The results as a ZIP: a folder per estimate (export_estimates()), with
# its table as a CSV file and its figures as PNG and PDF files; the settings as
# a CSV file; and a README that lists each estimate, what its rows are, its
# notes or error, and its figures' captions.
write_results <- function(estimates, settings, path) {
  dir <- tempfile("kb-results-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  write_csv <- function(rows, file) utils::write.csv(as.data.frame(rows), file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  estimated <- Filter(function(x) is.null(x$error), estimates)
  n_figures <- sum(lengths(lapply(estimated, `[[`, "figures")))
  done <- 0
  for (estimate in estimated) {
    folder <- file.path(dir, estimate$folder)
    dir.create(folder, recursive = TRUE)
    write_csv(estimate$table, file.path(folder, paste0(estimate$name, ".csv")))
    for (figure in estimate$figures) {
      done <- done + 1
      incProgress(0, detail = sprintf("Figure %d of %d", done, n_figures))
      for (ext in c("png", "pdf")) save_figure(figure, folder, ext)
      incProgress(0.5 / n_figures)
    }
  }
  write_csv(settings, file.path(dir, "settings.csv"))
  writeLines(results_readme(estimates), file.path(dir, "README.txt"), useBytes = TRUE)
  files <- withr::with_dir(dir, list.files(recursive = TRUE))
  status <- withr::with_dir(dir, utils::zip(path, files, flags = "-q"))
  if (status != 0) stop("The results could not be zipped.", call. = FALSE)
}

results_readme <- function(estimates) {
  entry <- function(estimate) {
    if (!is.null(estimate$error)) {
      return(c(paste0(estimate$folder, "/"), paste("  ", estimate$title), paste("   Could not be estimated:", estimate$error), ""))
    }
    c(
      paste0(estimate$folder, "/"),
      paste("  ", estimate$title),
      paste("   Rows:", estimate$rows),
      paste("  ", estimate$note),
      vapply(estimate$figures, function(figure) sprintf("   %s: %s", figure$name, figure$caption), ""),
      ""
    )
  }
  c(
    "kelpbio results",
    "",
    "Each folder holds one estimate: its table as a CSV file and its figures as PNG and PDF files. Biomass is in a folder per measure (wet, dry, carbon and nitrogen) within in_situ_biomass and canopy_biomass.",
    "settings.csv holds the sources, the prior and sampler settings each model was fitted with, and the representative sites of the run.",
    "",
    unlist(lapply(estimates, entry))
  )
}

# An export_estimates() figure saved in `dir` in the app's figure theme, as a
# PNG or PDF; returns its path.
save_figure <- function(figure, dir, ext) {
  file <- file.path(dir, paste0(figure$name, ".", ext))
  ggplot2::ggsave(file, figure$plot + figure_theme(), width = figure$width, height = figure$height, units = "in", dpi = 300)
  file
}

# A fit bundle: the run's data, sources, settings, fits to your data (with the
# settings each was fitted with and the warnings kelpbio gave), stand-in sites and estimate choices. Pre-fit
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
      fits = lapply(records[fitted], function(record) list(fit = record$fit, settings = record$settings, warnings = record$warnings)),
      stand_ins = store$stand_ins_chosen(), estimate_choices = store$estimate_choices()
    ),
    class = "kelpbioshiny_bundle"
  )
}

# A fit bundle read from `path`, checked to be one: it errors otherwise. Every
# field the Data step restores is checked, so a damaged bundle, or one from a
# version of the app with other models, settings or priors, is refused before
# any of it reaches the run.
read_bundle <- function(path) {
  bundle <- tryCatch(readRDS(path), error = function(e) NULL)
  if (!inherits(bundle, "kelpbioshiny_bundle")) {
    stop("The file is not a fit bundle. Download one from the Export step.", call. = FALSE)
  }
  if (!valid_bundle(bundle)) {
    version <- format(utils::packageVersion("kelpbioshiny"))
    saved <- if (is_string(bundle$kelpbioshiny)) bundle$kelpbioshiny else "an unknown version"
    stop(paste0(
      "The fit bundle is damaged or from another version of the app",
      if (!identical(saved, version)) sprintf(" (saved with kelpbioshiny %s, this is %s)", saved, version), "."
    ), call. = FALSE)
  }
  bundle
}

is_string <- function(x) is.character(x) && length(x) == 1 && !is.na(x)

# Whether `x` is a list with a value for each of `ids` (all of them, or only
# some with `all = FALSE`), each passing `check`.
valid_by_id <- function(x, ids, check, all = TRUE) {
  is.list(x) && (length(x) == 0 || !is.null(names(x))) &&
    all(names(x) %in% ids) && (!all || all(ids %in% names(x))) &&
    all(vapply(names(x), function(id) isTRUE(check(x[[id]], id)), logical(1)))
}

# A model's priors as the prior editor holds them: the rows of
# default_priors() for the species, with the same names and families.
valid_priors <- function(priors, id, species) {
  defaults <- default_priors(id, species)
  is.data.frame(priors) && all(names(defaults) %in% names(priors)) &&
    identical(priors$name, defaults$name) && identical(priors$family, defaults$family) &&
    is.numeric(priors$a) && is.numeric(priors$b)
}

valid_sampler <- function(sampler) {
  is.list(sampler) && setequal(names(sampler), names(default_sampler())) &&
    all(vapply(sampler, function(x) is.numeric(x) && length(x) == 1, logical(1)))
}

valid_bundle <- function(bundle) {
  species <- bundle$species
  if (!is_string(species) || !species %in% names(species_info)) {
    return(FALSE)
  }
  is_string(bundle$size_data) && bundle$size_data %in% names(size_data_info) &&
    (is.null(bundle$workbook) || is_string(bundle$workbook)) &&
    valid_by_id(bundle$sheets, sheet_ids, function(sheet, id) is.data.frame(sheet$rows) && is_string(sheet$file), all = FALSE) &&
    is.character(bundle$sources) && setequal(names(bundle$sources), component_ids) &&
    valid_by_id(bundle$priors, component_ids, function(priors, id) valid_priors(priors, id, species)) &&
    valid_by_id(bundle$samplers, component_ids, function(sampler, id) valid_sampler(sampler)) &&
    valid_by_id(bundle$fits, component_ids, function(fit, id) {
      inherits(fit$fit, "kb_fit") && valid_priors(fit$settings$priors, id, species) && valid_sampler(fit$settings$sampler) &&
        (is.null(fit$warnings) || is.character(fit$warnings))
    }, all = FALSE) &&
    valid_by_id(bundle$stand_ins, component_ids, function(sites, id) is.character(sites), all = FALSE) &&
    valid_by_id(bundle$estimate_choices, estimate_ids, function(choices, id) {
      is.list(choices) && all(vapply(choices, is_string, logical(1)))
    }, all = FALSE)
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
    page_header("Export"),
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
    enabled <- reactive(c(results = store$any_estimate(), bundle = any_fitted()))

    files <- list(
      results = cached_file(function(path) write_results(export_estimates(store), settings_rows(store), path), ".zip"),
      bundle = cached_file(function(path) saveRDS(run_bundle(store), path), ".rds")
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
          built <- withProgress(
            message = sprintf("Preparing the %s", tolower(item$label)),
            tryCatch(files[[key]](), error = function(e) e)
          )
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
      if (!store$any_estimate()) "Results become available once there is an estimate to save."
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

# The script lines of each model's fit (`blocks`), joined, without a block's
# opening comment where it repeats the opening comment of the block before, as
# when several models come from the same pre-fit source.
drop_repeated_comments <- function(blocks) {
  opening <- vapply(blocks, function(lines) if (startsWith(lines[[1]], "#")) lines[[1]] else NA_character_, character(1))
  repeated <- !is.na(opening) & (opening == c(NA_character_, utils::head(opening, -1))) %in% TRUE
  unlist(lapply(seq_along(blocks), function(i) if (repeated[[i]]) blocks[[i]][-1] else blocks[[i]]))
}
