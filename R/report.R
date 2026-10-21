# The report: a PDF of the run, written as Quarto markdown and rendered to PDF
# with Typst by the Quarto command-line tool, which quarto::quarto_path() finds
# (on the PATH, or at the QUARTO_PATH environment variable). Its text comes
# from the app's definitions and kelpbio: the run summary, kelpbio's
# description of each model, the diagnostics of each model fitted to the run's
# data, and the estimates of the results workbook (export_estimates()).

report_available <- function() !is.null(quarto::quarto_path())

# What the report says about the run, read from the store.
# jarl-ignore unused_function: the report download is off for now, so its call in mod_export.R is commented out.
report_content <- function(store) {
  sources <- store$sources()
  records <- store$records()
  sheets <- store$sheets()
  models <- intersect(model_order, store$shown_models())
  used <- Filter(function(id) !sources[[id]] %in% c("none", "census"), models)
  fitted <- Filter(function(id) sources[[id]] == "user" && records[[id]]$status == "fitted", used)
  describe <- function(fit, prose = FALSE) withr::with_output_sink(nullfile(), kb_model_describe(fit, prose = prose))
  list(
    species = store$species(),
    census = store$census(),
    sheets = lapply(sheets, function(sheet) list(file = sheet$file, rows = nrow(sheet$rows))),
    sources = vapply(models, function(id) source_label(sources[[id]]), ""),
    settings = settings_rows(store),
    methods = lapply(stats::setNames(nm = used), function(id) {
      fit <- store$fit_of(id)
      if (!is.null(fit)) list(prose = describe(fit, prose = TRUE), notation = describe(fit))
    }),
    diagnostics = lapply(stats::setNames(nm = fitted), function(id) {
      fit_diagnostics(records[[id]]$fit, store$sensitivity[[id]](), store$influence[[id]]())
    }),
    warnings = model_warnings(store$statuses(), fitted)
  )
}

# The diagnostics of a fit, in words: its convergence, divergent transitions,
# prior sensitivity (from kb_sensitivity() rows) and influential observations
# (from kb_influence() rows).
fit_diagnostics <- function(fit, sensitivity, influence) {
  fit_summary <- summary(fit)
  unconverged <- fit_summary$coefficients$term[!fit_summary$coefficients$converged]
  flags <- sensitivity_flags(sensitivity)
  influential <- sum(influence$influential)
  list(
    convergence = if (length(unconverged) == 0) "All parameters converged" else sprintf("Not converged: %s", and_list(unconverged)),
    divergence = sprintf("%s%% of draws", format(fit_summary$perc_divergent, digits = 2)),
    sensitivity = if (length(flags$influencing) > 0) {
      sprintf("Prior influencing the estimate: %s", and_list(flags$influencing))
    } else if (length(flags$weak_data) > 0) {
      sprintf("Not strongly informed by the data: %s", and_list(flags$weak_data))
    } else {
      "No prior sensitivity"
    },
    influence = if (influential == 0) "None" else sprintf("%d to check", influential)
  )
}

# Text from the user or kelpbio, escaped so Markdown shows it as written.
md_escape <- function(x) gsub("([][\\\\`*_{}<>#|$@~^])", "\\\\\\1", x)

# A data frame as a Markdown pipe table, its columns headed by column_label().
# Estimates and areas get thousands separators; text, years and survey ids are
# shown as they are.
md_table <- function(rows) {
  cells <- lapply(names(rows), function(name) {
    x <- rows[[name]]
    text <- if (is.numeric(x) && !name %in% c("year", "survey_id")) {
      vapply(x, format, "", scientific = FALSE, big.mark = ",", trim = TRUE)
    } else {
      as.character(x)
    }
    text[is.na(x)] <- ""
    md_escape(text)
  })
  c(
    paste0("| ", paste(md_escape(vapply(names(rows), column_label, "")), collapse = " | "), " |"),
    paste0("|", paste(rep("---", ncol(rows)), collapse = "|"), "|"),
    paste0("| ", do.call(paste, c(cells, sep = " | ")), " |")
  )
}

# The report's Quarto markdown. `figures` are the paths of the estimates'
# figures by figure name, relative to the report.
report_markdown <- function(content, estimates, figures) {
  url <- function(article) paste0(kelpbio_url, kelpbio_articles[[article]]$path)
  link <- function(article) sprintf("[%s](%s)", kelpbio_articles[[article]]$title, url(article))
  run <- content$settings[content$settings$model == "Run", ]
  run_value <- function(setting) run$value[run$setting == setting]
  settings <- content$settings[content$settings$model != "Run" & content$settings$setting != "Source", ]

  header <- c(
    "---",
    "title: \"Kelp biomass estimates\"",
    sprintf("subtitle: \"*%s*\"", species_info[[content$species]]$latin),
    sprintf("date: \"%s\"", format(Sys.Date(), "%Y-%m-%d")),
    "format:",
    "  typst:",
    "    papersize: us-letter",
    "    margin:",
    "      x: 2cm",
    "      y: 2cm",
    "    fontsize: 10pt",
    "    toc: true",
    "    section-numbering: \"1.1\"",
    # Links in the app's primary colour, and table cells not justified.
    "    include-in-header:",
    "      - text: |",
    sprintf("          #show link: set text(fill: rgb(\"%s\"))", app_palette$primary),
    "          #show table: set par(justify: false)",
    "---",
    ""
  )

  summary <- c(
    "# Run", "",
    sprintf(
      "This report was generated by kelpbioshiny %s with kelpbio %s on %s. Size data: %s. Every model fitted to the run's data was fitted with %s.",
      run_value("kelpbioshiny version"), run_value("kelpbio version"), run_value("Created"),
      tolower(run_value("Size data")), run_value("Draws per fit")
    ),
    "",
    "## Data", "",
    if (length(content$sheets) == 0) "No data were loaded." else {
      sprintf("- %s: %d rows, from %s", names(content$sheets), vapply(content$sheets, `[[`, integer(1), "rows"), md_escape(vapply(content$sheets, `[[`, "", "file")))
    },
    "",
    "## Models", "",
    md_table(data.frame(
      model = vapply(names(content$sources), label_of, ""),
      purpose = vapply(names(content$sources), function(id) components[[id]]$detail, ""),
      source = unname(content$sources)
    ) |> stats::setNames(c("Model", "Purpose", "Source"))),
    "",
    if (nrow(settings) > 0) {
      c(
        "The prior and sampler settings each model was fitted with, and the sites standing in for sites a model has no data for:", "",
        md_table(stats::setNames(settings, c("Model", "Setting", "Value"))), ""
      )
    }
  )

  methods <- c(
    "# Methods", "",
    sprintf("Each model is described below as kelpbio describes it. %s and %s give the details, and %s describes the reference data of the pre-fit models.", link("models"), link("priors"), link("prefit")),
    "",
    unlist(lapply(names(content$methods), function(id) {
      method <- content$methods[[id]]
      c(
        sprintf("## %s model", label_of(id)), "",
        sprintf("Source: %s.", content$sources[[id]]), "",
        if (!is.null(method)) c(md_escape(paste(method$prose, collapse = " ")), "", "```", method$notation, "```", "")
      )
    })),
    "## Further reading", "",
    vapply(names(kelpbio_articles), function(article) sprintf("- %s: %s", link(article), kelpbio_articles[[article]]$summary), ""),
    ""
  )

  diagnostics <- c(
    "# Diagnostics", "",
    sprintf("The checks of each model fitted to the run's data, as %s explains them. Pre-fit models are kelpbio's reference fits.", link("diagnostics")),
    "",
    if (length(content$diagnostics) == 0) "No model was fitted to the run's data." else {
      c(
        md_table(data.frame(
          model = vapply(names(content$diagnostics), label_of, ""),
          do.call(rbind, lapply(content$diagnostics, as.data.frame))
        ) |> stats::setNames(c("Model", "Convergence", "Divergent transitions", "Prior sensitivity", "Influential observations"))),
        ""
      )
    },
    if (length(content$warnings) > 0) {
      c(
        "Warnings, and what to do about them:", "",
        unlist(lapply(names(content$warnings), function(id) {
          keys <- c(`fit failed` = "failed", convergence = "convergence", `divergent transitions` = "divergence", `prior sensitivity` = "prior")
          vapply(content$warnings[[id]], function(w) {
            help <- warning_help[[keys[[w]]]]
            sprintf("- %s model, %s: %s", label_of(id), tolower(help$title), help$advice)
          }, "")
        })),
        ""
      )
    }
  )

  results <- c(
    "# Results", "",
    "Each estimate is given with its lower and upper 95% compatibility limits, at the choices made on the Estimates step.",
    "",
    unlist(lapply(estimates, function(estimate) {
      c(
        sprintf("## %s", md_escape(estimate$title)), "",
        if (!is.null(estimate$error)) {
          sprintf("Could not be estimated: %s", md_escape(estimate$error))
        } else {
          table <- estimate$table
          if ("flags" %in% names(table)) {
            table$flags <- gsub("|", "; ", table$flags, fixed = TRUE)
            if (all(table$flags == "")) table$flags <- NULL
          }
          c(
            sprintf("Rows: %s.", tolower(estimate$rows)), "",
            unlist(lapply(estimate$figures, function(figure) {
              width <- round(100 * figure$width / export_width)
              c(sprintf("![%s](%s){width=%d%%}", md_escape(figure$caption), figures[[figure$name]], width), "")
            })),
            md_table(table)
          )
        },
        ""
      )
    }))
  )

  c(header, summary, methods, diagnostics, results)
}

# Renders the report to `path`, with the estimates' figures as PNGs.
# jarl-ignore unused_function: the report download is off for now, so its call in mod_export.R is commented out.
write_report <- function(content, estimates, path) {
  dir <- tempfile("kb-report-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  done <- Filter(function(x) is.null(x$error), estimates)
  figures <- unlist(lapply(done, `[[`, "figures"), recursive = FALSE)
  files <- vapply(figures, function(figure) basename(save_figure(figure, dir, "png")), "")
  names(files) <- vapply(figures, `[[`, "", "name")
  qmd <- file.path(dir, "report.qmd")
  con <- file(qmd, open = "w", encoding = "UTF-8")
  writeLines(report_markdown(content, estimates, files), con)
  close(con)
  quarto::quarto_render(qmd, output_format = "typst", quiet = TRUE)
  file.copy(file.path(dir, "report.pdf"), path, overwrite = TRUE)
}
