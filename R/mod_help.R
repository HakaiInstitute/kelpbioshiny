# The Help tab: a user guide and an about page, as two pills. Help is not a
# step, so it carries no step marker and takes no part in step completion. The guide is built
# from the definitions the app already uses (app_steps, components, prefit_info,
# warning_help, help_topics), so it changes when they do; only the connecting
# sentences are written here.

source_url <- "https://github.com/HakaiInstitute/kelpbioshiny"

external_link <- function(href, text) {
  tags$a(href = href, target = "_blank", rel = "noopener", text)
}

guide_table <- function(header, rows) {
  tags$table(
    class = "table table-sm small mb-0",
    tags$thead(tags$tr(lapply(header, tags$th))),
    tags$tbody(lapply(rows, function(row) tags$tr(lapply(row, tags$td))))
  )
}

# A sheet's columns, by species where they differ.
guide_columns <- function(id) {
  columns <- lapply(names(species_info), function(sp) paste(sheet_columns(id, sp), collapse = ", "))
  if (!is.list(components[[id]]$columns)) {
    return(tags$code(columns[[1]]))
  }
  lapply(seq_along(columns), function(i) div(em(species_info[[i]]$latin), ": ", tags$code(columns[[i]])))
}

guide_sheets <- function() {
  guide_table(
    c("Sheet", "Model", "Columns", "Sheet needed"),
    lapply(component_ids, function(id) {
      def <- components[[id]]
      list(tags$code(def$sheet), def$label, guide_columns(id), if (sheet_required(id)) "Required" else "Optional")
    })
  )
}

guide_sources <- function() {
  guide_table(
    c("Model", "Sources"),
    lapply(component_ids, function(id) {
      list(label_of(id), paste(vapply(components[[id]]$sources, source_label, ""), collapse = ", "))
    })
  )
}

# Extra content under a step's heading in the guide.
guide_extras <- list(
  data = function() {
    tagList(
      div(class = "small text-body-secondary", "The columns of each sheet. Other columns are ignored."),
      div(class = "table-responsive", guide_sheets())
    )
  },
  models = function() {
    tagList(
      div(class = "small text-body-secondary", "Each model uses one source: your data, a pre-fit model, or none."),
      div(class = "table-responsive", guide_sources())
    )
  }
)

guide_glossary <- function() {
  tags$dl(
    class = "row mb-0",
    lapply(names(help_topics), function(key) {
      topic <- help_topics[[key]]
      tagList(
        tags$dt(class = "col-md-3 fw-medium", topic$title),
        tags$dd(
          class = "col-md-9 text-body-secondary mb-3",
          div(topic_body(topic)),
          div(class = "small mt-1", external_link(help_url(key), "Learn more"))
        )
      )
    })
  )
}

# The warnings the app shows, each with what to do about it, from warning_help.
guide_warnings <- function() {
  tags$dl(
    class = "row mb-0",
    lapply(warning_help, function(warning) {
      tagList(
        tags$dt(class = "col-md-3 fw-medium", warning$title),
        tags$dd(class = "col-md-9 text-body-secondary mb-3", warning$advice)
      )
    })
  )
}

# The pointer to kelpbio, which explains the statistics the app runs.
guide_docs_callout <- function() {
  card(card_body(div(
    class = "d-flex flex-wrap align-items-center gap-3",
    div(class = "kb-tile-icon bg-primary-subtle text-primary-emphasis", lucide("book-open")),
    div(
      class = "flex-grow-1",
      h2(class = "kb-card-title mb-0", "Statistical details"),
      div(class = "small text-body-secondary mt-1", "The models, priors and diagnostics are explained in the kelpbio documentation.")
    ),
    tags$a(
      href = kelpbio_url, target = "_blank", rel = "noopener", class = "btn btn-primary",
      span(class = "d-inline-flex align-items-center gap-2", "Open the kelpbio documentation", lucide("external-link"))
    )
  )))
}

# A Help page's opening line. The pill above names the page, so its title is
# hidden, there for screen readers and heading navigation.
help_lead <- function(title, text) {
  tagList(h1(class = "visually-hidden", title), div(class = "kb-lead text-body-secondary mb-4", text))
}

help_ui <- function() {
  navset_pill(
    id = "help_page",
    nav_panel("User guide", value = "guide", div(class = "pt-4", help_guide_ui())),
    nav_panel("About", value = "about", div(class = "pt-4", help_about_ui()))
  )
}

help_guide_ui <- function() {
  tagList(
    help_lead("User guide", "How to use the app, step by step."),
    div(
      class = "d-flex flex-column gap-3",
      panel(
        "Steps",
        description = "A run moves through four steps in the navbar. A step's marker shows a check mark once it is complete.",
        div(
          class = "d-flex flex-column gap-4",
          lapply(names(steps), function(value) {
            step_item(value, if (!is.null(guide_extras[[value]])) guide_extras[[value]](), small = FALSE)
          })
        )
      ),
      panel("Warnings and how to fix them", description = "The warnings a model or sheet can show.", guide_warnings()),
      guide_docs_callout(),
      panel("Glossary", description = "The terms behind the help icons in the app.", guide_glossary())
    )
  )
}

# A package's installed version, or "Not installed". The app currently uses
# mocks for the kelpbio functions not yet implemented (R/mock-kelpbio.R), so
# kelpbio need not be installed; once kelpbio is imported, the About page always
# shows its installed version.
package_version_text <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    return("Not installed")
  }
  as.character(utils::packageVersion(package))
}

# The kelpbio citation, from kelpbio's inst/CITATION. Switch to
# utils::citation("kelpbio") once kelpbio is a dependency.
kelpbio_citation <- function() {
  utils::bibentry(
    bibtype = "Manual",
    title = "kelpbio: Bayesian Kelp Biomass Estimation",
    author = utils::person("Seb", "Dalgarno"),
    year = "2026",
    url = "https://github.com/HakaiInstitute/kelpbio"
  )
}

# HTML style renders the title in italics and the URL as a link, where the text
# style would show R's markup characters.
citation_html <- function(citation) HTML(paste(format(citation, style = "html"), collapse = " "))

# A citation in a code block, with a button that copies its text.
citation_block <- function(id, label, citation) {
  div(
    class = "d-flex flex-column gap-2",
    div(
      class = "d-flex align-items-center justify-content-between gap-3",
      div(class = "fw-medium", label),
      copy_button(id, "citation", paste("Copy the", label, "citation"))
    ),
    div(id = id, class = "kb-citation small bg-body-tertiary border rounded-3 p-3", citation_html(citation))
  )
}

help_about_ui <- function() {
  tagList(
    help_lead("About", app_purpose),
    div(
      class = "d-flex flex-column gap-3",
      card(card_body(
        gap = "1rem",
        div(
          class = "text-body-secondary",
          "The app runs the Bayesian models of the kelpbio R package without writing code. ",
          "It was developed by Poisson Consulting for the Hakai Institute, funded by the Tula Foundation."
        ),
        tags$dl(
          class = "row small mb-0",
          tags$dt(class = "col-sm-3", "kelpbioshiny version"), tags$dd(class = "col-sm-9", package_version_text("kelpbioshiny")),
          tags$dt(class = "col-sm-3", "kelpbio version"), tags$dd(class = "col-sm-9", package_version_text("kelpbio")),
          tags$dt(class = "col-sm-3", "Licence"), tags$dd(class = "col-sm-9", "MIT"),
          tags$dt(class = "col-sm-3", "Source code"), tags$dd(class = "col-sm-9", external_link(source_url, source_url)),
          tags$dt(class = "col-sm-3", "kelpbio"), tags$dd(class = "col-sm-9 mb-0", external_link(kelpbio_url, kelpbio_url))
        ),
        div(
          class = "small text-body-secondary",
          "Estimates depend on the data and the model choices; check them with the diagnostics on the Models step."
        )
      )),
      panel(
        "Citation",
        description = "To cite the app and the kelpbio package in publications, use:",
        citation_block("citation_kelpbioshiny", "kelpbioshiny", utils::citation("kelpbioshiny")),
        citation_block("citation_kelpbio", "kelpbio", kelpbio_citation())
      )
    )
  )
}
