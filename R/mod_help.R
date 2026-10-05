# The Help tab: a user guide and an about page, picked in the sidebar as on the
# Models and Estimates steps. Help is not a step, so it carries no step marker
# and takes no part in step completion. The guide is built
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
    c("Sheet", "Model", "Columns"),
    lapply(component_ids, function(id) {
      def <- components[[id]]
      list(tags$code(def$sheet), def$label, guide_columns(id))
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
      div(class = "small text-body-secondary", "The columns of each sheet. Every sheet is optional, and other columns are ignored."),
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

# Predicting the weight of each plant without biomass, after the steps.
guide_plant_weights <- function() {
  div(
    class = "small text-body-secondary",
    sprintf(
      "The app can also predict the wet weight of each plant, without biomass. Load a %s sheet, use the %s model (fitted to your data or pre-fit), and open %s on the Estimates step with Group by set to %s. The %s example workbook shows this.",
      components$size$sheet, lower_label("weight"), label_of("weight"), prediction_groupings[["plant"]], example_workbooks$size_only$label
    )
  )
}

# The guide's sections, by anchor id, as titled on the page and in its contents.
guide_sections <- c(guide_steps = "Steps", guide_warnings = "Warnings", guide_methods = "Methods", guide_glossary = "Glossary")

# The pointer to kelpbio, which explains the statistics the app runs.
guide_docs_callout <- function() {
  card(id = "guide_methods", class = "kb-anchor", card_body(div(
    class = "d-flex flex-wrap align-items-center gap-3",
    div(class = "kb-tile-icon bg-primary-subtle text-primary-emphasis", lucide("book-open")),
    div(
      class = "flex-grow-1",
      h2(class = "kb-card-title mb-0", guide_sections[["guide_methods"]]),
      div(class = "small text-body-secondary mt-1", "The models, priors and diagnostics are explained in the kelpbio documentation.")
    ),
    tags$a(
      href = kelpbio_url, target = "_blank", rel = "noopener", class = "btn btn-light border",
      span(class = "d-inline-flex align-items-center gap-2", "Open the kelpbio documentation", lucide("external-link"))
    )
  )))
}

help_pages <- c(guide = "User guide", about = "About")

help_ui <- function() {
  step_layout(
    card(card_body(padding = "0.5rem", uiOutput("help_subnav"))),
    navset_hidden(
      id = "help_page",
      nav_panel_hidden("guide", help_guide_ui()),
      nav_panel_hidden("about", help_about_ui())
    )
  )
}

# The sidebar: a link per page, with the guide's sections under it while it is
# open; each section link jumps to its section, and the page stays scrollable.
help_server <- function(input, output, session) {
  observeEvent(input$help_nav_guide, nav_select("help_page", "guide"))
  observeEvent(input$help_nav_about, nav_select("help_page", "about"))
  output$help_subnav <- renderUI({
    page <- input$help_page %||% "guide"
    icons <- c(guide = "book-open", about = "info")
    tags$nav(
      class = "nav nav-pills flex-column gap-1",
      `aria-label` = "Help",
      lapply(names(help_pages), function(value) {
        tagList(
          subnav_link(paste0("help_nav_", value), page == value, lucide(icons[[value]], "text-body-secondary"), help_pages[[value]]),
          if (value == "guide" && page == "guide") {
            div(
              class = "d-flex flex-column gap-1 small ps-4 ms-2 pb-1",
              lapply(names(guide_sections), function(id) {
                tags$a(href = paste0("#", id), class = "link-body-emphasis text-decoration-none py-1", guide_sections[[id]])
              })
            )
          }
        )
      })
    )
  })
  # Rendered while Help is hidden: opening Help from a link switches the step
  # and the page at once, and Shiny can then miss that the sidebar is shown.
  outputOptions(output, "help_subnav", suspendWhenHidden = FALSE)
}

help_guide_ui <- function() {
  tagList(
    page_header(help_pages[["guide"]], "How to use the app, step by step."),
    div(
      class = "d-flex flex-column gap-3",
      panel(
        guide_sections[["guide_steps"]],
        id = "guide_steps", class = "kb-anchor",
        description = "A run moves through four steps in the navbar. A step's marker shows a check mark once the step is complete: data loaded, models ready, or estimates saved.",
        div(
          class = "d-flex flex-column gap-4",
          lapply(names(steps), function(value) {
            step_item(value, if (!is.null(guide_extras[[value]])) guide_extras[[value]](), small = FALSE)
          })
        ),
        guide_plant_weights()
      ),
      panel(
        guide_sections[["guide_warnings"]],
        id = "guide_warnings", class = "kb-anchor",
        description = "The warnings a model or sheet can show, and how to fix them.",
        guide_warnings()
      ),
      guide_docs_callout(),
      panel(
        guide_sections[["guide_glossary"]],
        id = "guide_glossary", class = "kb-anchor",
        description = "The terms behind the help icons in the app.",
        guide_glossary()
      )
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
    page_header(help_pages[["about"]], app_purpose),
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
