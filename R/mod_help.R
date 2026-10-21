# The Help tab: a user guide, a methods page and an about page, picked in the
# sidebar as on the Models and Estimates steps. Help is not a step, so it carries no step marker
# and takes no part in step completion. The guide is built
# from the definitions the app already uses (app_steps, sheet_info, components,
# prefit_info, warning_help, help_topics), so it changes when they do; only the
# connecting sentences are written here.

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

# A sheet's columns, by species and kind of size data where they differ.
guide_columns <- function(id) {
  variants <- list(
    list(label = em(species_info$nereo$latin), species = "nereo", census = FALSE),
    list(label = tagList(em(species_info$nereo$latin), ", ", tolower(size_data_info$census$label)), species = "nereo", census = TRUE),
    list(label = em(species_info$macro$latin), species = "macro", census = TRUE)
  )
  columns <- lapply(variants, function(v) paste(sheet_columns(id, v$species, v$census), collapse = ", "))
  if (length(unique(columns)) == 1) {
    return(tags$code(columns[[1]]))
  }
  # The Nereocystis census only where its columns differ.
  shown <- if (identical(columns[[1]], columns[[2]])) c(1, 3) else 1:3
  lapply(shown, function(i) div(variants[[i]]$label, ": ", tags$code(columns[[i]])))
}

guide_sheets <- function() {
  guide_table(
    c("Sheet", "One row per", "Columns"),
    lapply(sheet_ids, function(id) list(tags$code(id), sheet_info[[id]]$row, guide_columns(id)))
  )
}

# Each model's sources, naming the species of a pre-fit model only one has.
guide_sources <- function() {
  guide_table(
    c("Model", "Sources"),
    lapply(component_ids, function(id) {
      by_species <- lapply(names(species_info), function(species) model_sources(id, species = species))
      sources <- unique(unlist(by_species))
      labels <- vapply(sources, function(source) {
        species <- names(species_info)[vapply(by_species, function(x) source %in% x, logical(1))]
        label <- htmltools::htmlEscape(source_label(source))
        if (length(species) == 1) sprintf("%s (<em>%s</em> only)", label, species_info[[species]]$latin) else label
      }, "")
      list(label_of(id), HTML(paste(labels, collapse = ", ")))
    })
  )
}

# Extra content under a step's heading in the guide.
guide_extras <- list(
  data = function() {
    tagList(
      div(
        class = "small text-body-secondary",
        paste(
          "The columns of each sheet. Every sheet is optional, and other columns are ignored.",
          sprintf(
            "For Nereocystis, choose under Size data whether a sample of plants was measured or all of them. With %s, a census, the size sheet holds every counted plant and the survey sheet has no plants column. Macrocystis surveys are always a census.",
            tolower(size_data_info$census$label)
          )
        )
      ),
      div(class = "table-responsive", guide_sheets())
    )
  },
  models = function() {
    tagList(
      div(class = "small text-body-secondary", "Each model uses one source: your data, a pre-fit model, or none. With all plants measured (a census) there is no size model."),
      div(class = "table-responsive", guide_sources())
    )
  },
  export = function() {
    tagList(
      div(class = "small text-body-secondary", "The results workbook and the figures hold every available estimate, at the choices made on its page on the Estimates step (such as Group by, Scale and Show), in each measure available. To resume a run later, download its fit bundle and add it under Resume a run on the Data step."),
      tags$ul(class = "small text-body-secondary mb-0", lapply(download_items, function(item) tags$li(sprintf("%s (%s)", item$label, item$detail))))
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

# The warnings and notes the app shows, each with what to do about it, from
# warning_help.
guide_warnings <- function() {
  entries <- function(items) {
    tags$dl(
      class = "row mb-0",
      lapply(items, function(warning) {
        tagList(
          tags$dt(class = "col-md-3 fw-medium", warning$title),
          tags$dd(class = "col-md-9 text-body-secondary mb-3", warning$advice)
        )
      })
    )
  }
  note <- vapply(warning_help, function(warning) isTRUE(warning$note), logical(1))
  tagList(
    h3(class = "fs-6 fw-semibold", "Warnings"),
    p(class = "small text-body-secondary", "The fit or data cannot be relied on until they are fixed."),
    entries(warning_help[!note]),
    h3(class = "fs-6 fw-semibold mt-2", "Notes"),
    p(class = "small text-body-secondary", "The fit is valid; check what is flagged."),
    entries(warning_help[note])
  )
}

# Predicting the weight of each plant without biomass, after the steps.
guide_plant_weights <- function() {
  div(
    class = "small text-body-secondary",
    sprintf(
      "The app can also predict the wet weight of each plant, without biomass. Load a %s sheet, use the %s model (fitted to your data or pre-fit), and open %s on the Estimates step with Group by set to %s.",
      components$size$sheet, lower_label("weight"), label_of("weight"), prediction_groupings[["plant"]]
    )
  )
}

# The guide's sections, by anchor id, as titled on the page and in its contents.
guide_sections <- c(guide_steps = "Steps", guide_warnings = "Warnings and notes", guide_glossary = "Glossary")

help_pages <- c(guide = "User guide", methods = "Methods", about = "About")
help_page_icons <- c(guide = "book-open", methods = "sigma", about = "info")

help_ui <- function() {
  step_layout(
    card(card_body(padding = "0.5rem", uiOutput("help_subnav"))),
    navset_hidden(
      id = "help_page",
      nav_panel_hidden("guide", help_guide_ui()),
      nav_panel_hidden("methods", help_methods_ui()),
      nav_panel_hidden("about", help_about_ui())
    )
  )
}

# The sidebar: a link per page, with the guide's sections under it while it is
# open; each section link jumps to its section, and the page stays scrollable.
help_server <- function(input, output, session) {
  lapply(names(help_pages), function(value) {
    observeEvent(input[[paste0("help_nav_", value)]], nav_select("help_page", value))
  })
  output$help_subnav <- renderUI({
    page <- input$help_page %||% "guide"
    tags$nav(
      class = "nav nav-pills flex-column gap-1",
      `aria-label` = "Help",
      lapply(names(help_pages), function(value) {
        tagList(
          subnav_link(paste0("help_nav_", value), page == value, lucide(help_page_icons[[value]], "text-body-secondary"), help_pages[[value]]),
          if (value == "guide" && page == "guide") {
            div(
              class = "d-flex flex-column gap-1 small ps-4 ms-2 pb-1",
              lapply(names(guide_sections), function(id) {
                tags$a(href = paste0("#", id), class = "link-body-emphasis text-decoration-none py-1", guide_sections[[id]])
              })
            )
          }
        )
      }),
      # Not a page: GitHub's new issue form, in a new tab.
      tags$hr(class = "my-1"),
      tags$a(
        href = paste0(source_url, "/issues/new"), target = "_blank", rel = "noopener", class = "nav-link py-2 px-2",
        div(
          class = "d-flex align-items-center gap-2",
          lucide("github", "text-body-secondary"), span(class = "flex-grow-1", "Report an issue"),
          lucide("external-link", "text-body-secondary")
        )
      )
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
        description = "The warnings and notes a model or sheet can show, and what to do about them.",
        guide_warnings()
      ),
      panel(
        guide_sections[["guide_glossary"]],
        id = "guide_glossary", class = "kb-anchor",
        description = "The terms behind the help icons in the app.",
        guide_glossary()
      )
    )
  )
}

# A kelpbio article as a tile: the whole tile opens it in a new tab.
article_tile <- function(article) {
  div(
    class = "kb-tile position-relative d-flex flex-column gap-1 rounded-3 p-3 small",
    tags$a(
      href = paste0(kelpbio_url, article$path), target = "_blank", rel = "noopener",
      class = "stretched-link fw-medium link-body-emphasis text-decoration-none d-inline-flex align-items-center gap-1",
      article$title, lucide("external-link", "text-body-secondary")
    ),
    div(class = "text-body-secondary", article$summary)
  )
}

# The statistics are documented in kelpbio, so this page only points to them.
help_methods_ui <- function() {
  tagList(
    page_header(help_pages[["methods"]], "The models, priors and diagnostics the app runs are documented in the kelpbio R package."),
    div(
      class = "d-flex flex-column gap-3",
      panel(
        "kelpbio articles",
        description = "Each article opens in a new tab.",
        layout_column_wrap(width = "16rem", fill = FALSE, gap = "0.75rem", !!!unname(lapply(kelpbio_articles, article_tile)))
      ),
      card(card_body(div(
        class = "d-flex flex-wrap align-items-center gap-3",
        div(class = "kb-tile-icon bg-primary-subtle text-primary-emphasis", lucide("book-open")),
        div(
          class = "flex-grow-1",
          h2(class = "kb-card-title mb-0", "kelpbio documentation"),
          div(class = "small text-body-secondary mt-1", "Every article, and the reference for each function the app calls.")
        ),
        tags$a(
          href = kelpbio_url, target = "_blank", rel = "noopener", class = "btn btn-light border",
          span(class = "d-inline-flex align-items-center gap-2", "Open the kelpbio documentation", lucide("external-link"))
        )
      )))
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

# A fact about the app, such as a version or its licence, as a muted badge.
about_fact <- function(...) span(class = "badge border text-body-secondary fw-medium", ...)

# A link out of the app, as a small outline button with an icon.
about_link <- function(href, icon, text) {
  tags$a(
    href = href, target = "_blank", rel = "noopener", class = "btn btn-light border btn-sm",
    span(class = "d-inline-flex align-items-center gap-2", lucide(icon), text)
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
          "It was developed by Poisson Consulting for the Hakai Institute, funded by the Tula Foundation. ",
          "Estimates depend on the data and the model choices; check them with the diagnostics on the Models step."
        ),
        div(
          class = "d-flex flex-wrap align-items-center gap-2",
          about_fact("kelpbioshiny ", package_version_text("kelpbioshiny")),
          about_fact("kelpbio ", package_version_text("kelpbio")),
          about_fact("MIT licence"),
          span(class = "flex-grow-1"),
          about_link(kelpbio_url, "book-open", "kelpbio"),
          about_link(source_url, "github", "GitHub")
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
