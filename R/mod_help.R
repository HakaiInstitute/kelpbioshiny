# The Help tab: a user guide, the data format, the methods, a glossary and an
# about page, picked in the sidebar as on the Models and Estimates steps. Help
# is not a step, so it carries no step marker and takes no part in step
# completion. The pages are built from the definitions the app already uses
# (app_steps, sheet_info, components, download_items, help_topics) and from
# kelpbio: the columns of each sheet from its data dictionary
# (kb_data_columns()), and the methods and the survey terms from its installed
# vignettes (kelpbio_vignette()); only connecting sentences are written here.

source_url <- "https://github.com/HakaiInstitute/kelpbioshiny"

external_link <- function(href, text) {
  tags$a(href = href, target = "_blank", rel = "noopener", text)
}

# An outline button that opens a page of kelpbio's site in a new tab.
kelpbio_button <- function(key, text) {
  tags$a(
    href = kelpbio_link(key),
    target = "_blank",
    rel = "noopener",
    class = "btn btn-light border btn-sm",
    span(
      class = "d-inline-flex align-items-center gap-2",
      text,
      lucide("external-link")
    )
  )
}

help_pages <- c(
  guide = "User guide",
  format = "Data format",
  methods = "Methods",
  glossary = "Glossary",
  about = "About"
)
help_page_icons <- c(
  guide = "book-open",
  format = "file-spreadsheet",
  methods = "sigma",
  glossary = "list-checks",
  about = "info"
)

# User guide --------------------------------------------------------------------------

# A few short points under a step.
guide_points <- function(...) {
  tags$ul(
    class = "small text-body-secondary mb-0 d-flex flex-column gap-1",
    lapply(list(...), tags$li)
  )
}

guide_extras <- list(
  data = function() {
    guide_points(
      "In the sidebar, choose the species and, for Nereocystis, whether all plants were measured vs. a sample of plants were measured. The app is designed to work on one species at a time. For beds with both species, complete separate runs.",
      tagList(
        "Upload an Excel workbook with one sheet per kind of data, or a CSV file per sheet, or try the example workbook. ",
        actionLink("help_to_format", help_pages[["format"]]),
        " lists the sheets and their columns."
      ),
      "Open each sheet's tab to check its data, and correct any data error or warnings before continuing."
    )
  },
  models = function() {
    guide_points(
      "Choose a source for each model: your data, your data appended to a public dataset, a model pre-fit to a public dataset, or none (although some are required).",
      "'Fit all' fits all of the models in the background. If there are model fit warnings, check each model's Diagnostics tab before using its estimates; follow specific recommendations in the warning messages and re-fit models if necessary. Model convergence is especially important for valid estimates."
    )
  },
  estimates = function() {
    guide_points(
      "Review in situ survey biomass, canopy biomass and each model's predictions, each as a figure and a table."
    )
  },
  export = function() {
    guide_points(
      sprintf(
        "Download the %s and %s, or copy an R script that reruns the analysis with kelpbio.",
        tolower(download_items$results$label),
        tolower(download_items$figures$label)
      ),
      sprintf(
        "Save the %s to resume the run later, under Resume a run on the Data step.",
        tolower(download_items$bundle$label)
      )
    )
  }
)

# Where to read more about a step, under its description in the guide: the
# app's Data format and Methods pages, and kelpbio's site for the articles the
# app does not show.
learn_more <- function(...) {
  links <- list(...)
  n <- length(links)
  separators <- c(rep(", ", max(n - 2, 0)), if (n > 1) if (n > 2) ", and " else " and ")
  div(
    class = "small",
    "Learn more: ",
    lapply(seq_len(n), function(i) {
      tagList(links[[i]], if (i < n) separators[[i]] else ".")
    })
  )
}

guide_learn_more <- list(
  data = function() learn_more(
    inline_link("help_to_format_guide", help_pages[["format"]])
  ),
  models = function() learn_more(
    methods_link("modelling-approach", "Modelling approach"),
    tags$a(href = kelpbio_link("prefit"), target = "_blank", rel = "noopener", "Pre-fit models", .noWS = "outside"),
    tags$a(href = kelpbio_link("diagnostics"), target = "_blank", rel = "noopener", "Diagnostics", .noWS = "outside")
  )
)

# The workflow: which sheets feed which models, and how the models combine
# into the estimates, in three columns. Each group is one estimate, its inputs
# stacked on the left and the estimate spanning them on the right.
flow_node <- function(kind, title, detail = NULL) {
  div(
    class = paste0("kb-flow-node kb-flow-", kind),
    div(class = "fw-medium", title),
    if (!is.null(detail)) div(class = "text-body-secondary", detail)
  )
}

# A sheet in the flow; the size sheet's row covers both kinds of size data.
flow_sheet <- function(id, name = tags$code(sheet_name(id))) {
  detail <- paste0("One row per ", row_html(sheet_row(id)))
  if (id == "size") {
    detail <- paste0(detail, "; in a census, ", row_html(sheet_row(id, census = TRUE)))
  }
  flow_node("sheet", name, HTML(detail))
}

flow_model <- function(id, detail = components[[id]]$detail) {
  flow_node("model", paste(label_of(id), "model"), detail)
}

flow_arrow <- function() div(class = "kb-flow-arrow", lucide("arrow-right"))

# One input row: a sheet or estimate, then its model, or a line straight to the
# estimate where nothing is fitted to it.
flow_group <- function(inputs, estimate) {
  div(
    class = "kb-flow-group",
    unlist(
      lapply(seq_along(inputs), function(i) {
        row <- inputs[[i]]
        list(
          tagAppendAttributes(
            row$from,
            style = sprintf("grid-area: %d / 1", i)
          ),
          if (is.null(row$model)) {
            div(
              class = "kb-flow-line",
              style = sprintf("grid-area: %d / 2 / %d / 4", i, i)
            )
          } else {
            list(
              tagAppendAttributes(
                flow_arrow(),
                style = sprintf("grid-area: %d / 2", i)
              ),
              tagAppendAttributes(
                row$model,
                style = sprintf("grid-area: %d / 3", i)
              )
            )
          }
        )
      }),
      recursive = FALSE
    ),
    tagAppendAttributes(
      flow_arrow(),
      style = sprintf("grid-area: 1 / 4 / span %d", length(inputs))
    ),
    tagAppendAttributes(
      estimate,
      style = sprintf("grid-area: 1 / 5 / span %d", length(inputs))
    )
  )
}

guide_workflow <- function() {
  size_names <- tagList(
    tags$code(size_sheet_names[["sample"]]),
    " or ",
    tags$code(size_sheet_names[["census"]])
  )
  div(
    class = "overflow-x-auto",
    div(
      class = "kb-flow small",
      div(class = "kb-flow-heading", style = "grid-column: 1", "Sheets"),
      div(class = "kb-flow-heading", style = "grid-column: 3", "Models"),
      div(class = "kb-flow-heading", style = "grid-column: 5", "Estimates"),
      flow_group(
        list(
          list(from = flow_sheet("weight"), model = flow_model("weight")),
          list(
            from = flow_sheet("size", size_names),
            model = flow_model(
              "size",
              sprintf("%s; none in a census", components$size$detail)
            )
          ),
          list(from = flow_sheet("survey"))
        ),
        flow_node("estimate", estimate_label("survey"))
      ),
      flow_group(
        list(
          list(
            from = flow_node(
              "estimate",
              estimate_label("survey"),
              sprintf(
                "Of the %s rows with canopy imagery",
                sheet_name("survey")
              )
            ),
            model = flow_model("cover")
          ),
          list(from = flow_sheet("canopy"))
        ),
        flow_node("estimate", estimate_label("canopy"))
      ),
      flow_group(
        list(
          list(from = flow_sheet("wetdry"), model = flow_model("wetdry")),
          list(from = flow_sheet("carbon"), model = flow_model("carbon")),
          list(from = flow_sheet("nitrogen"), model = flow_model("nitrogen"))
        ),
        flow_node(
          "estimate",
          sprintf(
            "%s, %s and %s",
            output_info$dry$label,
            tolower(output_info$carbon$label),
            tolower(output_info$nitrogen$label)
          ),
          "Of either estimate"
        )
      )
    )
  )
}

# Data format --------------------------------------------------------------------------

# The ways a sheet's dataset can differ: by species and, for the size sheet,
# by the kind of size data.
format_variants <- list(
  list(species = "nereo", census = FALSE),
  list(species = "nereo", census = TRUE),
  list(species = "macro", census = TRUE)
)

# A dataset's columns from kelpbio's data dictionary, as a table.
format_table <- function(dataset) {
  columns <- kb_data_columns(dataset)
  rows <- lapply(seq_len(nrow(columns)), function(i) {
    tags$tr(
      tags$td(
        tags$code(columns$column[[i]]),
        if (!columns$required[[i]]) {
          div(class = "text-body-secondary", "optional")
        }
      ),
      tags$td(if (!is.na(columns$units[[i]])) columns$units[[i]]),
      tags$td(columns$allowed[[i]]),
      tags$td(tags$code(columns$example[[i]])),
      tags$td(columns$description[[i]])
    )
  })
  div(
    class = "table-responsive",
    tags$table(
      class = "table table-sm small mb-0",
      tags$thead(tags$tr(lapply(
        c("Column", "Units", "Allowed", "Example", "Description"),
        tags$th
      ))),
      tags$tbody(rows)
    )
  )
}

# The guide sections of kelpbio's installed Data format article. Its Datasets,
# Columns and data checks are left out, as each sheet tab shows its row
# (kb_datasets()) and columns, and the Data step checks the sheets. Its links
# to the Methods article open the app's Methods page (methods_link()).
format_sections <- function() {
  sections <- kelpbio_vignette("data-format", "format-")
  sections <- Filter(
    function(x) {
      !x$id %in% c("format-datasets", "format-columns", "format-checking-the-data")
    },
    sections
  )
  lapply(sections, function(section) {
    section$html <- HTML(methods_links(as.character(section$html)))
    section
  })
}

# Links to kelpbio's Methods article, as kelpbio_vignette() writes them, made
# into links that open the app's Methods page (help_to_methods) and, for a link
# to a section, scroll to it once the page shows. The section's id on the
# Methods page carries its "methods-" prefix (methods_sections()).
methods_links <- function(html) {
  pattern <- sprintf(
    '<a href="%s(#[^"]*)?" target="_blank" rel="noopener">',
    gsub(".", "\\.", kelpbio_link("methods"), fixed = TRUE)
  )
  links <- regmatches(html, gregexpr(pattern, html))[[1]]
  for (link in unique(links)) {
    anchor <- sub(".*methods\\.html#?([^\"]*)\".*", "\\1", link)
    html <- gsub(
      link,
      sprintf('<a href="#" onclick="%s">', methods_onclick(anchor)),
      html,
      fixed = TRUE
    )
  }
  html
}

# The inline script that opens the Methods page (help_to_methods) and, for a
# section's `anchor`, scrolls to it once the page shows.
methods_onclick <- function(anchor = "") {
  scroll <- if (nzchar(anchor)) {
    sprintf(
      paste(
        "$(document).one('shown.bs.tab', function() {",
        "var e = document.getElementById('methods-%s');",
        "if (e) e.scrollIntoView();",
        "});"
      ),
      anchor
    )
  } else {
    ""
  }
  paste(
    scroll,
    "Shiny.setInputValue('help_to_methods', true, {priority: 'event'});",
    "return false;"
  )
}

# A link to a section of the Methods page, inside a sentence.
methods_link <- function(anchor, text) {
  tags$a(href = "#", onclick = methods_onclick(anchor), text, .noWS = "outside")
}

# The format tabs: one per sheet name a workbook or CSV file can use, so the
# size sheet has one for each kind of size data. Each lists its columns, by
# species where they differ.
format_tabs <- function() {
  names <- list()
  for (id in sheet_ids) {
    kinds <- if (id == "size") c(FALSE, TRUE) else NA
    for (census in kinds) {
      names[[length(names) + 1]] <- list(id = id, census = census)
    }
  }
  tabs <- lapply(names, function(tab) {
    variants <- Filter(
      function(v) is.na(tab$census) || v$census == tab$census,
      format_variants
    )
    datasets <- unique(vapply(
      variants,
      function(v) sheet_dataset(tab$id, v$species, v$census),
      ""
    ))
    contents <- lapply(datasets, function(d) {
      as.data.frame(kb_data_columns(d))[, -1]
    })
    same <- length(unique(contents)) == 1
    name <- sheet_name(tab$id, isTRUE(tab$census))
    row <- sheet_row(tab$id, isTRUE(tab$census))
    tables <- if (same) {
      list(format_table(datasets[[1]]))
    } else {
      lapply(datasets, function(d) {
        species <- if (endsWith(d, "_macro")) "macro" else "nereo"
        div(
          class = "d-flex flex-column gap-2",
          div(class = "small fw-semibold", em(species_info[[species]]$latin)),
          format_table(d)
        )
      })
    }
    nav_panel(
      title = span(class = "small", name),
      value = name,
      folio_sheet(
        div(
          class = "small text-body-secondary",
          HTML(paste0("One row per ", row_html(row), "."))
        ),
        !!!tables
      )
    )
  })
  folio(navset_tab(id = "format_tabs", !!!tabs))
}

# The Sheets panel first, with the article's Overview above the sheet tabs;
# its other sections follow.
help_format_ui <- function() {
  sections <- format_sections()
  overview <- Filter(function(x) x$id == "format-overview", sections)
  rest <- Filter(function(x) x$id != "format-overview", sections)
  tagList(
    page_header(
      help_pages[["format"]],
      "The sheets that the app reads, required column names, and data format."
    ),
    div(
      class = "d-flex flex-column gap-3",
      panel(
        "Sheets",
        description = paste(
          "Name each sheet of the Excel workbook, or each CSV file, as its tab below.",
          "Other sheets are ignored.",
          "Choose a size sample or a size census under Size data on the Data step.",
          "The template workbook on the Data step has every sheet and column name pre-filled."
        ),
        action = kelpbio_button("data_format", "In kelpbio"),
        if (length(sections) == 0) {
          vignette_missing("The Data format article", "data_format")
        } else {
          lapply(overview, function(x) div(class = "kb-article", x$html))
        },
        format_tabs()
      ),
      lapply(rest, function(x) {
        panel(x$title, id = x$id, class = "kb-anchor", div(class = "kb-article", x$html))
      })
    )
  )
}

# Methods --------------------------------------------------------------------------

methods_sections <- function() kelpbio_vignette("methods", "methods-")

# Shown when kelpbio was installed without its vignettes.
vignette_missing <- function(what, key) {
  div(
    class = "d-flex flex-wrap align-items-center gap-3",
    div(
      class = "small text-body-secondary flex-grow-1",
      sprintf("%s is not installed with this copy of kelpbio.", what)
    ),
    kelpbio_button(key, "Read it on kelpbio's site")
  )
}

help_methods_ui <- function() {
  sections <- methods_sections()
  tagList(
    page_header(
      help_pages[["methods"]],
      sprintf(
        "How kelpbio estimates biomass: its Methods article, for kelpbio %s.",
        package_version_text("kelpbio")
      )
    ),
    if (is.null(sections)) {
      card(card_body(vignette_missing("The Methods article", "methods")))
    } else {
      div(
        class = "d-flex flex-column gap-3",
        lapply(sections, function(section) {
          panel(
            section$title,
            id = section$id,
            class = "kb-anchor",
            div(class = "kb-article", section$html)
          )
        })
      )
    }
  )
}

# Glossary --------------------------------------------------------------------------

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

help_glossary_ui <- function() {
  # kelpbio's glossary of survey terms, from its installed Get started vignette.
  survey_terms <- Filter(
    function(section) section$id == "glossary-glossary",
    kelpbio_vignette("kelpbio", "glossary-") %||% list()
  )
  tagList(
    page_header(
      help_pages[["glossary"]],
      "The terms that kelpbio uses, and the terms behind the help icons in the app."
    ),
    div(
      class = "d-flex flex-column gap-3",
      panel(
        "Survey terms",
        if (length(survey_terms) == 0) {
          vignette_missing("kelpbio's glossary", "glossary")
        } else {
          div(class = "kb-article", survey_terms[[1]]$html)
        }
      ),
      panel("Terms in the app", guide_glossary())
    )
  )
}

# Layout --------------------------------------------------------------------------

help_ui <- function() {
  step_layout(
    card(card_body(padding = "0.5rem", uiOutput("help_subnav"))),
    navset_hidden(
      id = "help_page",
      nav_panel_hidden("guide", help_guide_ui()),
      nav_panel_hidden("format", help_format_ui()),
      nav_panel_hidden("methods", help_methods_ui()),
      nav_panel_hidden("glossary", help_glossary_ui()),
      nav_panel_hidden("about", help_about_ui())
    )
  )
}

# The sidebar: a link per page, with the Methods sections under it while it
# is open; each section link jumps to its section, and the page stays
# scrollable.
help_server <- function(input, output, session) {
  lapply(names(help_pages), function(value) {
    observeEvent(
      input[[paste0("help_nav_", value)]],
      nav_select("help_page", value)
    )
  })
  observeEvent(input$help_to_format, nav_select("help_page", "format"))
  observeEvent(input$help_to_format_guide, nav_select("help_page", "format"))
  # The Data format article's links to Methods (format_sections()).
  observeEvent(input$help_to_methods, nav_select("help_page", "methods"))
  sections <- methods_sections()
  output$help_subnav <- renderUI({
    page <- input$help_page %||% "guide"
    tags$nav(
      class = "nav nav-pills flex-column gap-1",
      `aria-label` = "Help",
      lapply(names(help_pages), function(value) {
        tagList(
          subnav_link(
            paste0("help_nav_", value),
            page == value,
            lucide(help_page_icons[[value]], "text-body-secondary"),
            help_pages[[value]]
          ),
          if (value == "methods" && page == "methods" && length(sections) > 0) {
            div(
              class = "d-flex flex-column gap-1 small ps-4 ms-2 pb-1",
              lapply(sections, function(section) {
                tags$a(
                  href = paste0("#", section$id),
                  class = "link-body-emphasis text-decoration-none py-1",
                  section$title
                )
              })
            )
          }
        )
      }),
      # Not a page: GitHub's new issue form, in a new tab.
      tags$hr(class = "my-1"),
      tags$a(
        href = paste0(source_url, "/issues/new"),
        target = "_blank",
        rel = "noopener",
        class = "nav-link py-2 px-2",
        div(
          class = "d-flex align-items-center gap-2",
          lucide("github", "text-body-secondary"),
          span(class = "flex-grow-1", "Report an issue"),
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
    page_header(
      help_pages[["guide"]],
      "How to get biomass estimates from your data."
    ),
    div(
      class = "d-flex flex-column gap-3",
      panel(
        "Steps",
        description = "Not every sheet is needed: a run uses only the sheets of the estimates it makes, and models pre-fit to public datasets can be used instead.",
        div(
          class = "d-flex flex-column gap-4",
          lapply(names(steps), function(value) {
            step_item(
              value,
              if (!is.null(guide_learn_more[[value]])) guide_learn_more[[value]](),
              guide_extras[[value]](),
              small = FALSE
            )
          })
        )
      ),
      panel(
        "Workflow",
        description = "Which sheets feed which models, and how the models combine to get estimates.",
        guide_workflow()
      )
    )
  )
}

# A package's installed version.
package_version_text <- function(package) {
  as.character(utils::packageVersion(package))
}

# HTML style renders the title in italics and the URL as a link, where the text
# style would show R's markup characters.
citation_html <- function(citation) {
  HTML(paste(format(citation, style = "html"), collapse = " "))
}

# A citation in a code block, with a button that copies its text.
citation_block <- function(id, label, citation) {
  div(
    class = "d-flex flex-column gap-2",
    div(
      class = "d-flex align-items-center justify-content-between gap-3",
      div(class = "fw-medium", label),
      copy_button(id, "citation", paste("Copy the", label, "citation"))
    ),
    div(
      id = id,
      class = "kb-citation small bg-body-tertiary border rounded-3 p-3",
      citation_html(citation)
    )
  )
}

# A fact about the app, such as a version or its licence, as a muted badge.
about_fact <- function(...) {
  span(class = "badge border text-body-secondary fw-medium", ...)
}

# A link out of the app, as a small outline button with an icon.
about_link <- function(href, icon, text) {
  tags$a(
    href = href,
    target = "_blank",
    rel = "noopener",
    class = "btn btn-light border btn-sm",
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
          "kelpbioshiny was developed by Poisson Consulting for the Hakai Institute, funded by the Tula Foundation. ",
          "Validity of estimates depends on the data and the model choices; check them with the diagnostics on the Models step."
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
        citation_block(
          "citation_kelpbioshiny",
          "kelpbioshiny",
          utils::citation("kelpbioshiny")
        ),
        citation_block(
          "citation_kelpbio",
          "kelpbio",
          utils::citation("kelpbio")
        )
      )
    )
  )
}
