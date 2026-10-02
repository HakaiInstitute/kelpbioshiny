# Biomass step: locked until every model in use is ready, then a figure and a
# table of site-year estimates for the chosen output, per unit area or, with a
# fitted biomass:cover model, as totals for the drone-covered site-years.

# The estimates table: kb_predict_biomass() or kb_predict_biomass_total() rows
# with a flags column naming the population-level estimates and pre-fit models
# each site-year uses.
biomass_table <- function(rows, sources, output) {
  prefit <- Filter(function(id) is_prefit(sources[[id]]), output_components[[output]])
  prefit_flags <- vapply(prefit, function(id) sprintf("%s (%s)", label_of(id), source_note(sources[[id]])), "")
  population_cover <- if ("population_cover" %in% names(rows)) rows$population_cover else rep(FALSE, nrow(rows))
  flags <- vapply(seq_len(nrow(rows)), function(i) {
    paste(c(
      if (population_cover[i]) "Population-level cover",
      if (rows$population_size[i]) "Population-level size",
      if (rows$population_weight[i]) "Population-level weight",
      prefit_flags
    ), collapse = "|")
  }, character(1))
  columns <- intersect(c("site", "year", "canopy_area", "estimate", "lower", "upper"), names(rows))
  data.frame(as.data.frame(rows)[columns], flags = flags)
}

biomass_scales <- list(
  area = list(label = "Per unit area", detail = "By site-year"),
  total = list(label = "Total per site-year", detail = "Site-years with drone imagery")
)

mod_biomass_ui <- function(id) {
  ns <- NS(id)
  sidebar <- card(card_body(
    sidebar_section("Estimate", uiOutput(ns("scale_choice"))),
    sidebar_section("Output", uiOutput(ns("output_choice"))),
    div(
      class = "d-flex flex-column gap-2",
      div(class = "kb-eyebrow text-body-secondary", "Sources"),
      uiOutput(ns("sources")),
      div(button(ns("change_sources"), "Change sources", variant = "outline", size = "sm"))
    )
  ))
  main <- tagList(
    page_header(
      "Biomass", step_description("biomass"),
      span(class = "badge border text-body-secondary fw-normal", "Prototype data")
    ),
    uiOutput(ns("main"))
  )
  layout_columns(col_widths = c(3, 9), div(class = "kb-aside", sidebar), div(main))
}

# Radio buttons where some choices are shown but cannot be picked.
choice_group <- function(input_id, keys, open, selected, label) {
  tagList(
    radioButtons(
      input_id, NULL,
      choiceNames = lapply(open, label, available = TRUE),
      choiceValues = open,
      selected = selected
    ) |>
      tagAppendAttributes(class = "mb-0"),
    lapply(setdiff(keys, open), function(key) {
      div(
        class = "form-check",
        tags$input(class = "form-check-input", type = "radio", disabled = NA, id = paste0(input_id, "_", key)),
        tags$label(class = "form-check-label", `for` = paste0(input_id, "_", key), label(key, available = FALSE))
      )
    })
  )
}

choice_label <- function(title, detail) {
  span(class = "d-inline-flex flex-column align-top", title, span(class = "small text-body-secondary", detail))
}

mod_biomass_server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    chosen <- reactiveVal("wet")
    chosen_scale <- reactiveVal("area")
    output_key <- reactive(if (store$outputs()[[chosen()]]$available) chosen() else "wet")
    scale_key <- reactive(if (store$totals()$available) chosen_scale() else "area")

    observeEvent(input$output, chosen(input$output))
    observeEvent(input$scale, chosen_scale(input$scale))
    observeEvent(input$change_sources, store$go_to("models"))
    observeEvent(input$to_data, store$go_to("data"))
    observeEvent(input$fit_all, store$queue_fits(store$fittable()))
    observeEvent(input$to_models, store$go_to("models"))
    observeEvent(input$open_cover, store$go_to("models", "cover"))
    for (cid in component_ids) {
      local({
        cid <- cid
        observeEvent(input[[paste0("diagnostics_", cid)]], store$open_tab(cid, "diagnostics"))
        observeEvent(input[[paste0("settings_", cid)]], store$open_settings(cid))
        observeEvent(input[[paste0("dismiss_", cid)]], store$dismiss(cid, "convergence"))
      })
    }

    output$scale_choice <- renderUI({
      open <- if (store$totals()$available) names(biomass_scales) else "area"
      choice_group(ns("scale"), names(biomass_scales), open, isolate(scale_key()), function(key, available) {
        choice_label(biomass_scales[[key]]$label, if (available) biomass_scales[[key]]$detail else "Unavailable")
      })
    })

    output$output_choice <- renderUI({
      available <- store$outputs()
      keys <- names(output_info)
      open <- keys[vapply(available, `[[`, logical(1), "available")]
      unit <- if (scale_key() == "total") "Tonnes" else NULL
      choice_group(ns("output"), keys, open, isolate(output_key()), function(key, available) {
        choice_label(output_info[[key]]$label, if (available) unit %||% output_info[[key]]$unit else "Unavailable")
      })
    })

    output$sources <- renderUI({
      sources <- store$sources()
      status_list(store$statuses(), as.list(source_labels(sources)))
    })

    output$main <- renderUI({
      if (!store$has_density() || !store$biomass_ready()) {
        return(locked_state(ns, store))
      }
      key <- output_key()
      total <- scale_key() == "total"
      statuses <- store$statuses()
      used <- c(output_components[[key]], if (total) "cover")
      warnings <- Filter(function(id) shows_convergence_warning(statuses[[id]]), used)
      available <- store$outputs()
      unavailable <- lapply(Filter(function(k) !available[[k]]$available, names(available)), function(k) available[[k]]$reason)
      totals <- store$totals()
      notices <- c(
        lapply(warnings, function(id) {
          convergence_notice(
            sprintf("The %s model has a convergence warning", lower_label(id)), ns(paste0("settings_", id)),
            "Every estimate below uses it, so treat its limits with caution until then.",
            dismiss_id = ns(paste0("dismiss_", id)),
            secondary = button(ns(paste0("diagnostics_", id)), "View diagnostics", variant = "outline", size = "sm")
          )
        }),
        lapply(unavailable, function(reason) notice("minus-circle", reason, tone = "muted")),
        if (!totals$available) {
          list(notice(
            "minus-circle", totals$reason,
            tone = "muted",
            action = if (!statuses$cover$kind %in% c("not-used", "no-data")) {
              button(ns("open_cover"), "Open biomass:cover", variant = "outline", size = "sm")
            }
          ))
        }
      )
      panels <- if (total) total_panels(ns, key) else area_panels(ns, key)
      tagList(
        if (length(notices) > 0) div(class = "d-flex flex-column gap-2 mb-3", notices),
        plot_estimates_nav(ns("view"), panels$plot, panels$estimates, selected = isolate(input$view))
      )
    })

    # Biomass per unit area, or totals, for the chosen output.
    estimates <- reactive({
      req(store$has_density(), store$biomass_ready())
      if (scale_key() == "total") store$biomass_total(output_key()) else store$biomass(output_key())
    })

    output$figure <- render_figure(
      function() kb_plot_biomass(estimates()), "figure",
      aspect = function() if (scale_key() == "total") 4.5 / 8 else 6 / 8,
      alt = function() {
        label <- output_info[[output_key()]]$label
        sprintf("Estimated %s by site and year", tolower(if (scale_key() == "total") paste("Total", label) else label))
      }
    )

    output$table <- reactable::renderReactable({
      total <- scale_key() == "total"
      rows <- biomass_table(estimates(), store$sources(), output_key())
      population <- grepl("Population", rows$flags)
      number <- function(name) reactable::colDef(name = name, cell = number_text, align = "right")
      app_table(
        rows,
        row_style = function(index) if (population[index]) app_row_colour$info,
        columns = Filter(Negate(is.null), list(
          site = reactable::colDef(name = "Site"),
          year = reactable::colDef(name = "Year"),
          canopy_area = if (total) {
            reactable::colDef(name = "Canopy area (m\u00b2)", align = "right", format = reactable::colFormat(separators = TRUE, digits = 0))
          },
          estimate = number("Estimate"),
          lower = number("Lower"),
          upper = number("Upper"),
          flags = reactable::colDef(
            name = "Flags", minWidth = 220, sortable = FALSE,
            cell = function(value) {
              if (value == "") {
                return(NULL)
              }
              div(class = "d-flex flex-wrap gap-1", lapply(strsplit(value, "|", fixed = TRUE)[[1]], function(flag) {
                if (startsWith(flag, "Population")) {
                  badge(flag, "bg-secondary-subtle text-secondary-emphasis fw-normal")
                } else {
                  badge(flag, "border text-body fw-normal")
                }
              }))
            }
          )
        ))
      )
    })
  })
}

flags_note <- function(end) {
  span(
    class = "d-inline-flex flex-wrap align-items-center gap-1",
    "Flags show where a pre-fit or", with_help("population-level estimate", "population"), paste0("was used", end)
  )
}

# The figure and table panels, for the Plot and Estimates pills.
area_panels <- function(ns, key) {
  info <- output_info[[key]]
  list(
    plot = panel(
      info$label,
      description = sprintf(
        "Estimated %s (%s) by year, faceted by site, with 95%% compatibility intervals.",
        tolower(info$label), info$unit
      ),
      figure_plot(ns("figure"))
    ),
    estimates = panel(
      "Estimates",
      description = tagList(
        with_help(sprintf("%s (%s) with 95%% compatibility intervals, to 3 significant figures.", info$label, info$unit), "interval"),
        flags_note(".")
      ),
      reactable::reactableOutput(ns("table"))
    )
  )
}

total_panels <- function(ns, key) {
  info <- output_info[[key]]
  label <- paste("Total", tolower(info$label))
  list(
    plot = panel(
      with_help(label, "total_biomass"),
      description = sprintf(
        "Estimated %s (t) by year, faceted by site, with 95%% compatibility intervals, for site-years with drone imagery.",
        tolower(label)
      ),
      figure_plot(ns("figure"))
    ),
    estimates = panel(
      "Totals",
      description = tagList(
        with_help(sprintf("%s (t) with 95%% compatibility intervals, to 3 significant figures.", label), "interval"),
        flags_note(", including the population-level cover relationship for site-years without plot cover.")
      ),
      reactable::reactableOutput(ns("table"))
    )
  )
}

locked_state <- function(ns, store) {
  if (!store$has_density()) {
    return(empty_state(
      "lock", "Biomass is locked", "Upload a workbook with a density sheet first.",
      div(class = "mt-2", button(ns("to_data"), "Go to data", variant = "outline"))
    ))
  }
  statuses <- store$statuses()
  blocking <- store$blocking()
  empty_state(
    "lock", "Biomass is locked", "Every model in use needs to be fitted or set to pre-fit first.",
    tags$ul(
      class = "list-unstyled border rounded-3 p-3 text-start small d-flex flex-column gap-2 w-100 mb-0 mt-2",
      style = "max-width: 24rem",
      lapply(blocking, function(id) {
        tags$li(
          class = "d-flex align-items-center gap-2",
          status_icon(statuses[[id]]),
          span(class = "flex-grow-1 fw-medium", label_of(id)),
          span(class = "text-body-secondary", block_reason(statuses[[id]]))
        )
      })
    ),
    div(
      class = "d-flex gap-2 mt-2",
      if (length(store$fittable()) > 0) button(ns("fit_all"), "Fit all", "play"),
      button(ns("to_models"), if (is.null(store$fitting())) "Go to Models" else "View progress", variant = "outline")
    )
  )
}
