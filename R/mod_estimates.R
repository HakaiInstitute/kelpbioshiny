# Estimates step: a sidebar listing every estimate (each model's predictions,
# then biomass per unit area and total biomass from the combined models) beside
# the chosen estimate's page, switched with a hidden navset. An estimate whose
# models are not ready shows why instead.

mod_estimates_ui <- function(id) {
  ns <- NS(id)
  pages <- c(
    lapply(component_ids, function(cid) nav_panel_hidden(cid, mod_estimate_ui(ns(cid), cid))),
    list(
      nav_panel_hidden("biomass", mod_biomass_estimate_ui(ns("biomass"), "biomass")),
      nav_panel_hidden("total", mod_biomass_estimate_ui(ns("total"), "total"))
    )
  )
  main <- tagList(
    page_header("Estimates", step_description("estimates"), uiOutput(ns("continue_ui"), inline = TRUE)),
    do.call(navset_hidden, c(list(id = ns("view")), unname(pages)))
  )
  step_layout(card(card_body(padding = "0.5rem", uiOutput(ns("subnav")))), main)
}

mod_estimates_server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    for (cid in component_ids) mod_estimate_server(cid, store)
    mod_biomass_estimate_server("biomass", store)
    mod_biomass_estimate_server("total", store)

    # The estimate shown: the one asked for, or else the first available, with
    # plot biomass first as most runs are for it.
    shown <- reactive({
      chosen <- store$estimate()
      if (!is.null(chosen)) {
        return(chosen)
      }
      estimates <- store$estimates()
      available <- names(estimates)[vapply(estimates, `[[`, logical(1), "available")]
      first <- intersect(c("biomass", setdiff(estimate_ids, "biomass")), available)
      if (length(first) > 0) first[[1]] else "biomass"
    })
    observe(nav_select("view", shown(), session = session))

    # The next step once there is an estimate to save, as Continue to models is
    # on the Data step.
    output$continue_ui <- renderUI({
      if (store$any_estimate()) button(session$ns("continue"), span("Continue to export ", lucide("arrow-right")))
    })
    observeEvent(input$continue, store$go_to("export"))
    lapply(estimate_ids, function(eid) observeEvent(input[[paste0("nav_", eid)]], store$estimate(eid)))

    output$subnav <- renderUI({
      shown <- shown()
      statuses <- store$statuses()
      estimates <- store$estimates()
      link <- function(eid) {
        available <- estimates[[eid]]$available
        if (eid %in% component_ids) {
          icon <- status_icon(statuses[[eid]])
          hidden <- status_hidden(statuses[[eid]])
        } else {
          icon <- if (available) lucide("check-circle-2", "text-success") else lucide("lock", "text-body-secondary")
          hidden <- span(class = "visually-hidden", if (available) ", available" else ", unavailable", .noWS = "before")
        }
        subnav_link(session$ns(paste0("nav_", eid)), eid == shown, icon, estimate_label(eid), hidden)
      }
      tags$nav(
        class = "nav nav-pills flex-column gap-1",
        `aria-label` = "Estimates",
        lapply(component_ids, link),
        tags$hr(class = "my-1"),
        link("biomass"),
        link("total")
      )
    })
  })
}

# The header of an estimate's page: its name, a line under it, and an action.
estimate_header <- function(title, detail, action = NULL) {
  div(
    class = "d-flex flex-wrap align-items-start justify-content-between gap-3 mb-3",
    div(h2(class = "fs-5 fw-semibold mb-1", title), div(class = "small text-body-secondary", detail)),
    action
  )
}

estimate_unavailable <- function(ns, reason) {
  empty_state(
    "lock", "Not available yet", reason,
    div(class = "mt-2", button(ns("to_models"), "Go to Models", variant = "outline"))
  )
}

# The warnings of the models an estimate uses, in one line (`own` on a model's
# own estimate page), then any `extra` notices, above its figure and table.
estimate_notices <- function(ns, store, ids, extra = list(), own = FALSE) {
  notices <- Filter(Negate(is.null), c(list(warnings_summary(ns, store$statuses(), ids, own)), extra))
  if (length(notices) > 0) div(class = "d-flex flex-column gap-2 mb-3", notices)
}

# One model's estimates ---------------------------------------------------------
# Its predictions by grouping, as a figure and a table; the weight model can
# also predict each plant in the size sheet.

mod_estimate_ui <- function(id, cid) {
  ns <- NS(id)
  tagList(
    estimate_header(
      label_of(cid), textOutput(ns("source"), inline = TRUE),
      button(ns("open_model"), "Open model", variant = "outline", size = "sm")
    ),
    uiOutput(ns("notices")),
    uiOutput(ns("body"))
  )
}

mod_estimate_server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    cid <- id
    ns <- session$ns
    fit <- dedupe(reactive(store$fit_of(cid)))
    plants <- dedupe(reactive(if (cid == "weight") plant_rows(store$sheets(), store$species())))
    # The body re-renders only when what it shows changes, so the chosen grouping
    # and pill survive status changes elsewhere.
    page <- dedupe(reactive({
      list(
        available = store$estimates()[[cid]], prefit = is_prefit(store$sources()[[cid]]),
        species = store$species(), plants = !is.null(plants())
      )
    }))

    observeEvent(input$open_model, store$go_to("models", cid))
    observeEvent(input$to_models, store$go_to("models", cid))
    observe_warning_links(input, store)

    output$source <- renderText(sprintf("Source: %s", source_label(store$sources()[[cid]])))

    output$notices <- renderUI({
      req(page()$available$available)
      estimate_notices(ns, store, cid, own = TRUE)
    })

    output$body <- renderUI({
      page <- page()
      if (!page$available$available) {
        return(estimate_unavailable(ns, page$available$reason))
      }
      predictions_panel(ns, cid, page$prefit, page$species, page$plants)
    })

    grouping <- reactive({
      choices <- prediction_choices(cid, page()$prefit, page()$species, page()$plants)
      chosen <- input$grouping
      if (is.null(chosen) || !chosen %in% names(choices)) default_grouping(choices) else chosen
    })

    # The Group by choice names the grouping, so the figure has no title.
    output$figure <- renderUI({
      grouping <- grouping()
      card(card_body(figure_plot(
        ns("plot"), prediction_caption(cid, grouping),
        class = if (grouping == "population") "kb-figure-narrow"
      )))
    })

    # The predictions for the figure, and for the table: the same call unless the
    # model predicts along a predictor, where the table is at its reference value.
    predictions <- reactive(model_predictions(cid, req(fit()), grouping(), page()$species, plants = plants()))
    predictions_at <- reactive({
      if (is.null(prediction_info[[cid]]$along) || grouping() == "plant") {
        predictions()
      } else {
        model_predictions(cid, req(fit()), grouping(), page()$species, at = TRUE)
      }
    })

    # An overall prediction of a model without a predictor has no column for the
    # x-axis, so it gets one naming what it is for. Each plant's weight is shown
    # in one panel: faceted by site and year it would split into a panel per
    # site-year, so every site-year is kept (max_facets) and the facets dropped.
    output$plot <- render_figure(
      function() {
        grouping <- grouping()
        predictions <- predictions()
        if (grouping == "plant") {
          kb_plot_predictions(predictions, x = attr(predictions, "kb_predictor"), max_facets = Inf) + ggplot2::facet_null()
        } else if (grouping == "population" && is.null(prediction_info[[cid]]$along)) {
          predictions$group <- population_label(cid, page()$species)
          kb_plot_predictions(predictions, x = "group") + ggplot2::labs(x = NULL)
        } else {
          kb_plot_predictions(predictions)
        }
      },
      "plot",
      aspect = function() prediction_aspect(cid, grouping()),
      alt = function() sprintf("Predicted %s for the %s model", prediction_info[[cid]]$response, lower_label(cid))
    )

    output$table_title <- renderText({
      info <- prediction_info[[cid]]
      if (grouping() == "plant") info$plant_table else info$table
    })

    output$table <- reactable::renderReactable({
      rows <- predictions_at()
      plant <- grouping() == "plant"
      number <- function(name) reactable::colDef(name = name, cell = number_text, align = "right", class = "kb-tabular")
      columns <- c("site", "year", if (plant) intersect(c("diameter_mm", "fronds"), names(rows)), "estimate", "lower", "upper")
      app_table(rows[intersect(columns, names(rows))], columns = Filter(Negate(is.null), list(
        site = if ("site" %in% names(rows)) reactable::colDef(name = "Site"),
        year = if ("year" %in% names(rows)) reactable::colDef(name = "Year"),
        diameter_mm = if (plant && "diameter_mm" %in% names(rows)) reactable::colDef(name = "Sub-bulb diameter (mm)", align = "right", class = "kb-tabular"),
        fronds = if (plant && "fronds" %in% names(rows)) reactable::colDef(name = "Fronds", align = "right", class = "kb-tabular"),
        estimate = number("Estimate"),
        lower = number("Lower"),
        upper = number("Upper")
      )))
    })
  })
}

# What an overall prediction is for: "All sites and years", "All sites" or,
# for a model without site effects, "All samples".
population_label <- function(cid, species) {
  if (has_effect(cid, "year", species)) {
    return("All sites and years")
  }
  if (has_effect(cid, "site", species)) "All sites" else "All samples"
}

# By site where the model has site effects, as that is what most users want to
# see; then each plant, for a weight model with a size sheet (a pre-fit model's
# sites are the reference data's rather than the user's); else the population.
default_grouping <- function(choices) {
  for (grouping in c("site", "plant")) {
    if (grouping %in% names(choices)) {
      return(grouping)
    }
  }
  "population"
}

predictions_panel <- function(ns, cid, prefit, species, plants) {
  choices <- prediction_choices(cid, prefit, species, plants)
  note <- if (prefit) {
    "A pre-fit model's predictions are overall, for a typical site and year. Biomass estimates use site-level estimates for sites in the reference data."
  } else if (!has_effect(cid, "site", species)) {
    "This model has no site or year effects, so its predictions are overall, for a typical site and year."
  } else if (!has_effect(cid, "year", species)) {
    "This model has a site effect but no year effect, so there are no predictions by year."
  }
  tagList(
    div(
      class = "d-flex flex-wrap align-items-center column-gap-3 row-gap-1 mb-3",
      span(class = "small fw-medium", with_help(span(id = ns("grouping_title"), if (length(choices) > 1) "Group by" else "Overall"), "prediction_groups")),
      if (length(choices) > 1) {
        radioButtons(
          ns("grouping"), NULL,
          choices = stats::setNames(names(choices), choices), selected = default_grouping(choices), inline = TRUE
        ) |>
          tagAppendAttributes(class = "mb-0", `aria-labelledby` = ns("grouping_title"))
      },
      if (!is.null(note)) div(class = "small text-body-secondary w-100", note)
    ),
    plot_table_nav(
      ns("view"),
      uiOutput(ns("figure")),
      panel(
        textOutput(ns("table_title"), inline = TRUE),
        description = with_help("With 95% compatibility intervals, to 3 significant figures.", "interval"),
        reactable::reactableOutput(ns("table"))
      )
    )
  )
}

# Biomass -----------------------------------------------------------------------
# Plot biomass by site-year, or total biomass of each drone survey of a whole
# site, for the chosen measure (wet, dry or carbon).

mod_biomass_estimate_ui <- function(id, eid) {
  ns <- NS(id)
  title <- if (eid == "total") with_help(estimate_label(eid), "total_biomass") else estimate_label(eid)
  tagList(
    estimate_header(
      title,
      if (eid == "total") "Biomass over the canopy mapped in each drone survey of a whole site." else "Biomass per m\u00b2 at the plot scale, by site-year.",
      button(ns("change_sources"), "Change sources", variant = "outline", size = "sm")
    ),
    uiOutput(ns("notices")),
    uiOutput(ns("body"))
  )
}

mod_biomass_estimate_server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    eid <- id
    total <- eid == "total"
    ns <- session$ns
    chosen <- reactiveVal("wet")
    output_key <- reactive(if (store$outputs()[[chosen()]]$available) chosen() else "wet")
    available <- dedupe(reactive(store$estimates()[[eid]]))

    observeEvent(input$output, chosen(input$output))
    observeEvent(input$change_sources, store$go_to("models"))
    observeEvent(input$to_models, store$go_to("models"))
    observe_warning_links(input, store)

    output$notices <- renderUI({
      req(available()$available)
      key <- output_key()
      outputs <- store$outputs()
      unavailable <- lapply(Filter(function(k) !outputs[[k]]$available, names(outputs)), function(k) outputs[[k]]$reason)
      result <- estimates()
      estimate_notices(ns, store, c(output_components[[key]], if (total) "cover"), extra = c(
        lapply(unavailable, function(reason) notice("minus-circle", reason, tone = "muted")),
        if (inherits(result, "error")) list(notice("x-circle", "Biomass could not be estimated", conditionMessage(result), tone = "warning"))
      ))
    })

    output$body <- renderUI({
      if (!available()$available) {
        return(estimate_unavailable(ns, available()$reason))
      }
      outputs <- store$outputs()
      open <- names(outputs)[vapply(outputs, `[[`, logical(1), "available")]
      key <- isolate(output_key())
      tagList(
        div(
          class = "d-flex flex-wrap align-items-center column-gap-3 row-gap-1 mb-3",
          span(class = "small fw-medium", id = ns("output_title"), "Output"),
          radioButtons(
            ns("output"), NULL,
            choices = stats::setNames(open, vapply(output_info[open], `[[`, "", "label")), selected = key, inline = TRUE
          ) |>
            tagAppendAttributes(class = "mb-0", `aria-labelledby` = ns("output_title"))
        ),
        plot_table_nav(ns("view"), uiOutput(ns("plot_panel")), uiOutput(ns("table_panel")), selected = isolate(input$view))
      )
    })

    output$plot_panel <- renderUI(biomass_panels(ns, output_key(), total)$plot)
    output$table_panel <- renderUI(biomass_panels(ns, output_key(), total)$table)

    # Plot biomass, or total site biomass, for the chosen measure, or the error
    # kelpbio gave.
    estimates <- reactive({
      req(available()$available)
      tryCatch(
        if (total) store$biomass_total(output_key()) else store$biomass(output_key()),
        error = function(e) e
      )
    })
    estimated <- reactive(if (!inherits(estimates(), "error")) estimates() else req(FALSE))

    # One facet per site, so the figure grows with the rows of facets
    # (kb_plot_predictions() uses facet_wrap()'s default layout).
    output$figure <- render_figure(
      function() kb_plot_predictions(estimated()), "figure",
      height = function() figure_px(60 + 170 * ggplot2::wrap_dims(length(unique(estimated()$site)))[1]),
      alt = function() {
        label <- output_info[[output_key()]]$label
        sprintf("Estimated %s by site and year", tolower(if (total) paste("Total", label) else label))
      }
    )

    output$table <- reactable::renderReactable({
      rows <- biomass_table(estimated())
      flagged <- rows$flags != ""
      number <- function(name) reactable::colDef(name = name, cell = number_text, align = "right", class = "kb-tabular")
      app_table(
        rows,
        row_style = function(index) if (flagged[index]) app_row_colour$info,
        columns = Filter(Negate(is.null), list(
          site = reactable::colDef(name = "Site"),
          year = reactable::colDef(name = "Year"),
          canopy_area_m2 = if (total) {
            reactable::colDef(
              name = "Site canopy area (m\u00b2)", align = "right", class = "kb-tabular",
              format = reactable::colFormat(separators = TRUE, digits = 0)
            )
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
                badge(flag, "border text-body fw-normal")
              }))
            }
          )
        ))
      )
    })
  })
}

# The estimates table: kb_predict_plot_biomass() or kb_predict_site_biomass()
# rows with a flags column naming the models without data for each site-year,
# from the *_support columns, which give what each model's data hold for it.
# The sources, pre-fit or not, are the same for every row and are listed on the
# Models step.
biomass_table <- function(rows) {
  supports <- intersect(names(support_info), names(rows))
  flags <- vapply(seq_len(nrow(rows)), function(i) {
    paste(unlist(lapply(supports, function(column) support_flag(column, rows[[column]][i]))), collapse = "|")
  }, character(1))
  columns <- intersect(c("site", "year", "canopy_area_m2", "estimate", "lower", "upper"), names(rows))
  data.frame(as.data.frame(rows)[columns], flags = flags)
}

# Each *_support column: the model it is for, and its value when the model has
# data for the site-year itself (the cover biomass model has no site-year effect).
support_info <- list(
  size_support = list(id = "size", full = "site-year"),
  weight_support = list(id = "weight", full = "site-year"),
  cover_support = list(id = "cover", full = "site, year")
)

# "No size data: uses its site and year", or NULL where the model has data for
# the site-year.
support_flag <- function(column, support) {
  info <- support_info[[column]]
  if (support == info$full) {
    return(NULL)
  }
  model <- lower_label(info$id)
  switch(support,
    "site, year" = sprintf("No %s data: uses its site and year", model),
    site = sprintf("No %s data: uses its site", model),
    year = sprintf("No %s data: uses its year", model),
    sprintf("No %s data for its site or year", model)
  )
}

flags_note <- function() {
  span(
    class = "d-inline-flex flex-wrap align-items-center gap-1",
    "Flags show where a model has no data for the", with_help("site-year.", "coverage")
  )
}

# The figure and table panels of a biomass estimate, for the Plot and Table pills.
biomass_panels <- function(ns, key, total) {
  info <- output_info[[key]]
  label <- if (total) paste("Total", tolower(info$label)) else info$label
  unit <- info$unit[[if (total) "site" else "plot"]]
  list(
    plot = panel(
      label,
      description = sprintf(
        "Estimated %s (%s) %sby year, faceted by site, with 95%% compatibility intervals.",
        tolower(label), unit, if (total) "of each drone survey of a whole site " else ""
      ),
      figure_plot(ns("figure"))
    ),
    table = panel(
      sprintf("%s (%s)", label, unit),
      description = tagList(
        with_help("With 95% compatibility intervals, to 3 significant figures.", "interval"),
        flags_note()
      ),
      reactable::reactableOutput(ns("table"))
    )
  )
}
