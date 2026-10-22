# Estimates step: a sidebar listing every estimate (survey and canopy biomass
# from the combined models, then each model's predictions) beside the chosen estimate's page, switched with a hidden navset. An
# estimate whose models are not ready shows why instead.

mod_estimates_ui <- function(id) {
  ns <- NS(id)
  pages <- c(
    lapply(component_ids, function(cid) nav_panel_hidden(cid, mod_estimate_ui(ns(cid), cid))),
    lapply(derived_ids, function(eid) nav_panel_hidden(eid, mod_derived_estimate_ui(ns(eid), eid)))
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
    for (eid in derived_ids) mod_derived_estimate_server(eid, store)

    # The estimate shown: the one asked for, or else the first of the run's own
    # (a biomass estimate first, as most runs are for them, then a model fitted
    # to its data), or else survey biomass, which says what it needs.
    shown <- reactive({
      chosen <- store$estimate()
      if (!is.null(chosen) && chosen %in% store$shown_estimates()) {
        return(chosen)
      }
      first <- intersect(c("survey", "canopy", component_ids), store$own_estimates())
      if (length(first) > 0) first[[1]] else "survey"
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
      # The biomass estimates first, then each model's predictions, in the
      # order of the Models step.
      heading <- function(text) div(class = "kb-eyebrow text-body-secondary px-2 pt-1", text)
      tags$nav(
        class = "nav nav-pills flex-column gap-1",
        `aria-label` = "Estimates",
        heading("Biomass"),
        lapply(intersect(derived_ids, store$shown_estimates()), link),
        tags$hr(class = "my-1"),
        heading("Model predictions"),
        lapply(intersect(model_order, store$shown_estimates()), link)
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
    div(class = "mt-2", button(ns("to_models"), "Go to models", variant = "outline"))
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
      button(ns("open_model"), "View model", variant = "outline", size = "sm")
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
    plants <- dedupe(reactive(if (cid == "weight") plant_rows(store$sheets())))
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
      predictions_panel(ns, cid, page$prefit, page$species, page$plants, isolate(grouping()))
    })

    # The store holds the choice, so a resumed run shows the grouping it was
    # saved with, and the Export step uses the grouping shown here. It holds
    # every estimate's choices, so the grouping is deduplicated, to follow only
    # this model's.
    grouping <- dedupe(reactive({
      chosen_grouping(store$estimate_choices()[[cid]]$grouping, prediction_choices(cid, page()$prefit, page()$species, page()$plants))
    }))
    observeEvent(input$grouping, store$set_estimate_choice(cid, "grouping", input$grouping))
    observeEvent(grouping(), {
      if (!is.null(input$grouping) && !identical(input$grouping, grouping())) updateSelectInput(session, "grouping", selected = grouping())
    })

    # Titled and captioned as the biomass figures are.
    output$figure <- renderUI({
      grouping <- grouping()
      panel(
        sentence_case(prediction_info[[cid]]$response),
        description = prediction_caption(cid, grouping, page()$species),
        figure_plot(ns("plot"), class = if (grouping == "population") "kb-figure-narrow", aspect = prediction_aspect(cid, grouping))
      )
    })

    # The predictions for the figure, and for the table.
    predictions <- reactive(model_predictions(cid, req(fit()), grouping(), page()$species, plants = plants()))
    predictions_at <- reactive(table_predictions(cid, req(fit()), grouping(), page()$species, predictions()))

    output$plot <- render_figure(
      function() prediction_figure(cid, predictions(), grouping(), page()$species),
      "plot",
      aspect = function() prediction_aspect(cid, grouping()),
      alt = function() sprintf("Predicted %s for the %s model", prediction_info[[cid]]$response, lower_label(cid))
    )

    output$table_title <- renderText(prediction_title(cid, grouping()))

    output$table <- reactable::renderReactable({
      rows <- prediction_table(predictions_at(), cid, grouping())
      predictor <- shows_predictor(cid, grouping())
      number <- function(name) reactable::colDef(name = name, cell = number_text, align = "right", class = "kb-tabular")
      app_table(rows, columns = Filter(Negate(is.null), list(
        site = if ("site" %in% names(rows)) reactable::colDef(name = column_label("site")),
        year = if ("year" %in% names(rows)) reactable::colDef(name = column_label("year")),
        diameter_mm = if (predictor && "diameter_mm" %in% names(rows)) reactable::colDef(name = column_label("diameter_mm"), cell = number_text, align = "right", class = "kb-tabular"),
        fronds = if (predictor && "fronds" %in% names(rows)) reactable::colDef(name = column_label("fronds"), align = "right", class = "kb-tabular"),
        cover = if (predictor && "cover" %in% names(rows)) reactable::colDef(name = column_label("cover"), cell = number_text, align = "right", class = "kb-tabular"),
        estimate = number(column_label("estimate")),
        lower = number(column_label("lower")),
        upper = number(column_label("upper")),
        data_support = if (!is.null(rows$data_support)) support_column(rows)
      )))
    })
    output$support_note <- renderUI({
      if (!is.null(prediction_table(predictions_at(), cid, grouping())$data_support)) support_note()
    })
  })
}

# The heading of an estimates table column; another column, such as a grouping
# column of the canopy sheet, is named in sentence case.
column_labels <- c(
  site = "Site", year = "Year", survey_id = "Survey", survey_area_m2 = "Area surveyed (m\u00b2)",
  canopy_area_m2 = "Canopy area (m\u00b2)", plants = "Plants", diameter_mm = "diameter_mm",
  fronds = "Fronds", cover = "Tide-corrected cover", estimate = "Estimate", lower = "Lower", upper = "Upper", data_support = "Data support"
)
column_label <- function(name) if (name %in% names(column_labels)) column_labels[[name]] else sentence_case(name)

# The predictions for a model's table: those of the figure unless the model
# predicts along a predictor and the table is by group, where it is at the
# reference value.
table_predictions <- function(cid, fit, grouping, species, predictions) {
  if (shows_predictor(cid, grouping) || is.null(prediction_info[[cid]]$along)) {
    return(predictions)
  }
  model_predictions(cid, fit, grouping, species, at = TRUE)
}

# Whether a model's table has a row per predictor value: each plant's weight,
# or the overall curve of a curve model.
shows_predictor <- function(cid, grouping) {
  grouping == "plant" || (grouping == "population" && !is.null(prediction_info[[cid]]$along))
}

prediction_title <- function(cid, grouping) {
  info <- prediction_info[[cid]]
  if (grouping == "plant") {
    return(info$plant_table)
  }
  if (shows_predictor(cid, grouping)) info$curve_table else info$table
}

# The decimal places a predictor is shown to, in the app and the export: the
# curve's evenly spaced values are not round.
predictor_digits <- c(diameter_mm = 1, fronds = 0, cover = 2)

# The columns of a model's predictions table, with a data_support column
# (data_support()) where a row's site-year lacks the model's data, as each
# plant's can; a model's own site-years always have its data.
prediction_table <- function(rows, cid, grouping) {
  predictors <- if (shows_predictor(cid, grouping)) names(predictor_digits)
  columns <- c("site", "year", predictors, "estimate", "lower", "upper")
  support <- data_support(rows)
  rows <- as.data.frame(rows)[intersect(columns, names(rows))]
  for (name in intersect(names(predictor_digits), names(rows))) rows[[name]] <- round(rows[[name]], predictor_digits[[name]])
  if (any(support != "")) rows$data_support <- support
  rows
}

# A model's predictions figure. An overall prediction of a model without a
# predictor has no column for the x-axis, so it gets one naming what it is for.
# Each plant's weight is shown in one panel: faceted by site and year it would
# split into a panel per site-year, so every site-year is kept (max_facets) and
# the facets dropped.
prediction_figure <- function(cid, predictions, grouping, species) {
  if (grouping == "plant") {
    return(kb_plot_predictions(predictions, x = attr(predictions, "kb_predictor"), max_facets = Inf) + ggplot2::facet_null())
  }
  if (grouping == "population" && is.null(prediction_info[[cid]]$along)) {
    predictions$group <- population_label(cid, species)
    return(kb_plot_predictions(predictions, x = "group") + ggplot2::labs(x = NULL))
  }
  kb_plot_predictions(predictions)
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
# see; then each plant, for a weight model with a size sheet (a pre-fit model
# offers no sites, as its reference data stay private); else the population.
default_grouping <- function(choices) {
  for (grouping in c("site", "plant")) {
    if (grouping %in% names(choices)) {
      return(grouping)
    }
  }
  "population"
}

# The grouping chosen on a model's page, or the default where none is chosen or
# the choice is no longer offered.
chosen_grouping <- function(chosen, choices) {
  if (is.null(chosen) || !chosen %in% names(choices)) default_grouping(choices) else chosen
}

predictions_panel <- function(ns, cid, prefit, species, plants, selected = NULL) {
  choices <- prediction_choices(cid, prefit, species, plants)
  note <- if (prefit) {
    "A pre-fit model's predictions are for a typical site and year, so that its reference data stay private."
  } else if (!has_effect(cid, "site", species)) {
    "This model has no site or year effects, so its predictions are overall, for a typical site and year."
  } else if (!has_effect(cid, "year", species)) {
    "This model has a site effect but no year effect, so there are no predictions by year."
  }
  # A labelled dropdown, as on the biomass pages; a model with one grouping
  # has none, and its note says why.
  tagList(
    div(
      class = "d-flex flex-wrap align-items-start column-gap-4 row-gap-2 mb-3",
      if (length(choices) > 1) {
        div(
          class = "d-flex flex-column gap-1",
          span(class = "small fw-medium", with_help(span(id = ns("grouping_title"), "Group by"), "prediction_groups")),
          selectInput(
            ns("grouping"), NULL,
            choices = stats::setNames(names(choices), choices), selected = chosen_grouping(selected, choices),
            selectize = FALSE, width = "16rem"
          ) |>
            tagAppendAttributes(class = "mb-0", `aria-labelledby` = ns("grouping_title"))
        )
      },
      if (!is.null(note)) div(class = "small text-body-secondary w-100", note)
    ),
    plot_table_nav(
      ns("view"),
      uiOutput(ns("figure")),
      panel(
        textOutput(ns("table_title"), inline = TRUE),
        description = tagList(
          with_help("With 95% compatibility intervals, to 3 significant figures.", "interval"),
          uiOutput(ns("support_note"), inline = TRUE)
        ),
        reactable::reactableOutput(ns("table"))
      )
    )
  )
}

# Derived estimates -----------------------------------------------------------
# Survey biomass and total biomass over the canopy mapped by drone, for the
# chosen measure (wet, dry or carbon) and rows; survey biomass per m\u00b2
# surveyed or in total over the area surveyed (for a census of whole sites,
# the site's total). kelpbio offers more (mean plant weight, canopy biomass per
# m\u00b2, any sum); the app keeps to those a report uses.

derived_info <- list(
  survey = list(
    detail = "Biomass of the area surveyed in situ.",
    help = "survey_biomass", scales = c(area = "Per m\u00b2 surveyed", total = "Total over the area surveyed")
  ),
  canopy = list(
    detail = "Biomass over the canopy mapped at each site.",
    help = "canopy_biomass", per_area = FALSE
  )
)

# The grouping choices of a derived estimate.
derived_sums <- function(eid, sheets) {
  switch(eid,
    survey = survey_sums,
    canopy = canopy_sums(if (!is.null(sheets$canopy)) canopy_groups(sheets$canopy$rows) else character())
  )
}

mod_derived_estimate_ui <- function(id, eid) {
  ns <- NS(id)
  tagList(
    estimate_header(
      with_help(estimate_label(eid), derived_info[[eid]]$help),
      derived_info[[eid]]$detail,
      button(ns("change_sources"), "View models", variant = "outline", size = "sm")
    ),
    uiOutput(ns("notices")),
    uiOutput(ns("body"))
  )
}

mod_derived_estimate_server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    eid <- id
    info <- derived_info[[eid]]
    ns <- session$ns
    chosen <- reactiveVal("wet")
    output_key <- reactive(if (store$outputs()[[chosen()]]$available) chosen() else "wet")
    available <- dedupe(reactive(store$estimates()[[eid]]))
    sums <- dedupe(reactive(derived_sums(eid, store$sheets())))
    # The store holds the scale and sum chosen, so a resumed run shows those it
    # was saved with, and the Export step uses those shown here; deduplicated,
    # as it holds every estimate's choices.
    choice <- dedupe(reactive(store$estimate_choices()[[eid]]))
    per_area <- reactive(chosen_per_area(eid, choice()$scale, store$census()))
    sum_key <- reactive(chosen_sum(choice()$sum, sums()))

    observeEvent(input$output, chosen(input$output))
    observeEvent(input$scale, store$set_estimate_choice(eid, "scale", input$scale))
    observeEvent(input$sum, store$set_estimate_choice(eid, "sum", input$sum))
    observeEvent(per_area(), {
      scale <- if (per_area()) "area" else "total"
      if (!is.null(input$scale) && !identical(input$scale, scale)) updateSelectInput(session, "scale", selected = scale)
    })
    observeEvent(sum_key(), {
      if (!is.null(input$sum) && !identical(input$sum, sum_key())) updateSelectInput(session, "sum", selected = sum_key())
    })
    observeEvent(input$change_sources, store$go_to("models"))
    observeEvent(input$to_models, store$go_to("models"))
    observe_warning_links(input, store)

    output$notices <- renderUI({
      req(available()$available)
      key <- output_key()
      outputs <- store$outputs()
      unavailable <- lapply(Filter(function(k) !outputs[[k]]$available, names(outputs)), function(k) outputs[[k]]$reason)
      estimate_notices(ns, store, unique(c(derived_components[[eid]], output_components[[key]])), extra = lapply(
        unavailable, function(reason) notice("minus-circle", reason, tone = "muted")
      ))
    })

    output$body <- renderUI({
      if (!available()$available) {
        return(estimate_unavailable(ns, available()$reason))
      }
      outputs <- store$outputs()
      open <- names(outputs)[vapply(outputs, `[[`, logical(1), "available")]
      choices <- sums()
      # Each control is a group with its label above it, so the groups wrap
      # whole on a narrow screen.
      group <- function(title, input_id, control) {
        div(
          class = "d-flex flex-column gap-1",
          span(class = "small fw-medium", id = ns(paste0(input_id, "_title")), title),
          tagAppendAttributes(control, class = "mb-0", `aria-labelledby` = ns(paste0(input_id, "_title")))
        )
      }
      tagList(
        div(
          class = "d-flex flex-wrap align-items-start column-gap-4 row-gap-2 mb-3",
          group("Output", "output", selectInput(
            ns("output"), NULL,
            choices = stats::setNames(open, vapply(output_info[open], `[[`, "", "label")), selected = isolate(output_key()),
            selectize = FALSE, width = "12rem"
          )),
          if (!is.null(info$scales)) {
            group("Scale", "scale", selectInput(
              ns("scale"), NULL,
              choices = stats::setNames(names(info$scales), info$scales), selected = if (isolate(per_area())) "area" else "total",
              selectize = FALSE, width = "14rem"
            ))
          },
          if (!is.null(choices)) {
            group("Show", "sum", selectInput(
              ns("sum"), NULL,
              choices = stats::setNames(names(choices), vapply(choices, `[[`, "", "label")), selected = isolate(sum_key()),
              selectize = FALSE, width = "16rem"
            ))
          },
          if (!is.null(choices)) div(class = "small text-body-secondary w-100", textOutput(ns("sum_detail"), inline = TRUE))
        ),
        uiOutput(ns("stand_ins")),
        plot_table_nav(ns("view"), uiOutput(ns("plot_panel")), uiOutput(ns("table_panel")), selected = isolate(input$view))
      )
    })

    # Representative sites: for each fit that has no data for some of the sites
    # predicted, the sites of its data that stand in for them.
    stand_in_models <- dedupe(reactive({
      rows <- store$sheets()[[stand_in_rows[[eid]]]]$rows
      models <- lapply(stats::setNames(nm = stand_in_ids[[eid]]), function(id) {
        fit <- store$fit_of(id)
        if (!is.null(fit) && !is.null(rows)) list(sites = fit_sites(fit), new = new_sites(fit, rows))
      })
      Filter(function(model) length(model$new) > 0, models)
    }))
    output$stand_ins <- renderUI({
      models <- stand_in_models()
      if (length(models) == 0) {
        return(NULL)
      }
      chosen <- isolate(store$stand_ins_chosen())
      div(class = "d-flex flex-wrap align-items-start column-gap-4 row-gap-2 mb-3", lapply(names(models), function(id) {
        title_id <- ns(paste0("stand_in_", id, "_title"))
        div(
          class = "d-flex flex-column gap-1",
          span(
            class = "small fw-medium", id = title_id,
            with_help(sprintf("Representative site (%s model)", label_of(id)), "stand_ins")
          ),
          tagAppendAttributes(
            selectizeInput(
              ns(paste0("stand_in_", id)), NULL,
              choices = models[[id]]$sites, selected = intersect(chosen[[id]], models[[id]]$sites), multiple = TRUE,
              width = "16rem", options = list(placeholder = sprintf("None (%d new %s)", length(models[[id]]$new), if (length(models[[id]]$new) == 1) "site" else "sites"))
            ),
            class = "mb-0", `aria-labelledby` = title_id
          )
        )
      }))
    })
    lapply(stand_in_ids[[eid]], function(id) {
      observeEvent(input[[paste0("stand_in_", id)]], store$set_stand_ins(id, input[[paste0("stand_in_", id)]] %||% character()), ignoreNULL = FALSE, ignoreInit = TRUE)
    })

    # A note only where the rows are not obvious from their label.
    output$sum_detail <- renderText({
      detail <- sums()[[req(sum_key())]]$detail
      if (is.list(detail)) detail[[if (per_area()) "area" else "total"]] else detail %||% ""
    })

    labels <- reactive(derived_labels(eid, output_key(), per_area()))
    # Each survey is plotted one site at a time, by plot within each year, so
    # the surveys of a site-year sit side by side; the table holds every survey.
    by_survey <- reactive(eid == "survey" && identical(sum_key(), "survey"))
    sites <- reactive(unique(as.character(estimated()$site)))
    site <- reactive({
      choices <- sites()
      if (!is.null(input$site) && input$site %in% choices) input$site else choices[[1]]
    })
    # Shiny sends the outputs of one flush together, so the panels hold only
    # what is quick to render and show at once; whatever needs the estimates
    # (the site choice, an error, the figure and table) is an output inside
    # them, rendered in the flush after.
    # The figure's height is reserved from the sheet rows, as its estimates are
    # still to come; the site is the one chosen or else the first.
    output$plot_panel <- renderUI({
      rows <- store$sheets()[[stand_in_rows[[eid]]]]$rows
      pool_by <- sums()[[sum_key()]]$pool_by
      site <- if (by_survey()) {
        chosen <- isolate(input$site)
        if (!is.null(chosen) && chosen %in% rows$site) chosen else as.character(rows$site[[1]])
      }
      height <- derived_height(rows, site, faceted = is.null(pool_by) || "site" %in% pool_by)
      panel(
        labels()$label,
        description = derived_caption(eid, labels(), sum_key()),
        if (by_survey()) uiOutput(ns("site_ui")),
        uiOutput(ns("plot_error")),
        figure_plot(ns("figure"), height = figure_px(height))
      )
    })
    output$site_ui <- renderUI({
      div(
        class = "d-flex align-items-center gap-2",
        span(class = "small fw-medium", id = ns("site_title"), "Site"),
        selectInput(ns("site"), NULL, choices = sites(), selected = isolate(site()), selectize = FALSE, width = "12rem") |>
          tagAppendAttributes(class = "mb-0", `aria-labelledby` = ns("site_title"))
      )
    })
    output$table_panel <- renderUI({
      panel(
        sprintf("%s (%s)", labels()$label, labels()$unit),
        description = tagList(
          with_help("With 95% compatibility intervals, to 3 significant figures.", "interval"),
          support_note()
        ),
        uiOutput(ns("table_error")),
        reactable::reactableOutput(ns("table"))
      )
    })
    estimate_error <- function() {
      result <- estimates()
      if (inherits(result, "error")) {
        notice("x-circle", sprintf("%s could not be estimated", estimate_label(eid)), condition_text(result), tone = "warning")
      }
    }
    output$plot_error <- renderUI(estimate_error())
    output$table_error <- renderUI(estimate_error())

    # The estimates for the chosen measure, scale and rows, or the error kelpbio gave.
    estimates <- reactive({
      req(available()$available)
      key <- output_key()
      tryCatch(
        switch(eid,
          survey = store$survey_biomass(key, per_area(), sums()[[sum_key()]]$pool_by),
          canopy = store$canopy_biomass(key, sums()[[sum_key()]]$pool_by)
        ),
        error = function(e) e
      )
    })
    estimated <- reactive(if (!inherits(estimates(), "error")) estimates() else req(FALSE))

    output$figure <- render_figure(
      function() derived_figure(estimated(), if (by_survey()) site()),
      "figure",
      height = function() figure_px(derived_height(estimated(), if (by_survey()) site())),
      alt = function() sprintf("Estimated %s", tolower(labels()$label))
    )

    output$table <- reactable::renderReactable({
      rows <- biomass_table(estimated(), store$stand_ins())
      number <- function(name) reactable::colDef(name = name, cell = number_text, align = "right", class = "kb-tabular")
      area <- function(name) {
        reactable::colDef(name = name, align = "right", class = "kb-tabular", format = reactable::colFormat(separators = TRUE, digits = 0))
      }
      columns <- lapply(stats::setNames(nm = setdiff(names(rows), c("estimate", "lower", "upper", "data_support"))), function(name) {
        switch(name,
          survey_area_m2 = ,
          canopy_area_m2 = area(column_label(name)),
          plants = reactable::colDef(name = column_label(name), align = "right", class = "kb-tabular"),
          reactable::colDef(name = column_label(name))
        )
      })
      app_table(
        rows,
        columns = c(columns, list(
          estimate = number(column_label("estimate")),
          lower = number(column_label("lower")),
          upper = number(column_label("upper")),
          data_support = support_column(rows)
        ))
      )
    })
  })
}

# A derived estimate's figure: by site and year, one facet per site, so the
# figure grows with the rows of facets (kb_plot_predictions() uses
# facet_wrap()'s default layout); summed over every row, the one estimate gets
# a label on the x-axis. With `site`, each survey of that site, by year.
derived_figure <- function(predictions, site = NULL) {
  if (!is.null(site)) {
    predictions <- predictions[predictions$site == site, ]
    return(kb_plot_predictions(predictions, x = "survey_id", max_facets = Inf) + ggplot2::facet_wrap(~year, scales = "free_x") + ggplot2::labs(x = "Survey"))
  }
  if (length(attr(predictions, "kb_group_vars")) == 0) {
    predictions$group <- "All"
    return(kb_plot_predictions(predictions, x = "group") + ggplot2::labs(x = NULL))
  }
  kb_plot_predictions(predictions)
}

# The height of derived_figure() in pixels at 96 dpi, from its predictions, or
# from the sheet rows they are made from before they are computed (with
# `faceted`, whether the rows chosen are by site and year).
derived_height <- function(rows, site = NULL, faceted = all(c("site", "year") %in% attr(rows, "kb_group_vars"))) {
  if (!is.null(site)) {
    years <- unique(rows$year[rows$site == site])
    return(60 + 170 * ggplot2::wrap_dims(length(years))[1])
  }
  if (!faceted) {
    return(360)
  }
  60 + 170 * ggplot2::wrap_dims(length(unique(rows$site)))[1]
}

# The scale chosen for a derived estimate: survey biomass is per m\u00b2
# surveyed unless chosen otherwise, or in total in a census, where the area
# surveyed is often the whole site; canopy biomass is in total.
chosen_per_area <- function(eid, chosen, census) {
  if (is.null(derived_info[[eid]]$scales)) {
    return(derived_info[[eid]]$per_area)
  }
  (chosen %||% if (census) "total" else "area") == "area"
}

# The rows chosen for a derived estimate (a derived_sums() name), or the first.
chosen_sum <- function(chosen, choices) {
  if (is.null(choices)) {
    return(NULL)
  }
  if (!is.null(chosen) && chosen %in% names(choices)) chosen else names(choices)[1]
}

# A derived estimate's label and unit, for the chosen measure and scale.
derived_labels <- function(eid, key, per_area) {
  info <- output_info[[key]]
  list(label = info$label, unit = info$unit[[if (per_area) "area" else "total"]])
}

# With `site`, the caption of the figure of that site's surveys.
derived_caption <- function(eid, labels, sum_key, site = NULL) {
  by <- switch(sum_key,
    survey = sprintf(" of each survey at %s, faceted by year", if (is.null(site)) "the chosen site" else site),
    site_year = if (eid == "canopy") " of each drone map of a site by year, faceted by site" else " of each site-year by year, faceted by site",
    year = " summed over sites by year",
    sprintf(" summed by %s and year, faceted by %s", sum_key, sum_key)
  )
  sprintf("Estimated %s (%s)%s, with 95%% compatibility intervals.", tolower(labels$label), labels$unit, by)
}

# The estimates table: the rows of a derived prediction without their draws,
# with a data_support column (data_support()). The sources, pre-fit or not,
# are the same for every row and are listed on the Models step.
biomass_table <- function(rows, stand_ins = list()) {
  keys <- c("survey_id", attr(rows, "kb_group_vars"), "survey_area_m2", "plants", "canopy_area_m2")
  columns <- intersect(c(unique(keys), "estimate", "lower", "upper"), names(rows))
  data.frame(as.data.frame(summary_columns(rows))[columns], data_support = data_support(rows, stand_ins))
}

# Each *_support column: the model it is for, and its value when the model has
# data for the site-year itself (the cover calibration has no site-year effect).
support_info <- list(
  size_support = list(id = "size", full = "site-year"),
  weight_support = list(id = "weight", full = "site-year"),
  cover_support = list(id = "cover", full = "site, year")
)

# Each row's models without data for its site-year and how their estimates
# were made, joined by "; " ("" where none), from the *_support columns, which
# give what each model's data hold for it, and the sites standing in where a
# model has no data for the site (`stand_ins`, by model).
data_support <- function(rows, stand_ins = list()) {
  supports <- intersect(names(support_info), names(rows))
  vapply(seq_len(nrow(rows)), function(i) {
    paste(unlist(lapply(supports, function(column) {
      support_detail(column, rows[[column]][i], stand_ins[[support_info[[column]]$id]])
    })), collapse = "; ")
  }, character(1))
}

# "No size data: estimated from year", or NULL where the model has data for the
# site-year. A site without data takes the effects of the stand-in sites
# (`stand_ins`), if any.
support_detail <- function(column, support, stand_ins = NULL) {
  info <- support_info[[column]]
  if (support == info$full) {
    return(NULL)
  }
  from <- if (length(stand_ins) > 0 && support %in% c("year", "none")) {
    switch(support,
      year = sprintf("from year and %s", and_list(stand_ins)),
      sprintf("from %s", and_list(stand_ins))
    )
  } else {
    switch(support,
      "site, year" = "from site and year",
      site = "from site",
      year = "from year",
      "for a typical site-year"
    )
  }
  sprintf("No %s data: estimated %s", lower_label(info$id), from)
}

# The Data support column, one model to a line, shown only where a row lacks data.
support_column <- function(rows) {
  reactable::colDef(
    name = column_label("data_support"), minWidth = 260, show = any(rows$data_support != ""),
    cell = function(value) {
      if (value != "") div(lapply(strsplit(value, "; ", fixed = TRUE)[[1]], div))
    }
  )
}

support_note <- function() {
  span(
    class = "d-inline-flex flex-wrap align-items-center gap-1",
    "Data support names the models without data for the", with_help("site-year", "coverage"),
    "and how their estimates were made."
  )
}
