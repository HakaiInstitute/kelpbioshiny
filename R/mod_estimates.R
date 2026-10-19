# Estimates step: a sidebar listing every estimate (each model's predictions,
# then mean plant weight, survey biomass and canopy biomass from the combined
# models) beside the chosen estimate's page, switched with a hidden navset. An
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
      first <- intersect(c("canopy", "survey", "mean_weight", component_ids), store$own_estimates())
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
      tags$nav(
        class = "nav nav-pills flex-column gap-1",
        `aria-label` = "Estimates",
        lapply(intersect(component_ids, store$shown_estimates()), link),
        tags$hr(class = "my-1"),
        lapply(intersect(derived_ids, store$shown_estimates()), link)
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
      predictions_panel(ns, cid, page$prefit, page$species, page$plants)
    })

    grouping <- reactive({
      choices <- prediction_choices(cid, page()$prefit, page()$species, page()$plants)
      chosen <- input$grouping
      if (is.null(chosen) || !chosen %in% names(choices)) default_grouping(choices) else chosen
    })

    # Titled and captioned as the biomass figures are.
    output$figure <- renderUI({
      grouping <- grouping()
      panel(
        sentence_case(prediction_info[[cid]]$response),
        description = prediction_caption(cid, grouping, page()$species),
        figure_plot(ns("plot"), class = if (grouping == "population") "kb-figure-narrow")
      )
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
    "A pre-fit model was fitted to other sites and years, so its predictions here are for a typical site and year."
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
            choices = stats::setNames(names(choices), choices), selected = default_grouping(choices),
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
        description = with_help("With 95% compatibility intervals, to 3 significant figures.", "interval"),
        reactable::reactableOutput(ns("table"))
      )
    )
  )
}

# Derived estimates -----------------------------------------------------------
# Mean wet weight of a plant by site-year, survey biomass per m\u00b2 searched,
# and total biomass over the canopy mapped by drone, the last two for the
# chosen measure (wet, dry or carbon) and rows. kelpbio offers more (other
# measures of mean weight, survey totals, canopy biomass per m\u00b2, any
# sum); the app keeps to those a report uses.

derived_info <- list(
  mean_weight = list(
    detail = "The expected wet weight of a plant at each site-year, from the weight curve over the site-year's size distribution.",
    help = "mean_weight"
  ),
  survey = list(
    detail = "Biomass per m\u00b2 of the area searched by each in situ survey, from its plants and their weights.",
    help = "survey_biomass", per_area = TRUE
  ),
  canopy = list(
    detail = "Total biomass over the canopy mapped in each drone map of a site, from the cover calibration.",
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
    per_area <- reactive(info$per_area)
    sum_key <- reactive({
      choices <- sums()
      if (is.null(choices)) {
        return(NULL)
      }
      if (!is.null(input$sum) && input$sum %in% names(choices)) input$sum else names(choices)[1]
    })

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
      estimate_notices(ns, store, unique(c(derived_components[[eid]], output_components[[key]])), extra = c(
        lapply(unavailable, function(reason) notice("minus-circle", reason, tone = "muted")),
        if (inherits(result, "error")) list(notice("x-circle", sprintf("%s could not be estimated", estimate_label(eid)), conditionMessage(result), tone = "warning"))
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
          if (eid != "mean_weight") {
            group("Output", "output", selectInput(
              ns("output"), NULL,
              choices = stats::setNames(open, vapply(output_info[open], `[[`, "", "label")), selected = isolate(output_key()),
              selectize = FALSE, width = "12rem"
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
        plot_table_nav(ns("view"), uiOutput(ns("plot_panel")), uiOutput(ns("table_panel")), selected = isolate(input$view))
      )
    })

    output$sum_detail <- renderText(sums()[[req(sum_key())]]$detail)

    labels <- reactive(derived_labels(eid, output_key(), per_area()))
    # Each survey is plotted one site at a time, by plot within each year, so
    # the surveys of a site-year sit side by side; the table holds every survey.
    by_survey <- reactive(eid == "survey" && identical(sum_key(), "survey"))
    sites <- reactive(unique(as.character(estimated()$site)))
    site <- reactive({
      choices <- sites()
      if (!is.null(input$site) && input$site %in% choices) input$site else choices[[1]]
    })
    output$plot_panel <- renderUI({
      panel(
        labels()$label,
        description = derived_caption(eid, labels(), sum_key()),
        if (by_survey()) {
          div(
            class = "d-flex align-items-center gap-2",
            span(class = "small fw-medium", id = ns("site_title"), "Site"),
            selectInput(ns("site"), NULL, choices = sites(), selected = isolate(site()), selectize = FALSE, width = "12rem") |>
              tagAppendAttributes(class = "mb-0", `aria-labelledby` = ns("site_title"))
          )
        },
        figure_plot(ns("figure"))
      )
    })
    output$table_panel <- renderUI({
      panel(
        sprintf("%s (%s)", labels()$label, labels()$unit),
        description = tagList(
          with_help("With 95% compatibility intervals, to 3 significant figures.", "interval"),
          flags_note()
        ),
        reactable::reactableOutput(ns("table"))
      )
    })

    # The estimates for the chosen measure, scale and rows, or the error kelpbio gave.
    estimates <- reactive({
      req(available()$available)
      key <- output_key()
      tryCatch(
        switch(eid,
          mean_weight = store$mean_weight("wet"),
          survey = store$survey_biomass(key, per_area(), sums()[[sum_key()]]$sum_by),
          canopy = store$canopy_biomass(key, per_area(), sums()[[sum_key()]]$sum_by)
        ),
        error = function(e) e
      )
    })
    estimated <- reactive(if (!inherits(estimates(), "error")) estimates() else req(FALSE))

    # By site and year, one facet per site, so the figure grows with the rows of
    # facets (kb_plot_predictions() uses facet_wrap()'s default layout). Summed
    # over every row, the one estimate gets a label on the x-axis.
    output$figure <- render_figure(
      function() {
        predictions <- estimated()
        if (by_survey()) {
          predictions <- predictions[predictions$site == site(), ]
          return(kb_plot_predictions(predictions, x = "plot_id", max_facets = Inf) + ggplot2::facet_wrap(~year) + ggplot2::labs(x = "Plot"))
        }
        if (length(attr(predictions, "kb_group_vars")) == 0) {
          predictions$group <- "All"
          return(kb_plot_predictions(predictions, x = "group") + ggplot2::labs(x = NULL))
        }
        kb_plot_predictions(predictions)
      },
      "figure",
      height = function() {
        predictions <- estimated()
        if (by_survey()) {
          years <- unique(predictions$year[predictions$site == site()])
          return(figure_px(60 + 170 * ggplot2::wrap_dims(length(years))[1]))
        }
        if (!all(c("site", "year") %in% attr(predictions, "kb_group_vars"))) {
          return(figure_px(360))
        }
        figure_px(60 + 170 * ggplot2::wrap_dims(length(unique(predictions$site)))[1])
      },
      alt = function() sprintf("Estimated %s", tolower(labels()$label))
    )

    output$table <- reactable::renderReactable({
      rows <- biomass_table(estimated())
      flagged <- rows$flags != ""
      number <- function(name) reactable::colDef(name = name, cell = number_text, align = "right", class = "kb-tabular")
      area <- function(name) {
        reactable::colDef(name = name, align = "right", class = "kb-tabular", format = reactable::colFormat(separators = TRUE, digits = 0))
      }
      named <- c(site = "Site", year = "Year", plot_id = "Plot", date = "Date")
      columns <- lapply(stats::setNames(nm = setdiff(names(rows), c("estimate", "lower", "upper", "flags"))), function(name) {
        switch(name,
          survey_area_m2 = area("Area searched (m\u00b2)"),
          canopy_area_m2 = area("Canopy area (m\u00b2)"),
          plants = reactable::colDef(name = "Plants", align = "right", class = "kb-tabular"),
          reactable::colDef(name = if (name %in% names(named)) named[[name]] else sentence_case(name))
        )
      })
      app_table(
        rows,
        row_style = function(index) if (flagged[index]) app_row_colour$info,
        columns = c(columns, list(
          estimate = number("Estimate"),
          lower = number("Lower"),
          upper = number("Upper"),
          flags = reactable::colDef(
            name = "Flags", minWidth = 220, sortable = FALSE, show = any(flagged),
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

# A derived estimate's label and unit, for the chosen measure and scale.
derived_labels <- function(eid, key, per_area) {
  if (eid == "mean_weight") {
    return(list(
      label = "Mean plant wet weight", unit = "kg"
    ))
  }
  info <- output_info[[key]]
  list(label = info$label, unit = info$unit[[if (per_area) "area" else "total"]])
}

derived_caption <- function(eid, labels, sum_key) {
  by <- if (eid == "mean_weight") {
    " of each site-year by year, faceted by site"
  } else {
    switch(sum_key,
      survey = " of each survey at the chosen site by plot, faceted by year",
      site_year = if (eid == "canopy") " of each drone map of a site by year, faceted by site" else " of each site-year by year, faceted by site",
      year = " summed over sites by year",
      sprintf(" summed by %s and year, faceted by %s", sum_key, sum_key)
    )
  }
  sprintf("Estimated %s (%s)%s, with 95%% compatibility intervals.", tolower(labels$label), labels$unit, by)
}

# The estimates table: the rows of a derived prediction without their draws,
# with a flags column naming the models without data for each row's site-year,
# from the *_support columns, which give what each model's data hold for it.
# The sources, pre-fit or not, are the same for every row and are listed on the
# Models step.
biomass_table <- function(rows) {
  supports <- intersect(names(support_info), names(rows))
  flags <- vapply(seq_len(nrow(rows)), function(i) {
    paste(unlist(lapply(supports, function(column) support_flag(column, rows[[column]][i]))), collapse = "|")
  }, character(1))
  keys <- c(attr(rows, "kb_group_vars"), "plot_id", "date", "survey_area_m2", "plants", "canopy_area_m2")
  columns <- intersect(c(unique(keys), "estimate", "lower", "upper"), names(rows))
  data.frame(as.data.frame(summary_columns(rows))[columns], flags = flags)
}

# Each *_support column: the model it is for, and its value when the model has
# data for the site-year itself (the cover calibration has no site-year effect).
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
