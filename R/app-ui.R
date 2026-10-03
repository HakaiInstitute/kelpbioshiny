# The app's page: a navbar with one step per tab (Data -> Models -> Biomass ->
# Export), a fit activity badge, a Help tab and a footer.

# Data and Models carry a marker that shows when they are done; Biomass and
# Export keep their number.
step_title <- function(value) {
  marker <- if (value %in% c("data", "models")) {
    uiOutput(paste0("mark_", value), inline = TRUE)
  } else {
    step_marker(match(value, names(steps)))
  }
  span(class = "d-inline-flex align-items-center gap-2", marker, steps[[value]])
}

# Inline-flex and vertically centred, so the brand sits on the navbar's centre
# line with the step tabs rather than at the top of an inline line box. Below the
# md breakpoint only the name shows, so the brand fits beside the menu toggler.
brand <- function() {
  div(
    class = "d-inline-flex align-items-center align-middle gap-3",
    tags$img(src = "hakai.png", alt = "Hakai Institute", class = "kb-brand-logo d-none d-md-inline"),
    span(class = "kb-brand-divider d-none d-md-inline", `aria-hidden` = "true"),
    div(
      class = "lh-sm",
      div(class = "fs-5 fw-semibold", "kelpbio"),
      div(class = "small fw-normal kb-brand-subtitle d-none d-md-block", "Bayesian kelp biomass estimation")
    )
  )
}
fit_activity <- function() {
  conditionalPanel(
    "output.fitting_active",
    actionLink(
      "activity",
      span(
        class = "badge rounded-pill d-inline-flex align-items-center gap-2 border border-primary-subtle bg-primary-subtle text-primary-emphasis kb-tabular",
        lucide("loader-2", "kb-spin"), textOutput("activity_label", inline = TRUE)
      ),
      class = "text-decoration-none"
    )
  )
}

app_ui <- function() {
  page_navbar(
    title = brand(),
    id = "step",
    theme = app_theme(),
    fluid = FALSE,
    fillable = FALSE,
    window_title = "kelpbio",
    navbar_options = navbar_options(collapsible = TRUE),
    header = tags$head(
      tags$link(rel = "stylesheet", href = "styles.css"),
      tags$link(rel = "icon", type = "image/png", href = "favicon-96x96.png")
    ),
    nav_spacer(),
    nav_item(fit_activity()),
    nav_panel(step_title("data"), value = "data", div(class = "py-4", mod_data_ui("data"))),
    nav_panel(step_title("models"), value = "models", div(class = "py-4", mod_models_ui("models"))),
    nav_panel(step_title("biomass"), value = "biomass", div(class = "py-4", mod_biomass_ui("biomass"))),
    nav_panel(step_title("export"), value = "export", div(class = "py-4", mod_export_ui("export"))),
    # Help is not a step, so its tab carries no marker. The spacer above already
    # pushes the tabs right, so Help needs none of its own.
    nav_panel("Help", value = "help", div(class = "py-4", help_ui())),
    footer = div(
      class = "border-top text-center small text-body-secondary py-4 mt-4",
      sprintf("kelpbioshiny v%s \u00b7 Hakai Institute \u00b7 Poisson Consulting", utils::packageVersion("kelpbioshiny"))
    )
  )
}
