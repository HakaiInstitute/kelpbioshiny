# The app's page: a navbar with one step per tab (Data -> Models -> Biomass ->
# Export), a fit activity badge, a Help tab and a footer.

# Each step carries a marker that shows when it is done. The marker follows the
# name in the page, so a screen reader reads "Models complete", and order-first
# shows it before the name.
step_title <- function(value) {
  span(class = "d-inline-flex align-items-center gap-2", steps[[value]], span(class = "order-first d-inline-flex", step_marker_switch(value)))
}

# The navbar's markers and kelp drawing are in the page from the start and
# switched in the browser by conditionalPanel(), so they appear with the rest of
# the page rather than after the server's first response. Shiny evaluates the
# conditions as the page loads, before any output arrives.

# A step's marker in each state, shown by the state the server sends as
# output.mark_<step> ("todo", "done", "warning" or "busy"). The number shows
# until the server sends another state.
step_marker_switch <- function(value) {
  number <- match(value, names(steps))
  lapply(c("todo", "done", "warning", "busy"), function(state) {
    conditionalPanel(sprintf("(output.mark_%s || 'todo') === '%s'", value, state), step_marker(number, state))
  })
}

# The species' kelp drawing, switched by the species input of the Data step
# (data-species), so it follows the choice at once.
kelp_art_switch <- function() {
  default <- names(species_info)[[1]]
  lapply(names(species_info), function(species) {
    conditionalPanel(sprintf("(input['data-species'] || '%s') === '%s'", default, species), kelp_art(species))
  })
}

# Inline-flex and vertically centred, so the brand sits on the navbar's centre
# line with the step tabs rather than at the top of an inline line box. The logo
# shows from the lg breakpoint and the subtitle from xl, so the brand leaves the
# step tabs room on one line.
brand <- function() {
  div(
    class = "d-inline-flex align-items-center align-middle gap-3",
    tags$img(src = "hakai.png", alt = "Hakai Institute", class = "kb-brand-logo d-none d-lg-inline"),
    span(class = "kb-brand-divider d-none d-lg-inline", `aria-hidden` = "true"),
    kelp_art_switch(),
    div(
      class = "lh-sm",
      div(class = "fs-5 fw-semibold", "kelpbio"),
      div(class = "small fw-normal kb-brand-subtitle d-none d-xl-block", "Bayesian kelp biomass estimation")
    )
  )
}

# A tab's content as the page's main landmark. Only the open tab is shown, so
# only one main is visible at a time.
step_main <- function(...) tags$main(class = "py-4", ...)

# The fit running and its progress, beside the step tabs: "Size 45%", with the
# fits queued on extra-wide screens only, so the tabs stay on one line.
fit_activity <- function() {
  conditionalPanel(
    "output.fitting_active",
    actionLink(
      "activity",
      span(
        class = "kb-progress badge rounded-pill d-inline-flex align-items-center gap-2 border border-primary-subtle bg-primary-subtle text-primary-emphasis kb-tabular",
        lucide("loader-2", "kb-spin"), span(class = "visually-hidden", "Fitting"),
        textOutput("activity_label", inline = TRUE),
        textOutput("activity_queued", inline = TRUE) |> tagAppendAttributes(class = "d-none d-xxl-inline")
      ),
      class = "text-decoration-none"
    )
  )
}

# jarl-ignore unused_function: called from inst/app/ui.R.
app_ui <- function() {
  page_navbar(
    title = brand(),
    id = "step",
    theme = app_theme(),
    fluid = FALSE,
    fillable = FALSE,
    window_title = "kelpbio",
    lang = "en",
    navbar_options = navbar_options(collapsible = TRUE),
    # Shiny's busy pulse would flash at the top of the page on every tick of a
    # fit; the fit progress shows in the app instead.
    header = tagList(useBusyIndicators(pulse = FALSE), tags$head(
      # Versioned by the file's modification time, so a browser fetches the
      # stylesheet again when it changes rather than using a cached copy.
      tags$link(rel = "stylesheet", href = paste0("styles.css?v=", styles_version())),
      tags$link(rel = "icon", type = "image/png", href = "favicon-96x96.png")
    )),
    nav_spacer(),
    nav_item(fit_activity()),
    nav_panel(step_title("data"), value = "data", step_main(mod_data_ui("data"))),
    nav_panel(step_title("models"), value = "models", step_main(mod_models_ui("models"))),
    nav_panel(step_title("estimates"), value = "estimates", step_main(mod_estimates_ui("estimates"))),
    nav_panel(step_title("export"), value = "export", step_main(mod_export_ui("export"))),
    # Help is not a step, so its tab carries no marker. The spacer above already
    # pushes the tabs right, so Help needs none of its own.
    nav_panel("Help", value = "help", step_main(help_ui())),
    footer = div(
      class = "border-top text-center small text-body-secondary py-4 mt-4",
      sprintf("kelpbioshiny v%s \u00b7 Hakai Institute \u00b7 Poisson Consulting", utils::packageVersion("kelpbioshiny"))
    )
  )
}

styles_version <- function() {
  path <- system.file("app", "www", "styles.css", package = "kelpbioshiny")
  as.integer(file.mtime(path))
}
