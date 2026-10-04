# Small UI building blocks. They compose Bootstrap utility classes; the few
# app classes they use (kb-*) are styled in inst/app/www/styles.css.

page_header <- function(title, description, action = NULL) {
  div(
    class = "d-flex flex-wrap align-items-end justify-content-between gap-3 mb-4",
    div(h1(class = "kb-page-title", title), div(class = "kb-lead text-body-secondary", description)),
    action
  )
}

# A card with a shadcn-style heading (title, muted description, optional action).
panel <- function(title, ..., description = NULL, action = NULL, class = NULL, id = NULL) {
  card(
    id = id,
    class = class,
    card_body(
      gap = "1rem",
      div(
        class = "d-flex align-items-start justify-content-between gap-3",
        div(
          h2(class = "kb-card-title mb-0", title),
          if (!is.null(description)) div(class = "small text-body-secondary mt-1", description)
        ),
        if (!is.null(action)) div(class = "flex-shrink-0", action)
      ),
      ...
    )
  )
}

# A step's numbered circle, as in the navbar: its number until the step is done,
# then a check mark (or a warning or busy icon). Screen readers get the state as
# hidden text; the number is hidden from them, as the step order is the tab order.
step_marker <- function(number, state = c("todo", "done", "warning", "busy"), class = NULL) {
  state <- match.arg(state)
  add <- function(x) paste(c("kb-step-marker", x, class), collapse = " ")
  hidden <- function(text) span(class = "visually-hidden", text)
  switch(state,
    # A white disc with a slate tick stands out on the slate navbar.
    done = span(class = add("bg-white text-secondary-emphasis"), lucide("check"), hidden("complete")),
    warning = span(class = add("bg-warning text-white"), lucide("alert-triangle"), hidden("complete with warnings")),
    busy = span(class = add("bg-primary-subtle text-primary-emphasis"), lucide("loader-2", "kb-spin"), hidden("fitting")),
    todo = span(class = add("kb-step-todo"), `aria-hidden` = "true", number)
  )
}

# One step as a row: its marker beside the step name and description, with any
# detail below them. The welcome card and the user guide list the steps this way.
step_item <- function(value, ..., small = TRUE) {
  div(
    class = "d-flex align-items-start gap-3",
    step_marker(match(value, names(steps)), class = "flex-shrink-0 mt-1"),
    div(
      class = "flex-grow-1 d-flex flex-column gap-2",
      div(
        div(class = "fw-medium", steps[[value]]),
        div(class = paste(c("text-body-secondary", if (small) "small"), collapse = " "), step_description(value))
      ),
      ...
    )
  )
}

# A step page: the aside (choices and statuses) beside the main content on large
# screens, and below it on small ones, so a phone shows the content first.
step_layout <- function(aside, main) {
  div(
    class = "row g-4",
    div(class = "col-lg-3 order-last order-lg-first", div(class = "kb-aside", aside)),
    div(class = "col-lg-9", main)
  )
}

# `id` names the title, so a radio group below can be labelled by it.
sidebar_section <- function(title, ..., id = NULL) {
  div(class = "d-flex flex-column gap-2 mb-4", div(id = id, class = "kb-eyebrow text-body-secondary", title), ...)
}

notice <- function(icon, title, ..., tone = c("info", "warning", "muted"), action = NULL) {
  tone <- match.arg(tone)
  box <- switch(tone,
    info = "bg-info-subtle border-info-subtle",
    warning = "bg-warning-subtle border-warning-subtle",
    muted = "bg-body-tertiary"
  )
  icon_class <- switch(tone, info = "text-info", warning = "text-warning", muted = "text-body-secondary")
  body <- Filter(Negate(is.null), list(...))
  div(
    class = paste("d-flex align-items-start gap-3 rounded-3 border p-3 small", box),
    span(class = "mt-1", lucide(icon, icon_class)),
    div(
      class = "flex-grow-1 d-flex flex-column gap-1",
      div(class = "fw-medium", title),
      if (length(body) > 0) div(class = "text-body-secondary", body)
    ),
    if (!is.null(action)) div(class = "flex-shrink-0", action)
  )
}

empty_state <- function(icon, title, description, ...) {
  card(card_body(
    class = "d-flex flex-column align-items-center text-center gap-2 py-5",
    div(class = "kb-empty-icon bg-primary-subtle text-primary-emphasis", lucide(icon)),
    div(class = "fw-medium mt-1", title),
    div(class = "text-body-secondary", style = "max-width: 32rem", description),
    ...
  ))
}

# icon is a Lucide icon name, or an icon already drawn with lucide().
badge <- function(text, class, icon = NULL) {
  if (is.character(icon) && !inherits(icon, "html")) icon <- lucide(icon)
  span(class = paste("badge d-inline-flex align-items-center gap-1", class), icon, text)
}

success_badge <- function(text, icon = "check-circle-2") badge(text, "bg-success-subtle text-success-emphasis", icon)
warning_badge <- function(text, icon = "alert-triangle") badge(text, "bg-warning-subtle text-warning-emphasis", icon)

# Buttons: "btn-primary" is the default variant (the next step of the run, one
# per screen), "kb-btn-soft" the soft variant (a step still to do, such as a
# model's Fit in the Models list), "btn-light border" the outline variant (any
# other action) and "btn-light" the ghost variant (a small adjustment inside a
# panel); $light is white in the theme. Each non-primary variant carries
# btn-light, which keeps Shiny's btn-default styles off the button.
button <- function(id, label, icon = NULL, variant = c("primary", "soft", "outline", "ghost"), size = NULL, ...) {
  variant <- match.arg(variant)
  class <- c(
    switch(variant, primary = "btn-primary", soft = "btn-light kb-btn-soft", outline = "btn-light border", ghost = "btn-light"),
    if (!is.null(size)) paste0("btn-", size)
  )
  if (is.character(icon) && !inherits(icon, "html")) icon <- lucide(icon)
  actionButton(id, span(class = "d-inline-flex align-items-center gap-2", icon, label), class = paste(class, collapse = " "), ...)
}

# A status in words: the badges, the hidden text beside status icons and the
# reasons biomass is locked all use it.
status_label <- function(status) {
  if (status$kind == "ready" && has_warning(status)) {
    return("Ready with warnings")
  }
  switch(status$kind,
    "not-used" = "Not used",
    "no-data" = "Needs data",
    "data-error" = "Data error",
    "not-fitted" = "Not fitted",
    "queued" = "Queued",
    "fitting" = "Fitting",
    "ready" = "Ready",
    "failed" = "Failed"
  )
}

# The status after a model's name, for screen readers, where only an icon shows it.
status_hidden <- function(status) span(class = "visually-hidden", paste0(", ", tolower(status_label(status))), .noWS = "before")

status_icon <- function(status) {
  if (has_warning(status)) {
    return(lucide("alert-triangle", "text-warning"))
  }
  switch(status$kind,
    "not-used" = lucide("minus-circle", "text-body-secondary"),
    "no-data" = lucide("x-circle", "text-danger"),
    "data-error" = lucide("x-circle", "text-danger"),
    "not-fitted" = lucide("circle", "text-body-secondary opacity-75"),
    "queued" = lucide("clock", "text-body-secondary"),
    "fitting" = lucide("loader-2", "kb-spin text-primary"),
    "ready" = lucide("check-circle-2", "text-success"),
    "failed" = lucide("x-circle", "text-danger")
  )
}

# A fit's progress shows in the Models list banner and on the model page, so
# the fitting badge carries no percentage and does not re-render as it moves.
status_badge <- function(status) {
  label <- status_label(status)
  switch(status$kind,
    "not-used" = span(class = "small text-body-secondary", label),
    "no-data" = badge(label, "text-bg-danger", "x-circle"),
    "data-error" = badge(label, "text-bg-danger", "x-circle"),
    "not-fitted" = badge(label, "bg-secondary-subtle text-secondary-emphasis", "circle-dashed"),
    "queued" = badge(label, "border text-body", "clock"),
    "fitting" = span(
      class = "badge d-inline-flex align-items-center gap-1 border border-primary-subtle text-primary",
      lucide("loader-2", "kb-spin"), label
    ),
    "ready" = if (has_warning(status)) warning_badge(label) else success_badge(label),
    "failed" = badge(label, "text-bg-danger", "x-circle")
  )
}

# A link in a step's side navigation (Models, Estimates): an icon and a label,
# with any hidden status text after it, highlighted when active.
subnav_link <- function(input_id, active, icon, label, hidden = NULL) {
  actionLink(
    input_id, div(class = "d-flex align-items-center gap-2", icon, span(class = "flex-grow-1", label, hidden)),
    class = paste("nav-link py-2 px-2", if (active) "active fw-medium")
  )
}

# A model's fit control: Fit (Refit once fitted or failed) while it uses your
# data, and Cancel while it is queued or fitting; none while it has no checked
# data. Fit is disabled until the model can be fitted and while a setting is
# invalid. `model`, when given, names the model for screen readers, where
# several controls share a page.
fit_control <- function(fit_id, cancel_id, status, invalid, fit_label = "Fit", variant = "primary", size = NULL, model = NULL) {
  if (status$source != "user" || status$kind %in% c("no-data", "data-error")) {
    return(NULL)
  }
  aria <- function(label) if (!is.null(model)) sprintf("%s the %s model", label, lower_label(model))
  if (is_pending_status(status)) {
    return(button(cancel_id, "Cancel", "x", "outline", size = size, `aria-label` = aria("Cancel fitting")))
  }
  refit <- status$kind %in% c("ready", "failed")
  label <- if (refit) "Refit" else fit_label
  button(
    fit_id, label, "play", if (refit) "outline" else variant,
    size = size, disabled = !can_fit(status) || invalid, `aria-label` = aria(if (refit) "Refit" else "Fit")
  )
}

# "density, size and weight"
and_list <- function(x) {
  if (length(x) < 2) {
    return(x)
  }
  paste(paste(x[-length(x)], collapse = ", "), "and", x[length(x)])
}

block_reason <- function(status) tolower(status_label(status))

# A fit's progress as "45%", padded to three digits with figure spaces (as wide
# as a digit in tabular figures), so text around it does not shift as it grows.
percent_text <- function(value) {
  paste0(gsub(" ", "\u2007", formatC(floor(value), width = 3), fixed = TRUE), "%")
}

progress_bar <- function(value) {
  div(
    class = "progress", style = "height: 0.5rem", role = "progressbar", `aria-label` = "Fitting progress",
    `aria-valuenow` = floor(value), `aria-valuemin` = 0, `aria-valuemax` = 100,
    div(class = "progress-bar", style = sprintf("width: %.1f%%", value))
  )
}

# A figure and its table as two pills, so the table is one click away rather
# than below the figure.
plot_table_nav <- function(id, plot, table, selected = NULL) {
  navset_pill(
    id = id,
    selected = selected,
    nav_panel("Plot", value = "plot", div(class = "pt-3", plot)),
    nav_panel("Table", value = "table", div(class = "pt-3", table))
  )
}

# Figures render at more than the 96 dpi screen default, so their text is legible;
# figure_px() scales a height in pixels at 96 dpi to match.
figure_res <- 120
figure_px <- function(px) round(px * figure_res / 96)

# A ggplot from kelpbio drawn by render_figure(), with its caption.
figure_plot <- function(output_id, caption = NULL, class = NULL) {
  tags$figure(
    class = paste(c("d-flex flex-column gap-2 mb-0", class), collapse = " "),
    div(class = "rounded-3 border bg-white overflow-hidden", plotOutput(output_id, height = "auto")),
    if (!is.null(caption)) tags$figcaption(class = "small text-body-secondary", caption)
  )
}

# Renders plot() at the figure's width, with a height in pixels or as a share
# of the width (aspect); either can be a function, to follow other inputs.
render_figure <- function(plot, output_id, aspect = 0.5, height = NULL, alt = NA,
                          session = getDefaultReactiveDomain()) {
  value <- function(x) if (is.function(x)) x() else x
  renderPlot(
    plot(),
    res = figure_res,
    alt = alt,
    height = function() {
      if (!is.null(height)) {
        return(value(height))
      }
      width <- session$clientData[[paste0("output_", session$ns(output_id), "_width")]]
      if (is.null(width) || width == 0) 400 else round(width * value(aspect))
    }
  )
}

# One row per component: icon, label, a muted note on the right.
status_list <- function(statuses, notes) {
  tags$ul(
    class = "list-unstyled d-flex flex-column gap-2 small mb-0",
    lapply(component_ids, function(id) {
      tags$li(
        class = "d-flex align-items-center gap-2",
        status_icon(statuses[[id]]),
        span(class = "flex-grow-1", label_of(id), status_hidden(statuses[[id]])),
        span(class = "text-body-secondary", notes[[id]])
      )
    })
  )
}

# Each warning's title and what to do about it, read by the notices, the fit
# messages and the User guide's warnings section.
warning_help <- list(
  convergence = list(
    title = "Convergence warning",
    advice = "Some parameters have not converged. Increase thinning (nthin) in Sampler settings, for example to 2, and refit."
  ),
  prior = list(
    title = "Prior sensitivity warning",
    advice = "If the flagged prior was not chosen on purpose, make it less informative on the Settings tab and refit."
  ),
  failed = list(title = "The fit failed", advice = "Refit the model to see its results."),
  data_check = list(title = "The data check failed", advice = "Correct the sheet and upload it again."),
  mismatch = list(title = "Site names differ across sheets", advice = mismatch_advice)
)

# The action of a model's warning notice: opens its Settings tab. Outline, so
# the page's Fit or Refit button stays the main action.
settings_button <- function(id) button(id, "Open settings", "sliders-horizontal", variant = "outline", size = "sm")

# A convergence warning whose action opens the model's settings.
convergence_notice <- function(title, settings_id) {
  notice(
    "alert-triangle", title, warning_help$convergence$advice,
    tone = "warning",
    action = settings_button(settings_id)
  )
}

# One notice per warning of the models `ids`, for the steps after Models: a
# failed fit, a convergence warning or a prior sensitivity warning. sensitivity
# holds the kb_sensitivity() rows of each model fitted to your data. The buttons
# are ns("open_<id>"), ns("settings_<id>") and ns("priors_<id>").
warning_notices <- function(ns, statuses, sensitivity, ids = component_ids) {
  title <- function(id, key) sprintf("%s model: %s", label_of(id), tolower(warning_help[[key]]$title))
  notices <- lapply(ids, function(id) {
    status <- statuses[[id]]
    list(
      if (status$kind == "failed") {
        notice(
          "x-circle", title(id, "failed"), status$message,
          tone = "warning", action = button(ns(paste0("open_", id)), "Open model", variant = "outline", size = "sm")
        )
      },
      if (isTRUE(status$convergence)) convergence_notice(title(id, "convergence"), ns(paste0("settings_", id))),
      if (isTRUE(status$prior)) {
        rows <- sensitivity[[id]]
        notice(
          "alert-triangle", title(id, "prior"), paste(prior_influence(rows[!rows$weak_prior, ]), warning_help$prior$advice),
          tone = "warning", action = settings_button(ns(paste0("priors_", id)))
        )
      }
    )
  })
  Filter(Negate(is.null), unlist(notices, recursive = FALSE))
}

# How to make one prior less informative, from its family in the fit's priors
# list, as a phrase: "reduce its rate (for example from 1 to 0.5)". The example
# halves the rate or doubles the SD: a suggested setting for the prior editor,
# not a model estimate. With a parameter, the phrase names it, for a notice that
# flags several priors.
prior_advice <- function(prior, parameter = NULL) {
  its <- if (is.null(parameter)) "its" else "the"
  action <- if (inherits(prior, "kb_prior_exponential")) {
    sprintf("reduce %s rate (for example from %s to %s)", its, as.character(prior$rate), as.character(prior$rate / 2))
  } else if (inherits(prior, "kb_prior_normal")) {
    sprintf("increase %s SD (for example from %s to %s)", its, as.character(prior$sd), as.character(prior$sd * 2))
  } else {
    "use a wider prior"
  }
  if (is.null(parameter)) action else sprintf("for %s, %s", parameter, action)
}

# "The prior for sYear is influencing the estimate.", from kb_sensitivity() rows
# whose prior is not weak.
prior_influence <- function(flagged) {
  many <- nrow(flagged) > 1
  sprintf(
    "The %s for %s %s influencing the %s.", if (many) "priors" else "prior", and_list(flagged$parameter),
    if (many) "are" else "is", if (many) "estimates" else "estimate"
  )
}

# Prior sensitivity: a warning, with a link to the priors, for parameters whose
# prior is not weak. priors is the fit's priors list, whose entries the rows'
# prior column names.
prior_notice <- function(rows, priors, settings_id) {
  if (!prior_flagged(rows)) {
    return(NULL)
  }
  flagged <- rows[!rows$weak_prior, ]
  many <- nrow(flagged) > 1
  advice <- vapply(seq_len(nrow(flagged)), function(i) {
    prior_advice(priors[[flagged$prior[i]]], if (many) flagged$parameter[i])
  }, "")
  action <- if (many) {
    sprintf(
      "Unless they were chosen on purpose, make them less informative on the Settings tab and refit: %s.",
      paste(advice, collapse = "; ")
    )
  } else {
    sprintf("Unless it was chosen on purpose, %s on the Settings tab and refit.", advice)
  }
  notice(
    "alert-triangle", warning_help$prior$title,
    paste(prior_influence(flagged), action),
    tone = "warning",
    action = settings_button(settings_id)
  )
}

# Above the prior sensitivity table: a muted note for parameters with a weak
# prior that the data say little about. The prior warning itself is in the model
# page header.
data_strength_notice <- function(rows) {
  weak_data <- rows$parameter[rows$weak_prior & !rows$strong_data]
  if (all(rows$weak_prior) && length(weak_data) == 0) {
    return(notice("check-circle-2", "All parameters have weak priors and strong data", tone = "muted"))
  }
  if (length(weak_data) == 0) {
    return(NULL)
  }
  many <- length(weak_data) > 1
  notice(
    "info", sprintf("The data say little about %s", and_list(weak_data)),
    sprintf(
      "%s uncertain. More data would narrow %s.",
      if (many) "Their estimates are" else "Its estimate is", if (many) "them" else "it"
    ),
    tone = "muted"
  )
}

sentence_case <- function(x) paste0(toupper(substr(x, 1, 1)), substring(x, 2))

# Estimates arrive from kelpbio rounded to 3 significant figures; show them as given.
number_text <- function(value) as.character(value)


# A Copy button for the text of the element with id `target`. Once the text is on
# the clipboard it sets the `copied` input to `what`, and the server confirms
# with copied_messages[[what]].
copied_messages <- c(citation = "Citation copied to the clipboard.", script = "R script copied to the clipboard.")

copy_button <- function(target, what, aria_label) {
  onclick <- sprintf(
    "navigator.clipboard.writeText(document.getElementById('%s').innerText).then(() => Shiny.setInputValue('copied', '%s', {priority: 'event'}))",
    target, what
  )
  tags$button(
    type = "button", class = "btn btn-light border btn-sm", onclick = onclick, `aria-label` = aria_label,
    span(class = "d-inline-flex align-items-center gap-2", lucide("copy"), "Copy")
  )
}

# Shared reactable options, so every table reads the same.
app_table <- function(data, columns = NULL, page_size = 10, row_style = NULL, height = "auto", pagination = TRUE, ...) {
  reactable::reactable(
    data,
    columns = columns,
    defaultPageSize = page_size,
    pagination = pagination,
    paginationType = "simple",
    showPageInfo = TRUE,
    height = height,
    rowStyle = row_style,
    highlight = TRUE,
    outlined = TRUE,
    theme = app_table_theme,
    ...,
    language = reactable::reactableLang(
      pageInfo = "Showing {rowStart} to {rowEnd} of {rows} rows",
      pagePrevious = "\u2039", pageNext = "\u203a",
      pagePreviousLabel = "Previous page", pageNextLabel = "Next page",
      pageNumbers = "Page {page} of {pages}"
    )
  )
}
