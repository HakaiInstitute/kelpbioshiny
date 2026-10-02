# Small UI building blocks. They compose Bootstrap utility classes; the few
# app classes they use (kb-*) are styled in inst/app/www/styles.css.

page_header <- function(title, description, action = NULL) {
  div(
    class = "d-flex flex-wrap align-items-end justify-content-between gap-3 mb-4",
    div(h2(class = "kb-page-title", title), div(class = "kb-lead text-body-secondary", description)),
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
          div(class = "kb-card-title", title),
          if (!is.null(description)) div(class = "small text-body-secondary mt-1", description)
        ),
        action
      ),
      ...
    )
  )
}

# A step's numbered circle, as in the navbar: its number until the step is done,
# then a check mark (or a warning or busy icon).
step_marker <- function(number, state = c("todo", "done", "warning", "busy"), class = NULL) {
  state <- match.arg(state)
  add <- function(x) paste(c("kb-step-marker", x, class), collapse = " ")
  switch(state,
    done = span(class = add("bg-primary text-white"), `aria-label` = "complete", lucide("check")),
    warning = span(class = add("bg-warning text-white"), `aria-label` = "complete with warnings", lucide("alert-triangle")),
    busy = span(class = add("bg-primary-subtle text-primary-emphasis"), lucide("loader-2", "kb-spin")),
    todo = span(class = add("kb-step-todo"), number)
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

sidebar_section <- function(title, ...) {
  div(class = "d-flex flex-column gap-2 mb-4", div(class = "kb-eyebrow text-body-secondary", title), ...)
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
    action
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

# Buttons: "btn-primary" is the default variant, "btn-light border" the outline
# variant and "btn-light" the ghost variant ($light is white in the theme).
button <- function(id, label, icon = NULL, variant = c("primary", "outline", "ghost"), size = NULL, ...) {
  variant <- match.arg(variant)
  class <- c(
    switch(variant, primary = "btn-primary", outline = "btn-light border", ghost = "btn-light"),
    if (!is.null(size)) paste0("btn-", size)
  )
  if (is.character(icon) && !inherits(icon, "html")) icon <- lucide(icon)
  actionButton(id, span(class = "d-inline-flex align-items-center gap-2", icon, label), class = paste(class, collapse = " "), ...)
}

status_icon <- function(status) {
  switch(status$kind,
    "not-used" = lucide("minus-circle", "text-body-secondary opacity-50"),
    "no-data" = lucide("x-circle", "text-danger"),
    "ready" = lucide("check-circle-2", "text-success"),
    "data-ok" = if (status$warning) {
      lucide("alert-triangle", "text-warning")
    } else {
      lucide("circle", "text-body-secondary opacity-75")
    },
    "queued" = lucide("clock", "text-body-secondary"),
    "waiting" = lucide("clock", "text-body-secondary opacity-75"),
    "fitting" = lucide("loader-2", "kb-spin text-primary"),
    "fitted" = if (status$converged) {
      lucide("check-circle-2", "text-success")
    } else if (warning_dismissed(status)) {
      lucide("alert-triangle", "text-warning opacity-50")
    } else {
      lucide("alert-triangle", "text-warning")
    }
  )
}

status_badge <- function(status, progress = 0) {
  switch(status$kind,
    "not-used" = span(class = "small text-body-secondary", "Not used"),
    "no-data" = badge("No data uploaded", "text-bg-danger", "x-circle"),
    "ready" = success_badge("Ready"),
    "data-ok" = if (status$warning) {
      warning_badge("Data warning, not fitted")
    } else {
      badge("Data OK, not fitted", "bg-secondary-subtle text-secondary-emphasis", "circle-dashed")
    },
    "queued" = badge("Queued", "border text-body", "clock"),
    "waiting" = badge(waiting_text(status), "border text-body-secondary fw-normal", if (status$queued) "clock" else "circle-dashed"),
    "fitting" = span(
      class = "badge d-inline-flex align-items-center gap-1 border border-primary-subtle text-primary kb-tabular",
      lucide("loader-2", "kb-spin"), sprintf("Fitting %d%%", floor(progress))
    ),
    "fitted" = if (status$converged) {
      success_badge("Fitted, converged")
    } else if (warning_dismissed(status)) {
      badge("Fitted, warning dismissed", "border text-body-secondary fw-normal", lucide("alert-triangle", "text-warning opacity-75"))
    } else {
      warning_badge("Fitted, convergence warning")
    }
  )
}

# A fitted model's convergence warning shows unless it was dismissed for this fit.
warning_dismissed <- function(status) "convergence" %in% status$dismissed
shows_convergence_warning <- function(status) {
  status$kind == "fitted" && !status$converged && !warning_dismissed(status)
}

# "Waiting for density, size": the upstream models a fit still needs.
waiting_text <- function(status) {
  sprintf("Waiting for %s", paste(vapply(status$on, lower_label, ""), collapse = ", "))
}

# "density, size and weight"
and_list <- function(x) {
  if (length(x) < 2) {
    return(x)
  }
  paste(paste(x[-length(x)], collapse = ", "), "and", x[length(x)])
}

block_reason <- function(status) {
  switch(status$kind,
    "no-data" = "no data uploaded",
    "queued" = "queued",
    "fitting" = "fitting",
    "waiting" = tolower(waiting_text(status)),
    "not fitted"
  )
}

progress_bar <- function(value) {
  div(
    class = "progress", style = "height: 0.5rem", role = "progressbar",
    `aria-valuenow` = floor(value), `aria-valuemin` = 0, `aria-valuemax` = 100,
    div(class = "progress-bar", style = sprintf("width: %.1f%%", value))
  )
}

# A figure and its estimates table as two pills, so the table is one click
# away rather than below the figure.
plot_estimates_nav <- function(id, plot, estimates, selected = NULL) {
  navset_pill(
    id = id,
    selected = selected,
    nav_panel("Plot", value = "plot", div(class = "pt-3", plot)),
    nav_panel("Estimates", value = "estimates", div(class = "pt-3", estimates))
  )
}

# A tab or pill label, with a slot (uiOutput) for its warning marker.
marked_title <- function(label, marker_id) {
  span(class = "d-inline-flex align-items-center gap-1", label, uiOutput(marker_id, inline = TRUE))
}

warning_marker <- function(show) {
  if (show) span(role = "img", `aria-label` = "Has warnings", title = "Has warnings", lucide("alert-triangle", "text-warning"))
}

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
    res = 96,
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
        span(class = "flex-grow-1", label_of(id)),
        span(class = "text-body-secondary", notes[[id]])
      )
    })
  )
}

convergence_advice <- "Increase thinning (nthin) in Sampler settings and refit."

# The secondary action on a warning: hides it for the model's current fit.
dismiss_button <- function(id) button(id, "I understand, dismiss", variant = "outline", size = "sm")

# A convergence warning whose main action opens the model's sampler settings.
convergence_notice <- function(title, settings_id, ..., dismiss_id = NULL, secondary = NULL) {
  notice(
    "alert-triangle", title, convergence_advice, ...,
    tone = "warning",
    action = div(
      class = "d-flex flex-wrap gap-2 flex-shrink-0",
      secondary,
      if (!is.null(dismiss_id)) dismiss_button(dismiss_id),
      button(settings_id, "Open sampler settings", "sliders-horizontal", size = "sm")
    )
  )
}

# How to make one prior less informative, from its family in the fit's priors
# list. The example halves the rate or doubles the SD: a suggested setting for
# the prior editor, not a model estimate. With parameter NULL, the advice is for
# the one flagged prior.
prior_advice <- function(prior, parameter = NULL) {
  subject <- function(family) {
    if (is.null(parameter)) paste("this", family, "prior") else sprintf("%s (%s prior)", parameter, family)
  }
  if (inherits(prior, "kb_prior_exponential")) {
    return(sprintf(
      "For %s, try reducing the rate (for example from %s to %s).",
      subject("exponential"), as.character(prior$rate), as.character(prior$rate / 2)
    ))
  }
  if (inherits(prior, "kb_prior_normal")) {
    return(sprintf(
      "For %s, try increasing the SD (for example from %s to %s).",
      subject("normal"), as.character(prior$sd), as.character(prior$sd * 2)
    ))
  }
  if (is.null(parameter)) "Try a wider prior." else sprintf("For %s, try a wider prior.", parameter)
}

# Prior sensitivity: a warning, with a link to the priors, for parameters whose
# prior is not weak; the compact form sits in the model page header. priors is
# the fit's priors list, whose entries the rows' prior column names.
prior_notice <- function(rows, priors, settings_id, dismiss_id = NULL, compact = FALSE) {
  flagged <- rows[!rows$weak_prior, ]
  if (nrow(flagged) == 0) {
    return(NULL)
  }
  many <- nrow(flagged) > 1
  influence <- sprintf(
    "The %s for %s %s influencing the %s", if (many) "priors" else "prior", and_list(flagged$parameter),
    if (many) "are" else "is", if (many) "estimates" else "estimate"
  )
  advice <- vapply(seq_len(nrow(flagged)), function(i) {
    prior_advice(priors[[flagged$prior[i]]], if (many) flagged$parameter[i])
  }, "")
  body <- paste(
    sprintf("If this is unintended, try making the %s less informative.", if (many) "priors" else "prior"),
    paste(advice, collapse = " ")
  )
  notice(
    "alert-triangle",
    if (compact) "Prior sensitivity warning" else influence,
    if (compact) paste0(influence, ". ", body) else body,
    tone = "warning",
    action = div(
      class = "d-flex flex-wrap gap-2 flex-shrink-0",
      if (!is.null(dismiss_id)) dismiss_button(dismiss_id),
      button(settings_id, "Open prior settings", "sliders-horizontal", size = "sm")
    )
  )
}

# The prior sensitivity table's notices: the prior warning unless it was
# dismissed, then a muted note for parameters with a weak prior that the data
# say little about.
sensitivity_notices <- function(rows, priors, settings_id, dismiss_id, dismissed = FALSE) {
  weak_data <- rows$parameter[rows$weak_prior & !rows$strong_data]
  if (all(rows$weak_prior) && length(weak_data) == 0) {
    return(notice("check-circle-2", "All parameters have weak priors and strong data", tone = "muted"))
  }
  many <- length(weak_data) > 1
  tagList(
    if (!dismissed) prior_notice(rows, priors, settings_id, dismiss_id),
    if (length(weak_data) > 0) {
      notice(
        "info", sprintf("The data say little about %s", and_list(weak_data)),
        sprintf(
          "%s uncertain. More data would narrow %s.",
          if (many) "Their estimates are" else "Its estimate is", if (many) "them" else "it"
        ),
        tone = "muted"
      )
    }
  )
}

# "bull kelp (Nereocystis luetkeana)", with no spaces inside the brackets.
# One HTML string, since separate tags would be rendered with whitespace between them.
species_phrase <- function(sp, end = "") {
  HTML(sprintf("%s (<em>%s</em>)%s", htmltools::htmlEscape(sp$common), htmltools::htmlEscape(sp$latin), end))
}

sentence_case <- function(x) paste0(toupper(substr(x, 1, 1)), substring(x, 2))

# Estimates arrive from kelpbio rounded to 3 significant figures; show them as given.
number_text <- function(value) as.character(value)


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
