# The theme: an ocean and kelp palette (a dark teal navbar, teal-tinted greys,
# white cards) with one teal accent and a kelp gold for the navbar's highlights,
# expressed as Bootstrap Sass variables. Everything else in the app reads colours
# from var(--bs-*) rather than from this palette. The colours are sampled from
# photographs of a California kelp forest: the navbar from its shade, the teal
# from the water among the fronds, the info blue from the open water beyond,
# and the gold from sunlit kelp.
#
# Teal is Bootstrap's primary: primary and soft buttons, links, the active tab
# and pill, checked radios and progress bars. Gold marks the open step, the
# steps done and the kelp drawing in the navbar only. Green, amber and red are
# kept for status (ready, warning, error), always with an icon and text.

app_palette <- list(
  bg = "#f4f6f7", fg = "#1b2a30", card = "#ffffff",
  primary = "#2c606e", muted = "#eef2f2", muted_fg = "#55625f",
  accent = "#e3edef", accent_fg = "#1f4a55", accent_border = "#bdd3d8",
  secondary_fg = "#2a3a3e",
  # info is the open water's blue: informational notes and data coverage.
  info = "#346a9e", info_bg = "#e7eff7", info_border = "#c3d5e8", info_fg = "#214c75",
  # input: the border of inputs, selects and radios, at 3:1 contrast on white (WCAG 1.4.11).
  border = "#d8e0e1", input = "#86948f", ring = "#387777", ring_rgb = "56, 119, 119",
  # The navbar: white text on the forest's shade, the open step and the kelp drawing in gold.
  navbar_bg = "#1b2d33", navbar_border = "#142227", navbar_fg = "rgba(255, 255, 255, 0.75)",
  navbar_hover = "#ffffff", navbar_active = "#bfb755", navbar_brand = "#ffffff"
)

app_status_colours <- list(
  success = "#187c49", success_muted = "#daf7e3",
  # warning_fg keeps small warning text (badges, notices) at WCAG AA contrast on
  # warning_muted and white.
  warning = "#a76100", warning_fg = "#8a5000", warning_muted = "#fff0cc", warning_border = "#f3d68f",
  danger = "#c62a1f"
)

app_theme <- function() {
  col <- c(app_palette, app_status_colours)
  bslib::bs_theme(
    version = 5,
    bg = col$bg,
    fg = col$fg,
    primary = col$primary,
    secondary = col$muted_fg,
    success = col$success,
    warning = col$warning,
    danger = col$danger,
    info = col$info,
    light = "#ffffff",
    base_font = bslib::font_collection(bslib::font_google("Inter", wght = "400..700", local = FALSE), "system-ui", "sans-serif"),
    code_font = bslib::font_collection("ui-monospace", "SFMono-Regular", "Menlo", "Consolas", "monospace"),
    "font-size-base" = "0.875rem",
    "line-height-base" = 1.5,
    "headings-font-weight" = 600,
    "body-secondary-color" = col$muted_fg,
    "body-tertiary-bg" = col$muted,
    "border-color" = col$border,
    "border-radius" = "0.5rem",
    "border-radius-sm" = "0.375rem",
    "border-radius-lg" = "0.625rem",
    "border-radius-xl" = "0.875rem",
    "box-shadow-sm" = sprintf("0 0 0 1px %s, 0 1px 3px rgba(14, 26, 32, 0.08)", col$border),
    "link-color" = col$primary,
    "link-decoration" = "none",
    "link-hover-decoration" = "underline",
    "code-color" = col$fg,
    # Subtle colour pairs used by badges and notices
    "primary-bg-subtle" = col$accent,
    "primary-border-subtle" = col$accent_border,
    "primary-text-emphasis" = col$accent_fg,
    "secondary-bg-subtle" = col$muted,
    "secondary-text-emphasis" = col$secondary_fg,
    "info-bg-subtle" = col$info_bg,
    "info-border-subtle" = col$info_border,
    "info-text-emphasis" = col$info_fg,
    "success-bg-subtle" = col$success_muted,
    "success-text-emphasis" = col$success,
    "warning-bg-subtle" = col$warning_muted,
    "warning-border-subtle" = col$warning_border,
    "warning-text-emphasis" = col$warning_fg,
    # Cards
    "card-bg" = col$card,
    "card-border-radius" = "0.875rem",
    "card-spacer-y" = "1.5rem",
    "card-spacer-x" = "1.5rem",
    "card-cap-bg" = "transparent",
    # Buttons and inputs
    "btn-font-weight" = 500,
    "btn-padding-y" = "0.4375rem",
    "btn-padding-x" = "0.875rem",
    "btn-hover-bg-shade-amount" = "8%",
    "btn-active-bg-shade-amount" = "12%",
    "input-btn-font-size" = "0.875rem",
    "input-bg" = col$card,
    "input-border-color" = col$input,
    "form-check-input-border" = sprintf("1px solid %s", col$input),
    "input-focus-border-color" = col$ring,
    "input-focus-box-shadow" = sprintf("0 0 0 3px rgba(%s, 0.25)", col$ring_rgb),
    "form-label-font-weight" = 500,
    "badge-font-size" = "0.75rem",
    "badge-font-weight" = 500,
    "badge-padding-y" = "0.25rem",
    "badge-padding-x" = "0.5rem",
    "badge-border-radius" = "0.375rem",
    "progress-bg" = col$muted,
    "progress-height" = "0.5rem",
    # Navigation: a dark teal navbar with white text, its colours set directly
    # (the navbar keeps the light colour mode, so dropdowns and badges in it
    # keep the page's colours). The toggler's icon is drawn in the icon colour.
    "navbar-bg" = col$navbar_bg,
    # The underlined step tabs get extra bottom padding equal to the navbar's,
    # so equal top padding keeps their labels on the navbar's centre line.
    "navbar-padding-y" = "0.75rem",
    "nav-link-padding-y" = "0.75rem",
    # Narrower than Bootstrap's 1rem, so the Data step's seven sheet tabs fit
    # on one row at laptop widths.
    "nav-link-padding-x" = "0.75rem",
    "navbar-light-color" = col$navbar_fg,
    "navbar-light-hover-color" = col$navbar_hover,
    "navbar-light-active-color" = col$navbar_active,
    "navbar-light-brand-color" = col$navbar_brand,
    "navbar-light-brand-hover-color" = col$navbar_brand,
    "navbar-light-icon-color" = col$navbar_fg,
    "navbar-light-toggler-border-color" = "rgba(255, 255, 255, 0.25)",
    "nav-link-font-weight" = 500,
    "nav-link-color" = col$fg,
    "nav-link-hover-color" = col$fg,
    "nav-underline-gap" = "1.25rem",
    "nav-underline-link-active-color" = col$primary,
    # Folder tabs (the Data step's sheets): the open tab is white, joined to its
    # white sheet.
    "nav-tabs-border-color" = col$border,
    "nav-tabs-link-active-bg" = col$card,
    "nav-tabs-link-active-color" = col$primary,
    "nav-tabs-link-active-border-color" = paste(col$border, col$border, col$card),
    "nav-pills-link-active-bg" = col$accent,
    "nav-pills-link-active-color" = col$accent_fg,
    # Popovers (help text)
    "popover-max-width" = "20rem",
    "popover-header-bg" = col$card,
    "popover-header-font-size" = "0.875rem",
    "popover-body-padding-y" = "0.75rem",
    "popover-border-color" = col$border,
    # Accordion
    "accordion-bg" = col$card,
    "accordion-border-color" = col$border,
    "accordion-button-active-bg" = col$card,
    "accordion-button-active-color" = col$fg,
    "accordion-button-padding-y" = "0.875rem",
    "accordion-button-padding-x" = "1rem",
    "accordion-body-padding-x" = "1rem",
    # Tables
    "table-cell-padding-y" = "0.75rem",
    "table-cell-padding-x" = "0.75rem",
    "table-border-color" = col$border,
    "table-th-font-weight" = 500,
    "table-bg" = "transparent"
  ) |>
    # The one rule on a Bootstrap class: Bootstrap has no variable for a
    # navbar border, and the navbar needs a hairline to stand apart from the
    # page. The selector matches bslib's own, which removes it.
    bslib::bs_add_rules(sprintf(".bslib-page-navbar .navbar { border-bottom: var(--bs-border-width) solid %s; }", col$navbar_border))
}

# reactable styles from the theme's CSS variables.
app_table_theme <- reactable::reactableTheme(
  color = "var(--bs-body-color)",
  backgroundColor = "var(--bs-card-bg, var(--bs-body-bg))",
  borderColor = "var(--bs-border-color)",
  highlightColor = "var(--bs-tertiary-bg)",
  cellPadding = "0.5rem 0.75rem",
  style = list(fontFamily = "inherit", fontSize = "0.875rem"),
  headerStyle = list(
    background = "var(--bs-tertiary-bg)", color = "var(--bs-secondary-color)", fontWeight = 500,
    borderBottomColor = "var(--bs-border-color)"
  ),
  paginationStyle = list(color = "var(--bs-secondary-color)", borderTopColor = "var(--bs-border-color)"),
  pageButtonStyle = list(border = "1px solid var(--bs-border-color)", borderRadius = "0.375rem", margin = "0 0.125rem"),
  pageButtonHoverStyle = list(background = "var(--bs-primary-bg-subtle)")
)

# Row and cell colours for tables, also from the theme.
app_row_colour <- list(
  warning = list(background = "var(--bs-warning-bg-subtle)")
)

# The theme of every figure, on screen and exported (render_figure(),
# save_figure()): kelpbio's figures use the active ggplot2 theme, so the app
# gives them all a plain black and grey look that suits a report, and none of
# the app's colours.
figure_theme <- function() {
  ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
}
