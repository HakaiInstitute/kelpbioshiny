# The theme: the Hakai palette of the kelpbio pkgdown site (Hakai red on a dark
# slate navbar) expressed as Bootstrap Sass variables. Everything else in the app
# reads colours from var(--bs-*) rather than from this palette.
#
# Hakai red is Bootstrap's primary: primary and soft buttons, links, the active
# tab and pill, checked radios and progress bars. Errors use the brighter danger
# red, always with an icon and text.

app_palette <- list(
  bg = "#f6f7f9", fg = "#1d2733", card = "#ffffff",
  primary = "#aa2025", muted = "#eef1f4", muted_fg = "#5a6672",
  accent = "#fbeced", accent_fg = "#7c161a", accent_border = "#efc3c5",
  secondary_fg = "#2c3e50",
  info = "#3b6a8c", info_bg = "#eaf0f5", info_border = "#cddae5", info_fg = "#24455e",
  # input: the border of inputs, selects and radios, at 3:1 contrast on white (WCAG 1.4.11).
  border = "#e1e5ea", input = "#8a949e", ring = "#c9484d", ring_rgb = "201, 72, 77",
  navbar_bg = "#2c3e50", navbar_fg = "rgba(255, 255, 255, 0.72)", navbar_hover = "#ffffff",
  navbar_active = "#ffffff", navbar_brand = "#ffffff"
)

app_status_colours <- list(
  success = "#187c49", success_muted = "#daf7e3",
  # warning_fg keeps small warning text (badges, notices) at WCAG AA contrast on
  # warning_muted and white.
  warning = "#a76100", warning_fg = "#8a5000", warning_muted = "#fff0cc", warning_border = "#f3d68f",
  danger = "#e7000b"
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
    "box-shadow-sm" = sprintf("0 0 0 1px %s, 0 1px 2px rgba(14, 26, 32, 0.05)", col$border),
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
    # Navigation: the navbar colours are set directly so the dark navbar keeps
    # readable text.
    "navbar-bg" = col$navbar_bg,
    # The underlined step tabs get extra bottom padding equal to the navbar's,
    # so equal top padding keeps their labels on the navbar's centre line.
    "navbar-padding-y" = "0.75rem",
    "nav-link-padding-y" = "0.75rem",
    "navbar-light-color" = col$navbar_fg,
    "navbar-light-hover-color" = col$navbar_hover,
    "navbar-light-active-color" = col$navbar_active,
    "navbar-light-brand-color" = col$navbar_brand,
    "navbar-light-brand-hover-color" = col$navbar_brand,
    "nav-link-font-weight" = 500,
    "nav-link-color" = col$fg,
    "nav-link-hover-color" = col$fg,
    "nav-underline-gap" = "1.25rem",
    "nav-underline-link-active-color" = col$primary,
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
  )
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
  warning = list(background = "var(--bs-warning-bg-subtle)"),
  info = list(background = "var(--bs-info-bg-subtle)")
)
