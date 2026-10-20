# jarl-ignore-file unused_function: the app calls the mocks by built name through kelpbio_fn().
# Mock kelpbio functions -----------------------------------------------------------
#
# The app calls kelpbio for every model result. What kelpbio does not have yet
# is mocked here: the figures of a sheet's data on the Data step. Each mock is
# marked Planned (not in kelpbio; named and shaped following the package
# conventions). To switch one over once kelpbio has it: delete it here and add
# `@importFrom kelpbio <fun>` to R/namespace.R. Everything with a leading dot
# is mock plumbing and goes with the file, as do the ggplot2 imports below.

#' @importFrom ggplot2 ggplot aes facet_wrap expand_limits labs theme_bw
#' @noRd
NULL

.mock_stop <- function(...) stop(sprintf(...), call. = FALSE)

.mock_site_levels <- function(site) {
  site <- unique(as.character(site))
  site[order(suppressWarnings(as.numeric(gsub("\\D", "", site))), site)]
}

# The plants counted in each survey of a census: its size census rows, a partial
# plant counted as half.
.mock_census_counts <- function(survey, size_census) {
  half <- if ("partial" %in% names(size_census)) size_census$partial else FALSE
  counts <- tapply(ifelse(half, 0.5, 1), as.character(size_census$survey_id), sum)
  out <- unname(counts[as.character(survey$survey_id)])
  out[is.na(out)] <- 0
  out
}

# Planned; the names are a proposal, following kb_check_data_<model>_<species>().
# A figure of a sheet's data, to look at before fitting: weight by size, the
# size distribution, the dry:wet ratio and carbon fraction of the samples, the
# canopy cover fraction of each plot flown by drone by the plants counted, and
# the canopy area mapped at each site by year. All the data in one panel, or a
# panel per site or year (`facet`).
.mock_plot_data <- function(data, sheet, species, facet = NULL) {
  data <- as.data.frame(data)
  macro <- species == "macro"
  if (!is.null(facet) && !facet %in% c("site", "year")) .mock_stop("`facet` must be \"site\" or \"year\", or `NULL`.")
  if (!is.null(facet) && !facet %in% names(data)) .mock_stop("`data` has no %s to facet by.", facet)
  if ("site" %in% names(data)) data$site <- factor(data$site, levels = .mock_site_levels(data$site))
  if ("year" %in% names(data)) data$year <- factor(data$year)
  size_x <- if (macro) "fronds" else "diameter_mm"
  size_label <- if (macro) "Fronds" else "Diameter (mm)"
  points <- function(...) ggplot2::geom_point(..., alpha = 0.5, size = 1)
  histogram <- function() ggplot2::geom_histogram(bins = 30, fill = "grey70", colour = "grey40", linewidth = 0.2)
  p <- switch(sheet,
    size = ggplot(data, aes(.data[[size_x]])) + histogram() + labs(x = size_label, y = "Plants"),
    weight = ggplot(data, aes(.data[[size_x]], .data$weight_kg)) + points() + labs(x = size_label, y = "Wet weight (kg)"),
    wetdry = {
      data$ratio <- data$dry_mass_g / data$wet_mass_g
      ggplot(data, aes(.data$ratio)) + histogram() + labs(x = "Ratio of dry to wet mass", y = "Samples")
    },
    carbon = {
      data$fraction <- data$carbon_mass_ug / 1000 / data$sample_mass_mg
      ggplot(data, aes(.data$fraction)) + histogram() + labs(x = "Carbon fraction of dry mass", y = "Samples")
    },
    survey = {
      data <- data[!is.na(data$canopy_area_m2), ]
      data$cover <- data$canopy_area_m2 / data$plot_area_m2
      ggplot(data, aes(.data$plants, .data$cover)) + points() + labs(x = "Plants counted", y = "Canopy cover fraction")
    },
    canopy = ggplot(data, aes(.data$year, .data$canopy_area_m2)) + points() + labs(x = "Year", y = "Canopy area (m²)")
  )
  if (!is.null(facet)) p <- p + facet_wrap(facet)
  p + expand_limits(y = 0) + theme_bw()
}

# Planned (see .mock_plot_data()).
kb_plot_data_size_nereo <- function(data, facet = NULL) .mock_plot_data(data, "size", "nereo", facet)
kb_plot_data_size_macro <- function(data, facet = NULL) .mock_plot_data(data, "size", "macro", facet)
kb_plot_data_weight_nereo <- function(data, facet = NULL) .mock_plot_data(data, "weight", "nereo", facet)
kb_plot_data_weight_macro <- function(data, facet = NULL) .mock_plot_data(data, "weight", "macro", facet)
kb_plot_data_wetdry_nereo <- function(data, facet = NULL) .mock_plot_data(data, "wetdry", "nereo", facet)
kb_plot_data_wetdry_macro <- function(data, facet = NULL) .mock_plot_data(data, "wetdry", "macro", facet)
kb_plot_data_carbon_nereo <- function(data, facet = NULL) .mock_plot_data(data, "carbon", "nereo", facet)
kb_plot_data_carbon_macro <- function(data, facet = NULL) .mock_plot_data(data, "carbon", "macro", facet)
# Planned. Of the surveys flown by drone, as kb_check_data_survey() is for both
# species: the canopy cover fraction of each survey's plot by the plants
# counted, from its plants tally or, in a census, its rows of `size_census`.
kb_plot_data_survey <- function(data, size_census = NULL, facet = NULL) {
  if (!is.null(size_census)) data$plants <- .mock_census_counts(data, size_census)
  .mock_plot_data(data, "survey", "nereo", facet)
}
# Planned.
kb_plot_data_canopy <- function(data, facet = NULL) .mock_plot_data(data, "canopy", "nereo", facet)
