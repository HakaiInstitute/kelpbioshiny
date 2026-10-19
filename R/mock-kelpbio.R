# jarl-ignore-file unused_function: the app calls the mocks by built name through kelpbio_fn().
# Mock kelpbio functions -----------------------------------------------------------
#
# These mocks stand in for kelpbio functions. They return fake results from the
# JSON files in inst/extdata/, and draw their figures at run time from those fake
# values. Delete each mock as the real function lands in kelpbio, and this file
# once all of them have. The example data the app loads are in R/mock-example.R.
#
# The app calls these functions unqualified (kb_fit_weight_nereo(), tidy(),
# kb_converged()), as it will once they are imported from kelpbio. To switch a
# function over: delete its mock here, add kelpbio to Imports in DESCRIPTION
# (once), and add `@importFrom kelpbio <fun>` to R/namespace.R. Everything with a
# leading dot is mock plumbing and goes with the file, as do the fake-*.json
# files in inst/extdata/, the ggplot2 imports below and the kb_fit S3 method
# registration.
#
# Each mock is marked:
# - Exists: exported by kelpbio on its cover-calibration branch, which holds
#   the settled API but is not yet merged into main; the mock's signature,
#   checks and messages match it.
# - Planned: not in kelpbio yet; named and shaped following the package conventions.
#
# kelpbio's derived predictions (mean weight, survey and canopy biomass) carry
# their response as a posterior::rvar column of draws next to estimate, lower
# and upper. The mocks carry a list column of numeric draws in its place, so
# the app treats the response column as draws without reading it.

#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_pointrange facet_wrap
#' @importFrom ggplot2 expand_limits labs theme_bw theme element_text scale_x_log10
#' @noRd
NULL

# Fake values ------------------------------------------------------------------------

# A JSON file from inst/extdata, read once per R session.
.mock_json_cache <- new.env(parent = emptyenv())
.mock_json <- function(name) {
  if (is.null(.mock_json_cache[[name]])) {
    path <- system.file("extdata", paste0(name, ".json"), package = "kelpbioshiny", mustWork = TRUE)
    .mock_json_cache[[name]] <- jsonlite::fromJSON(path)
  }
  .mock_json_cache[[name]]
}

.mock_results <- function() .mock_json("fake-results")
.mock_predictions <- function() .mock_json("fake-predictions")
.mock_sensitivity <- function() .mock_json("fake-sensitivity")
.mock_totals <- function() .mock_json("fake-totals")
# As kb_model_describe() prints the descriptions of kelpbio's example fits.
.mock_describe <- function() .mock_json("fake-describe")

.mock_species_names <- c(nereo = "nereocystis", macro = "macrocystis")
.mock_model_of <- function(fit) fit$meta$mock_model
.mock_species_of <- function(fit) names(.mock_species_names)[.mock_species_names == fit$meta$species]

# A fit's terms: the names of its model's priors, as kelpbio names them.
.mock_terms <- function(model, species) names(get(sprintf("kb_priors_%s_%s", model, species))())

# Sampler diagnostics at the default 4 chains of 1000 draws with no thinning.
# The fake Nereocystis weight and size values come from inst/extdata.
.mock_diagnostics <- function(model, species) {
  terms <- .mock_terms(model, species)
  i <- seq_along(terms) - 1
  out <- data.frame(rhat = 1.001 + i * 0.001, ess_bulk = 1250 - i * 100)
  if (species == "nereo" && model == "weight") {
    conv <- .mock_results()$weight_convergence
    out <- data.frame(rhat = conv$rhat, ess_bulk = conv$ess_bulk)
  }
  if (model == "size") out <- data.frame(rhat = c(1.002, 1.004, 1.011, 1.026, 1.071), ess_bulk = c(1452, 1318, 688, 431, 182))
  data.frame(variable = terms, out, ess_tail = out$ess_bulk * 0.9)
}

.mock_estimates <- function(model, species) {
  terms <- .mock_terms(model, species)
  json <- .mock_results()[[paste0(model, "_tidy")]]
  if (species == "nereo" && identical(json$term, terms)) {
    return(json)
  }
  estimate <- ifelse(startsWith(terms, "sd_"), 0.2, 0.5) + 0.05 * (seq_along(terms) - 1)
  data.frame(term = terms, estimate = estimate, lower = estimate * 0.6, upper = estimate * 1.5)
}

# Simulated sampling time, in seconds, at 1000 draws per chain without thinning.
.mock_fit_seconds <- c(size = 4, weight = 5, wetdry = 3, carbon = 3, cover_calibration = 3)

# Runs code with a fixed seed and leaves the session's random numbers untouched.
.mock_with_seed <- function(seed, code) withr::with_seed(seed, code)

.mock_resolve_priors <- function(priors, defaults) {
  if (is.null(priors)) {
    return(defaults)
  }
  defaults[names(priors)] <- priors
  defaults
}

# The year of each row: its year column, or the year of its date.
.mock_year <- function(data) {
  if ("year" %in% names(data)) as.character(data$year) else format(data$date, "%Y")
}

# A fake kb_fit object, shaped like kelpbio's new_kb_fit(): draws (here the fake
# tidy() rows), diagnostics, data and meta. The mock_ meta entries are mock
# plumbing. Thinning makes the kept draws less alike, so R-hat moves towards 1
# and the effective sample size rises, as it would in a real refit.
.mock_new_fit <- function(model, species, data, priors, chains = 4L, niters = 1000L, nthin = 1L,
                          source = "user", reference = NULL) {
  defaults <- get(sprintf("kb_priors_%s_%s", model, species))()
  diagnostics <- .mock_diagnostics(model, species)
  ndraws <- chains * niters
  diagnostics$rhat <- 1 + (diagnostics$rhat - 1) / nthin
  diagnostics$ess_bulk <- round(diagnostics$ess_bulk * nthin * ndraws / 4000)
  diagnostics$ess_tail <- round(diagnostics$ess_tail * nthin * ndraws / 4000)
  priors <- .mock_resolve_priors(priors, defaults)
  data <- tibble::as_tibble(data)
  if (nrow(data) > 0 && !"year" %in% names(data) && "date" %in% names(data)) data$year <- .mock_year(data)
  # The cover calibration keeps the surveys with drone cover, with the mean
  # and SD of each one's biomass draws.
  if ("biomass_kg_m2" %in% names(data) && is.list(data$biomass_kg_m2)) {
    data <- data[!is.na(data$canopy_area_m2), ]
    draws <- data$biomass_kg_m2
    data$biomass_kg_m2 <- vapply(draws, mean, numeric(1))
    data$biomass_sd_kg_m2 <- vapply(draws, stats::sd, numeric(1))
    data <- data[setdiff(names(data), c("estimate", "lower", "upper"))]
  }
  structure(
    list(
      draws = .mock_estimates(model, species),
      diagnostics = list(summary = diagnostics, perc_divergent = 0),
      data = data,
      meta = list(
        species = .mock_species_names[[species]], priors = priors, nthin = nthin,
        terms = list(fixed = names(priors), random = character()),
        mock_model = model, mock_chains = chains, mock_ndraws = ndraws, mock_source = source, mock_reference = reference
      )
    ),
    class = c(sprintf("kb_fit_%s_%s", model, species), paste0("kb_fit_", model), "kb_fit")
  )
}

# Simulated sampling: writes the completed fraction to progress_dir over a few
# seconds, scaled by the iterations sampled, for kb_progress() to read from
# another process. With no progress_dir it returns at once. Each write goes to a
# temporary file first, so a reader never sees a partial file.
.mock_sample <- function(model, niters, nthin, progress_dir) {
  if (is.null(progress_dir)) {
    return(invisible())
  }
  seconds <- .mock_fit_seconds[[model]] * niters / 1000 * nthin
  steps <- 20
  file <- file.path(progress_dir, "mock-progress.rds")
  for (i in seq_len(steps)) {
    Sys.sleep(seconds / steps)
    saveRDS(i / steps, paste0(file, ".tmp"))
    file.rename(paste0(file, ".tmp"), file)
  }
  invisible()
}

# kelpbio's .chk_sampler_args().
.mock_chk_sampler <- function(prior_only, chains, niters, nthin, cores, seed, progress, progress_dir) {
  chk::chk_flag(prior_only)
  chk::chk_whole_number(chains)
  chk::chk_gt(chains, value = 0)
  chk::chk_whole_number(niters)
  chk::chk_gte(niters, value = 2)
  chk::chk_whole_number(nthin)
  chk::chk_gt(nthin, value = 0)
  if (!(is.character(progress) && length(progress) == 1 && progress %in% c("bar", "verbose", "none"))) {
    stop("`progress` must be one of \"bar\", \"verbose\", or \"none\".", call. = FALSE)
  }
  if (!is.null(progress_dir) && !dir.exists(progress_dir)) {
    stop("`progress_dir` must be a path to an existing directory, or `NULL`.", call. = FALSE)
  }
  if (!is.null(cores)) {
    chk::chk_whole_number(cores)
    chk::chk_gt(cores, value = 0)
  }
  if (!is.null(seed)) chk::chk_whole_number(seed)
}

# Prototype only: KELPBIOSHINY_MOCK_FAIL lists model names (e.g. "size,weight")
# whose fits fail after sampling, to show a failed fit.
.mock_maybe_fail <- function(model) {
  if (model %in% strsplit(Sys.getenv("KELPBIOSHINY_MOCK_FAIL"), ",", fixed = TRUE)[[1]]) {
    stop("Sampling failed: the simulated chains did not initialise (prototype failure).", call. = FALSE)
  }
}

# The shared body of the mock fits: kelpbio's sampler and data checks, simulated
# sampling, then the fake fit.
.mock_fit <- function(model, species, data, priors, prior_only, chains, niters, nthin, cores, seed, progress,
                      progress_dir) {
  progress <- progress[1]
  .mock_chk_sampler(prior_only, chains, niters, nthin, cores, seed, progress, progress_dir)
  get(sprintf("kb_check_data_%s_%s", model, species))(data, x_name = "`data`")
  .mock_sample(model, niters, nthin, progress_dir)
  .mock_maybe_fail(model)
  .mock_new_fit(model, species, data, priors, chains, niters, nthin)
}

# Data checks --------------------------------------------------------------------------
# Like kelpbio's: a check returns the data invisibly and aborts, naming the column,
# on a missing column, a wrong type, an out-of-range value or a missing value,
# and warns on implausible values. Messages are kelpbio's, as plain text.

.mock_xname <- function(x_name, col) paste0("Column `", col, "` of ", x_name)

.mock_stop <- function(...) stop(sprintf(...), call. = FALSE)
.mock_warn <- function(...) warning(sprintf(...), call. = FALSE)

# kelpbio's .chk_measure_columns().
.mock_chk_measures <- function(data, cols, x_name, count = FALSE, zero = FALSE) {
  for (col in cols) {
    nm <- .mock_xname(x_name, col)
    chk::chk_numeric(data[[col]], x_name = nm)
    chk::chk_not_any_na(data[[col]], x_name = nm)
    if (zero) chk::chk_gte(data[[col]], value = 0, x_name = nm) else chk::chk_gt(data[[col]], value = 0, x_name = nm)
    if (count) chk::chk_whole_numeric(data[[col]], x_name = nm)
  }
}

# kelpbio's .chk_group_columns().
.mock_chk_groups <- function(data, x_name, cols = c("site", "year")) {
  for (col in cols) {
    nm <- .mock_xname(x_name, col)
    chk::chk_character_or_factor(data[[col]], x_name = nm)
    chk::chk_not_any_na(data[[col]], x_name = nm)
  }
}

# kelpbio's .chk_dated_site(): the site, and a date whose year any year column
# agrees with.
.mock_chk_dated <- function(data, x_name) {
  .mock_chk_groups(data, x_name, "site")
  nm <- .mock_xname(x_name, "date")
  if (!inherits(data$date, "Date")) .mock_stop("%s must be a <Date>.", nm)
  if (anyNA(data$date)) .mock_stop("%s must not have missing values.", nm)
  if ("year" %in% names(data) && !all(as.character(data$year) == format(data$date, "%Y"), na.rm = TRUE)) {
    .mock_stop("%s must be the year of date.", .mock_xname(x_name, "year"))
  }
}

.mock_chk_columns <- function(data, columns, x_name) {
  chk::chk_data(data, x_name = x_name)
  chk::chk_superset(names(data), columns, x_name = x_name)
}

.mock_n <- function(n, one, many) if (n == 1) one else many

# kelpbio's warn_implausible_units(): a column whose median lies outside its
# plausible limits is probably in another unit.
.mock_units <- c(
  diameter_mm = "millimetres", weight_kg = "kilograms", survey_area_m2 = "square metres",
  wet_mass_g = "grams", dry_mass_g = "grams", plot_area_m2 = "square metres", site_area_m2 = "square metres",
  tide_height_m = "metres"
)
.mock_warn_units <- function(data, x_name) {
  limits <- list(
    diameter_mm = c(10, 200), weight_kg = c(-Inf, 100), survey_area_m2 = c(1, 5000),
    plot_area_m2 = c(1, 1e6), site_area_m2 = c(10, 1e9), tide_height_m = c(-1, 5),
    wet_mass_g = c(-Inf, 1000), dry_mass_g = c(-Inf, 1000)
  )
  for (col in intersect(names(limits), names(data))) {
    x <- suppressWarnings(as.numeric(data[[col]]))
    if (all(is.na(x))) next
    m <- stats::median(x, na.rm = TRUE)
    if (m < limits[[col]][1] || m > limits[[col]][2]) {
      .mock_warn(
        "%s has median %s, which is unusually %s for %s.", .mock_xname(x_name, col), signif(m, 3),
        if (m < limits[[col]][1]) "small" else "large", .mock_units[[col]]
      )
    }
  }
}

# kelpbio's warn_group_names().
.mock_warn_group_names <- function(data, x_name) {
  for (col in intersect(c("site", "year"), names(data))) {
    values <- unique(as.character(data[[col]]))
    bad <- values[grepl("[],[]", values)]
    if (length(bad) > 0) {
      .mock_warn(
        "%s has %s %s containing a comma or square bracket.", .mock_xname(x_name, col),
        .mock_n(length(bad), "value", "values"), paste(sprintf("\"%s\"", bad), collapse = ", ")
      )
    }
  }
}

# kelpbio's warn_survey_area().
.mock_warn_survey_area <- function(data, x_name) {
  if (!all(c("survey_area_m2", "plot_area_m2") %in% names(data))) {
    return()
  }
  n <- sum(data$survey_area_m2 > data$plot_area_m2, na.rm = TRUE)
  if (n > 0) {
    .mock_warn(
      "%d %s in %s %s survey_area_m2 above %s plot_area_m2.",
      n, .mock_n(n, "survey", "surveys"), x_name, .mock_n(n, "has a", "have"), .mock_n(n, "its", "their")
    )
  }
}

.mock_chk_wetdry <- function(data, x_name) {
  .mock_chk_columns(data, c("wet_mass_g", "dry_mass_g"), x_name)
  .mock_chk_measures(data, c("wet_mass_g", "dry_mass_g"), x_name)
  if (any(data$dry_mass_g >= data$wet_mass_g)) .mock_stop("%s must be less than wet_mass_g.", .mock_xname(x_name, "dry_mass_g"))
  ratio <- data$dry_mass_g / data$wet_mass_g
  n <- sum(ratio < 0.02 | ratio > 0.5)
  if (n > 0) {
    .mock_warn(
      "%d %s in %s %s outside 0.02 to 0.5, the plausible range for kelp tissue.",
      n, .mock_n(n, "sample", "samples"), x_name, .mock_n(n, "has a dry:wet mass ratio", "have dry:wet mass ratios")
    )
  }
  .mock_warn_units(data, x_name)
}

.mock_chk_carbon <- function(data, x_name) {
  .mock_chk_columns(data, c("sample_mass_mg", "carbon_mass_ug"), x_name)
  .mock_chk_measures(data, c("sample_mass_mg", "carbon_mass_ug"), x_name)
  fraction <- data$carbon_mass_ug / 1000 / data$sample_mass_mg
  if (any(fraction >= 1)) .mock_stop("%s must be less than the sample mass.", .mock_xname(x_name, "carbon_mass_ug"))
  n <- sum(fraction < 0.10 | fraction > 0.50)
  if (n > 0) {
    .mock_warn(
      "%d %s in %s %s outside 0.10 to 0.50, the plausible range for kelp tissue.",
      n, .mock_n(n, "sample", "samples"), x_name, .mock_n(n, "has a carbon fraction", "have carbon fractions")
    )
  }
}

# kelpbio's .chk_drone_columns(): the plot polygon, the canopy within it and the
# tide height at the flight, given together or left missing together.
.mock_chk_drone <- function(data, x_name) {
  cols <- c("plot_area_m2", "canopy_area_m2", "tide_height_m")
  absent <- setdiff(cols, names(data))
  if (length(absent) == length(cols)) {
    return(invisible(data))
  }
  if (length(absent) > 0) {
    .mock_stop("%s must have all of plot_area_m2, canopy_area_m2 and tide_height_m or none of them. Missing: %s.", x_name, paste(absent, collapse = ", "))
  }
  missing <- is.na(data[cols])
  partial <- rowSums(missing) %in% c(1, 2)
  if (any(partial)) {
    col <- cols[colSums(missing[partial, , drop = FALSE]) > 0][1]
    .mock_stop(
      "%s must not be missing where the other drone columns are given. Give plot_area_m2, canopy_area_m2 and tide_height_m together, or leave all three missing for a survey without drone cover.",
      .mock_xname(x_name, col)
    )
  }
  d <- data[!missing[, 1], cols, drop = FALSE]
  .mock_chk_measures(d, "plot_area_m2", x_name)
  .mock_chk_measures(d, "canopy_area_m2", x_name, zero = TRUE)
  if (any(d$canopy_area_m2 > d$plot_area_m2)) .mock_stop("%s must not exceed plot_area_m2.", .mock_xname(x_name, "canopy_area_m2"))
  chk::chk_numeric(d$tide_height_m, x_name = .mock_xname(x_name, "tide_height_m"))
}

.mock_survey_key <- function(x) paste(x$site, x$plot_id, format(x$date), sep = ":")

# kelpbio's .chk_plants_data(): one row per counted plant, with its survey and size.
.mock_chk_plants <- function(data, size, x_name) {
  .mock_chk_columns(data, c("site", "plot_id", "date", size), x_name)
  .mock_chk_groups(data, x_name, c("site", "plot_id"))
  nm <- .mock_xname(x_name, "date")
  if (!inherits(data$date, "Date")) .mock_stop("%s must be a <Date>.", nm)
  if (anyNA(data$date)) .mock_stop("%s must not have missing values.", nm)
  if (size == "fronds") {
    nm <- .mock_xname(x_name, "fronds")
    chk::chk_numeric(data$fronds, x_name = nm)
    chk::chk_not_any_na(data$fronds, x_name = nm)
    if (any(data$fronds < 1)) .mock_stop("%s must be at least 1: leave out plants with no frond reaching 1 m.", nm)
    chk::chk_whole_numeric(data$fronds, x_name = nm)
  } else {
    .mock_chk_measures(data, size, x_name)
  }
  if ("partial" %in% names(data)) {
    nm <- .mock_xname(x_name, "partial")
    chk::chk_logical(data$partial, x_name = nm)
    if (anyNA(data$partial)) .mock_stop("%s must not have missing values.", nm)
  }
  .mock_warn_units(data, x_name)
  .mock_warn_group_names(data, x_name)
}

# kelpbio's .chk_survey_data(): with `plants`, the plant table and its join too.
.mock_chk_survey <- function(data, plants, x_name, plants_name) {
  required <- c("site", "plot_id", "date", "survey_area_m2", if (is.null(plants)) "plants")
  .mock_chk_columns(data, required, x_name)
  .mock_chk_dated(data, x_name)
  .mock_chk_groups(data, x_name, "plot_id")
  .mock_chk_measures(data, "survey_area_m2", x_name)
  if (is.null(plants)) {
    .mock_chk_measures(data, "plants", x_name, count = TRUE, zero = TRUE)
  } else if ("plants" %in% names(data)) {
    .mock_stop("%s must not have a plants column when plant rows are given. The counts come from the plant rows.", x_name)
  }
  key <- .mock_survey_key(data)
  if (anyDuplicated(key)) .mock_stop("%s must have one row per site, plot_id, and date.", x_name)
  .mock_chk_drone(data, x_name)
  .mock_warn_units(data, x_name)
  .mock_warn_group_names(data, x_name)
  .mock_warn_survey_area(data, x_name)
  if (!is.null(plants)) {
    size <- intersect(c("diameter_mm", "fronds"), names(plants))
    if (length(size) != 1) .mock_stop("%s must have one of diameter_mm (Nereocystis) or fronds (Macrocystis).", plants_name)
    .mock_chk_plants(plants, size, plants_name)
    unmatched <- unique(.mock_survey_key(plants)[!.mock_survey_key(plants) %in% key])
    if (length(unmatched) > 0) {
      .mock_stop(
        "%d %s in %s %s no survey. Plants join to their survey by site, plot_id, and date. Unmatched: %s.",
        length(unmatched), .mock_n(length(unmatched), "plant", "plants"), plants_name,
        .mock_n(length(unmatched), "matches", "match"), paste(sprintf("\"%s\"", utils::head(unmatched, 5)), collapse = ", ")
      )
    }
  }
}

# Exists.
kb_check_data_size_nereo <- function(data, x_name = chk::deparse_backtick_chk(substitute(data))) {
  .mock_chk_columns(data, c("diameter_mm", "site", "date"), x_name)
  .mock_chk_measures(data, "diameter_mm", x_name)
  .mock_chk_dated(data, x_name)
  if ("plot_id" %in% names(data)) .mock_chk_groups(data, x_name, "plot_id")
  .mock_warn_units(data, x_name)
  .mock_warn_group_names(data, x_name)
  invisible(data)
}
# Exists.
kb_check_data_weight_nereo <- function(data, x_name = chk::deparse_backtick_chk(substitute(data))) {
  .mock_chk_columns(data, c("diameter_mm", "weight_kg", "site", "date"), x_name)
  .mock_chk_measures(data, c("diameter_mm", "weight_kg"), x_name)
  .mock_chk_dated(data, x_name)
  .mock_warn_units(data, x_name)
  .mock_warn_group_names(data, x_name)
  invisible(data)
}
# Exists.
kb_check_data_weight_macro <- function(data, x_name = chk::deparse_backtick_chk(substitute(data))) {
  .mock_chk_columns(data, c("fronds", "weight_kg", "site", "date"), x_name)
  .mock_chk_measures(data, "fronds", x_name, count = TRUE)
  .mock_chk_measures(data, "weight_kg", x_name)
  .mock_chk_dated(data, x_name)
  .mock_warn_units(data, x_name)
  .mock_warn_group_names(data, x_name)
  invisible(data)
}
# Exists. One row per counted plant of a census.
kb_check_data_plants_nereo <- function(data, x_name = chk::deparse_backtick_chk(substitute(data))) {
  .mock_chk_plants(data, "diameter_mm", x_name)
  invisible(data)
}
# Exists.
kb_check_data_plants_macro <- function(data, x_name = chk::deparse_backtick_chk(substitute(data))) {
  .mock_chk_plants(data, "fronds", x_name)
  invisible(data)
}
# Exists. One row per in situ survey of a plot; `plants` is the plant table of a
# census, checked with its join to the surveys when supplied.
kb_check_data_survey <- function(data, plants = NULL, x_name = chk::deparse_backtick_chk(substitute(data))) {
  plants_name <- chk::deparse_backtick_chk(substitute(plants))
  .mock_chk_survey(data, plants, x_name, plants_name)
  invisible(data)
}
# Exists. One row per tissue sample; no site or year.
kb_check_data_wetdry_nereo <- function(data, x_name = chk::deparse_backtick_chk(substitute(data))) {
  .mock_chk_wetdry(data, x_name)
  invisible(data)
}
# Exists.
kb_check_data_wetdry_macro <- function(data, x_name = chk::deparse_backtick_chk(substitute(data))) {
  .mock_chk_wetdry(data, x_name)
  invisible(data)
}
# Exists. One row per dried tissue sample; no site or year.
kb_check_data_carbon_nereo <- function(data, x_name = chk::deparse_backtick_chk(substitute(data))) {
  .mock_chk_carbon(data, x_name)
  invisible(data)
}
# Exists.
kb_check_data_carbon_macro <- function(data, x_name = chk::deparse_backtick_chk(substitute(data))) {
  .mock_chk_carbon(data, x_name)
  invisible(data)
}

.mock_chk_cover_calibration <- function(data, x_name) {
  chk::chk_data(data, x_name = x_name)
  hint <- "Use the output of kb_predict_biomass_survey() with its defaults: wet biomass per m\u00b2, one row per survey."
  if (!"biomass_kg_m2" %in% names(data)) .mock_stop("%s must have a biomass_kg_m2 column. %s", x_name, hint)
  if (!is.list(data$biomass_kg_m2)) .mock_stop("%s must be a column of draws. %s", .mock_xname(x_name, "biomass_kg_m2"), hint)
  missing <- setdiff(c("site", "year", "plot_area_m2", "canopy_area_m2", "tide_height_m"), names(data))
  if (length(missing) > 0) .mock_stop("%s must have the columns %s. %s", x_name, paste(missing, collapse = ", "), hint)
  .mock_chk_groups(data, x_name)
  .mock_chk_drone(data, x_name)
}
# Exists. The unsummed output of kb_predict_biomass_survey(): one row per survey.
kb_check_data_cover_calibration_nereo <- function(data, x_name = chk::deparse_backtick_chk(substitute(data))) {
  .mock_chk_cover_calibration(data, x_name)
  invisible(data)
}
# Exists.
kb_check_data_cover_calibration_macro <- function(data, x_name = chk::deparse_backtick_chk(substitute(data))) {
  .mock_chk_cover_calibration(data, x_name)
  invisible(data)
}
# Planned; the name is a proposal. kelpbio checks the drone-mapped canopy of
# each site-year (site, year, canopy_area_m2, tide_height_m and an optional
# site_area_m2) only inside kb_predict_biomass_canopy(), with its internal
# .chk_site_surveys(); exporting it would let the canopy be checked on upload.
# This mock is that check, with the unit and site name warnings of the others.
kb_check_data_canopy <- function(data, x_name = chk::deparse_backtick_chk(substitute(data))) {
  .mock_chk_columns(data, c("canopy_area_m2", "tide_height_m", "site", "year"), x_name)
  .mock_chk_measures(data, "canopy_area_m2", x_name, zero = TRUE)
  nm <- .mock_xname(x_name, "tide_height_m")
  chk::chk_numeric(data$tide_height_m, x_name = nm)
  chk::chk_not_any_na(data$tide_height_m, x_name = nm)
  .mock_chk_groups(data, x_name)
  if ("site_area_m2" %in% names(data)) {
    .mock_chk_measures(data, "site_area_m2", x_name)
    if (any(data$canopy_area_m2 > data$site_area_m2)) .mock_stop("%s must not exceed site_area_m2.", .mock_xname(x_name, "canopy_area_m2"))
  }
  .mock_warn_units(data, x_name)
  .mock_warn_group_names(data, x_name)
  invisible(data)
}

# Priors -----------------------------------------------------------------------------
# Each entry is named after the parameter it applies to, as tidy() and
# kb_sensitivity() name it.

# Exists.
kb_prior_normal <- function(mean = 0, sd = 1) {
  chk::chk_number(mean)
  chk::chk_number(sd)
  chk::chk_gt(sd, value = 0)
  structure(list(mean = mean, sd = sd), class = c("kb_prior_normal", "kb_prior"))
}

# Exists.
kb_prior_lognormal <- function(meanlog = 0, sdlog = 1) {
  chk::chk_number(meanlog)
  chk::chk_number(sdlog)
  chk::chk_gt(sdlog, value = 0)
  structure(list(meanlog = meanlog, sdlog = sdlog), class = c("kb_prior_lognormal", "kb_prior"))
}

# Exists.
kb_prior_exponential <- function(rate = 1) {
  chk::chk_number(rate)
  chk::chk_gt(rate, value = 0)
  structure(list(rate = rate), class = c("kb_prior_exponential", "kb_prior"))
}

.mock_sd_priors <- function(site_year = TRUE) {
  priors <- list(sd_site = kb_prior_exponential(rate = 1), sd_year = kb_prior_exponential(rate = 1))
  if (site_year) priors$sd_site_year <- kb_prior_exponential(rate = 1)
  priors
}

# Exists.
kb_priors_size_nereo <- function() {
  c(list(intercept = kb_prior_normal(mean = 0, sd = 2), shape = kb_prior_exponential(rate = 0.1)), .mock_sd_priors())
}
# Exists.
kb_priors_weight_nereo <- function() {
  c(
    list(
      intercept = kb_prior_normal(mean = 0, sd = 2),
      diameter_power = kb_prior_normal(mean = 2, sd = 1),
      weight_floor = kb_prior_normal(mean = 0, sd = 0.5)
    ),
    .mock_sd_priors(),
    list(sd_residual = kb_prior_exponential(rate = 1))
  )
}
# Exists.
kb_priors_weight_macro <- function() {
  c(
    list(
      intercept = kb_prior_normal(mean = 0, sd = 2),
      fronds_slope = kb_prior_normal(mean = 1, sd = 0.5),
      shape = kb_prior_exponential(rate = 0.1)
    ),
    .mock_sd_priors()
  )
}
# Exists.
kb_priors_wetdry_nereo <- function() {
  list(intercept = kb_prior_normal(mean = 0, sd = 2), precision = kb_prior_exponential(rate = 0.01))
}
# Exists.
kb_priors_wetdry_macro <- kb_priors_wetdry_nereo
# Exists.
kb_priors_carbon_nereo <- function() {
  list(intercept = kb_prior_normal(mean = -0.8, sd = 0.3), precision = kb_prior_exponential(rate = 0.001))
}
# Exists.
kb_priors_carbon_macro <- kb_priors_carbon_nereo
# Exists.
kb_priors_cover_calibration_nereo <- function() {
  c(
    list(
      cover_slope = kb_prior_lognormal(meanlog = 2, sdlog = 1),
      biomass_floor = kb_prior_normal(mean = 0, sd = 0.1),
      tide_height_slope = kb_prior_normal(mean = 0.276, sd = 0.04),
      sd_residual = kb_prior_exponential(rate = 2),
      cv_residual = kb_prior_normal(mean = 0, sd = 1)
    ),
    .mock_sd_priors(site_year = FALSE)
  )
}
# Exists.
kb_priors_cover_calibration_macro <- function() {
  priors <- kb_priors_cover_calibration_nereo()
  priors$biomass_floor <- kb_prior_normal(mean = 0.4, sd = 0.3)
  priors$tide_height_slope <- kb_prior_normal(mean = 0.227, sd = 0.03)
  priors
}

# Fitting -----------------------------------------------------------------------------------
# The mock fits check their arguments and data as kelpbio does, then sample for a
# few seconds when given a progress_dir.

# Exists.
kb_fit_size_nereo <- function(data, priors = NULL, ..., prior_only = FALSE, chains = 4L, niters = 1000L,
                              nthin = 1L, cores = NULL, seed = NULL, progress = c("bar", "verbose", "none"),
                              progress_dir = NULL) {
  .mock_fit("size", "nereo", data, priors, prior_only, chains, niters, nthin, cores, seed, progress, progress_dir)
}
# Exists.
kb_fit_weight_nereo <- function(data, priors = NULL, form = c("packard_floor", "power"), ..., prior_only = FALSE,
                                chains = 4L, niters = 1000L, nthin = 1L, cores = NULL, seed = NULL,
                                progress = c("bar", "verbose", "none"), progress_dir = NULL) {
  .mock_fit("weight", "nereo", data, priors, prior_only, chains, niters, nthin, cores, seed, progress, progress_dir)
}
# Exists.
kb_fit_weight_macro <- function(data, priors = NULL, ..., prior_only = FALSE, chains = 4L, niters = 1000L,
                                nthin = 1L, cores = NULL, seed = NULL, progress = c("bar", "verbose", "none"),
                                progress_dir = NULL) {
  .mock_fit("weight", "macro", data, priors, prior_only, chains, niters, nthin, cores, seed, progress, progress_dir)
}
# Exists.
kb_fit_wetdry_nereo <- function(data, priors = NULL, ..., prior_only = FALSE, chains = 4L, niters = 1000L,
                                nthin = 1L, cores = NULL, seed = NULL, progress = c("bar", "verbose", "none"),
                                progress_dir = NULL) {
  .mock_fit("wetdry", "nereo", data, priors, prior_only, chains, niters, nthin, cores, seed, progress, progress_dir)
}
# Exists.
kb_fit_wetdry_macro <- function(data, priors = NULL, ..., prior_only = FALSE, chains = 4L, niters = 1000L,
                                nthin = 1L, cores = NULL, seed = NULL, progress = c("bar", "verbose", "none"),
                                progress_dir = NULL) {
  .mock_fit("wetdry", "macro", data, priors, prior_only, chains, niters, nthin, cores, seed, progress, progress_dir)
}
# Exists.
kb_fit_carbon_nereo <- function(data, priors = NULL, ..., prior_only = FALSE, chains = 4L, niters = 1000L,
                                nthin = 1L, cores = NULL, seed = NULL, progress = c("bar", "verbose", "none"),
                                progress_dir = NULL) {
  .mock_fit("carbon", "nereo", data, priors, prior_only, chains, niters, nthin, cores, seed, progress, progress_dir)
}
# Exists.
kb_fit_carbon_macro <- function(data, priors = NULL, ..., prior_only = FALSE, chains = 4L, niters = 1000L,
                                nthin = 1L, cores = NULL, seed = NULL, progress = c("bar", "verbose", "none"),
                                progress_dir = NULL) {
  .mock_fit("carbon", "macro", data, priors, prior_only, chains, niters, nthin, cores, seed, progress, progress_dir)
}

# The surveys with drone cover are the observations, each with the mean and SD
# of its biomass draws; the others are left out, and a message says how many.
.mock_fit_cover_calibration <- function(species, data, priors, prior_only, chains, niters, nthin, cores, seed,
                                        progress, progress_dir) {
  get(sprintf("kb_check_data_cover_calibration_%s", species))(data, x_name = "`data`")
  .mock_chk_sampler(prior_only, chains, niters, nthin, cores, seed, progress[1], progress_dir)
  drone <- !is.na(data$canopy_area_m2)
  if (any(!drone)) {
    n <- sum(!drone)
    message(sprintf("%d %s without drone cover %s left out.", n, .mock_n(n, "survey", "surveys"), .mock_n(n, "is", "are")))
  }
  if (!any(drone)) stop("`data` has no surveys with drone cover.", call. = FALSE)
  .mock_sample("cover_calibration", niters, nthin, progress_dir)
  .mock_maybe_fail("cover_calibration")
  .mock_new_fit("cover_calibration", species, data, priors, chains, niters, nthin)
}

# Exists. `data` is the unsummed output of kb_predict_biomass_survey(): each
# survey's wet biomass per m2 as draws, with its drone columns on its row.
kb_fit_cover_calibration_nereo <- function(data, priors = NULL, ..., prior_only = FALSE, chains = 4L, niters = 1000L,
                                           nthin = 1L, cores = NULL, seed = NULL,
                                           progress = c("bar", "verbose", "none"), progress_dir = NULL) {
  .mock_fit_cover_calibration("nereo", data, priors, prior_only, chains, niters, nthin, cores, seed, progress, progress_dir)
}
# Exists.
kb_fit_cover_calibration_macro <- function(data, priors = NULL, ..., prior_only = FALSE, chains = 4L, niters = 1000L,
                                           nthin = 1L, cores = NULL, seed = NULL,
                                           progress = c("bar", "verbose", "none"), progress_dir = NULL) {
  .mock_fit_cover_calibration("macro", data, priors, prior_only, chains, niters, nthin, cores, seed, progress, progress_dir)
}

# Exists. The completed fraction of a fit, kb_predict_mean_weight() or
# kb_predict_biomass_survey() writing to progress_dir.
kb_progress <- function(progress_dir) {
  chk::chk_string(progress_dir)
  file <- file.path(progress_dir, "mock-progress.rds")
  if (!file.exists(file)) {
    return(0)
  }
  tryCatch(readRDS(file), error = function(e) 0)
}

# Pre-fit models --------------------------------------------------------------------------
# Planned. A pre-fit accessor returns a model fitted in advance to a reference
# dataset, chosen with `reference`: "coastwide" (data compiled from surveys along
# the coast) or "hakai" (Hakai Institute survey data). The real pre-fit fits will
# live in kelpbiodata, as data objects these accessors return. They were thinned
# when built, so they converged.

.mock_prefit <- function(model, species, reference) {
  .mock_new_fit(model, species, data.frame(), NULL, nthin = 10L, source = "prefit", reference = reference)
}

# Planned.
kb_prefit_size_nereo <- function(reference = "hakai") {
  reference <- rlang::arg_match(reference)
  .mock_prefit("size", "nereo", reference)
}
# Planned.
kb_prefit_weight_nereo <- function(reference = c("coastwide", "hakai")) {
  reference <- rlang::arg_match(reference)
  .mock_prefit("weight", "nereo", reference)
}
# Planned.
kb_prefit_weight_macro <- function(reference = c("coastwide", "hakai")) {
  reference <- rlang::arg_match(reference)
  .mock_prefit("weight", "macro", reference)
}
# Planned.
kb_prefit_wetdry_nereo <- function(reference = "hakai") {
  reference <- rlang::arg_match(reference)
  .mock_prefit("wetdry", "nereo", reference)
}
# Planned.
kb_prefit_wetdry_macro <- function(reference = "hakai") {
  reference <- rlang::arg_match(reference)
  .mock_prefit("wetdry", "macro", reference)
}
# Planned.
kb_prefit_carbon_nereo <- function(reference = "hakai") {
  reference <- rlang::arg_match(reference)
  .mock_prefit("carbon", "nereo", reference)
}
# Planned.
kb_prefit_carbon_macro <- function(reference = "hakai") {
  reference <- rlang::arg_match(reference)
  .mock_prefit("carbon", "macro", reference)
}

# Summaries and convergence -------------------------------------------------------------

# Exists: kelpbio re-exports the generic from generics and provides the kb_fit
# method. The app imports the generic (R/namespace.R) and the mock registers the
# method.
#' @exportS3Method generics::tidy
#' @noRd
tidy.kb_fit <- function(x, ..., conf_level = 0.95, estimate = stats::median, sig_fig = 3,
                        include_random_effects = FALSE) {
  rows <- x$draws
  rows[c("estimate", "lower", "upper")] <- lapply(rows[c("estimate", "lower", "upper")], signif, sig_fig)
  tibble::as_tibble(rows)
}

# Exists. Every R-hat below `rhat`, every bulk and tail effective sample size at
# least `ess` times the number of chains, and divergences at or below
# max_perc_divergent percent.
kb_converged <- function(fit, ..., rhat = 1.01, ess = 100, max_perc_divergent = 0.2) {
  s <- fit$diagnostics$summary
  min_ess <- ess * fit$meta$mock_chains
  all(s$rhat < rhat) && all(s$ess_bulk >= min_ess) && all(s$ess_tail >= min_ess) &&
    fit$diagnostics$perc_divergent <= max_perc_divergent
}

# Planned; the name is a proposal. Per-term convergence: R-hat, bulk and tail
# ESS, and whether each term meets kb_converged()'s thresholds. kelpbio has the
# values in summary(fit)$coefficients but not the per-term flag.
kb_convergence <- function(fit, ..., rhat = 1.01, ess = 100) {
  s <- fit$diagnostics$summary
  min_ess <- ess * fit$meta$mock_chains
  # Worked out before tibble(), where the column names would mask the thresholds.
  ok <- s$rhat < rhat & s$ess_bulk >= min_ess & s$ess_tail >= min_ess
  tibble::tibble(term = s$variable, rhat = round(s$rhat, 3), ess_bulk = round(s$ess_bulk), ess_tail = round(s$ess_tail), converged = ok)
}

# Exists. Prints the description of the model as fitted, with its priors, and
# returns the lines invisibly.
kb_model_describe <- function(fit, prose = FALSE) {
  lines <- strsplit(.mock_describe()[[.mock_model_of(fit)]][[.mock_species_of(fit)]], "\n", fixed = TRUE)[[1]]
  cat(lines, sep = "\n")
  invisible(lines)
}

# Predictions ---------------------------------------------------------------------------
# A kb_predictions object: a tibble of the new_data columns and estimate, lower
# and upper, with the predictor, grouping columns, response and whether it is a
# curve as attributes, for kb_plot_predictions().

.mock_new_predictions <- function(rows, group_vars, response, predictor = NULL, curve = FALSE) {
  structure(
    tibble::as_tibble(rows),
    class = c("kb_predictions", "tbl_df", "tbl", "data.frame"),
    kb_predictor = predictor, kb_group_vars = group_vars, kb_response = response, kb_curve = curve
  )
}

# The fake estimates of a fit at a grouping ("population", "site", "year" or
# "site_year"), from fake-predictions.json; a curve model's are its value at
# the reference predictor value (50 mm, a cover of 0.5).
.mock_group_rows <- function(fit, grouping) {
  rows <- .mock_predictions()[[.mock_model_of(fit)]][[grouping]]
  if (is.null(rows)) .mock_stop("The %s model has no %s effect.", .mock_model_of(fit), sub("_", ":", grouping))
  rows
}

.mock_grouping <- function(group_vars) {
  key <- paste(sort(group_vars), collapse = " ")
  c("population", "site", "year", "site_year")[match(key, c("", "site", "year", "site year"))]
}

# The fake estimates for each row of new_data, from its site and year columns.
.mock_rows_estimates <- function(fit, new_data) {
  group_vars <- intersect(c("site", "year"), names(new_data))
  rows <- .mock_group_rows(fit, .mock_grouping(group_vars))
  if (length(group_vars) == 0) {
    return(rows[rep(1, nrow(new_data)), c("estimate", "lower", "upper")])
  }
  key <- function(x) do.call(paste, lapply(group_vars, function(v) as.character(x[[v]])))
  i <- match(key(new_data), key(rows))
  # A level the fake values lack takes the population estimate.
  population <- .mock_group_rows(fit, "population")
  out <- rows[i, c("estimate", "lower", "upper")]
  out[is.na(i), ] <- population[rep(1, sum(is.na(i))), c("estimate", "lower", "upper")]
  out
}

.mock_predictor <- c(weight_nereo = "diameter_mm", weight_macro = "fronds", cover_calibration = "cover")

.mock_predictor_of <- function(fit) {
  model <- .mock_model_of(fit)
  key <- if (model == "weight") paste0("weight_", .mock_species_of(fit)) else model
  if (key %in% names(.mock_predictor)) .mock_predictor[[key]]
}

# Exists. A grid of rows to predict at: the fitted sites, years or site-years
# named in `by` (here the fake values'), crossed with the predictor of a weight
# or cover calibration fit: values in `...`, or 30 values over the observed
# range (0 to 1 for cover). A cover grid is a unit plot at zero tide height.
kb_new_data <- function(fit, by = NULL, ...) {
  dots <- list(...)
  predictor <- .mock_predictor_of(fit)
  bad <- setdiff(names(dots), c(predictor, "site", "year"))
  if (length(bad) > 0) {
    .mock_stop(
      "A %s <%s> grid has no column `%s`.", fit$meta$species, class(fit)[2], bad[1]
    )
  }
  grid <- if (length(by) > 0) .mock_group_rows(fit, .mock_grouping(by))[by]
  if (!is.null(predictor)) {
    x <- dots[[predictor]] %||% switch(predictor, diameter_mm = seq(10, 90, length.out = 30), fronds = 1:30, cover = seq(0, 1, length.out = 30))
    values <- stats::setNames(data.frame(x), predictor)
    grid <- if (is.null(grid)) values else merge(grid, values, by = NULL)
    if (predictor == "cover") grid <- data.frame(grid, canopy_area_m2 = grid$cover, plot_area_m2 = 1, tide_height_m = 0)
  }
  grid <- if (is.null(grid)) tibble::tibble(.rows = 1) else tibble::as_tibble(grid)
  structure(grid, mock_grid = TRUE)
}

.mock_point_predictions <- function(fit, new_data, response) {
  rows <- tibble::as_tibble(new_data %||% fit$data)
  rows[c("estimate", "lower", "upper")] <- .mock_rows_estimates(fit, rows)
  .mock_new_predictions(rows, intersect(c("site", "year"), names(rows)), response)
}

# Exists.
kb_predict_size <- function(fit, new_data = NULL, ..., new_levels = c("average", "sample"), representative_site = NULL,
                            conf_level = 0.95, estimate = stats::median, sig_fig = 3) {
  .mock_point_predictions(fit, new_data, "diameter_mm")
}

# Exists. The expected weight at each row of new_data, from its diameter_mm
# (Nereocystis) or fronds (Macrocystis); other columns are kept. The mock
# scales each row's fake weight at 50 mm (12.5 fronds) by a power of 2.6, with
# limits widening away from it on a grid.
kb_predict_weight <- function(fit, new_data = NULL, ..., new_levels = c("average", "sample"), representative_site = NULL,
                              conf_level = 0.95, estimate = stats::median, sig_fig = 3) {
  predictor <- .mock_predictor_of(fit)
  grid <- isTRUE(attr(new_data, "mock_grid"))
  rows <- tibble::as_tibble(new_data %||% fit$data)
  if (!predictor %in% names(rows)) .mock_stop("`new_data` must have a %s column.", predictor)
  at <- rows[[predictor]] / if (predictor == "fronds") 12.5 else 50
  # Plants (not a grid) take the population estimate.
  base <- .mock_rows_estimates(fit, if (grid) rows else rows[0])
  spread <- if (grid) 1 + 0.5 * abs(log(at)) else 1
  rows$estimate <- signif(base$estimate * at^2.6, 3)
  rows$lower <- signif(base$estimate * at^2.6 * (base$lower / base$estimate)^spread, 3)
  rows$upper <- signif(base$estimate * at^2.6 * (base$upper / base$estimate)^spread, 3)
  .mock_new_predictions(rows, intersect(c("site", "year"), names(rows)), "weight_kg", predictor, curve = grid)
}

# Exists. One row: the expected ratio of dry to wet mass.
kb_predict_wetdry <- function(fit, ..., conf_level = 0.95, estimate = stats::median, sig_fig = 3) {
  .mock_new_predictions(.mock_group_rows(fit, "population"), character(), "dry_wet_ratio")
}

# Exists. One row: the expected carbon fraction of dry mass.
kb_predict_carbon <- function(fit, ..., conf_level = 0.95, estimate = stats::median, sig_fig = 3) {
  .mock_new_predictions(.mock_group_rows(fit, "population"), character(), "carbon_fraction")
}

# The fake cover floor: the wet biomass (kg/m2) at zero cover, and its limits.
.mock_cover_floor <- c(estimate = 0.144, lower = 0.0874, upper = 0.217)

# Exists. The expected wet biomass (kg/m2) of a plot at each row of new_data;
# the mock draws each group's straight line over tide-corrected cover from the
# floor at zero cover through its fake value at a cover of 0.5. The tide
# correction divides the canopy by (1 - slope x tide).
kb_predict_cover_calibration <- function(fit, new_data = NULL, ..., new_levels = c("average", "sample"),
                                         representative_site = NULL, conf_level = 0.95, estimate = stats::median,
                                         sig_fig = 3) {
  grid <- isTRUE(attr(new_data, "mock_grid"))
  rows <- tibble::as_tibble(new_data %||% fit$data[c("site", "year", "canopy_area_m2", "plot_area_m2", "tide_height_m")])
  cover <- pmin(1, rows$canopy_area_m2 / (1 - 0.276 * rows$tide_height_m) / rows$plot_area_m2)
  group <- .mock_rows_estimates(fit, rows)
  for (column in c("estimate", "lower", "upper")) {
    floor <- .mock_cover_floor[[column]]
    rows[[column]] <- signif(floor + (group[[column]] - floor) * 2 * cover, 3)
  }
  .mock_new_predictions(
    rows, intersect(c("site", "year"), names(rows)), "biomass_kg_m2", "cover",
    curve = grid
  )
}

.mock_site_levels <- function(site) {
  site <- unique(site)
  site[order(as.numeric(gsub("\\D", "", site)), site)]
}

# Derived predictions ---------------------------------------------------------------------
# Mean weight, survey biomass and canopy biomass carry draws: here 100 lognormal
# draws per row, centred on a fake estimate with its fake limits. Sums are taken
# draw by draw, as kelpbio's sum_by does.

.mock_n_draws <- 100

# A list of draws, one numeric vector per row.
.mock_draws <- function(estimate, lower, upper, seed) {
  sdlog <- ifelse(estimate > 0, log(upper / lower) / (2 * 1.96), 0)
  z <- .mock_with_seed(seed, matrix(stats::rnorm(length(estimate) * .mock_n_draws), nrow = length(estimate)))
  lapply(seq_along(estimate), function(i) if (estimate[i] > 0) exp(log(estimate[i]) + sdlog[i] * z[i, ]) else rep(0, .mock_n_draws))
}

# Rows with their draws as the response column, summarised to estimate, lower
# and upper.
.mock_summarise <- function(rows, draws, response, sig_fig = 3) {
  rows <- tibble::as_tibble(rows)
  rows[[response]] <- draws
  rows$estimate <- signif(vapply(draws, stats::median, numeric(1)), sig_fig)
  rows$lower <- signif(vapply(draws, stats::quantile, numeric(1), probs = 0.025, names = FALSE), sig_fig)
  rows$upper <- signif(vapply(draws, stats::quantile, numeric(1), probs = 0.975, names = FALSE), sig_fig)
  rows
}

# kg draws of each row summed within the levels of `sum_by` (character() sums
# every row), per m2 of the summed area when `area` is given.
.mock_sum_by <- function(grid, kg, area, sum_by) {
  key <- if (length(sum_by) > 0) do.call(paste, c(lapply(sum_by, function(v) as.character(grid[[v]])), sep = "\r")) else rep("", nrow(grid))
  groups <- unique(key)
  rows <- tibble::as_tibble(grid[match(groups, key), sum_by, drop = FALSE])
  if (length(sum_by) == 0) rows <- tibble::tibble(.rows = 1)
  draws <- lapply(groups, function(g) {
    total <- Reduce(`+`, kg[key == g])
    if (is.null(area)) total else total / sum(area[key == g])
  })
  list(rows = rows, draws = draws)
}

# kelpbio's .chk_measure_fits().
.mock_chk_measure_fits <- function(measure, wetdry, carbon) {
  if (measure %in% c("dry", "carbon") && is.null(wetdry)) .mock_stop("`wetdry` is required for %s biomass.", measure)
  if (measure == "carbon" && is.null(carbon)) stop("`carbon` is required for carbon biomass.", call. = FALSE)
}

# Draws of the factor that turns wet mass into the measure: the dry:wet ratio,
# times the carbon fraction for carbon (in g with carbon_scale = 1000).
.mock_measure_factor <- function(measure, wetdry, carbon, carbon_scale = 1) {
  if (measure == "wet") {
    return(1)
  }
  ratio <- .mock_group_rows(wetdry, "population")
  factor <- .mock_draws(ratio$estimate, ratio$lower, ratio$upper, seed = 31)[[1]]
  if (measure == "carbon") {
    fraction <- .mock_group_rows(carbon, "population")
    factor <- factor * .mock_draws(fraction$estimate, fraction$lower, fraction$upper, seed = 37)[[1]] * carbon_scale
  }
  factor
}

.mock_weight_response <- c(wet = "weight_kg", dry = "dry_weight_kg", carbon = "carbon_weight_g")
.mock_biomass_response <- list(
  area = c(wet = "biomass_kg_m2", dry = "dry_biomass_kg_m2", carbon = "carbon_biomass_g_m2"),
  total = c(wet = "biomass_kg", dry = "dry_biomass_kg", carbon = "carbon_biomass_kg")
)

.mock_site_years <- function(data) unique(paste(data[["site"]], .mock_year(data), sep = "|"))

# What a fit's data hold for a site-year, from most to least local, as kelpbio's
# *_support columns give it: "site-year", "site, year", "site", "year" or "none".
.mock_support <- function(fit, site, year, site_year = TRUE) {
  data <- fit$data
  in_site <- site %in% data[["site"]]
  in_year <- year %in% (if (nrow(data) > 0) .mock_year(data) else character())
  in_site_year <- paste(site, year, sep = "|") %in% (if (nrow(data) > 0) .mock_site_years(data) else character())
  ifelse(site_year & in_site_year, "site-year",
    ifelse(in_site & in_year, "site, year", ifelse(in_site, "site", ifelse(in_year, "year", "none")))
  )
}

# The fake mean wet weight of a plant (kg) at each site-year: the fake weight at
# 50 mm scaled by the fake mean diameter to the power 2.6, its log-scale
# uncertainty combining both.
.mock_mean_weight <- function(weight, size, site, year) {
  keys <- data.frame(site = site, year = year)
  w <- .mock_rows_estimates(weight, keys)
  d <- .mock_rows_estimates(size, keys)
  estimate <- w$estimate * (d$estimate / 50)^2.6
  sdlog <- sqrt((log(w$upper / w$lower) / 3.92)^2 + (2.6 * log(d$upper / d$lower) / 3.92)^2)
  data.frame(estimate = estimate, lower = estimate * exp(-1.96 * sdlog), upper = estimate * exp(1.96 * sdlog))
}

# Exists. The expected weight of a plant at each site-year of new_data (by
# default the size fit's site-years), from the weight curve averaged over the
# site-year's size distribution.
kb_predict_mean_weight <- function(weight, size, new_data = NULL, wetdry = NULL, carbon = NULL, ...,
                                   measure = c("wet", "dry", "carbon"), new_levels = c("sample", "average"),
                                   representative_site = NULL, n_plants = 100L, conf_level = 0.95,
                                   estimate = stats::median, sig_fig = 3, progress = c("bar", "none"),
                                   progress_dir = NULL) {
  measure <- rlang::arg_match(measure)
  if (!inherits(weight, "kb_fit_weight")) stop("`weight` must be a <kb_fit_weight> object.", call. = FALSE)
  if (!inherits(size, "kb_fit_size")) stop("`size` must be a <kb_fit_size> object.", call. = FALSE)
  .mock_chk_measure_fits(measure, wetdry, carbon)
  grid <- if (is.null(new_data)) {
    keys <- unique(data.frame(site = as.character(size$data$site), year = .mock_year(size$data)))
    keys[order(keys$site, keys$year), ]
  } else {
    data.frame(site = as.character(new_data$site), year = as.character(new_data$year))
  }
  grid$weight_support <- .mock_support(weight, grid$site, grid$year)
  grid$size_support <- .mock_support(size, grid$site, grid$year)
  mw <- .mock_mean_weight(weight, size, grid$site, grid$year)
  factor <- .mock_measure_factor(measure, wetdry, carbon, carbon_scale = 1000)
  draws <- lapply(.mock_draws(mw$estimate, mw$lower, mw$upper, seed = 41), function(x) x * factor)
  .mock_new_predictions(.mock_summarise(grid, draws, .mock_weight_response[[measure]], sig_fig), c("site", "year"), .mock_weight_response[[measure]])
}

# Exists. The biomass of each in situ survey, per m2 of the area searched
# (per_area) or in kg: its tally of plants times the site-year's mean plant
# weight with a size fit, or the sum of its counted plants' weights (a partial
# plant counted as half) with a table of every counted plant as `size`.
kb_predict_biomass_survey <- function(weight, size, survey, wetdry = NULL, carbon = NULL, ...,
                                      measure = c("wet", "dry", "carbon"), per_area = TRUE, sum_by = NULL,
                                      new_levels = c("sample", "average"), representative_site = NULL,
                                      n_plants = 100L, conf_level = 0.95, estimate = stats::median, sig_fig = 3,
                                      progress = c("bar", "none"), progress_dir = NULL) {
  measure <- rlang::arg_match(measure)
  chk::chk_flag(per_area)
  if (!inherits(weight, "kb_fit_weight")) stop("`weight` must be a <kb_fit_weight> object.", call. = FALSE)
  census <- !inherits(size, "kb_fit")
  if (!census && !inherits(size, "kb_fit_size")) stop("`size` must be a <kb_fit_size> object.", call. = FALSE)
  .mock_chk_survey(survey, if (census) size, "`survey`", "`size`")
  .mock_chk_measure_fits(measure, wetdry, carbon)
  grid <- tibble::as_tibble(survey)
  grid$year <- format(grid$date, "%Y")
  grid <- grid[c("site", "year", setdiff(names(grid), c("site", "year")))]
  if (!is.null(sum_by) && length(setdiff(sum_by, names(grid))) > 0) {
    .mock_stop("`sum_by` must name columns of `survey`: %s.", paste(setdiff(sum_by, names(grid)), collapse = ", "))
  }
  grid$weight_support <- .mock_support(weight, grid$site, grid$year)
  kg <- if (census) {
    half <- if ("partial" %in% names(size)) size$partial else FALSE
    plant_kg <- kb_predict_weight(weight, size)$estimate * ifelse(half, 0.5, 1)
    totals <- tapply(plant_kg, .mock_survey_key(size), sum)
    kg <- unname(totals[.mock_survey_key(grid)])
    kg[is.na(kg)] <- 0
    empty <- sum(kg == 0)
    if (empty > 0) message(sprintf("%d %s with no plant rows %s no biomass.", empty, .mock_n(empty, "survey", "surveys"), .mock_n(empty, "has", "have")))
    .mock_draws(kg, kg * 0.8, kg * 1.25, seed = 43)
  } else {
    grid$size_support <- .mock_support(size, grid$site, grid$year)
    keys <- unique(data.frame(site = grid$site, year = grid$year))
    mw <- .mock_mean_weight(weight, size, keys$site, keys$year)
    mw_draws <- .mock_draws(mw$estimate, mw$lower, mw$upper, seed = 41)
    i <- match(paste(grid$site, grid$year), paste(keys$site, keys$year))
    lapply(seq_len(nrow(grid)), function(row) grid$plants[row] * mw_draws[[i[row]]])
  }
  factor <- .mock_measure_factor(measure, wetdry, carbon, carbon_scale = if (per_area) 1000 else 1)
  kg <- lapply(kg, function(x) x * factor)
  response <- .mock_biomass_response[[if (per_area) "area" else "total"]][[measure]]
  if (is.null(sum_by)) {
    draws <- if (per_area) Map(`/`, kg, grid$survey_area_m2) else kg
    return(.mock_new_predictions(.mock_summarise(grid, draws, response, sig_fig), c("site", "year"), response))
  }
  summed <- .mock_sum_by(grid, kg, if (per_area) grid$survey_area_m2, sum_by)
  .mock_new_predictions(.mock_summarise(summed$rows, summed$draws, response, sig_fig), sum_by, response)
}

# Exists. Biomass over the canopy mapped in each row of new_data, in kg
# (per_area = FALSE) or per m2 of canopy, from a cover calibration fit;
# cover_support gives what the fit has for the row's site and year. The mock
# takes the fake totals of the site-years in fake-totals.json and, for others,
# the fake rate per m2 of canopy times the canopy area.
kb_predict_biomass_canopy <- function(fit, new_data, wetdry = NULL, carbon = NULL, ..., measure = c("wet", "dry", "carbon"),
                                      per_area = FALSE, sum_by = NULL, new_levels = c("sample", "average"),
                                      representative_site = NULL, conf_level = 0.95, estimate = stats::median,
                                      sig_fig = 3) {
  measure <- rlang::arg_match(measure)
  chk::chk_flag(per_area)
  if (!inherits(fit, "kb_fit_cover_calibration")) stop("`fit` must be a <kb_fit_cover_calibration> object.", call. = FALSE)
  kb_check_data_canopy(new_data, x_name = "`new_data`")
  .mock_chk_measure_fits(measure, wetdry, carbon)
  if (!is.null(sum_by) && length(setdiff(sum_by, names(new_data))) > 0) {
    .mock_stop("`sum_by` must name columns of `new_data`: %s.", paste(setdiff(sum_by, names(new_data)), collapse = ", "))
  }
  fake <- .mock_totals()
  i <- match(paste(.mock_normalise_site(new_data$site), new_data$year), paste(fake$site, fake$year))
  rate <- stats::median(fake$wet / fake$canopy_area)
  value <- function(column) {
    fallback <- new_data$canopy_area_m2 * rate * fake[[column]][1] / fake$wet[1]
    ifelse(is.na(i), fallback, fake[[column]][i])
  }
  grid <- tibble::as_tibble(new_data)
  grid$site <- as.character(grid$site)
  grid$year <- as.character(grid$year)
  grid$cover_support <- .mock_support(fit, grid$site, grid$year, site_year = FALSE)
  kg <- .mock_draws(value("wet"), value("wet_lower"), value("wet_upper"), seed = 47)
  factor <- .mock_measure_factor(measure, wetdry, carbon, carbon_scale = if (per_area) 1000 else 1)
  kg <- lapply(kg, function(x) x * factor)
  response <- .mock_biomass_response[[if (per_area) "area" else "total"]][[measure]]
  if (is.null(sum_by)) {
    draws <- if (per_area) Map(`/`, kg, pmax(grid$canopy_area_m2, 1)) else kg
    return(.mock_new_predictions(.mock_summarise(grid, draws, response, sig_fig), c("site", "year"), response))
  }
  summed <- .mock_sum_by(grid, kg, if (per_area) grid$canopy_area_m2, sum_by)
  .mock_new_predictions(.mock_summarise(summed$rows, summed$draws, response, sig_fig), sum_by, response)
}

.mock_normalise_site <- function(site) gsub("[[:space:]_-]", "", tolower(site))

# Data figures ----------------------------------------------------------------------------

# Planned; the names are a proposal, following kb_check_data_<model>_<species>().
# A figure of a sheet's data, to look at before fitting: weight by size, the
# size distribution, the dry:wet ratio and carbon fraction of the samples, the
# canopy cover of each plot flown by drone by tide height, and the canopy area
# mapped at each site by year. All the data in one panel, or a panel per site
# or year (`facet`).
.mock_plot_data <- function(data, sheet, species, facet = NULL) {
  data <- as.data.frame(data)
  macro <- species == "macro"
  if (!is.null(facet) && !facet %in% c("site", "year")) .mock_stop("`facet` must be \"site\" or \"year\", or `NULL`.")
  if ("site" %in% names(data)) data$site <- factor(data$site, levels = .mock_site_levels(data$site))
  if (any(c("year", "date") %in% names(data))) data$year <- factor(.mock_year(data))
  if (!is.null(facet) && !facet %in% names(data)) .mock_stop("`data` has no %s to facet by.", facet)
  size_x <- if (macro) "fronds" else "diameter_mm"
  size_label <- if (macro) "Fronds" else "Sub-bulb diameter (mm)"
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
      ggplot(data, aes(.data$tide_height_m, .data$cover)) + points() + labs(x = "Tide height (m)", y = "Canopy cover")
    },
    canopy = ggplot(data, aes(.data$year, .data$canopy_area_m2)) + points() + labs(x = "Year", y = "Canopy area (m\u00b2)")
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
# Planned. Of the surveys flown by drone, as kb_check_data_survey() is for both species.
kb_plot_data_survey <- function(data, facet = NULL) .mock_plot_data(data, "survey", "nereo", facet)
# Planned.
kb_plot_data_canopy <- function(data, facet = NULL) .mock_plot_data(data, "canopy", "nereo", facet)

# kelpbio's axis_label(): the axis title of a column or response.
.mock_axis_label <- function(name) {
  labels <- c(
    diameter_mm = "Sub-bulb diameter (mm)", fronds = "Fronds", weight_kg = "Wet weight (kg)",
    dry_weight_kg = "Dry weight (kg)", carbon_weight_g = "Carbon weight (g C)",
    biomass_kg_m2 = "Wet biomass (kg/m\u00b2)", dry_biomass_kg_m2 = "Dry biomass (kg/m\u00b2)",
    carbon_biomass_g_m2 = "Carbon biomass (g C/m\u00b2)", biomass_kg = "Total wet biomass (kg)",
    dry_biomass_kg = "Total dry biomass (kg)", carbon_biomass_kg = "Total carbon biomass (kg C)",
    dry_wet_ratio = "Dry:wet mass ratio", carbon_fraction = "Carbon fraction of dry mass",
    cover = "Tide-corrected canopy cover", site = "Site", year = "Year", estimate = "Estimate"
  )
  if (name %in% names(labels)) labels[[name]] else paste0(toupper(substring(name, 1, 1)), substring(name, 2))
}

# Exists. The x-axis is the predictor where it varies, else the last grouping
# column; with neither, `x` must be given. A curve over a grid draws a line
# with a ribbon, anything else point ranges, faceted by the other groupings.
kb_plot_predictions <- function(predictions, ..., x = NULL, max_facets = 12L) {
  predictor <- attr(predictions, "kb_predictor")
  group_vars <- attr(predictions, "kb_group_vars")
  varies <- !is.null(predictor) && predictor %in% names(predictions) && length(unique(predictions[[predictor]])) > 1
  inferred <- if (varies) predictor else group_vars[length(group_vars)]
  x <- x %||% if (length(inferred) > 0) inferred
  if (is.null(x) || !x %in% names(predictions)) stop("Cannot infer the x-axis column from `predictions`.", call. = FALSE)
  facet <- setdiff(group_vars, x)
  data <- as.data.frame(predictions)
  if ("site" %in% names(data)) data$site <- factor(data$site, levels = .mock_site_levels(data$site))
  ribbon <- isTRUE(attr(predictions, "kb_curve")) && identical(x, predictor) && varies
  p <- ggplot(data, aes(.data[[x]], .data$estimate))
  p <- if (ribbon) {
    p + geom_ribbon(aes(ymin = .data$lower, ymax = .data$upper), alpha = 0.2) + geom_line()
  } else {
    p + geom_pointrange(aes(ymin = .data$lower, ymax = .data$upper))
  }
  if (length(facet) > 0) p <- p + facet_wrap(facet)
  p + expand_limits(y = 0) + labs(x = .mock_axis_label(x), y = .mock_axis_label(attr(predictions, "kb_response") %||% "estimate"))
}

# Diagnostics figures -----------------------------------------------------------------------

# Planned. Trace plots of the fit's terms by chain; the mock draws fake chains whose
# autocorrelation follows each term's effective sample rate, and whose chains sit
# apart when R-hat is high.
kb_plot_trace <- function(fit) {
  estimates <- tidy(fit)
  diagnostics <- fit$diagnostics$summary
  i_term <- match(estimates$term, diagnostics$variable)
  chains <- fit$meta$mock_chains
  rate <- diagnostics$ess_bulk[i_term] / fit$meta$mock_ndraws
  r <- diagnostics$rhat[i_term]
  iterations <- 250
  draws <- .mock_with_seed(11, do.call(rbind, lapply(seq_len(nrow(estimates)), function(i) {
    spread <- (estimates$upper[i] - estimates$lower[i]) / 4
    phi <- (1 - min(rate[[i]], 1)) / (1 + min(rate[[i]], 1))
    do.call(rbind, lapply(seq_len(chains), function(chain) {
      offset <- (chain - (chains + 1) / 2) * (r[[i]] - 1) * 20 * spread
      # Draws worth at least their number (a rate of 1 or more) are independent.
      value <- if (phi > 0) {
        as.numeric(stats::arima.sim(list(ar = min(phi, 0.98)), iterations, sd = sqrt(1 - min(phi, 0.98)^2)))
      } else {
        stats::rnorm(iterations)
      }
      value <- estimates$estimate[i] + offset + spread * value
      # Standard deviations are positive.
      if (startsWith(estimates$term[i], "sd_")) value <- abs(value)
      data.frame(term = estimates$term[i], chain = factor(chain), iteration = seq_len(iterations), value = value)
    }))
  })))
  draws$term <- factor(draws$term, levels = estimates$term)
  ggplot(draws, aes(.data$iteration, .data$value, colour = .data$chain)) +
    geom_line(alpha = 0.7, linewidth = 0.3) +
    facet_wrap(~term, scales = "free_y", ncol = 2) +
    labs(x = "Iteration", y = NULL, colour = "Chain") +
    theme_bw()
}

# Fake observed and replicated data for the posterior predictive checks. Each
# family gives a sampler, r(mu), and deviance residuals, d(y, mu). The size
# model's observed data have a higher peak and a heavier right tail than the
# model allows, to show what a misfit looks like.
.mock_gamma <- function(shape) {
  list(
    r = function(mu) stats::rgamma(length(mu), shape, shape / mu),
    d = function(y, mu) sign(y - mu) * sqrt(pmax(2 * shape * ((y - mu) / mu - log(y / mu)), 0))
  )
}

.mock_lognormal <- function(sdlog) {
  list(r = function(mu) stats::rlnorm(length(mu), log(mu), sdlog), d = function(y, mu) (log(y) - log(mu)) / sdlog)
}

# A Normal censored below at 0, as the cover calibration's likelihood.
.mock_censored_normal <- function(sd, cv) {
  sigma <- function(mu) sqrt(sd^2 + (cv * mu)^2)
  list(r = function(mu) pmax(stats::rnorm(length(mu), mu, sigma(mu)), 0), d = function(y, mu) (y - mu) / sigma(mu))
}

.mock_beta <- function(phi) {
  list(
    r = function(mu) stats::rbeta(length(mu), mu * phi, (1 - mu) * phi),
    d = function(y, mu) {
      saturated <- stats::dbeta(y, y * phi, (1 - y) * phi, log = TRUE)
      fitted <- stats::dbeta(y, mu * phi, (1 - mu) * phi, log = TRUE)
      sign(y - mu) * sqrt(pmax(2 * (saturated - fitted), 0))
    }
  )
}

.mock_ppc_models <- function() {
  site_year_means <- function(n, per, meanlog, sdlog) rep(stats::rlnorm(n, meanlog, sdlog), each = per)
  fractions <- function(n, mean, sd) stats::plogis(stats::rnorm(n, stats::qlogis(mean), sd))
  list(
    size = list(
      mu = site_year_means(40, 12, log(31), 0.18), family = .mock_gamma(6), x = "Sub-bulb diameter (mm)",
      observe = function(mu) {
        big <- stats::runif(length(mu)) < 0.15
        ifelse(big, stats::rgamma(length(mu), 8, 8 / (mu * 2)), stats::rgamma(length(mu), 16, 16 / (mu * 0.9)))
      }
    ),
    weight = list(mu = 0.0006 * stats::rgamma(320, 9, 9 / 32)^2.2, family = .mock_lognormal(0.45), x = "Wet weight (kg)", log = TRUE),
    wetdry = list(mu = fractions(150, 0.1, 0.2), family = .mock_beta(150), x = "Ratio of dry to wet mass"),
    carbon = list(mu = fractions(110, 0.3, 0.15), family = .mock_beta(120), x = "Carbon fraction of dry mass"),
    cover_calibration = list(
      mu = .mock_cover_floor[["estimate"]] + 2.7 * stats::runif(40, 0, 1), family = .mock_censored_normal(0.3, 0.25),
      x = expression("Wet biomass (kg/m"^2 * ")")
    )
  )
}

.mock_ppc_data <- function(fit, n_rep = 50) {
  model <- .mock_model_of(fit)
  seed <- 3 + match(model, names(.mock_fit_seconds))
  .mock_with_seed(seed, {
    spec <- .mock_ppc_models()[[model]]
    mu <- spec$mu
    y <- (spec$observe %||% spec$family$r)(mu)
    mu_rep <- lapply(seq_len(n_rep), function(i) {
      m <- mu * exp(stats::rnorm(1, 0, 0.03))
      if (all(mu < 1)) pmin(m, 0.99) else m
    })
    yrep <- t(vapply(mu_rep, spec$family$r, numeric(length(mu))))
    resid <- spec$family$d(y, mu)
    resid_rep <- t(vapply(seq_len(n_rep), function(i) spec$family$d(yrep[i, ], mu_rep[[i]]), numeric(length(mu))))
    list(y = y, yrep = yrep, resid = resid, resid_rep = resid_rep, x = spec$x, log = isTRUE(spec$log))
  })
}

.mock_ppc_plot <- function(y, yrep, x, log = FALSE) {
  p <- bayesplot::ppc_dens_overlay(y, yrep, size = 0.25, alpha = 0.7) +
    labs(x = x, y = "Density") +
    theme_bw() +
    theme(legend.position = "right")
  if (log) p <- p + scale_x_log10(labels = scales::label_number(drop0trailing = TRUE))
  p
}

# Planned. Densities of the observed data and of datasets simulated from the fit
# (bayesplot style). kelpbio has posterior_predict() for the replicates.
kb_ppc_dens <- function(fit) {
  d <- .mock_ppc_data(fit)
  .mock_ppc_plot(d$y, d$yrep, d$x, d$log)
}

# Planned. The same check on the deviance residual scale.
kb_ppc_resid <- function(fit) {
  d <- .mock_ppc_data(fit)
  .mock_ppc_plot(d$resid, d$resid_rep, "Deviance residual")
}

# Prior sensitivity --------------------------------------------------------------------------

.mock_prior_scale <- function(prior) {
  if (inherits(prior, "kb_prior_normal")) prior$sd else if (inherits(prior, "kb_prior_lognormal")) prior$sdlog else 1 / prior$rate
}

# Exists. Power-scaling prior sensitivity per parameter with a prior: the
# cumulative Jensen-Shannon distance when the prior (prior_cjs) and the
# likelihood (likelihood_cjs) are power-scaled. A prior is weak when prior_cjs is
# below `prior_threshold`, and the data strong when likelihood_cjs is at least
# `likelihood_threshold`. In the mock, widening a prior from its default lowers
# its prior CJS in proportion; terms without fake values get small ones.
kb_sensitivity <- function(fit, ..., prior_threshold = 0.1, likelihood_threshold = 0.05) {
  model <- .mock_model_of(fit)
  terms <- fit$meta$terms$fixed
  fake <- .mock_sensitivity()[[model]]
  i <- match(terms, fake$term)
  prior_cjs <- ifelse(is.na(i), 0.02, fake$prior_cjs[i])
  likelihood_cjs <- ifelse(is.na(i), 0.15, fake$likelihood_cjs[i])
  defaults <- get(sprintf("kb_priors_%s_%s", model, .mock_species_of(fit)))()
  ratio <- vapply(terms, function(name) {
    min(1, .mock_prior_scale(defaults[[name]]) / .mock_prior_scale(fit$meta$priors[[name]]))
  }, numeric(1))
  prior_cjs <- round(prior_cjs * ratio, 3)
  # A pre-fit model was fitted to more data than a single run has, so its priors
  # weigh less and its data more.
  if (identical(fit$meta$mock_source, "prefit")) {
    prior_cjs <- round(prior_cjs / 2, 3)
    likelihood_cjs <- round(likelihood_cjs * 2, 3)
  }
  # Worked out before tibble(), where the column names would mask the thresholds.
  weak_prior <- prior_cjs < prior_threshold
  strong_data <- likelihood_cjs >= likelihood_threshold
  tibble::tibble(
    term = terms, prior_cjs = prior_cjs, likelihood_cjs = likelihood_cjs,
    weak_prior = weak_prior, strong_data = strong_data
  )
}

# Influential observations ----------------------------------------------------------------

# Each observation's response on the model's scale, and its predictor where the
# response depends on one, from the fitted data.
.mock_influence_response <- function(fit) {
  d <- fit$data
  measure <- intersect(c("diameter_mm", "fronds"), names(d))
  switch(.mock_model_of(fit),
    size = list(y = log(d[[measure]])),
    weight = list(y = log(d$weight_kg), x = log(d[[measure]])),
    wetdry = list(y = stats::qlogis(d$dry_mass_g / d$wet_mass_g)),
    carbon = list(y = stats::qlogis(d$carbon_mass_ug / (d$sample_mass_mg * 1000))),
    cover_calibration = list(y = d$biomass_kg_m2, x = d$canopy_area_m2 / d$plot_area_m2)
  )
}

# Exists on kelpbio's add-loo branch (kelpbio#43), not yet merged. The fitted
# data with each observation's elpd_loo and pareto_k, and whether it is
# influential (pareto_k above `threshold`). In the mock, an observation's Pareto
# k grows with its robust z-score about the fitted trend, so values far from the
# rest, such as recording errors, are flagged.
kb_influence <- function(fit, ..., threshold = 0.7) {
  rlang::check_dots_empty()
  chk::chk_number(threshold)
  chk::chk_gt(threshold)
  if (nrow(fit$data) == 0) {
    stop("A zero-observation fit has no likelihood, so it cannot be cross-validated.", call. = FALSE)
  }
  response <- .mock_influence_response(fit)
  y <- response$y
  resid <- if (is.null(response$x)) y - stats::median(y) else stats::residuals(stats::lm(y ~ response$x))
  z <- abs(resid - stats::median(resid)) / stats::mad(resid)
  .mock_with_seed(17, {
    pareto_k <- round(0.12 * z + stats::runif(length(z), -0.05, 0.1), 2)
    elpd_loo <- round(-1 - z^2 / 2 + stats::rnorm(length(z), 0, 0.1), 2)
  })
  out <- tibble::as_tibble(fit$data)
  out$elpd_loo <- elpd_loo
  out$pareto_k <- pareto_k
  out$influential <- pareto_k > threshold
  out
}
