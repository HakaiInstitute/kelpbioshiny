# jarl-ignore-file unused_function: the app calls the mocks by built name through kelpbio_fn().
# Mock kelpbio functions -----------------------------------------------------------
#
# These mocks stand in for kelpbio functions. They return fake results from the
# JSON files in inst/extdata/, and draw their figures at run time from those fake
# values. Delete each mock as the real function lands in kelpbio, and this file
# once all of them have. The example data the app loads are in R/mock-example.R.
#
# The app calls these functions unqualified (kb_fit_density_nereo(), tidy(),
# converged()), as it will once they are imported from kelpbio. To switch a
# function over: delete its mock here, add kelpbio to Imports in DESCRIPTION
# (once), and add `@importFrom kelpbio <fun>` to R/namespace.R. Everything with a
# leading dot is mock plumbing and goes with the file, as do the fake-*.json
# files in inst/extdata/, the ggplot2 imports below and the kb_fit S3 method
# registrations.
#
# Each mock is marked:
# - Exists: kelpbio has the function today; the mock's signature and checks match it.
# - Exists in the working tree: on an unmerged kelpbio branch (add-wetdry-models,
#   add-carbon-models or add-cover-models), so it may change.
# - Planned: not in kelpbio yet; named and shaped following the package conventions.

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
.mock_biomass <- function() .mock_json("fake-biomass")
.mock_totals <- function() .mock_json("fake-totals")


# Each model's terms (tidy() rows) and the prior entry (kb_priors_*()) each draws on.
.mock_terms <- list(
  density = c(
    bStipes = "intercept", bZeroInflation = "zero_inflation", bDispersion = "dispersion",
    sSite = "sd_site", sYear = "sd_year", sSiteYear = "sd_site_year"
  ),
  size = c(bDiameter = "intercept", bShape = "shape", sSite = "sd_site", sYear = "sd_year", sSiteYear = "sd_site_year"),
  weight = c(
    bWeight = "intercept", bPower = "power", bFloor = "floor", bDensity = "density",
    sSite = "sd_site", sYear = "sd_year", sSiteYear = "sd_site_year", sWeight = "sd_residual"
  ),
  blade = c(bBlade = "intercept", sSite = "sd_site"),
  wetdry = c(bDryWet = "intercept", bPrecision = "precision"),
  carbon = c(bCarbon = "intercept", bPrecision = "precision"),
  cover = c(bCanopy = "canopy", bFloor = "floor", bTide = "tide", bScaling = "scaling", sSite = "sd_site", sYear = "sd_year")
)

# Sampler diagnostics at the default 4 chains of 1000 draws with no thinning.
.mock_diagnostics <- function(model) {
  terms <- names(.mock_terms[[model]])
  out <- switch(model,
    weight = {
      conv <- .mock_results()$weight_convergence
      data.frame(rhat = conv$rhat, ess_bulk = conv$ess_bulk)
    },
    size = data.frame(rhat = c(1.002, 1.004, 1.011, 1.026, 1.071), ess_bulk = c(1452, 1318, 688, 431, 182)),
    {
      i <- seq_along(terms) - 1
      data.frame(rhat = 1.001 + i * 0.001, ess_bulk = 1250 - i * 100)
    }
  )
  data.frame(variable = terms, out, ess_tail = out$ess_bulk * 0.9)
}

.mock_estimates <- function(model) {
  terms <- names(.mock_terms[[model]])
  json <- .mock_results()[[paste0(model, "_tidy")]]
  if (!is.null(json)) {
    return(json)
  }
  estimate <- ifelse(startsWith(terms, "s"), 0.2, 0.5) + 0.05 * (seq_along(terms) - 1)
  data.frame(term = terms, estimate = estimate, lower = estimate * 0.6, upper = estimate * 1.5)
}

# Simulated sampling time, in seconds, at 1000 draws per chain without thinning.
.mock_fit_seconds <- c(density = 3, size = 4, weight = 5, blade = 3, wetdry = 3, carbon = 3, cover = 3)

# Runs code with a fixed seed and leaves the session's random numbers untouched.
.mock_with_seed <- function(seed, code) withr::with_seed(seed, code)

.mock_resolve_priors <- function(priors, defaults) {
  if (is.null(priors)) {
    return(defaults)
  }
  defaults[names(priors)] <- priors
  defaults
}

# A fake kb_fit object, shaped like new_kb_fit(): draws, diagnostics, data and meta.
# Thinning makes the kept draws less alike, so R-hat moves towards 1 and the
# effective sample size rises, as it would in a real refit.
.mock_new_fit <- function(model, species, data, priors, chains = 4L, niters = 1000L, nthin = 1L,
                          source = "user", reference = NULL) {
  defaults <- get(sprintf("kb_priors_%s_%s", model, species))()
  diagnostics <- .mock_diagnostics(model)
  ndraws <- chains * niters
  diagnostics$rhat <- 1 + (diagnostics$rhat - 1) / nthin
  diagnostics$ess_bulk <- round(diagnostics$ess_bulk * nthin * ndraws / 4000)
  diagnostics$ess_tail <- round(diagnostics$ess_tail * nthin * ndraws / 4000)
  structure(
    list(
      draws = .mock_estimates(model),
      diagnostics = list(summary = diagnostics, perc_divergent = 0),
      data = tibble::as_tibble(data),
      meta = list(
        model = model, species = species, priors = .mock_resolve_priors(priors, defaults),
        chains = chains, niters = niters, nthin = nthin, ndraws = ndraws, source = source, reference = reference
      )
    ),
    class = c(sprintf("kb_fit_%s_%s", model, species), paste0("kb_fit_", model), "kb_fit")
  )
}

# Simulated sampling: writes the completed fraction to progress_dir over a few
# seconds, scaled by the iterations sampled, for kb_fit_progress() to read from
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

# The shared body of the mock fits: kelpbio's sampler and data checks, simulated
# sampling, then the fake fit. Prototype only: KELPBIOSHINY_MOCK_FAIL lists model
# names (e.g. "size,weight") whose fits fail after sampling, to show a failed fit.
.mock_fit <- function(model, species, data, priors, chains, niters, nthin, progress, progress_dir) {
  rlang::arg_match(progress, c("bar", "verbose", "none"))
  for (arg in c("chains", "niters", "nthin")) {
    value <- get(arg)
    chk::chk_whole_number(value, x_name = paste0("`", arg, "`"))
    chk::chk_gt(value, value = 0, x_name = paste0("`", arg, "`"))
  }
  get(sprintf("kb_check_data_%s_%s", model, species))(data, x_name = "`data`")
  .mock_sample(model, niters, nthin, progress_dir)
  if (model %in% strsplit(Sys.getenv("KELPBIOSHINY_MOCK_FAIL"), ",", fixed = TRUE)[[1]]) {
    stop("Sampling failed: the simulated chains did not initialise (prototype failure).", call. = FALSE)
  }
  .mock_new_fit(model, species, data, priors, chains, niters, nthin)
}

# Data checks --------------------------------------------------------------------------
# Like kelpbio's: a check returns the data invisibly and aborts, naming the column,
# on a missing column, a wrong type, an out-of-range value or a missing value.

.mock_xname <- function(x_name, col) paste0("Column `", col, "` of ", x_name)

.mock_chk_number_column <- function(data, col, x_name, gt = NULL, gte = NULL, whole = FALSE) {
  nm <- .mock_xname(x_name, col)
  chk::chk_numeric(data[[col]], x_name = nm)
  chk::chk_not_any_na(data[[col]], x_name = nm)
  if (!is.null(gt)) chk::chk_gt(data[[col]], value = gt, x_name = nm)
  if (!is.null(gte)) chk::chk_gte(data[[col]], value = gte, x_name = nm)
  if (whole) chk::chk_whole_numeric(data[[col]], x_name = nm)
}

.mock_chk_groups <- function(data, x_name) {
  for (col in c("site", "year")) {
    nm <- .mock_xname(x_name, col)
    chk::chk_character_or_factor(data[[col]], x_name = nm)
    chk::chk_not_any_na(data[[col]], x_name = nm)
  }
}

.mock_chk_columns <- function(data, columns, x_name) {
  chk::chk_data(data, x_name = x_name)
  chk::chk_superset(names(data), columns, x_name = x_name)
}

# Exists.
kb_check_data_density_nereo <- function(data, x_name = chk::deparse_backtick_chk(substitute(data))) {
  .mock_chk_columns(data, c("stipes", "area_m2", "site", "year"), x_name)
  .mock_chk_number_column(data, "stipes", x_name, gte = 0, whole = TRUE)
  .mock_chk_number_column(data, "area_m2", x_name, gt = 0)
  .mock_chk_groups(data, x_name)
  invisible(data)
}
# Exists.
kb_check_data_density_macro <- function(data, x_name = chk::deparse_backtick_chk(substitute(data))) {
  .mock_chk_columns(data, c("plants", "area_m2", "site", "year"), x_name)
  .mock_chk_number_column(data, "plants", x_name, gte = 0, whole = TRUE)
  .mock_chk_number_column(data, "area_m2", x_name, gt = 0)
  .mock_chk_groups(data, x_name)
  invisible(data)
}
# Exists.
kb_check_data_size_nereo <- function(data, x_name = chk::deparse_backtick_chk(substitute(data))) {
  .mock_chk_columns(data, c("diameter_mm", "site", "year"), x_name)
  .mock_chk_number_column(data, "diameter_mm", x_name, gt = 0)
  .mock_chk_groups(data, x_name)
  invisible(data)
}
# Exists.
kb_check_data_size_macro <- function(data, x_name = chk::deparse_backtick_chk(substitute(data))) {
  .mock_chk_columns(data, c("fronds", "site", "year"), x_name)
  .mock_chk_number_column(data, "fronds", x_name, gt = 0, whole = TRUE)
  .mock_chk_groups(data, x_name)
  invisible(data)
}
# Exists. stipes_m2 is optional: >= 0, NA where not recorded, one value per site-year.
kb_check_data_weight_nereo <- function(data, x_name = chk::deparse_backtick_chk(substitute(data))) {
  .mock_chk_columns(data, c("diameter_mm", "weight_kg", "site", "year"), x_name)
  .mock_chk_number_column(data, "diameter_mm", x_name, gt = 0)
  .mock_chk_number_column(data, "weight_kg", x_name, gt = 0)
  .mock_chk_groups(data, x_name)
  if ("stipes_m2" %in% names(data) && !all(is.na(data$stipes_m2))) {
    nm <- .mock_xname(x_name, "stipes_m2")
    chk::chk_numeric(data$stipes_m2, x_name = nm)
    chk::chk_gte(data$stipes_m2, value = 0, x_name = nm)
    recorded <- data[!is.na(data$stipes_m2), ]
    values <- tapply(recorded$stipes_m2, paste(recorded$site, recorded$year, sep = ":"), function(x) length(unique(x)))
    if (any(values > 1)) {
      stop(sprintf("%s must have one value per site-year: %s.", nm, names(values)[values > 1][1]), call. = FALSE)
    }
  }
  invisible(data)
}
# Exists.
kb_check_data_weight_macro <- function(data, x_name = chk::deparse_backtick_chk(substitute(data))) {
  .mock_chk_columns(data, c("fronds", "weight_kg", "site", "year"), x_name)
  .mock_chk_number_column(data, "fronds", x_name, gt = 0, whole = TRUE)
  .mock_chk_number_column(data, "weight_kg", x_name, gt = 0)
  .mock_chk_groups(data, x_name)
  invisible(data)
}
# Exists in the working tree. One row per tissue sample; no site or year.
kb_check_data_wetdry_nereo <- function(data, x_name = chk::deparse_backtick_chk(substitute(data))) {
  .mock_chk_columns(data, c("wet_mass_g", "dry_mass_g"), x_name)
  .mock_chk_number_column(data, "wet_mass_g", x_name, gt = 0)
  .mock_chk_number_column(data, "dry_mass_g", x_name, gt = 0)
  if (any(data$dry_mass_g >= data$wet_mass_g)) {
    stop(sprintf("%s must be less than wet_mass_g.", .mock_xname(x_name, "dry_mass_g")), call. = FALSE)
  }
  invisible(data)
}
# Exists in the working tree.
kb_check_data_wetdry_macro <- kb_check_data_wetdry_nereo
# Planned.
kb_check_data_blade_nereo <- function(data, x_name = chk::deparse_backtick_chk(substitute(data))) {
  .mock_chk_columns(data, c("blade_weight_kg", "total_weight_kg", "site", "year"), x_name)
  .mock_chk_number_column(data, "blade_weight_kg", x_name, gt = 0)
  .mock_chk_number_column(data, "total_weight_kg", x_name, gt = 0)
  .mock_chk_groups(data, x_name)
  invisible(data)
}
# Planned.
kb_check_data_blade_macro <- kb_check_data_blade_nereo
# Exists in the working tree. One row per dried tissue sample; no site or year.
kb_check_data_carbon_nereo <- function(data, x_name = chk::deparse_backtick_chk(substitute(data))) {
  .mock_chk_columns(data, c("sample_mass_mg", "carbon_mass_ug"), x_name)
  .mock_chk_number_column(data, "sample_mass_mg", x_name, gt = 0)
  .mock_chk_number_column(data, "carbon_mass_ug", x_name, gt = 0)
  fraction <- data$carbon_mass_ug / 1000 / data$sample_mass_mg
  if (any(fraction >= 1)) {
    stop(sprintf("%s must be less than the sample mass.", .mock_xname(x_name, "carbon_mass_ug")), call. = FALSE)
  }
  n <- sum(fraction < 0.10 | fraction > 0.50)
  if (n > 0) {
    warning(sprintf(
      "%d %s in %s %s outside 0.10 to 0.50, the plausible range for kelp tissue.",
      n, if (n == 1) "sample" else "samples", x_name, if (n == 1) "has a carbon fraction" else "have carbon fractions"
    ), call. = FALSE)
  }
  invisible(data)
}
# Exists in the working tree.
kb_check_data_carbon_macro <- kb_check_data_carbon_nereo
# Planned. The drone-survey columns that kelpbio's kb_check_data_cover_nereo()
# checks, for a sheet of surveys before the in situ biomass is added
# (kb_add_biomass()).
kb_check_data_cover_surveys_nereo <- function(data, x_name = chk::deparse_backtick_chk(substitute(data))) {
  .mock_chk_columns(data, c("canopy_m2", "polygon_m2", "tide_height_m", "site", "year"), x_name)
  .mock_chk_number_column(data, "canopy_m2", x_name, gte = 0)
  .mock_chk_number_column(data, "polygon_m2", x_name, gt = 0)
  .mock_chk_number_column(data, "tide_height_m", x_name)
  if (any(data$canopy_m2 > data$polygon_m2)) {
    stop(sprintf("%s must be at least canopy_m2.", .mock_xname(x_name, "polygon_m2")), call. = FALSE)
  }
  .mock_chk_groups(data, x_name)
  invisible(data)
}
# Planned.
kb_check_data_cover_surveys_macro <- kb_check_data_cover_surveys_nereo
# Exists in the working tree. One row per drone survey of a plot, with the in
# situ wet biomass (kg/m2) and its limits; the cover fits check their data with it.
kb_check_data_cover_nereo <- function(data, x_name = chk::deparse_backtick_chk(substitute(data))) {
  kb_check_data_cover_surveys_nereo(data, x_name)
  for (column in c("estimate", "lower", "upper")) .mock_chk_number_column(data, column, x_name, gt = 0)
  if (any(data$lower > data$estimate | data$estimate > data$upper | data$lower >= data$upper)) {
    stop(sprintf("%s must satisfy lower <= estimate <= upper, with lower < upper.", x_name), call. = FALSE)
  }
  invisible(data)
}
# Exists in the working tree.
kb_check_data_cover_macro <- kb_check_data_cover_nereo

# Planned; the name is a proposal. Adds stipes_m2, the observed stipe density of
# each row's site-year (stipes counted over the area surveyed), from Nereocystis
# density data, for the weight model's density effect. Site-years without density
# data get NA, which the weight fit treats as not recorded.
kb_add_stipes_m2 <- function(data, density) {
  kb_check_data_density_nereo(density)
  chk::chk_data(data)
  chk::chk_superset(names(data), c("site", "year"))
  key <- function(x) paste(x$site, x$year, sep = ":")
  stipes <- tapply(density$stipes, key(density), sum)
  area <- tapply(density$area_m2, key(density), sum)
  data$stipes_m2 <- unname((stipes / area)[key(data)])
  data
}

# Planned; the name is a proposal. Adds the in situ wet biomass of each drone
# survey's site-year (estimate, lower and upper, kg/m2) from kb_predict_biomass()
# output, for the cover model. Surveys of site-years without a biomass estimate
# are dropped.
kb_add_biomass <- function(data, biomass) {
  if (!inherits(biomass, "kb_biomass") || attr(biomass, "kb_type") != "wet" || attr(biomass, "kb_scale") != "area") {
    stop("`biomass` must be wet biomass per unit area from kb_predict_biomass().", call. = FALSE)
  }
  chk::chk_data(data)
  chk::chk_superset(names(data), c("site", "year"))
  i <- match(paste(data$site, data$year), paste(biomass$site, biomass$year))
  data[c("estimate", "lower", "upper")] <- as.data.frame(biomass)[i, c("estimate", "lower", "upper")]
  data[!is.na(i), ]
}

# Priors -----------------------------------------------------------------------------

# Exists.
kb_prior_normal <- function(mean = 0, sd = 1) {
  chk::chk_number(mean)
  chk::chk_number(sd)
  chk::chk_gt(sd, value = 0)
  structure(list(mean = mean, sd = sd), class = c("kb_prior_normal", "kb_prior"))
}

# Exists.
kb_prior_exponential <- function(rate = 1) {
  chk::chk_number(rate)
  chk::chk_gt(rate, value = 0)
  structure(list(rate = rate), class = c("kb_prior_exponential", "kb_prior"))
}

# Planned: each entry of a kb_priors_*() list names, in its "term" attribute, the
# model term it applies to, as tidy(), kb_convergence() and kb_sensitivity() name
# it, so the app shows one name per parameter. The macro-only entries are added
# to the mock's (Nereocystis) term names.
.mock_macro_terms <- list(size = c(bDispersion = "dispersion"), weight = c(bFronds = "fronds", bShape = "shape"))

.mock_with_terms <- function(priors, model) {
  terms <- c(.mock_terms[[model]], .mock_macro_terms[[model]])
  for (name in names(priors)) attr(priors[[name]], "term") <- names(terms)[match(name, terms)]
  priors
}

.mock_sd_priors <- function() {
  list(
    sd_site = kb_prior_exponential(rate = 1),
    sd_year = kb_prior_exponential(rate = 1),
    sd_site_year = kb_prior_exponential(rate = 1)
  )
}

# Exists.
kb_priors_density_nereo <- function() {
  priors <- c(
    list(
      intercept = kb_prior_normal(mean = 0, sd = 2),
      zero_inflation = kb_prior_normal(mean = 0, sd = 2),
      dispersion = kb_prior_exponential(rate = 1)
    ),
    .mock_sd_priors()
  )
  .mock_with_terms(priors, "density")
}
# Exists.
kb_priors_density_macro <- function() {
  priors <- c(list(intercept = kb_prior_normal(mean = 0, sd = 2), dispersion = kb_prior_exponential(rate = 1)), .mock_sd_priors())
  .mock_with_terms(priors, "density")
}
# Exists.
kb_priors_size_nereo <- function() {
  priors <- c(list(intercept = kb_prior_normal(mean = 0, sd = 2), shape = kb_prior_exponential(rate = 0.1)), .mock_sd_priors())
  .mock_with_terms(priors, "size")
}
# Exists.
kb_priors_size_macro <- function() {
  priors <- c(list(intercept = kb_prior_normal(mean = 0, sd = 2), dispersion = kb_prior_exponential(rate = 1)), .mock_sd_priors())
  .mock_with_terms(priors, "size")
}
# Exists.
kb_priors_weight_nereo <- function() {
  priors <- c(
    list(
      intercept = kb_prior_normal(mean = 0, sd = 2),
      power = kb_prior_normal(mean = 2, sd = 1),
      floor = kb_prior_normal(mean = 0, sd = 0.5),
      density = kb_prior_normal(mean = 0, sd = 0.5)
    ),
    .mock_sd_priors(),
    list(sd_residual = kb_prior_exponential(rate = 1))
  )
  .mock_with_terms(priors, "weight")
}
# Exists.
kb_priors_weight_macro <- function() {
  priors <- c(
    list(
      intercept = kb_prior_normal(mean = 0, sd = 2),
      fronds = kb_prior_normal(mean = 1, sd = 0.5),
      shape = kb_prior_exponential(rate = 0.1)
    ),
    .mock_sd_priors()
  )
  .mock_with_terms(priors, "weight")
}
# Exists in the working tree.
kb_priors_wetdry_nereo <- function() {
  .mock_with_terms(list(intercept = kb_prior_normal(mean = 0, sd = 2), precision = kb_prior_exponential(rate = 0.01)), "wetdry")
}
# Exists in the working tree.
kb_priors_wetdry_macro <- kb_priors_wetdry_nereo
# Planned.
kb_priors_blade_nereo <- function() {
  .mock_with_terms(list(intercept = kb_prior_normal(mean = 0, sd = 1), sd_site = kb_prior_exponential(rate = 1)), "blade")
}
# Planned.
kb_priors_blade_macro <- kb_priors_blade_nereo
# Exists in the working tree.
kb_priors_carbon_nereo <- function() {
  .mock_with_terms(list(intercept = kb_prior_normal(mean = -0.8, sd = 0.3), precision = kb_prior_exponential(rate = 0.001)), "carbon")
}
# Exists in the working tree.
kb_priors_carbon_macro <- kb_priors_carbon_nereo
# Planned.
kb_priors_cover_nereo <- function() {
  priors <- list(
    canopy = kb_prior_normal(mean = 2, sd = 1),
    floor = kb_prior_normal(mean = 0, sd = 0.1),
    tide = kb_prior_normal(mean = 0.276, sd = 0.04),
    scaling = kb_prior_normal(mean = 1, sd = 0.5),
    sd_site = kb_prior_exponential(rate = 1),
    sd_year = kb_prior_exponential(rate = 1)
  )
  .mock_with_terms(priors, "cover")
}
# Exists in the working tree.
kb_priors_cover_macro <- function() {
  priors <- kb_priors_cover_nereo()
  priors$floor <- kb_prior_normal(mean = 0.4, sd = 0.3)
  priors$tide <- kb_prior_normal(mean = 0.227, sd = 0.03)
  .mock_with_terms(priors, "cover")
}

# Fitting -----------------------------------------------------------------------------------
# The mock fits check their arguments and data as kelpbio does, then sample for a
# few seconds when given a progress_dir.

# Exists.
kb_fit_density_nereo <- function(data, priors = NULL, ..., prior_only = FALSE, chains = 4L, niters = 1000L,
                                 nthin = 1L, cores = NULL, seed = NULL, progress = c("bar", "verbose", "none"),
                                 progress_dir = NULL) {
  .mock_fit("density", "nereo", data, priors, chains, niters, nthin, progress, progress_dir)
}
# Exists.
kb_fit_density_macro <- function(data, priors = NULL, ..., prior_only = FALSE, chains = 4L, niters = 1000L,
                                 nthin = 1L, cores = NULL, seed = NULL, progress = c("bar", "verbose", "none"),
                                 progress_dir = NULL) {
  .mock_fit("density", "macro", data, priors, chains, niters, nthin, progress, progress_dir)
}
# Exists.
kb_fit_size_nereo <- function(data, priors = NULL, ..., prior_only = FALSE, chains = 4L, niters = 1000L,
                              nthin = 1L, cores = NULL, seed = NULL, progress = c("bar", "verbose", "none"),
                              progress_dir = NULL) {
  .mock_fit("size", "nereo", data, priors, chains, niters, nthin, progress, progress_dir)
}
# Exists.
kb_fit_size_macro <- function(data, priors = NULL, ..., prior_only = FALSE, chains = 4L, niters = 1000L,
                              nthin = 1L, cores = NULL, seed = NULL, progress = c("bar", "verbose", "none"),
                              progress_dir = NULL) {
  .mock_fit("size", "macro", data, priors, chains, niters, nthin, progress, progress_dir)
}
# Exists.
kb_fit_weight_nereo <- function(data, priors = NULL, form = c("packard_floor", "power"), ..., prior_only = FALSE,
                                chains = 4L, niters = 1000L, nthin = 1L, cores = NULL, seed = NULL,
                                progress = c("bar", "verbose", "none"), progress_dir = NULL) {
  .mock_fit("weight", "nereo", data, priors, chains, niters, nthin, progress, progress_dir)
}
# Exists.
kb_fit_weight_macro <- function(data, priors = NULL, ..., prior_only = FALSE, chains = 4L, niters = 1000L,
                                nthin = 1L, cores = NULL, seed = NULL, progress = c("bar", "verbose", "none"),
                                progress_dir = NULL) {
  .mock_fit("weight", "macro", data, priors, chains, niters, nthin, progress, progress_dir)
}
# Exists in the working tree.
kb_fit_wetdry_nereo <- function(data, priors = NULL, ..., prior_only = FALSE, chains = 4L, niters = 1000L,
                                nthin = 1L, cores = NULL, seed = NULL, progress = c("bar", "verbose", "none"),
                                progress_dir = NULL) {
  .mock_fit("wetdry", "nereo", data, priors, chains, niters, nthin, progress, progress_dir)
}
# Exists in the working tree.
kb_fit_wetdry_macro <- function(data, priors = NULL, ..., prior_only = FALSE, chains = 4L, niters = 1000L,
                                nthin = 1L, cores = NULL, seed = NULL, progress = c("bar", "verbose", "none"),
                                progress_dir = NULL) {
  .mock_fit("wetdry", "macro", data, priors, chains, niters, nthin, progress, progress_dir)
}
# Planned.
kb_fit_blade_nereo <- function(data, priors = NULL, ..., prior_only = FALSE, chains = 4L, niters = 1000L,
                               nthin = 1L, cores = NULL, seed = NULL, progress = c("bar", "verbose", "none"),
                               progress_dir = NULL) {
  .mock_fit("blade", "nereo", data, priors, chains, niters, nthin, progress, progress_dir)
}
# Planned.
kb_fit_blade_macro <- function(data, priors = NULL, ..., prior_only = FALSE, chains = 4L, niters = 1000L,
                               nthin = 1L, cores = NULL, seed = NULL, progress = c("bar", "verbose", "none"),
                               progress_dir = NULL) {
  .mock_fit("blade", "macro", data, priors, chains, niters, nthin, progress, progress_dir)
}
# Exists in the working tree.
kb_fit_carbon_nereo <- function(data, priors = NULL, ..., prior_only = FALSE, chains = 4L, niters = 1000L,
                                nthin = 1L, cores = NULL, seed = NULL, progress = c("bar", "verbose", "none"),
                                progress_dir = NULL) {
  .mock_fit("carbon", "nereo", data, priors, chains, niters, nthin, progress, progress_dir)
}
# Exists in the working tree.
kb_fit_carbon_macro <- function(data, priors = NULL, ..., prior_only = FALSE, chains = 4L, niters = 1000L,
                                nthin = 1L, cores = NULL, seed = NULL, progress = c("bar", "verbose", "none"),
                                progress_dir = NULL) {
  .mock_fit("carbon", "macro", data, priors, chains, niters, nthin, progress, progress_dir)
}

# Exists in the working tree. `data` holds the drone surveys with the in situ
# wet biomass of each (kb_add_biomass()); conf_level is the level of its limits.
kb_fit_cover_nereo <- function(data, priors = NULL, ..., conf_level = 0.95, prior_only = FALSE, chains = 4L,
                               niters = 1000L, nthin = 1L, cores = NULL, seed = NULL,
                               progress = c("bar", "verbose", "none"), progress_dir = NULL) {
  .mock_fit("cover", "nereo", data, priors, chains, niters, nthin, progress, progress_dir)
}
# Exists in the working tree.
kb_fit_cover_macro <- function(data, priors = NULL, ..., conf_level = 0.95, prior_only = FALSE, chains = 4L,
                               niters = 1000L, nthin = 1L, cores = NULL, seed = NULL,
                               progress = c("bar", "verbose", "none"), progress_dir = NULL) {
  .mock_fit("cover", "macro", data, priors, chains, niters, nthin, progress, progress_dir)
}

# Exists.
kb_fit_progress <- function(progress_dir) {
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
kb_prefit_size_macro <- function(reference = "hakai") {
  reference <- rlang::arg_match(reference)
  .mock_prefit("size", "macro", reference)
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
# Exists: kelpbio re-exports these generics from generics and universals and
# provides kb_fit methods for them. The app imports the generics (R/namespace.R)
# and the mocks register their kb_fit methods.

# Exists.
#' @exportS3Method generics::tidy
#' @noRd
tidy.kb_fit <- function(x, ..., conf_level = 0.95, estimate = stats::median, sig_fig = 3,
                        include_random_effects = FALSE) {
  rows <- x$draws
  rows[c("estimate", "lower", "upper")] <- lapply(rows[c("estimate", "lower", "upper")], signif, sig_fig)
  tibble::as_tibble(rows)
}

# Effective sample rate: bulk ESS over the draws kept.
.mock_esr <- function(x) x$diagnostics$summary$ess_bulk / x$meta$ndraws

# Exists.
#' @exportS3Method universals::converged
#' @noRd
converged.kb_fit <- function(x, ..., rhat = 1.01, esr = 0.1, max_perc_divergent = 0.2) {
  all(x$diagnostics$summary$rhat < rhat) && all(.mock_esr(x) > esr) && x$diagnostics$perc_divergent <= max_perc_divergent
}

# Planned. Per-term convergence: R-hat, bulk ESS and effective sample rate, and
# whether each term meets the thresholds, which default to converged()'s.
kb_convergence <- function(fit, ..., rhat = 1.01, esr = 0.1) {
  s <- fit$diagnostics$summary
  rate <- .mock_esr(fit)
  # Worked out before tibble(), where the column names would mask the thresholds.
  ok <- s$rhat < rhat & rate > esr
  tibble::tibble(term = s$variable, rhat = round(s$rhat, 3), ess_bulk = round(s$ess_bulk), esr = round(rate, 3), converged = ok)
}

# Model description -------------------------------------------------------------------

.mock_fraction_description <- function(title, response, parameter) {
  sprintf(
    "%s\nResponse: %s\n\nLikelihood\n  y ~ Beta(mu * phi, (1 - mu) * phi)\n  logit(mu) = %s + bSite[site]\n\nRandom effects\n  bSite[site] ~ Normal(0, sSite)\n\nPriors (placeholder, prototype only)\n  %s ~ Normal(0, 1)\n  sSite ~ Exponential(1)",
    title, response, parameter, parameter
  )
}

.mock_descriptions <- function() list(
  # The JSON description has a printed named vector appended after the text; keep the text only.
  weight = sub("(sSiteYear +~ Exponential\\(1\\)).*", "\\1", .mock_results()$weight_describe),
  density = paste(
    "Density - Nereocystis luetkeana",
    "Response: stipes counted on a transect, over the area surveyed",
    "",
    "Likelihood (placeholder, prototype only)",
    "  stipes ~ ZINegBinomial(mu * area_m2, bDispersion, bZeroInflation)",
    "  log(mu) = bStipes + bSite[site] + bYear[year] + bSiteYear[site, year]",
    "",
    "Random effects",
    "  bSite[site]           ~ Normal(0, sSite)",
    "  bYear[year]           ~ Normal(0, sYear)",
    "  bSiteYear[site, year] ~ Normal(0, sSiteYear)",
    "",
    "Priors",
    "  bStipes        ~ Normal(0, 2)",
    "  bZeroInflation ~ Normal(0, 2)",
    "  bDispersion    ~ Exponential(1)",
    "  sSite          ~ Exponential(1)",
    "  sYear          ~ Exponential(1)",
    "  sSiteYear      ~ Exponential(1)",
    sep = "\n"
  ),
  size = paste(
    "Size distribution - Nereocystis luetkeana",
    "Response: sub-bulb diameter (mm)",
    "",
    "Likelihood (placeholder, prototype only)",
    "  diameter_mm ~ Gamma(bShape, bShape / mu)",
    "  log(mu) = bDiameter + bSite[site] + bYear[year] + bSiteYear[site, year]",
    "",
    "Random effects",
    "  bSite[site]           ~ Normal(0, sSite)",
    "  bYear[year]           ~ Normal(0, sYear)",
    "  bSiteYear[site, year] ~ Normal(0, sSiteYear)",
    "",
    "Priors",
    "  bDiameter ~ Normal(0, 2)",
    "  bShape    ~ Exponential(0.1)",
    "  sSite     ~ Exponential(1)",
    "  sYear     ~ Exponential(1)",
    "  sSiteYear ~ Exponential(1)",
    sep = "\n"
  ),
  blade = .mock_fraction_description("Blade fraction", "blade weight / total wet weight", "bBlade"),
  wetdry = paste(
    "Wet:dry - Nereocystis luetkeana",
    "Response: dry mass / wet mass of each tissue sample",
    "",
    "Likelihood (placeholder, prototype only)",
    "  ratio ~ Beta(mu * bPrecision, (1 - mu) * bPrecision)",
    "  logit(mu) = bDryWet",
    "",
    "Priors",
    "  bDryWet    ~ Normal(0, 2)",
    "  bPrecision ~ Exponential(0.01)",
    sep = "\n"
  ),
  carbon = paste(
    "Carbon fraction - Nereocystis luetkeana",
    "Response: carbon_fraction = carbon_mass_ug / 1000 / sample_mass_mg, the fraction of a dry sample's mass that is carbon",
    "",
    "Likelihood",
    "  carbon_fraction ~ Beta(mu * bPrecision, (1 - mu) * bPrecision)",
    "  logit(mu) = bCarbon",
    "",
    "Priors",
    "  bCarbon        ~ Normal(-0.8, 0.3)",
    "  bPrecision     ~ Exponential(0.001)",
    sep = "\n"
  ),
  cover = paste(
    "Cover - Nereocystis luetkeana",
    "Response: estimate, the in situ wet biomass of a plot (kg/m\u00b2), with compatibility limits lower and upper",
    "",
    "Likelihood",
    "  log(estimate) ~ Normal(log(mu), bScaling * sd)",
    "  mu = bFloor",
    "     + bCanopy * exp(bYear[year] + bSite[site]) * cover",
    "  cover = min(1, canopy_m2 * (1 + bTide * tide_height_m) / polygon_m2)",
    "  sd = (log(upper) - log(lower)) / (2 * 1.96)  (log-scale SD of the in situ estimate)",
    "",
    "Random effects",
    "  bYear[year] ~ Normal(0, sYear)  year effect on log(bCanopy)",
    "  bSite[site] ~ Normal(0, sSite)  site effect on log(bCanopy)",
    "",
    "Priors",
    "  log(bCanopy)   ~ Normal(2, 1)",
    "  bFloor         ~ Normal(0, 0.1) T[0, ]",
    "  bTide          ~ Normal(0.276, 0.04) T[0, ]",
    "  bScaling       ~ Normal(1, 0.5) T[0, ]",
    "  sYear          ~ Exponential(1)",
    "  sSite          ~ Exponential(1)",
    sep = "\n"
  )
)

# Exists; the `priors` argument is planned: priors = FALSE leaves out the priors
# section, for a view that shows the priors elsewhere.
kb_model_describe <- function(fit, prose = FALSE, ..., priors = TRUE) {
  text <- .mock_descriptions()[[fit$meta$model]]
  if (fit$meta$species == "macro") text <- sub("Nereocystis luetkeana", "Macrocystis pyrifera", text, fixed = TRUE)
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  if (!priors) {
    lines <- lines[seq_len(which(startsWith(lines, "Priors"))[1] - 1)]
    while (lines[length(lines)] == "") lines <- lines[-length(lines)]
  }
  cat(lines, sep = "\n")
  invisible(lines)
}

# Predictions ---------------------------------------------------------------------------
# A kb_predictions object: a tibble of grouping columns, the predictor for a curve,
# and estimate, lower and upper, with metadata for kb_plot_predictions(). The mock
# metadata carries ready-made axis titles.

.mock_groupings <- c(population = "", site = "site", year = "year", site_year = "site year")

.mock_grouping <- function(by) {
  key <- paste(sort(by %||% character()), collapse = " ")
  names(.mock_groupings)[.mock_groupings == key]
}

.mock_new_predictions <- function(rows, by, response, predictor = NULL, curve = FALSE, x_label = NULL, y_label = NULL,
                                  population = "All sites and years") {
  structure(
    tibble::as_tibble(rows),
    class = c("kb_predictions", "tbl_df", "tbl", "data.frame"),
    kb_predictor = predictor, kb_group_vars = by, kb_response = response, kb_curve = curve,
    mock_x_label = x_label, mock_y_label = y_label, mock_population = population
  )
}

# Fake estimates per group, from fake-predictions.json; a curve model's are its
# value at the reference predictor value (50 mm, a cover of 0.5).
.mock_group_rows <- function(fit, by) {
  key <- fit$meta$model
  grouping <- .mock_grouping(by)
  rows <- .mock_predictions()[[key]][[grouping]]
  if (is.null(rows)) {
    stop(sprintf("The %s model has no year effect, so `by` cannot include \"year\".", fit$meta$model), call. = FALSE)
  }
  list(rows = rows, grouping = grouping)
}

# Standard errors of the fake link-scale predictions, widening with the effects included.
.mock_se <- c(population = 0.12, site = 0.16, year = 0.15, site_year = 0.2)

# Curves along a predictor for each group: fun(x, eta) is the response, eta the
# group's link value, and the limits widen away from x_ref.
.mock_curves <- function(fit, by, x, link, fun, x_ref, spread) {
  group <- .mock_group_rows(fit, by)
  eta <- link(group$rows$estimate)
  keys <- group$rows[intersect(c("site", "year"), names(group$rows))]
  out <- lapply(seq_along(eta), function(i) {
    se <- .mock_se[[group$grouping]] * (1 + spread * abs(x - x_ref) / max(abs(x - x_ref)))
    curve <- data.frame(x = x, estimate = fun(x, eta[i]), lower = fun(x, eta[i] - 1.96 * se), upper = fun(x, eta[i] + 1.96 * se))
    if (ncol(keys) == 0) curve else cbind(keys[rep(i, length(x)), , drop = FALSE], curve, row.names = NULL)
  })
  rows <- do.call(rbind, out)
  rows[c("estimate", "lower", "upper")] <- lapply(rows[c("estimate", "lower", "upper")], signif, 3)
  rows
}

.mock_point_predictions <- function(fit, by, response, y_label) {
  population <- if (is.null(.mock_predictions()[[fit$meta$model]]$year)) "All sites" else "All sites and years"
  .mock_new_predictions(.mock_group_rows(fit, by)$rows, by, response, y_label = y_label, population = population)
}

.mock_rename_x <- function(rows, name) {
  names(rows)[names(rows) == "x"] <- name
  rows
}

# Exists.
kb_predict_density_by <- function(fit, by = NULL, ..., new_levels = c("average", "sample"), conf_level = 0.95,
                                  estimate = stats::median, sig_fig = 3) {
  .mock_point_predictions(fit, by, "density", expression("Density (stipes/m"^2 * ")"))
}

# Exists.
kb_predict_size_by <- function(fit, by = NULL, ..., new_levels = c("average", "sample"), conf_level = 0.95,
                               estimate = stats::median, sig_fig = 3) {
  .mock_point_predictions(fit, by, "diameter", "Mean sub-bulb diameter (mm)")
}

# Exists (the Nereocystis method; Macrocystis takes `fronds` in place of `diameter_mm`).
kb_predict_weight_by <- function(fit, by = NULL, diameter_mm = NULL, ..., new_levels = c("average", "sample"),
                                 conf_level = 0.95, estimate = stats::median, sig_fig = 3) {
  if (length(diameter_mm) == 1) {
    label <- sprintf("Wet weight at %s mm diameter (kg)", diameter_mm)
    rows <- .mock_group_rows(fit, by)$rows
    rows$estimate <- signif(rows$estimate * (diameter_mm / 50)^2.6, 3)
    rows$lower <- signif(rows$lower * (diameter_mm / 50)^2.6, 3)
    rows$upper <- signif(rows$upper * (diameter_mm / 50)^2.6, 3)
    rows$diameter_mm <- diameter_mm
    return(.mock_new_predictions(rows, by, "weight", "diameter_mm", y_label = label))
  }
  x <- diameter_mm %||% seq(10, 90, by = 2)
  rows <- .mock_curves(fit, by, x, log, function(x, eta) exp(eta) * (x / 50)^2.6, 50, 1.5)
  .mock_new_predictions(
    .mock_rename_x(rows, "diameter_mm"), by, "weight", "diameter_mm",
    curve = TRUE, x_label = "Sub-bulb diameter (mm)", y_label = "Wet weight (kg)"
  )
}

# Exists (as the Nereocystis and Macrocystis methods of a generic). The expected
# weight of each row of new_data, from its diameter_mm (Nereocystis) or fronds
# (Macrocystis); site, year and stipes_m2 are passed through. A new site or year
# draws its effects ("sample", the default) or holds them at zero ("average").
# The mock scales the fake population-level weight at 50 mm by a power of 2.6.
kb_predict_weight <- function(fit, new_data = NULL, ..., new_levels = c("sample", "average"), representative_site = NULL,
                              conf_level = 0.95, estimate = stats::median, sig_fig = 3) {
  new_levels <- rlang::arg_match(new_levels)
  macro <- fit$meta$species == "macro"
  predictor <- if (macro) "fronds" else "diameter_mm"
  rows <- tibble::as_tibble(new_data %||% fit$data)
  mu <- .mock_group_rows(fit, NULL)$rows$estimate * (rows[[predictor]] / if (macro) 12.5 else 50)^2.6
  se <- .mock_se[["population"]] * if (new_levels == "sample") 1.6 else 1
  rows$estimate <- signif(mu, 3)
  rows$lower <- signif(mu * exp(-1.96 * se), 3)
  rows$upper <- signif(mu * exp(1.96 * se), 3)
  .mock_new_predictions(
    rows, NULL, "weight", predictor,
    x_label = if (macro) "Fronds" else "Sub-bulb diameter (mm)", y_label = "Wet weight (kg)"
  )
}

# Planned.
kb_predict_blade_by <- function(fit, by = NULL, ..., new_levels = c("average", "sample"), conf_level = 0.95,
                                estimate = stats::median, sig_fig = 3) {
  .mock_point_predictions(fit, by, "blade_fraction", "Blade fraction")
}

# Exists in the working tree. One row: the expected ratio of dry to wet mass.
kb_predict_wetdry <- function(fit, ..., conf_level = 0.95, estimate = stats::median, sig_fig = 3) {
  .mock_new_predictions(.mock_predictions()$wetdry$population, NULL, "dry_wet_ratio", y_label = "Ratio of dry to wet mass", population = "All samples")
}

# Exists in the working tree. One row: the expected carbon fraction of dry mass.
kb_predict_carbon <- function(fit, ..., conf_level = 0.95, estimate = stats::median, sig_fig = 3) {
  .mock_new_predictions(.mock_predictions()$carbon$population, NULL, "carbon_fraction", y_label = "Carbon fraction of dry mass", population = "All samples")
}

# The fake cover floor: the wet biomass (kg/m2) at zero cover, and its limits.
.mock_cover_floor <- c(estimate = 0.144, lower = 0.0874, upper = 0.217)

# Exists in the working tree. Curves over tide-corrected cover (0 to 1); the mock
# draws each group's straight line from the floor at zero cover through its fake
# value at a cover of 0.5.
kb_predict_cover_by <- function(fit, by = NULL, cover = NULL, ..., new_levels = c("average", "sample"),
                                conf_level = 0.95, estimate = stats::median, sig_fig = 3) {
  x <- cover %||% seq(0, 1, length.out = 30)
  group <- .mock_group_rows(fit, by)$rows
  keys <- group[intersect(c("site", "year"), names(group))]
  rows <- do.call(rbind, lapply(seq_len(nrow(group)), function(i) {
    curve <- data.frame(cover = x)
    for (column in c("estimate", "lower", "upper")) {
      floor <- .mock_cover_floor[[column]]
      curve[[column]] <- signif(floor + (group[[column]][i] - floor) * 2 * x, 3)
    }
    if (ncol(keys) == 0) curve else cbind(keys[rep(i, length(x)), , drop = FALSE], curve, row.names = NULL)
  }))
  .mock_new_predictions(
    rows, by, "biomass_kg_m2", "cover",
    curve = length(x) > 1, x_label = "Tide-corrected cover", y_label = expression("Wet biomass (kg/m"^2 * ")")
  )
}

.mock_site_levels <- function(site) {
  site <- unique(site)
  site[order(as.numeric(gsub("\\D", "", site)), site)]
}

# Exists.
kb_plot_predictions <- function(predictions, ..., x = NULL, max_facets = 12L) {
  data <- as.data.frame(predictions)
  by <- attr(predictions, "kb_group_vars")
  predictor <- attr(predictions, "kb_predictor")
  curve <- isTRUE(attr(predictions, "kb_curve"))
  if (!is.null(data$site)) data$site <- factor(data$site, levels = .mock_site_levels(data$site))
  y_label <- attr(predictions, "mock_y_label")

  if (curve) {
    p <- ggplot(data, aes(.data[[predictor]], .data$estimate)) +
      geom_ribbon(aes(ymin = .data$lower, ymax = .data$upper), alpha = 0.3) +
      geom_line()
    if (length(by) > 0) p <- p + facet_wrap(by)
    return(p + expand_limits(y = 0) + labs(x = attr(predictions, "mock_x_label"), y = y_label) + theme_bw())
  }

  x <- x %||% if (length(by) == 0) "group" else by[length(by)]
  if (length(by) == 0) data$group <- attr(predictions, "mock_population")
  p <- ggplot(data, aes(.data[[x]], .data$estimate)) +
    geom_pointrange(aes(ymin = .data$lower, ymax = .data$upper), size = 0.3) +
    expand_limits(y = 0) +
    labs(x = if (identical(x, predictor)) attr(predictions, "mock_x_label") else switch(x, group = NULL, site = "Site", "Year"), y = y_label) +
    theme_bw()
  facet <- setdiff(by, x)
  if (length(facet) > 0) {
    p <- p + facet_wrap(facet) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  p
}

# Diagnostics figures -----------------------------------------------------------------------

# Planned. Trace plots of the fit's terms by chain; the mock draws fake chains whose
# autocorrelation follows each term's effective sample rate, and whose chains sit
# apart when R-hat is high.
kb_plot_trace <- function(fit) {
  estimates <- tidy(fit)
  diagnostics <- fit$diagnostics$summary
  i_term <- match(estimates$term, diagnostics$variable)
  rate <- .mock_esr(fit)[i_term]
  r <- diagnostics$rhat[i_term]
  iterations <- 250
  chains <- fit$meta$chains
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
      if (startsWith(estimates$term[i], "s")) value <- abs(value)
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
.mock_nb_density <- function(theta, area) {
  list(
    r = function(mu) stats::rnbinom(length(mu), mu = mu * area, size = theta) / area,
    d = function(y, mu) {
      y <- y * area
      mu <- mu * area
      term <- ifelse(y == 0, 0, y * log(y / mu))
      sign(y - mu) * sqrt(pmax(2 * (term - (y + theta) * log((y + theta) / (mu + theta))), 0))
    }
  )
}

.mock_gamma <- function(shape) {
  list(
    r = function(mu) stats::rgamma(length(mu), shape, shape / mu),
    d = function(y, mu) sign(y - mu) * sqrt(pmax(2 * shape * ((y - mu) / mu - log(y / mu)), 0))
  )
}

.mock_lognormal <- function(sdlog) {
  list(r = function(mu) stats::rlnorm(length(mu), log(mu), sdlog), d = function(y, mu) (log(y) - log(mu)) / sdlog)
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
    density = list(mu = site_year_means(40, 3, log(1.4), 0.35), family = .mock_nb_density(8, 4), x = expression("Density (stipes/m"^2 * ")")),
    size = list(
      mu = site_year_means(40, 12, log(31), 0.18), family = .mock_gamma(6), x = "Sub-bulb diameter (mm)",
      observe = function(mu) {
        big <- stats::runif(length(mu)) < 0.15
        ifelse(big, stats::rgamma(length(mu), 8, 8 / (mu * 2)), stats::rgamma(length(mu), 16, 16 / (mu * 0.9)))
      }
    ),
    weight = list(mu = 0.0006 * stats::rgamma(320, 9, 9 / 32)^2.2, family = .mock_lognormal(0.45), x = "Wet weight (kg)", log = TRUE),
    blade = list(mu = fractions(120, 0.5, 0.3), family = .mock_beta(30), x = "Blade fraction"),
    wetdry = list(mu = fractions(150, 0.1, 0.2), family = .mock_beta(150), x = "Ratio of dry to wet mass"),
    carbon = list(mu = fractions(110, 0.3, 0.15), family = .mock_beta(120), x = "Carbon fraction of dry mass"),
    cover = list(
      mu = .mock_cover_floor[["estimate"]] + 2.7 * stats::runif(27, 0, 1), family = .mock_lognormal(0.35),
      x = expression("Wet biomass (kg/m"^2 * ")")
    )
  )
}

.mock_ppc_data <- function(fit, n_rep = 50) {
  seed <- 3 + match(fit$meta$model, names(.mock_terms))
  .mock_with_seed(seed, {
    model <- .mock_ppc_models()[[fit$meta$model]]
    mu <- model$mu
    y <- (model$observe %||% model$family$r)(mu)
    mu_rep <- lapply(seq_len(n_rep), function(i) {
      m <- mu * exp(stats::rnorm(1, 0, 0.03))
      if (all(mu < 1)) pmin(m, 0.99) else m
    })
    yrep <- t(vapply(mu_rep, model$family$r, numeric(length(mu))))
    resid <- model$family$d(y, mu)
    resid_rep <- t(vapply(seq_len(n_rep), function(i) model$family$d(yrep[i, ], mu_rep[[i]]), numeric(length(mu))))
    list(y = y, yrep = yrep, resid = resid, resid_rep = resid_rep, x = model$x, log = isTRUE(model$log))
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

# Planned. Densities of the observed data and of datasets simulated from the fit (bayesplot style).
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

.mock_prior_scale <- function(prior) if (inherits(prior, "kb_prior_normal")) prior$sd else 1 / prior$rate

# Planned. Power-scaling prior sensitivity per parameter: the cumulative
# Jensen-Shannon distance when the prior (prior_cjs) and the likelihood (lik_cjs)
# are power-scaled, with `prior` naming the parameter's entry in the priors list.
# A prior is weak when prior_cjs is at most `prior_cjs`, and the data strong when
# lik_cjs is at least `lik_cjs`. In the mock, widening a prior from its default
# lowers its prior CJS in proportion.
kb_sensitivity <- function(fit, ..., prior_cjs = 0.1, lik_cjs = 0.05) {
  rows <- .mock_sensitivity()[[fit$meta$model]]
  rows$prior <- unname(.mock_terms[[fit$meta$model]][rows$parameter])
  defaults <- get(sprintf("kb_priors_%s_%s", fit$meta$model, fit$meta$species))()
  ratio <- vapply(rows$prior, function(name) {
    if (is.null(defaults[[name]]) || is.null(fit$meta$priors[[name]])) {
      return(1)
    }
    min(1, .mock_prior_scale(defaults[[name]]) / .mock_prior_scale(fit$meta$priors[[name]]))
  }, numeric(1))
  rows$prior_cjs <- round(rows$prior_cjs * ratio, 3)
  # A pre-fit model was fitted to more data than a single run has, so its priors
  # weigh less and its data more.
  if (identical(fit$meta$source, "prefit")) {
    rows$prior_cjs <- round(rows$prior_cjs / 2, 3)
    rows$lik_cjs <- round(rows$lik_cjs * 2, 3)
  }
  # Worked out before tibble(), where the column names would mask the thresholds.
  weak_prior <- rows$prior_cjs <= prior_cjs
  strong_data <- rows$lik_cjs >= lik_cjs
  tibble::tibble(
    parameter = rows$parameter, prior = rows$prior, prior_cjs = rows$prior_cjs, lik_cjs = rows$lik_cjs,
    weak_prior = weak_prior, strong_data = strong_data
  )
}

# Biomass -----------------------------------------------------------------------------------

.mock_normalise_site <- function(site) gsub("[[:space:]_-]", "", tolower(site))

.mock_site_years <- function(data) unique(paste(data$site, data$year, sep = "|"))

# Planned. Biomass per unit area (kg/m2) by site-year, combining the sub-model fits.
# Every site-year in the density data gets an estimate; population_size and
# population_weight mark site-years without size or weight data in a fit to your
# data, which use the population-level estimate.
kb_predict_biomass <- function(density, size, weight, blade = NULL, wetdry = NULL, carbon = NULL, ...,
                               by = c("site", "year"), type = c("wet", "dry", "carbon"), conf_level = 0.95,
                               estimate = stats::median, sig_fig = 3) {
  type <- match.arg(type)
  for (fit in list(density, size, weight)) {
    if (!inherits(fit, "kb_fit")) stop("`density`, `size` and `weight` must be kb_fit objects.", call. = FALSE)
  }
  if (type != "wet" && is.null(wetdry)) stop("`wetdry` is required for dry biomass and carbon.", call. = FALSE)
  if (type == "carbon" && is.null(carbon)) stop("`carbon` is required for carbon.", call. = FALSE)
  keys <- .mock_site_years(density$data)
  missing_from <- function(fit) {
    if (fit$meta$source != "user") {
      return(rep(FALSE, length(keys)))
    }
    !keys %in% .mock_site_years(fit$data)
  }
  parts <- strsplit(keys, "|", fixed = TRUE)
  site <- vapply(parts, `[`, "", 1)
  year <- vapply(parts, `[`, "", 2)
  fake <- .mock_biomass()
  match <- match(paste(.mock_normalise_site(site), year), paste(fake$site, fake$year))
  rows <- tibble::tibble(
    site = site, year = year,
    estimate = fake[[type]][match],
    lower = fake[[paste0(type, "_lower")]][match],
    upper = fake[[paste0(type, "_upper")]][match],
    population_size = missing_from(size),
    population_weight = missing_from(weight)
  )
  rows <- rows[order(rows$year, as.numeric(gsub("\\D", "", rows$site)), rows$site), ]
  structure(rows, class = c("kb_biomass", class(rows)), kb_type = type, kb_scale = "area")
}

# Planned; the inputs are a proposal. Total biomass (t) per site-year with a
# drone survey in the cover model's data, from the fitted cover model and
# kb_predict_biomass() output, with the site-year's canopy area (m2). The
# per-area population flags are carried over.
kb_predict_biomass_total <- function(fit, biomass, ..., conf_level = 0.95, estimate = stats::median, sig_fig = 3) {
  type <- attr(biomass, "kb_type")
  area_key <- paste(.mock_normalise_site(biomass$site), biomass$year)
  surveyed <- paste(.mock_normalise_site(fit$data$site), fit$data$year)
  totals <- .mock_totals()
  totals <- totals[paste(totals$site, totals$year) %in% intersect(area_key, surveyed), ]
  i <- match(paste(totals$site, totals$year), area_key)
  rows <- tibble::tibble(
    site = totals$site, year = totals$year, canopy_m2 = totals$canopy_area,
    estimate = totals[[type]], lower = totals[[paste0(type, "_lower")]], upper = totals[[paste0(type, "_upper")]],
    population_size = biomass$population_size[i],
    population_weight = biomass$population_weight[i]
  )
  structure(rows, class = c("kb_biomass", class(rows)), kb_type = type, kb_scale = "total")
}

# Planned. Biomass estimates by year, faceted by site, with compatibility intervals.
kb_plot_biomass <- function(biomass) {
  type <- attr(biomass, "kb_type")
  y_label <- if (attr(biomass, "kb_scale") == "total") {
    c(wet = "Total wet biomass (t)", dry = "Total dry biomass (t)", carbon = "Total carbon (t)")[[type]]
  } else {
    list(
      wet = expression("Wet biomass (kg/m"^2 * ")"),
      dry = expression("Dry biomass (kg/m"^2 * ")"),
      carbon = expression("Carbon (kg/m"^2 * ")")
    )[[type]]
  }
  data <- as.data.frame(biomass)
  data$site <- factor(data$site, levels = .mock_site_levels(data$site))
  data$year <- factor(data$year)
  ggplot(data, aes(.data$year, .data$estimate)) +
    geom_pointrange(aes(ymin = .data$lower, ymax = .data$upper), size = 0.3) +
    facet_wrap(~site) +
    expand_limits(y = 0) +
    labs(x = "Year", y = y_label) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
