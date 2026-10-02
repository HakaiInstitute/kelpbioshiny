# jarl-ignore-file unused_function: the app calls the mocks by built name through kelpbio_fn().
# Mock kelpbio functions -----------------------------------------------------------
#
# These mocks stand in for kelpbio functions. They return fake results from the
# JSON files in inst/extdata/, and draw their figures at run time from those fake
# values. Delete each mock as the real function lands in kelpbio, and this file
# once all of them have.
#
# The app calls these functions unqualified (kb_fit_density_nereo(), tidy(),
# converged()), as it will once they are imported from kelpbio. To switch a
# function over: delete its mock here, add kelpbio to Imports in DESCRIPTION
# (once), and add `@importFrom kelpbio <fun>` to R/namespace.R. Everything with a
# leading dot is mock plumbing and goes with the file, as do the fake-*.json
# files in inst/extdata/ that only the mocks read, the ggplot2 imports below
# and the kb_fit S3 method registrations.
#
# Each mock is marked:
# - Exists: kelpbio has the function today; the mock's signature matches it exactly.
# - Planned: not in kelpbio yet; named and shaped following the package conventions.
#
# The example sheets keep the prototype's column names (diameter, weight,
# density), not kelpbio's unit-suffixed ones (diameter_mm, weight_kg, stipes_m2),
# so the mock data checks follow the sheets.

#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_pointrange facet_wrap
#' @importFrom ggplot2 expand_limits labs theme_bw theme element_text scale_x_log10
#' @noRd
NULL

# Fake values ------------------------------------------------------------------------

.mock_results <- function() read_extdata_json("fake-results")
.mock_predictions <- function() read_extdata_json("fake-predictions")
.mock_sensitivity <- function() read_extdata_json("fake-sensitivity")
.mock_biomass <- function() read_extdata_json("fake-biomass")
.mock_totals <- function() read_extdata_json("fake-totals")

# The fake JSON is keyed by the app's model ids, where biomass_cover is "cover".
.mock_key <- function(model) if (model == "biomass_cover") "cover" else model

# Each model's terms (tidy() rows) and the prior entry (kb_priors_*()) each draws on.
.mock_terms <- list(
  density = c(bDensity = "intercept", bDispersion = "dispersion", sSite = "sd_site", sYear = "sd_year"),
  size = c(bDiameter = "intercept", bShape = "shape", sSite = "sd_site", sYear = "sd_year", sSiteYear = "sd_site_year"),
  weight = c(
    bWeight = "intercept", bPower = "power", bFloor = "floor", bDensity = "density",
    sSite = "sd_site", sYear = "sd_year", sSiteYear = "sd_site_year", sWeight = "sd_residual"
  ),
  blade = c(bBlade = "intercept", sSite = "sd_site"),
  wetdry = c(bWetDry = "intercept", sSite = "sd_site"),
  carbon = c(bCarbon = "intercept", sSite = "sd_site"),
  biomass_cover = c(bCover0 = "intercept", bCover = "cover", sSite = "sd_site", sCover = "sd_residual")
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
      data.frame(rhat = 1.001 + i * 0.002, ess_bulk = 1250 - i * 140)
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

# Simulated sampling time, in ms, at 1000 draws per chain without thinning.
.mock_fit_ms <- c(density = 4000, size = 5000, weight = 6000, blade = 4000, wetdry = 4000, carbon = 4000, biomass_cover = 4000)

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
                          progress_dir = NULL, source = "user", reference = NULL) {
  defaults <- get(sprintf("kb_priors_%s_%s", model, species))()
  diagnostics <- .mock_diagnostics(model)
  ndraws <- chains * niters
  diagnostics$rhat <- 1 + (diagnostics$rhat - 1) / nthin
  diagnostics$ess_bulk <- round(diagnostics$ess_bulk * nthin * ndraws / 4000)
  diagnostics$ess_tail <- round(diagnostics$ess_tail * nthin * ndraws / 4000)
  if (!is.null(progress_dir)) {
    total <- ceiling(.mock_fit_ms[[model]] * niters / 1000 * nthin / .mock_poll_ms)
    saveRDS(list(polls = 0, total = total), file.path(progress_dir, "mock-progress.rds"))
  }
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

# Data checks --------------------------------------------------------------------------
# A check returns the data invisibly, aborts on data it cannot use and warns about
# rows it will drop. The biomass:cover check also reports site-years without plot cover.

.mock_check_data <- function(data, columns, x_name) {
  chk::chk_data(data, x_name = x_name)
  chk::chk_superset(names(data), columns, x_name = x_name)
  for (column in c("site", "year")) {
    missing <- sum(is.na(data[[column]]))
    if (missing > 0) {
      warning(sprintf("%d rows with missing %s will be dropped", missing, column), call. = FALSE)
    }
  }
  invisible(data)
}

.mock_check_cover <- function(data, x_name) {
  .mock_check_data(data, c("site", "year", "canopy_area", "plot", "plot_percent_cover"), x_name)
  key <- paste(data$site, data$year)
  if (any(tapply(data$canopy_area, key, function(x) length(unique(x))) > 1)) {
    stop(sprintf("`%s$canopy_area` must be the same on every row of a site-year.", x_name), call. = FALSE)
  }
  cover <- data$plot_percent_cover
  if (any(!is.na(cover) & (cover < 0 | cover > 100))) {
    stop(sprintf("`%s$plot_percent_cover` must be between 0 and 100.", x_name), call. = FALSE)
  }
  gaps <- sum(!tapply(!is.na(cover), key, any))
  if (gaps > 0) {
    message(sprintf("%d site-years have canopy area but no plot cover; totals use the population-level relationship.", gaps))
  }
  invisible(data)
}

# Exists.
kb_check_data_density_nereo <- function(data, x_name = chk::deparse_backtick_chk(substitute(data))) {
  .mock_check_data(data, c("site", "year", "quadrat", "density"), x_name)
}
# Exists.
kb_check_data_density_macro <- kb_check_data_density_nereo
# Exists.
kb_check_data_size_nereo <- function(data, x_name = chk::deparse_backtick_chk(substitute(data))) {
  .mock_check_data(data, c("site", "year", "diameter"), x_name)
}
# Exists.
kb_check_data_size_macro <- kb_check_data_size_nereo
# Exists.
kb_check_data_weight_nereo <- function(data, x_name = chk::deparse_backtick_chk(substitute(data))) {
  .mock_check_data(data, c("site", "year", "diameter", "weight"), x_name)
}
# Exists.
kb_check_data_weight_macro <- kb_check_data_weight_nereo
# Planned.
kb_check_data_blade_nereo <- function(data, x_name = chk::deparse_backtick_chk(substitute(data))) {
  .mock_check_data(data, c("site", "year", "blade_weight", "total_weight"), x_name)
}
# Planned.
kb_check_data_blade_macro <- kb_check_data_blade_nereo
# Planned.
kb_check_data_wetdry_nereo <- function(data, x_name = chk::deparse_backtick_chk(substitute(data))) {
  .mock_check_data(data, c("site", "year", "wet_weight", "dry_weight"), x_name)
}
# Planned.
kb_check_data_wetdry_macro <- kb_check_data_wetdry_nereo
# Planned.
kb_check_data_carbon_nereo <- function(data, x_name = chk::deparse_backtick_chk(substitute(data))) {
  .mock_check_data(data, c("site", "year", "dry_weight", "carbon"), x_name)
}
# Planned.
kb_check_data_carbon_macro <- kb_check_data_carbon_nereo
# Planned.
kb_check_data_biomass_cover_nereo <- function(data, x_name = chk::deparse_backtick_chk(substitute(data))) {
  .mock_check_cover(data, x_name)
}
# Planned.
kb_check_data_biomass_cover_macro <- kb_check_data_biomass_cover_nereo

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

.mock_sd_priors <- function() {
  list(
    sd_site = kb_prior_exponential(rate = 1),
    sd_year = kb_prior_exponential(rate = 1),
    sd_site_year = kb_prior_exponential(rate = 1)
  )
}

# Exists.
kb_priors_density_nereo <- function() {
  list(
    intercept = kb_prior_normal(mean = 0, sd = 2),
    zero_inflation = kb_prior_normal(mean = 0, sd = 2),
    dispersion = kb_prior_exponential(rate = 1),
    sd_site = kb_prior_exponential(rate = 1),
    sd_year = kb_prior_exponential(rate = 1),
    sd_site_year = kb_prior_exponential(rate = 1)
  )
}
# Exists.
kb_priors_density_macro <- function() {
  c(list(intercept = kb_prior_normal(mean = 0, sd = 2), dispersion = kb_prior_exponential(rate = 1)), .mock_sd_priors())
}
# Exists.
kb_priors_size_nereo <- function() {
  c(list(intercept = kb_prior_normal(mean = 0, sd = 2), shape = kb_prior_exponential(rate = 0.1)), .mock_sd_priors())
}
# Exists.
kb_priors_size_macro <- function() {
  c(list(intercept = kb_prior_normal(mean = 0, sd = 2), dispersion = kb_prior_exponential(rate = 1)), .mock_sd_priors())
}
# Exists.
kb_priors_weight_nereo <- function() {
  c(
    list(
      intercept = kb_prior_normal(mean = 0, sd = 2),
      power = kb_prior_normal(mean = 2, sd = 1),
      floor = kb_prior_normal(mean = 0, sd = 0.5),
      density = kb_prior_normal(mean = 0, sd = 0.5)
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
      fronds = kb_prior_normal(mean = 1, sd = 0.5),
      shape = kb_prior_exponential(rate = 0.1)
    ),
    .mock_sd_priors()
  )
}
# Planned.
kb_priors_blade_nereo <- function() {
  list(intercept = kb_prior_normal(mean = 0, sd = 1), sd_site = kb_prior_exponential(rate = 1))
}
# Planned.
kb_priors_blade_macro <- kb_priors_blade_nereo
# Planned.
kb_priors_wetdry_nereo <- function() {
  list(intercept = kb_prior_normal(mean = -2, sd = 1), sd_site = kb_prior_exponential(rate = 1))
}
# Planned.
kb_priors_wetdry_macro <- kb_priors_wetdry_nereo
# Planned.
kb_priors_carbon_nereo <- function() {
  list(intercept = kb_prior_normal(mean = -1, sd = 1), sd_site = kb_prior_exponential(rate = 1))
}
# Planned.
kb_priors_carbon_macro <- kb_priors_carbon_nereo
# Planned.
kb_priors_biomass_cover_nereo <- function() {
  list(
    intercept = kb_prior_normal(mean = 0, sd = 2),
    cover = kb_prior_normal(mean = 0, sd = 1),
    sd_site = kb_prior_exponential(rate = 1),
    sd_residual = kb_prior_exponential(rate = 1)
  )
}
# Planned.
kb_priors_biomass_cover_macro <- kb_priors_biomass_cover_nereo

# Model order ----------------------------------------------------------------------------

# Planned. The models whose fits each model's fit needs first, by model name:
# weight takes the density estimates, and biomass:cover is fitted to the biomass
# predictions, so it needs every model that combines into biomass. Pre-fit models
# need nothing.
kb_model_dependencies <- function() {
  list(
    density = character(),
    size = character(),
    weight = "density",
    blade = character(),
    wetdry = character(),
    carbon = character(),
    biomass_cover = c("density", "size", "weight", "blade", "wetdry", "carbon")
  )
}

# Fitting -----------------------------------------------------------------------------------
# The mock fits return at once. kb_fit_progress() then reports simulated sampling
# progress for the fit's progress_dir.

# Exists.
kb_fit_density_nereo <- function(data, priors = NULL, ..., prior_only = FALSE, chains = 4L, niters = 1000L,
                                 nthin = 1L, cores = NULL, seed = NULL, progress = c("bar", "verbose", "none"),
                                 progress_dir = NULL) {
  .mock_new_fit("density", "nereo", data, priors, chains, niters, nthin, progress_dir)
}
# Exists.
kb_fit_density_macro <- function(data, priors = NULL, ..., prior_only = FALSE, chains = 4L, niters = 1000L,
                                 nthin = 1L, cores = NULL, seed = NULL, progress = c("bar", "verbose", "none"),
                                 progress_dir = NULL) {
  .mock_new_fit("density", "macro", data, priors, chains, niters, nthin, progress_dir)
}
# Exists.
kb_fit_size_nereo <- function(data, priors = NULL, ..., prior_only = FALSE, chains = 4L, niters = 1000L,
                              nthin = 1L, cores = NULL, seed = NULL, progress = c("bar", "verbose", "none"),
                              progress_dir = NULL) {
  .mock_new_fit("size", "nereo", data, priors, chains, niters, nthin, progress_dir)
}
# Exists.
kb_fit_size_macro <- function(data, priors = NULL, ..., prior_only = FALSE, chains = 4L, niters = 1000L,
                              nthin = 1L, cores = NULL, seed = NULL, progress = c("bar", "verbose", "none"),
                              progress_dir = NULL) {
  .mock_new_fit("size", "macro", data, priors, chains, niters, nthin, progress_dir)
}
# Exists.
kb_fit_weight_nereo <- function(data, priors = NULL, form = c("packard_floor", "power"), ..., prior_only = FALSE,
                                chains = 4L, niters = 1000L, nthin = 1L, cores = NULL, seed = NULL,
                                progress = c("bar", "verbose", "none"), progress_dir = NULL) {
  .mock_new_fit("weight", "nereo", data, priors, chains, niters, nthin, progress_dir)
}
# Exists.
kb_fit_weight_macro <- function(data, priors = NULL, ..., prior_only = FALSE, chains = 4L, niters = 1000L,
                                nthin = 1L, cores = NULL, seed = NULL, progress = c("bar", "verbose", "none"),
                                progress_dir = NULL) {
  .mock_new_fit("weight", "macro", data, priors, chains, niters, nthin, progress_dir)
}
# Planned.
kb_fit_blade_nereo <- function(data, priors = NULL, ..., prior_only = FALSE, chains = 4L, niters = 1000L,
                               nthin = 1L, cores = NULL, seed = NULL, progress = c("bar", "verbose", "none"),
                               progress_dir = NULL) {
  .mock_new_fit("blade", "nereo", data, priors, chains, niters, nthin, progress_dir)
}
# Planned.
kb_fit_blade_macro <- function(data, priors = NULL, ..., prior_only = FALSE, chains = 4L, niters = 1000L,
                               nthin = 1L, cores = NULL, seed = NULL, progress = c("bar", "verbose", "none"),
                               progress_dir = NULL) {
  .mock_new_fit("blade", "macro", data, priors, chains, niters, nthin, progress_dir)
}
# Planned.
kb_fit_wetdry_nereo <- function(data, priors = NULL, ..., prior_only = FALSE, chains = 4L, niters = 1000L,
                                nthin = 1L, cores = NULL, seed = NULL, progress = c("bar", "verbose", "none"),
                                progress_dir = NULL) {
  .mock_new_fit("wetdry", "nereo", data, priors, chains, niters, nthin, progress_dir)
}
# Planned.
kb_fit_wetdry_macro <- function(data, priors = NULL, ..., prior_only = FALSE, chains = 4L, niters = 1000L,
                                nthin = 1L, cores = NULL, seed = NULL, progress = c("bar", "verbose", "none"),
                                progress_dir = NULL) {
  .mock_new_fit("wetdry", "macro", data, priors, chains, niters, nthin, progress_dir)
}
# Planned.
kb_fit_carbon_nereo <- function(data, priors = NULL, ..., prior_only = FALSE, chains = 4L, niters = 1000L,
                                nthin = 1L, cores = NULL, seed = NULL, progress = c("bar", "verbose", "none"),
                                progress_dir = NULL) {
  .mock_new_fit("carbon", "nereo", data, priors, chains, niters, nthin, progress_dir)
}
# Planned.
kb_fit_carbon_macro <- function(data, priors = NULL, ..., prior_only = FALSE, chains = 4L, niters = 1000L,
                                nthin = 1L, cores = NULL, seed = NULL, progress = c("bar", "verbose", "none"),
                                progress_dir = NULL) {
  .mock_new_fit("carbon", "macro", data, priors, chains, niters, nthin, progress_dir)
}

# The response of the biomass:cover model: each plot's wet biomass per unit area,
# from the biomass predictions, fake here as scatter around the fake cover curves.
.mock_cover_response <- function(data) {
  plots <- data[!is.na(data$plot_percent_cover), ]
  site <- .mock_predictions()$cover$site
  at_50 <- site$estimate[match(plots$site, site$site)]
  noise <- .mock_with_seed(42, stats::rlnorm(nrow(plots), 0, 0.25))
  plots$biomass_kg_m2 <- signif(at_50 * exp(0.018 * (plots$plot_percent_cover - 50)) * noise, 3)
  plots
}

# Planned. `biomass` is the per-unit-area output of kb_predict_biomass() for the
# site-years in `data`; it is name-only so priors stays second.
kb_fit_biomass_cover_nereo <- function(data, priors = NULL, ..., biomass, prior_only = FALSE, chains = 4L,
                                       niters = 1000L, nthin = 1L, cores = NULL, seed = NULL,
                                       progress = c("bar", "verbose", "none"), progress_dir = NULL) {
  .mock_new_fit("biomass_cover", "nereo", .mock_cover_response(data), priors, chains, niters, nthin, progress_dir)
}
# Planned.
kb_fit_biomass_cover_macro <- function(data, priors = NULL, ..., biomass, prior_only = FALSE, chains = 4L,
                                       niters = 1000L, nthin = 1L, cores = NULL, seed = NULL,
                                       progress = c("bar", "verbose", "none"), progress_dir = NULL) {
  .mock_new_fit("biomass_cover", "macro", .mock_cover_response(data), priors, chains, niters, nthin, progress_dir)
}

# The app polls kb_fit_progress() every 200 ms. The mock advances the simulated
# fit by one poll per call, so fits run on the app's clock, including under
# testServer(), where time is simulated.
.mock_poll_ms <- 200

# Exists.
kb_fit_progress <- function(progress_dir) {
  chk::chk_string(progress_dir)
  file <- file.path(progress_dir, "mock-progress.rds")
  if (!file.exists(file)) {
    return(0)
  }
  record <- readRDS(file)
  record$polls <- record$polls + 1
  saveRDS(record, file)
  min(record$polls / record$total, 1)
}

# Pre-fit models --------------------------------------------------------------------------
# Planned. The real pre-fit models will be stored fits in kelpbiodata (for example
# kelpbiodata::fit_weight_nereo_coastwide); these accessors stand in for them.
# They were thinned when built, so they converged.

.mock_prefit <- function(model, species, reference) {
  .mock_new_fit(model, species, data.frame(), NULL, nthin = 5L, source = "prefit", reference = reference)
}

# Planned.
kb_prefit_size_nereo <- function() .mock_prefit("size", "nereo", "coastwide")
# Planned.
kb_prefit_size_macro <- function() .mock_prefit("size", "macro", "coastwide")
# Planned.
kb_prefit_weight_nereo <- function() .mock_prefit("weight", "nereo", "coastwide")
# Planned.
kb_prefit_weight_macro <- function() .mock_prefit("weight", "macro", "coastwide")
# Planned.
kb_prefit_wetdry_nereo <- function() .mock_prefit("wetdry", "nereo", "hakai")
# Planned.
kb_prefit_wetdry_macro <- function() .mock_prefit("wetdry", "macro", "hakai")
# Planned.
kb_prefit_carbon_nereo <- function() .mock_prefit("carbon", "nereo", "hakai")
# Planned.
kb_prefit_carbon_macro <- function() .mock_prefit("carbon", "macro", "hakai")

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

# Exists.
#' @exportS3Method summary kb_fit
#' @noRd
summary.kb_fit <- function(object, ..., conf_level = 0.95, estimate = stats::median, sig_fig = 3,
                           include_random_effects = FALSE) {
  coefficients <- tidy(object, sig_fig = sig_fig)
  diagnostics <- object$diagnostics$summary
  i <- match(coefficients$term, diagnostics$variable)
  coefficients$rhat <- round(diagnostics$rhat[i], 3)
  coefficients$ess_bulk <- round(diagnostics$ess_bulk[i])
  coefficients$ess_tail <- round(diagnostics$ess_tail[i])
  structure(list(model = object$meta$model, coefficients = coefficients), class = "summary_kb_fit")
}

# Exists.
#' @exportS3Method universals::rhat
#' @noRd
rhat.kb_fit <- function(x, ...) {
  stats::setNames(x$diagnostics$summary$rhat, x$diagnostics$summary$variable)
}

# Exists.
#' @exportS3Method universals::esr
#' @noRd
esr.kb_fit <- function(x, ...) {
  stats::setNames(x$diagnostics$summary$ess_bulk / x$meta$ndraws, x$diagnostics$summary$variable)
}

# Exists.
#' @exportS3Method universals::converged
#' @noRd
converged.kb_fit <- function(x, ..., rhat = 1.01, esr = 0.1, max_perc_divergent = 0.2) {
  all(rhat.kb_fit(x) < rhat) && all(esr.kb_fit(x) > esr) && x$diagnostics$perc_divergent <= max_perc_divergent
}

# Exists.
#' @exportS3Method generics::glance
#' @noRd
glance.kb_fit <- function(x, ..., rhat = 1.01, esr = 0.1, max_perc_divergent = 0.2) {
  s <- x$diagnostics$summary
  is_converged <- converged.kb_fit(x, rhat = rhat, esr = esr, max_perc_divergent = max_perc_divergent)
  tibble::tibble(
    n = nrow(x$data), K = nrow(s), nchains = x$meta$chains, niters = x$meta$niters, nthin = x$meta$nthin,
    ess = min(s$ess_bulk), rhat = max(s$rhat), perc_divergent = x$diagnostics$perc_divergent,
    converged = is_converged
  )
}

# Exists. The mock gives fitted values and residuals for the biomass:cover model
# only; the app plots its plots over the predictions.
#' @exportS3Method generics::augment
#' @noRd
augment.kb_fit <- function(x, ...) {
  data <- x$data
  if (x$meta$model == "biomass_cover") {
    site <- .mock_predictions()$cover$site
    data$fitted <- site$estimate[match(data$site, site$site)] * exp(0.018 * (data$plot_percent_cover - 50))
    data$residual <- log(data$biomass_kg_m2 / data$fitted) / 0.25
  }
  data
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
    "Response: individuals per square metre (quadrat counts)",
    "",
    "Likelihood (placeholder, prototype only)",
    "  count ~ NegBinomial(mu, bDispersion)",
    "  log(mu) = bDensity + bSite[site] + bYear[year]",
    "",
    "Random effects",
    "  bSite[site] ~ Normal(0, sSite)",
    "  bYear[year] ~ Normal(0, sYear)",
    "",
    "Priors",
    "  bDensity    ~ Normal(0, 2)",
    "  bDispersion ~ Exponential(1)",
    "  sSite       ~ Exponential(1)",
    "  sYear       ~ Exponential(1)",
    sep = "\n"
  ),
  size = paste(
    "Size distribution - Nereocystis luetkeana",
    "Response: sub-bulb diameter (mm)",
    "",
    "Likelihood (placeholder, prototype only)",
    "  diameter ~ Gamma(bShape, bShape / mu)",
    "  log(mu) = bDiameter + bSite[site] + bYear[year] + bSiteYear[site, year]",
    "",
    "Random effects",
    "  bSite[site]           ~ Normal(0, sSite)",
    "  bYear[year]           ~ Normal(0, sYear)",
    "  bSiteYear[site, year] ~ Normal(0, sSiteYear)",
    "",
    "Priors",
    "  bDiameter ~ Normal(0, 2)",
    "  bShape    ~ Normal(2, 1) T[0, ]",
    "  sSite     ~ Exponential(1)",
    "  sYear     ~ Exponential(1)",
    "  sSiteYear ~ Exponential(1)",
    sep = "\n"
  ),
  blade = .mock_fraction_description("Blade fraction", "blade weight / total wet weight", "bBlade"),
  wetdry = .mock_fraction_description("Wet:dry ratio", "dry weight / wet weight", "bWetDry"),
  carbon = .mock_fraction_description("Carbon fraction", "carbon / dry weight", "bCarbon"),
  # Placeholder: the model's exact form is still to be designed in kelpbio.
  biomass_cover = paste(
    "Biomass:cover - Nereocystis luetkeana",
    "Response: plot biomass per unit area, from the biomass predictions",
    "Predictor: plot percent cover, from drone imagery",
    "",
    "Likelihood (placeholder, prototype only)",
    "  log(biomass density) ~ Normal(bCover0 + bCover * percent cover + bSite[site], sCover)",
    "",
    "Random effects",
    "  bSite[site] ~ Normal(0, sSite)",
    "",
    "Priors",
    "  bCover0 ~ Normal(0, 2)",
    "  bCover  ~ Normal(0, 1)",
    "  sSite   ~ Exponential(1)",
    "  sCover  ~ Exponential(1)",
    sep = "\n"
  )
)

# Exists.
kb_model_describe <- function(fit, prose = FALSE) {
  text <- .mock_descriptions()[[fit$meta$model]]
  if (fit$meta$species == "macro") text <- sub("Nereocystis luetkeana", "Macrocystis pyrifera", text, fixed = TRUE)
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
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

# Fake estimates per group, from data/fake-predictions.json; a curve model's are its
# value at the reference predictor value (50 mm, a ratio, 50% cover).
.mock_group_rows <- function(fit, by) {
  key <- .mock_key(fit$meta$model)
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
  population <- if (is.null(.mock_predictions()[[.mock_key(fit$meta$model)]]$year)) "All sites" else "All sites and years"
  .mock_new_predictions(.mock_group_rows(fit, by)$rows, by, response, y_label = y_label, population = population)
}

.mock_rename_x <- function(rows, name) {
  names(rows)[names(rows) == "x"] <- name
  rows
}

# Exists.
kb_predict_density_by <- function(fit, by = NULL, ..., new_levels = c("average", "sample"), conf_level = 0.95,
                                  estimate = stats::median, sig_fig = 3) {
  .mock_point_predictions(fit, by, "density", expression("Density (individuals/m"^2 * ")"))
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

# Planned.
kb_predict_blade_by <- function(fit, by = NULL, ..., new_levels = c("average", "sample"), conf_level = 0.95,
                                estimate = stats::median, sig_fig = 3) {
  .mock_point_predictions(fit, by, "blade_fraction", "Blade fraction")
}

# A ratio model's curve: the response is the predictor times the fraction.
.mock_ratio_predictions <- function(fit, by, at, x, predictor, response, x_label, y_label, table_label) {
  if (length(at) == 1) {
    rows <- .mock_group_rows(fit, by)$rows
    rows[c("estimate", "lower", "upper")] <- lapply(rows[c("estimate", "lower", "upper")], function(v) signif(v * at, 3))
    rows[[predictor]] <- at
    return(.mock_new_predictions(rows, by, response, predictor, y_label = table_label))
  }
  rows <- .mock_curves(fit, by, at %||% x, stats::qlogis, function(x, eta) x * stats::plogis(eta), 0, 0)
  .mock_new_predictions(.mock_rename_x(rows, predictor), by, response, predictor, curve = TRUE, x_label = x_label, y_label = y_label)
}

# Planned.
kb_predict_wetdry_by <- function(fit, by = NULL, wet_weight_kg = NULL, ..., new_levels = c("average", "sample"),
                                 conf_level = 0.95, estimate = stats::median, sig_fig = 3) {
  .mock_ratio_predictions(
    fit, by, wet_weight_kg, seq(0, 8, by = 0.2), "wet_weight_kg", "dry_weight",
    "Wet weight (kg)", "Dry weight (kg)", "Ratio of dry to wet weight"
  )
}

# Planned.
kb_predict_carbon_by <- function(fit, by = NULL, dry_weight_kg = NULL, ..., new_levels = c("average", "sample"),
                                 conf_level = 0.95, estimate = stats::median, sig_fig = 3) {
  .mock_ratio_predictions(
    fit, by, dry_weight_kg, seq(0, 1, by = 0.025), "dry_weight_kg", "carbon",
    "Dry weight (kg)", "Carbon (kg)", "Carbon fraction of dry weight"
  )
}

# Planned.
kb_predict_biomass_cover_by <- function(fit, by = NULL, plot_percent_cover = NULL, ...,
                                        new_levels = c("average", "sample"), conf_level = 0.95,
                                        estimate = stats::median, sig_fig = 3) {
  y_label <- expression("Wet biomass (kg/m"^2 * ")")
  if (length(plot_percent_cover) == 1) {
    rows <- .mock_group_rows(fit, by)$rows
    shift <- exp(0.018 * (plot_percent_cover - 50))
    rows[c("estimate", "lower", "upper")] <- lapply(rows[c("estimate", "lower", "upper")], function(v) signif(v * shift, 3))
    rows$plot_percent_cover <- plot_percent_cover
    return(.mock_new_predictions(rows, by, "biomass", "plot_percent_cover", y_label = y_label))
  }
  x <- plot_percent_cover %||% seq(0, 100, by = 2)
  rows <- .mock_curves(fit, by, x, function(v) log(v) - 0.018 * 50, function(x, eta) exp(eta + 0.018 * x), 55, 0.8)
  .mock_new_predictions(
    .mock_rename_x(rows, "plot_percent_cover"), by, "biomass", "plot_percent_cover",
    curve = TRUE, x_label = "Plot percent cover (%)", y_label = y_label
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
    labs(x = switch(x, group = NULL, site = "Site", "Year"), y = y_label) +
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
  rate <- esr(fit)[estimates$term]
  r <- rhat(fit)[estimates$term]
  iterations <- 250
  chains <- fit$meta$chains
  draws <- .mock_with_seed(11, do.call(rbind, lapply(seq_len(nrow(estimates)), function(i) {
    spread <- (estimates$upper[i] - estimates$lower[i]) / 4
    phi <- (1 - min(rate[[i]], 1)) / (1 + min(rate[[i]], 1))
    do.call(rbind, lapply(seq_len(chains), function(chain) {
      offset <- (chain - (chains + 1) / 2) * (r[[i]] - 1) * 20 * spread
      value <- as.numeric(stats::arima.sim(list(ar = min(phi, 0.98)), iterations, sd = sqrt(1 - min(phi, 0.98)^2)))
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
    density = list(mu = site_year_means(40, 3, log(1.4), 0.35), family = .mock_nb_density(8, 4), x = expression("Density (individuals/m"^2 * ")")),
    size = list(
      mu = site_year_means(40, 12, log(31), 0.18), family = .mock_gamma(6), x = "Sub-bulb diameter (mm)",
      observe = function(mu) {
        big <- stats::runif(length(mu)) < 0.15
        ifelse(big, stats::rgamma(length(mu), 8, 8 / (mu * 2)), stats::rgamma(length(mu), 16, 16 / (mu * 0.9)))
      }
    ),
    weight = list(mu = 0.0006 * stats::rgamma(320, 9, 9 / 32)^2.2, family = .mock_lognormal(0.45), x = "Wet weight (kg)", log = TRUE),
    blade = list(mu = fractions(120, 0.5, 0.3), family = .mock_beta(30), x = "Blade fraction"),
    wetdry = list(mu = fractions(150, 0.1, 0.2), family = .mock_beta(150), x = "Ratio of dry to wet weight"),
    carbon = list(mu = fractions(110, 0.3, 0.15), family = .mock_beta(120), x = "Carbon fraction of dry weight"),
    biomass_cover = list(
      mu = exp(log(0.6) + 0.018 * stats::runif(27, 15, 92)), family = .mock_lognormal(0.35),
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
  rows <- .mock_sensitivity()[[.mock_key(fit$meta$model)]]
  rows$prior <- unname(.mock_terms[[fit$meta$model]][rows$parameter])
  defaults <- get(sprintf("kb_priors_%s_%s", fit$meta$model, fit$meta$species))()
  ratio <- vapply(rows$prior, function(name) {
    if (is.null(defaults[[name]]) || is.null(fit$meta$priors[[name]])) {
      return(1)
    }
    min(1, .mock_prior_scale(defaults[[name]]) / .mock_prior_scale(fit$meta$priors[[name]]))
  }, numeric(1))
  rows$prior_cjs <- round(rows$prior_cjs * ratio, 3)
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

.mock_site_years <- function(data) {
  data <- data[!is.na(data$year), ]
  unique(paste(data$site, data$year, sep = "|"))
}

# Planned. Biomass per unit area (kg/m2) by site-year, combining the sub-model fits.
# Every site-year in the density data gets an estimate; population_size and
# population_weight mark site-years without size or weight data in a fit to your
# data, which use the population-level estimate.
kb_predict_biomass <- function(density, size, weight, blade = NULL, wetdry = NULL, carbon = NULL, ...,
                               by = c("site", "year"), type = c("wet", "dry", "carbon"), conf_level = 0.95,
                               estimate = stats::median, sig_fig = 3) {
  type <- match.arg(type)
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

# Planned. Total biomass (t) per site-year with a canopy area, from a fitted
# biomass:cover model and kb_predict_biomass() output. population_cover marks
# site-years without plot cover, which use the population-level cover relationship;
# the per-area population flags are carried over.
kb_predict_biomass_total <- function(fit, biomass, ..., conf_level = 0.95, estimate = stats::median, sig_fig = 3) {
  type <- attr(biomass, "kb_type")
  area_key <- paste(.mock_normalise_site(biomass$site), biomass$year)
  totals <- .mock_totals()
  totals <- totals[paste(totals$site, totals$year) %in% area_key, ]
  i <- match(paste(totals$site, totals$year), area_key)
  rows <- tibble::tibble(
    site = totals$site, year = totals$year, canopy_area = totals$canopy_area,
    estimate = totals[[type]], lower = totals[[paste0(type, "_lower")]], upper = totals[[paste0(type, "_upper")]],
    population_cover = totals$population,
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
