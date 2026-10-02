# Workflow definitions and the example workbook. Every model result comes from
# kelpbio functions (mocked in R/mock-kelpbio.R); this file holds the app's
# labels, choices and thresholds.

# Pre-fit models, by the reference dataset they were fitted to. A model's pre-fit
# source is "prefit_" and the reference, e.g. "prefit_hakai"; the reference is
# the value of the kelpbio accessor's reference argument
# (kb_prefit_weight_nereo(reference = "hakai")).
prefit_info <- list(
  coastwide = list(label = "Pre-fit coastwide", data = "data compiled from surveys along the coast"),
  hakai = list(label = "Pre-fit Hakai Institute", data = "Hakai Institute survey data")
)

prefit_source <- function(reference) paste0("prefit_", reference)
is_prefit <- function(source) startsWith(source, "prefit_")
prefit_reference <- function(source) sub("^prefit_", "", source)

component_ids <- c("density", "size", "weight", "blade", "wetdry", "carbon", "cover")
names(component_ids) <- component_ids

# A model's sources: "user" (your data), its pre-fit sources, and "none" (not
# used). The first pre-fit source is the default when there is no sheet.
components <- list(
  density = list(
    label = "Density", detail = "Individuals per square metre by site-year", sheet = "density",
    columns = c("site", "year", "quadrat", "density"), sources = "user"
  ),
  size = list(
    label = "Size", detail = "Distribution of sub-bulb diameter", sheet = "size",
    columns = c("site", "year", "diameter"), sources = c("user", prefit_source("hakai"))
  ),
  weight = list(
    label = "Weight", detail = "Wet weight by sub-bulb diameter", sheet = "weight",
    columns = c("site", "year", "diameter", "weight", "density"),
    sources = c("user", prefit_source(c("coastwide", "hakai"))), default = prefit_source("coastwide")
  ),
  blade = list(
    label = "Blade fraction", detail = "Proportion of wet weight in blades", sheet = "blade",
    columns = c("site", "year", "blade_weight", "total_weight"), sources = c("user", "none")
  ),
  wetdry = list(
    label = "Wet:dry", detail = "Ratio of dry to wet weight", sheet = "wetdry",
    columns = c("site", "year", "wet_weight", "dry_weight"), sources = c("user", prefit_source("hakai"), "none")
  ),
  carbon = list(
    label = "Carbon", detail = "Carbon fraction of dry weight", sheet = "carbon",
    columns = c("site", "year", "dry_weight", "carbon"), sources = c("user", prefit_source("hakai"), "none")
  ),
  # Optional: scales biomass per unit area up to a total per site-year. Fitted to
  # the biomass predictions, so after every other model in use.
  cover = list(
    label = "Biomass:cover", detail = "Relates plot biomass per unit area to plot percent cover", sheet = "cover",
    columns = c("site", "year", "canopy_area", "plot", "plot_percent_cover"), sources = c("user", "none"),
    fn = "biomass_cover"
  )
)

# The models that combine into biomass per unit area; the cover model builds on them.
biomass_ids <- setdiff(component_ids, "cover")

# kelpbio model names (as in kb_fit_<model>_<species>()) and the app's model ids.
fn_of <- function(id) components[[id]]$fn %||% id
id_of <- function(model) unname(vapply(model, function(m) if (m == "biomass_cover") "cover" else m, character(1)))

# The kelpbio function for a model and species: kelpbio_fn("fit", "cover", "nereo")
# is kb_fit_biomass_cover_nereo(). In the package these resolve to the functions
# imported from kelpbio.
kelpbio_verbs <- c(check = "check_data", priors = "priors", fit = "fit", prefit = "prefit")
kelpbio_fn <- function(verb, id, species) {
  get(sprintf("kb_%s_%s_%s", kelpbio_verbs[[verb]], fn_of(id), species), mode = "function")
}

# Models a fit to your data needs fitted first, from kb_model_dependencies(),
# leaving out the models not used in this run. Pre-fit models need nothing.
upstream_of <- function(id, sources) {
  if (sources[[id]] != "user") {
    return(character())
  }
  needs <- id_of(kb_model_dependencies()[[fn_of(id)]])
  needs[sources[needs] != "none"]
}

# A sheet is required when the model can only be fitted to your data.
sheet_required <- function(id) identical(components[[id]]$sources, "user")
required_sheets <- function() Filter(sheet_required, component_ids)

label_of <- function(id) components[[id]]$label
lower_label <- function(id) tolower(label_of(id))

source_label <- function(source) {
  if (is_prefit(source)) {
    return(prefit_info[[prefit_reference(source)]]$label)
  }
  switch(source,
    user = "Your data",
    none = "Not used"
  )
}

source_labels <- function(sources) vapply(sources, source_label, character(1))

# For use mid-sentence: "pre-fit Hakai Institute", "not used".
source_note <- function(source) {
  label <- source_label(source)
  paste0(tolower(substr(label, 1, 1)), substring(label, 2))
}

species_info <- list(
  nereo = list(latin = "Nereocystis luetkeana", common = "bull kelp", suffix = "nereo"),
  macro = list(latin = "Macrocystis pyrifera", common = "giant kelp", suffix = "macro")
)

output_info <- list(
  wet = list(label = "Wet biomass", unit = "kg/m\u00b2"),
  dry = list(label = "Dry biomass", unit = "kg/m\u00b2"),
  carbon = list(label = "Carbon", unit = "kg/m\u00b2")
)

output_components <- list(
  wet = c("density", "size", "weight", "blade"),
  dry = c("density", "size", "weight", "blade", "wetdry"),
  carbon = component_ids
)

default_source <- function(id, has_sheet) {
  options <- components[[id]]$sources
  if (!is.null(components[[id]]$default)) {
    return(components[[id]]$default)
  }
  if (has_sheet && "user" %in% options) {
    return("user")
  }
  prefit <- options[is_prefit(options)]
  if (length(prefit) > 0) {
    return(prefit[[1]])
  }
  "none"
}

default_sources <- function(sheets) {
  vapply(component_ids, function(id) default_source(id, !is.null(sheets[[id]])), character(1))
}

# Example workbook ------------------------------------------------------------------

# A JSON file from inst/extdata, read once per R session.
json_cache <- new.env(parent = emptyenv())
read_extdata_json <- function(name) {
  if (is.null(json_cache[[name]])) {
    path <- system.file("extdata", paste0(name, ".json"), package = "kelpbioshiny", mustWork = TRUE)
    json_cache[[name]] <- jsonlite::fromJSON(path)
  }
  json_cache[[name]]
}

density_rows <- function() {
  raw <- read_extdata_json("fake-density")
  raw$site[raw$site == "site1" & raw$year == 2022] <- "Site 1"
  raw$year <- as.character(raw$year)
  raw[c("site", "year", "quadrat", "density")]
}

# Size and weight sheets run 2019-2025.
size_rows <- function() {
  rows <- read_extdata_json("fake-size")
  rows[!(rows$site == "site8" & rows$year == "2021"), ]
}

weight_rows <- function() {
  rows <- read_extdata_json("fake-weight")
  rows$year[c(42, 318)] <- NA
  rows
}

# Park-Miller generator, so the fraction sheets are the same in every session.
seeded <- function(seed) {
  s <- seed
  function() {
    s <<- (s * 16807) %% 2147483647
    (s - 1) / 2147483646
  }
}

fake_rows <- function(id) {
  rand <- seeded(nchar(id) * 97 + 11)
  grid <- expand.grid(rep = 1:3, year = c("2019", "2020"), site = paste0("site", 1:6), stringsAsFactors = FALSE)
  rows <- lapply(seq_len(nrow(grid)), function(i) {
    wet <- round(0.5 + rand() * 3, 2)
    key <- data.frame(site = grid$site[i], year = grid$year[i])
    if (id == "blade") {
      return(cbind(key, blade_weight = round(wet * (0.4 + rand() * 0.2), 2), total_weight = wet))
    }
    if (id == "wetdry") {
      return(cbind(key, wet_weight = wet, dry_weight = round(wet * (0.08 + rand() * 0.04), 3)))
    }
    dry <- round(wet * 0.1, 3)
    cbind(key, dry_weight = dry, carbon = round(dry * (0.27 + rand() * 0.06), 4))
  })
  do.call(rbind, rows)
}

# Drone canopy area and plot percent cover for a subset of site-years.
cover_rows <- function() read_extdata_json("fake-cover")

# Site-years with a canopy area but no plot cover.
no_plot_cover <- function(rows) {
  has_cover <- tapply(!is.na(rows$plot_percent_cover), paste(rows$site, rows$year, sep = "|"), any)
  names(has_cover)[!has_cover]
}

# The result of the model's kelpbio data check: an error, a warning about rows
# that will be dropped, or a pass, plus any note the check reports.
check_sheet <- function(id, rows, species) {
  warnings <- character()
  notes <- character()
  result <- tryCatch(
    withCallingHandlers(
      kelpbio_fn("check", id, species)(rows),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      },
      message = function(m) {
        notes <<- c(notes, trimws(conditionMessage(m)))
        invokeRestart("muffleMessage")
      }
    ),
    error = function(e) e
  )
  validation <- if (inherits(result, "error")) {
    list(level = "error", message = conditionMessage(result))
  } else if (length(warnings) > 0) {
    list(level = "warning", message = warnings[1])
  } else {
    list(level = "ok", message = "All checks passed")
  }
  list(validation = validation, note = if (length(notes) > 0) paste(notes, collapse = " "))
}

example_sheet <- function(id, file, species = "nereo") {
  sheet <- list(component = id, name = components[[id]]$sheet, file = file)
  sheet$rows <- switch(id,
    density = density_rows(),
    size = size_rows(),
    weight = weight_rows(),
    cover = cover_rows(),
    fake_rows(id)
  )
  check <- check_sheet(id, sheet$rows, species)
  sheet$validation <- check$validation
  sheet$note <- check$note
  if (id == "cover") {
    sheet$checks <- "Canopy area is the same on every row of a site-year; plot percent cover is between 0 and 100 or missing."
  }
  sheet
}

example_workbook <- "example-nereo.xlsx"
example_workbook_sheets <- c("density", "size", "weight", "cover")

site_years <- function(rows) {
  rows <- rows[!is.na(rows$year), ]
  unique(paste(rows$site, rows$year, sep = "|"))
}

normalise_site <- function(site) gsub("[[:space:]_-]", "", tolower(site))

# Priors, sampler and convergence ---------------------------------------------------
# The prior editor works on a table built from the model's kb_priors_*() list: one
# row per entry, with its family and hyperparameters (a: mean or rate, b: SD).

prior_labels <- c(
  intercept = "Intercept", zero_inflation = "Zero inflation (logit)", dispersion = "Dispersion",
  shape = "Shape", power = "Power", floor = "Floor", density = "Density", fronds = "Fronds",
  cover = "Cover slope", sd_site = "SD of site effect", sd_year = "SD of year effect",
  sd_site_year = "SD of site-year effect", sd_residual = "Residual SD"
)

prior_label <- function(id, name) {
  if (name == "intercept" && id %in% c("blade", "wetdry", "carbon")) {
    return("Intercept (logit)")
  }
  if (name %in% names(prior_labels)) prior_labels[[name]] else name
}

default_priors <- function(id, species = "nereo") {
  priors <- kelpbio_fn("priors", id, species)()
  rows <- lapply(names(priors), function(name) {
    prior <- priors[[name]]
    normal <- inherits(prior, "kb_prior_normal")
    data.frame(
      name = name, label = prior_label(id, name), family = if (normal) "normal" else "exponential",
      a = if (normal) prior$mean else prior$rate, b = if (normal) prior$sd else NA_real_
    )
  })
  do.call(rbind, rows)
}

# The edited table as the priors list a kelpbio fit takes.
prior_list <- function(rows) {
  priors <- lapply(seq_len(nrow(rows)), function(i) {
    if (rows$family[i] == "normal") kb_prior_normal(rows$a[i], rows$b[i]) else kb_prior_exponential(rows$a[i])
  })
  stats::setNames(priors, rows$name)
}

format_prior <- function(prior) {
  ifelse(
    prior$family == "normal",
    sprintf("%s ~ Normal(%s, %s)", prior$name, as.character(prior$a), as.character(prior$b)),
    sprintf("%s ~ Exponential(%s)", prior$name, as.character(prior$a))
  )
}

default_sampler <- function() list(chains = 4, iterations = 1000, thin = 1)

# Thresholds passed to kelpbio. A parameter is flagged when its R-hat is above
# RHAT_MAX or its effective sample rate (ESS divided by the draws) is below ESR_MIN;
# a prior is informative when its prior CJS is above PRIOR_CJS_MAX, and the data say
# little about a parameter when its likelihood CJS is below LIK_CJS_MIN.
RHAT_MAX <- 1.05
ESR_MIN <- 0.1
PRIOR_CJS_MAX <- 0.1
LIK_CJS_MIN <- 0.05

is_converged <- function(fit) !is.null(fit) && converged(fit, rhat = RHAT_MAX, esr = ESR_MIN)

sensitivity <- function(fit) kb_sensitivity(fit, prior_cjs = PRIOR_CJS_MAX, lik_cjs = LIK_CJS_MIN)

# Predictions ---------------------------------------------------------------------

prediction_groupings <- c(population = "Population level", site = "By site", year = "By year", site_year = "By site and year")

# What each model predicts. `along` names the predictor of a curve model; its
# table, and its by-site-and-year figure, give the value at `at`.
prediction_info <- list(
  density = list(response = "density", table = "Density (individuals/m\u00b2)"),
  size = list(response = "mean sub-bulb diameter", table = "Mean sub-bulb diameter (mm)"),
  weight = list(
    response = "wet weight", along = "sub-bulb diameter", at = "a plant with a 50 mm sub-bulb diameter",
    table = "Wet weight (kg) of a plant with a 50 mm sub-bulb diameter"
  ),
  blade = list(response = "blade fraction", table = "Blade fraction"),
  wetdry = list(response = "dry weight", along = "wet weight", table = "Ratio of dry to wet weight"),
  carbon = list(response = "carbon", along = "dry weight", table = "Carbon fraction of dry weight"),
  cover = list(
    response = "wet biomass per square metre", along = "plot percent cover",
    table = "Wet biomass (kg/m\u00b2) at 50% plot cover", note = " Points are plots."
  )
)

has_year_effect <- function(id, species = "nereo") "sd_year" %in% names(kelpbio_fn("priors", id, species)())

# The groupings a model's predictions can be shown at. A pre-fit model was
# fitted to other sites and years, so only its population-level predictions apply.
prediction_choices <- function(id, prefit = FALSE, species = "nereo") {
  if (prefit) {
    return(prediction_groupings["population"])
  }
  if (has_year_effect(id, species)) prediction_groupings else prediction_groupings[c("population", "site")]
}

prediction_by <- list(population = NULL, site = "site", year = "year", site_year = c("site", "year"))

# A model's predictions at a grouping: curves along the predictor for the figure,
# or, for the table (at = TRUE) and the by-site-and-year figure, the value at a
# reference predictor value: a 50 mm plant, 1 kg (giving the ratio), 50% cover.
model_predictions <- function(id, fit, grouping, at = FALSE) {
  by <- prediction_by[[grouping]]
  at <- at || grouping == "site_year"
  switch(id,
    density = kb_predict_density_by(fit, by),
    size = kb_predict_size_by(fit, by),
    weight = kb_predict_weight_by(fit, by, diameter_mm = if (at) 50),
    blade = kb_predict_blade_by(fit, by),
    wetdry = kb_predict_wetdry_by(fit, by, wet_weight_kg = if (at) 1),
    carbon = kb_predict_carbon_by(fit, by, dry_weight_kg = if (at) 1),
    cover = kb_predict_biomass_cover_by(fit, by, plot_percent_cover = if (at) 50)
  )
}

# A predictions figure's height as a share of its width.
prediction_aspect <- function(id, grouping) {
  curve <- !is.null(prediction_info[[id]]$along) && grouping != "site_year"
  if (grouping == "population") {
    return(if (curve) 4 / 6 else 3.5 / 4)
  }
  if (grouping == "site_year") {
    return(6 / 8)
  }
  if (id == "cover") {
    return(4.5 / 8)
  }
  if (curve) 6 / 8 else 4 / 8
}

prediction_caption <- function(id, grouping) {
  info <- prediction_info[[id]]
  year <- has_year_effect(id)
  curve <- !is.null(info$along)
  what <- if (curve) paste(info$response, "by", info$along) else info$response
  if (grouping == "site_year" && curve) what <- paste(info$response, "of", info$at)
  by <- switch(grouping,
    population = paste(" for", if (year) "a typical site and year" else "a typical site"),
    site = paste0(if (curve) ", faceted by site" else " by site", if (year) ", for a typical year"),
    year = paste0(if (curve) ", faceted by year" else " by year", ", for a typical site"),
    site_year = " by year, faceted by site"
  )
  sprintf("Predicted %s%s, with 95%% compatibility intervals.%s", what, by, info$note %||% "")
}
