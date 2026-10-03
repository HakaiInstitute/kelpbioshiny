# Workflow definitions. Every model result comes from kelpbio functions (mocked in
# R/mock-kelpbio.R); this file holds the app's labels and choices.

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
# `columns` are the sheet's required columns, by species where they differ.
components <- list(
  density = list(
    label = "Density", detail = "Stipes per square metre by site-year", sheet = "density",
    columns = list(nereo = c("site", "year", "stipes", "area_m2"), macro = c("site", "year", "plants", "area_m2")),
    sources = "user"
  ),
  size = list(
    label = "Size", detail = "Distribution of plant size", sheet = "size",
    columns = list(nereo = c("site", "year", "diameter_mm"), macro = c("site", "year", "fronds")),
    sources = c("user", prefit_source("hakai"))
  ),
  weight = list(
    label = "Weight", detail = "Wet weight by plant size", sheet = "weight",
    columns = list(nereo = c("site", "year", "diameter_mm", "weight_kg"), macro = c("site", "year", "fronds", "weight_kg")),
    sources = c("user", prefit_source(c("coastwide", "hakai")))
  ),
  blade = list(
    label = "Blade fraction", detail = "Proportion of wet weight in blades", sheet = "blade",
    columns = c("site", "year", "blade_weight_kg", "total_weight_kg"), sources = c("user", "none")
  ),
  wetdry = list(
    label = "Wet:dry", detail = "Ratio of dry to wet mass", sheet = "wetdry",
    columns = c("wet_mass_g", "dry_mass_g"), sources = c("user", prefit_source("hakai"), "none")
  ),
  carbon = list(
    label = "Carbon", detail = "Carbon fraction of dry mass", sheet = "carbon",
    columns = c("site", "year", "dry_mass_g", "carbon_mass_g"), sources = c("user", prefit_source("hakai"), "none")
  ),
  # Optional: scales biomass per unit area up to total biomass per site-year.
  # Fitted to the biomass predictions, so once every other model in use is ready.
  cover = list(
    label = "Cover", detail = "Relates plot biomass per unit area to plot percent cover", sheet = "cover",
    columns = c("site", "year", "canopy_area_m2", "plot", "plot_percent_cover"), sources = c("user", "none"),
    fn = "biomass_cover"
  )
)

sheet_columns <- function(id, species) {
  columns <- components[[id]]$columns
  if (is.list(columns)) columns[[species]] else columns
}

# The models that combine into biomass per unit area; the cover model builds on them.
biomass_ids <- component_ids[component_ids != "cover"]

# kelpbio model names (as in kb_fit_<model>_<species>()).
fn_of <- function(id) components[[id]]$fn %||% id

# The kelpbio function for a model and species: kelpbio_fn("fit", "cover", "nereo")
# is kb_fit_biomass_cover_nereo(). In the package these resolve to the functions
# imported from kelpbio.
kelpbio_verbs <- c(check = "check_data", priors = "priors", fit = "fit", prefit = "prefit")
kelpbio_fn <- function(verb, id, species) {
  get(sprintf("kb_%s_%s_%s", kelpbio_verbs[[verb]], fn_of(id), species), mode = "function")
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
  carbon = biomass_ids
)

default_source <- function(id, has_sheet) {
  options <- components[[id]]$sources
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

# Sheets --------------------------------------------------------------------------------

# A sheet: its rows and the result of the model's kelpbio data check, which
# either passes or aborts with a message naming the column. Messages and
# warnings the check reports become the sheet's note.
new_sheet <- function(id, rows, file, species) {
  notes <- character()
  result <- tryCatch(
    withCallingHandlers(
      kelpbio_fn("check", id, species)(rows, x_name = sprintf("`%s`", components[[id]]$sheet)),
      warning = function(w) {
        notes <<- c(notes, conditionMessage(w))
        invokeRestart("muffleWarning")
      },
      message = function(m) {
        notes <<- c(notes, trimws(conditionMessage(m)))
        invokeRestart("muffleMessage")
      }
    ),
    error = function(e) e
  )
  list(
    component = id, name = components[[id]]$sheet, file = file, rows = rows,
    error = if (inherits(result, "error")) conditionMessage(result),
    note = if (length(notes) > 0) paste(notes, collapse = " ")
  )
}

site_years <- function(rows) {
  rows <- rows[!is.na(rows$year), ]
  unique(paste(rows$site, rows$year, sep = "|"))
}

normalise_site <- function(site) gsub("[[:space:]_-]", "", tolower(site))

# Site names that differ across the sheets only in case, spacing, hyphens or
# underscores. They are treated as different sites, so biomass waits until they
# are renamed to match.
site_mismatches <- function(sheets) {
  sites <- unique(unlist(lapply(sheets, function(sheet) as.character(sheet$rows$site))))
  key <- normalise_site(sites)
  sort(sites[key %in% key[duplicated(key)]])
}

mismatch_advice <- "Rename the sites in the workbook so the names match exactly, then upload again."

# Priors and sampler ----------------------------------------------------------------
# The prior editor works on a table built from the model's kb_priors_*() list: one
# row per entry, with the model term it applies to (as named in the diagnostics),
# its family and hyperparameters (a: mean or rate, b: SD).

prior_labels <- c(
  intercept = "Intercept", zero_inflation = "Zero inflation (logit)", dispersion = "Dispersion",
  shape = "Shape", power = "Power", floor = "Floor", density = "Density", fronds = "Fronds",
  precision = "Precision", cover = "Cover slope", sd_site = "SD of site effect", sd_year = "SD of year effect",
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
      name = name, term = attr(prior, "term") %||% NA_character_, label = prior_label(id, name),
      family = if (normal) "normal" else "exponential",
      a = if (normal) prior$mean else prior$rate, b = if (normal) prior$sd else NA_real_
    )
  })
  do.call(rbind, rows)
}

# One row of the prior table as a kelpbio prior; errors on invalid hyperparameters.
as_prior <- function(row) {
  if (row$family == "normal") kb_prior_normal(row$a, row$b) else kb_prior_exponential(row$a)
}

# The edited table as the priors list a kelpbio fit takes.
prior_list <- function(rows) {
  stats::setNames(lapply(seq_len(nrow(rows)), function(i) as_prior(rows[i, ])), rows$name)
}

# The message of the error kelpbio gives for each invalid prior, by prior name.
prior_errors <- function(rows) {
  errors <- lapply(seq_len(nrow(rows)), function(i) {
    tryCatch({
      as_prior(rows[i, ])
      NULL
    }, error = function(e) conditionMessage(e))
  })
  unlist(stats::setNames(errors, rows$name))
}

# "Normal(0, 2)", "Exponential(1)".
format_prior <- function(prior) {
  ifelse(
    prior$family == "normal",
    sprintf("Normal(%s, %s)", as.character(prior$a), as.character(prior$b)),
    sprintf("Exponential(%s)", as.character(prior$a))
  )
}

# The sampler settings, named as the kelpbio fit arguments.
default_sampler <- function() list(chains = 4, niters = 1000, nthin = 1)

# The message for each invalid sampler setting, by setting: each must be a
# positive whole number, checked as kelpbio checks its fit arguments.
sampler_errors <- function(sampler) {
  errors <- lapply(names(sampler), function(name) {
    tryCatch({
      x_name <- paste0("`", name, "`")
      chk::chk_whole_number(sampler[[name]], x_name = x_name)
      chk::chk_gt(sampler[[name]], value = 0, x_name = x_name)
      NULL
    }, error = function(e) conditionMessage(e))
  })
  unlist(stats::setNames(errors, names(sampler)))
}

# The default of an argument of a kelpbio function, for help text that names a
# threshold: default_arg(kb_convergence, "rhat").
default_arg <- function(fn, name) eval(formals(fn)[[name]])

# Predictions ---------------------------------------------------------------------

prediction_groupings <- c(population = "Population level", site = "By site", year = "By year", site_year = "By site and year")

# What each model predicts. `along` names the predictor of a curve model; its
# table, and its by-site-and-year figure, give the value at `at`.
prediction_info <- list(
  density = list(response = "density", table = "Density (stipes/m\u00b2)"),
  size = list(response = "mean sub-bulb diameter", table = "Mean sub-bulb diameter (mm)"),
  weight = list(
    response = "wet weight", along = "sub-bulb diameter", at = "a plant with a 50 mm sub-bulb diameter",
    table = "Wet weight (kg) of a plant with a 50 mm sub-bulb diameter"
  ),
  blade = list(response = "blade fraction", table = "Blade fraction"),
  wetdry = list(response = "ratio of dry to wet mass", table = "Ratio of dry to wet mass"),
  carbon = list(response = "carbon fraction of dry mass", table = "Carbon fraction of dry mass"),
  cover = list(
    response = "wet biomass per square metre", along = "plot percent cover",
    table = "Wet biomass (kg/m\u00b2) at 50% plot cover", note = " Points are plots."
  )
)

# The random effects a model has, from its prior entries.
has_effect <- function(id, effect, species = "nereo") {
  paste0("sd_", effect) %in% names(kelpbio_fn("priors", id, species)())
}

# The groupings a model's predictions can be shown at. A pre-fit model was
# fitted to other sites and years, so only its population-level predictions apply.
prediction_choices <- function(id, prefit = FALSE, species = "nereo") {
  if (prefit || !has_effect(id, "site", species)) {
    return(prediction_groupings["population"])
  }
  if (has_effect(id, "year", species)) prediction_groupings else prediction_groupings[c("population", "site")]
}

prediction_by <- list(population = NULL, site = "site", year = "year", site_year = c("site", "year"))

# A model's predictions at a grouping: curves along the predictor for the figure,
# or, for the table (at = TRUE) and the by-site-and-year figure, the value at a
# reference predictor value: a 50 mm plant, 50% cover.
model_predictions <- function(id, fit, grouping, at = FALSE) {
  by <- prediction_by[[grouping]]
  at <- at || grouping == "site_year"
  switch(id,
    density = kb_predict_density_by(fit, by),
    size = kb_predict_size_by(fit, by),
    weight = kb_predict_weight_by(fit, by, diameter_mm = if (at) 50),
    blade = kb_predict_blade_by(fit, by),
    wetdry = kb_predict_wetdry(fit),
    carbon = kb_predict_carbon_by(fit, by),
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
  site <- has_effect(id, "site")
  year <- has_effect(id, "year")
  curve <- !is.null(info$along)
  what <- if (curve) paste(info$response, "by", info$along) else info$response
  if (grouping == "site_year" && curve) what <- paste(info$response, "of", info$at)
  by <- switch(grouping,
    population = if (year) " for a typical site and year" else if (site) " for a typical site" else "",
    site = paste0(if (curve) ", faceted by site" else " by site", if (year) ", for a typical year"),
    year = paste0(if (curve) ", faceted by year" else " by year", ", for a typical site"),
    site_year = " by year, faceted by site"
  )
  sprintf("Predicted %s%s, with 95%% compatibility intervals.%s", what, by, info$note %||% "")
}
