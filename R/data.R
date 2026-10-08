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

component_ids <- c("density", "size", "weight", "wetdry", "carbon", "cover")
names(component_ids) <- component_ids

# A model's sources: "user" (your data), its pre-fit sources, and "none" (not
# used). Your data is the default when there is a sheet; otherwise the first
# pre-fit source, or none. No model is required: a run can, for example, use
# only a size sheet and a pre-fit weight model to predict plant weights.
# `columns` are the sheet's required columns, by species where they differ, and
# `row` is what one row of the sheet holds.
components <- list(
  density = list(
    label = "Density", detail = "Stipes per square metre by site-year", sheet = "density", row = "Transect",
    columns = list(nereo = c("site", "year", "stipes", "area_m2"), macro = c("site", "year", "plants", "area_m2")),
    sources = c("user", "none")
  ),
  size = list(
    label = "Size", detail = "Distribution of plant size", sheet = "size", row = "Plant",
    columns = list(nereo = c("site", "year", "diameter_mm"), macro = c("site", "year", "fronds")),
    sources = c("user", prefit_source("hakai"), "none")
  ),
  weight = list(
    label = "Weight", detail = "Wet weight by plant size", sheet = "weight", row = "Plant",
    columns = list(nereo = c("site", "year", "diameter_mm", "weight_kg"), macro = c("site", "year", "fronds", "weight_kg")),
    sources = c("user", prefit_source(c("coastwide", "hakai")))
  ),
  wetdry = list(
    label = "Wet:dry", detail = "Ratio of dry to wet mass", sheet = "wetdry", row = "Tissue sample",
    columns = c("wet_mass_g", "dry_mass_g"), sources = c("user", prefit_source("hakai"), "none")
  ),
  carbon = list(
    label = "Carbon", detail = "Carbon fraction of dry mass", sheet = "carbon", row = "Dried tissue sample",
    columns = c("sample_mass_mg", "carbon_mass_ug"), sources = c("user", prefit_source("hakai"), "none")
  ),
  # Optional: scales plot biomass up to total biomass per site. The sheet holds
  # drone surveys of plots, which the model is fitted to with the plot biomass
  # of their site-years once every other model in use is ready, and of whole
  # sites, whose total biomass the fitted model predicts; a row can hold both
  # (plot_surveys(), site_surveys()). kelpbio names the model cover_biomass
  # (`model`).
  cover = list(
    label = "Cover biomass", detail = "Relates plot wet biomass to tide-corrected canopy cover from drone surveys",
    sheet = "cover", row = "Drone survey of a plot, a site or both",
    columns = c("site", "year", "tide_height_m", "plot_canopy_area_m2", "plot_boundary_area_m2", "site_canopy_area_m2"),
    sources = c("user", "none"), model = "cover_biomass"
  )
)

sheet_columns <- function(id, species) {
  columns <- components[[id]]$columns
  if (is.list(columns)) columns[[species]] else columns
}

# The models that combine into plot biomass; the cover biomass model builds on them.
biomass_ids <- component_ids[component_ids != "cover"]

# The kelpbio name of a model (as in kb_fit_<name>_<species>()).
model_name <- function(id) components[[id]]$model %||% id

# The kelpbio function for a model and species: kelpbio_fn("fit", "cover", "nereo")
# is kb_fit_cover_biomass_nereo(). In the package these resolve to the functions
# imported from kelpbio.
kelpbio_verbs <- c(check = "check_data", plot_data = "plot_data", priors = "priors", fit = "fit", prefit = "prefit")
kelpbio_fn <- function(verb, id, species) {
  get(sprintf("kb_%s_%s_%s", kelpbio_verbs[[verb]], model_name(id), species), mode = "function")
}

# The cover sheet ------------------------------------------------------------------
# kelpbio takes the two kinds of drone survey as separate tables, each with a
# canopy_area_m2 column; the sheet's columns, by kelpbio column, for each.
cover_columns <- list(
  plot = c(canopy_area_m2 = "plot_canopy_area_m2", plot_area_m2 = "plot_boundary_area_m2"),
  site = c(canopy_area_m2 = "site_canopy_area_m2")
)

# The drone surveys of plots, in kelpbio's columns, for
# kb_fit_cover_biomass_*(): the rows with a plot canopy area.
plot_surveys <- function(rows) {
  rows <- rows[!is.na(rows$plot_canopy_area_m2), ]
  data.frame(
    site = rows$site, year = rows$year, canopy_area_m2 = rows$plot_canopy_area_m2,
    plot_area_m2 = rows$plot_boundary_area_m2, tide_height_m = rows$tide_height_m
  )
}

# The drone surveys of whole sites, in kelpbio's columns, for
# kb_predict_site_biomass(): the rows with a site canopy area, once each, as
# the survey's site canopy area can repeat on the rows of its plots.
site_surveys <- function(rows) {
  rows <- rows[!is.na(rows$site_canopy_area_m2), ]
  unique(data.frame(site = rows$site, year = rows$year, canopy_area_m2 = rows$site_canopy_area_m2, tide_height_m = rows$tide_height_m))
}

# A kelpbio message about plot or site surveys (`kind`), naming the cover
# sheet's columns.
sheet_message <- function(message, kind) {
  columns <- cover_columns[[kind]]
  for (name in names(columns)) message <- gsub(sprintf("\\b%s\\b", name), columns[[name]], message, perl = TRUE)
  message
}

# Runs a kelpbio check of plot or site surveys, with its warnings and error
# naming the cover sheet's columns.
with_sheet_messages <- function(kind, code) {
  withCallingHandlers(
    code,
    warning = function(w) {
      warning(sheet_message(conditionMessage(w), kind), call. = FALSE)
      invokeRestart("muffleWarning")
    },
    error = function(e) stop(sheet_message(conditionMessage(e), kind), call. = FALSE)
  )
}

# The cover sheet's check: its columns, that each row is a survey of a plot (both
# plot columns), of a site (site_canopy_area_m2) or both, then kelpbio's checks
# of each kind of survey.
check_cover_sheet <- function(rows, species, x_name) {
  chk::chk_data(rows, x_name = x_name)
  chk::chk_superset(names(rows), components$cover$columns, x_name = x_name)
  plot <- !is.na(rows$plot_canopy_area_m2)
  half <- which(plot != !is.na(rows$plot_boundary_area_m2))
  if (length(half) > 0) {
    stop(sprintf(
      "%s of %s %s only one of plot_canopy_area_m2 and plot_boundary_area_m2. Give both for a survey of a plot, or neither.",
      sheet_rows(half), x_name, if (length(half) == 1) "has" else "have"
    ), call. = FALSE)
  }
  none <- which(!plot & is.na(rows$site_canopy_area_m2))
  if (length(none) > 0) {
    stop(sprintf(
      "%s of %s %s no survey. Give plot_canopy_area_m2 and plot_boundary_area_m2 for a plot, site_canopy_area_m2 for a whole site, or both.",
      sheet_rows(none), x_name, if (length(none) == 1) "has" else "have"
    ), call. = FALSE)
  }
  if (any(plot)) with_sheet_messages("plot", kelpbio_fn("check", "cover", species)(plot_surveys(rows), x_name = x_name))
  if (any(!is.na(rows$site_canopy_area_m2))) with_sheet_messages("site", kb_check_data_site_biomass(site_surveys(rows), x_name = x_name))
  invisible(rows)
}

# "Row 4", "Rows 4, 7 and 9": the spreadsheet rows of data rows `i`, under a
# header row.
sheet_rows <- function(i) {
  rows <- i + 1
  shown <- if (length(rows) > 5) c(rows[1:5], "more") else rows
  paste(if (length(rows) == 1) "Row" else "Rows", and_list(as.character(shown)))
}

# The caption of each model's data figure (kb_plot_data_<model>_<species>()),
# by species where they differ, and the figure's height as a share of its width.
data_plot_info <- list(
  density = list(
    caption = c(nereo = "Stipe density of each transect by year, faceted by site.", macro = "Plant density of each transect by year, faceted by site."),
    aspect = 6 / 8
  ),
  size = list(caption = c(nereo = "Distribution of sub-bulb diameter by year.", macro = "Distribution of frond counts by year."), aspect = 5 / 8),
  weight = list(caption = c(nereo = "Wet weight of each plant by sub-bulb diameter.", macro = "Wet weight of each plant by frond count."), aspect = 4 / 8),
  wetdry = list(caption = "Distribution of the ratio of dry to wet mass of the tissue samples.", aspect = 4 / 8),
  carbon = list(caption = "Distribution of the carbon fraction of dry mass of the tissue samples.", aspect = 4 / 8),
  cover = list(caption = "Canopy cover of each plot surveyed by drone, by tide height.", aspect = 4 / 8)
)

data_plot_caption <- function(id, species) {
  caption <- data_plot_info[[id]]$caption
  if (length(caption) > 1) caption[[species]] else caption
}

# The example workbooks the Data step offers, by kb_example_data()'s `example`,
# in the order shown.
example_workbooks <- list(
  full = list(label = "All sheets", detail = "Density, size, weight, wet:dry, carbon and cover, each fitted to the data, through to total site biomass"),
  density_size = list(label = "Density, size and cover", detail = "As a typical monitoring program collects, with drone surveys; weight, wet:dry and carbon are pre-fit"),
  bad_weight = list(label = "Sheets with an error and a warning", detail = "Density and size, a weight sheet with missing years and a carbon sheet with implausible values"),
  size_only = list(label = "Size only", detail = "Plant diameters, to predict the weight of each plant")
)

# Biomass per unit area combines these models at the least, so each must be in
# use (fitted to your data or pre-fit).
biomass_core_ids <- c("density", "size", "weight")
biomass_possible <- function(sources) all(sources[biomass_core_ids] != "none")

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

# Each biomass measure (kelpbio's `measure`), with its unit per m\u00b2 of plot
# and in total per site.
output_info <- list(
  wet = list(label = "Wet biomass", unit = c(plot = "kg/m\u00b2", site = "kg")),
  dry = list(label = "Dry biomass", unit = c(plot = "kg/m\u00b2", site = "kg")),
  carbon = list(label = "Carbon", unit = c(plot = "g C/m\u00b2", site = "kg C"))
)

# The estimates on the Estimates step: each model's predictions, then plot
# biomass and total site biomass from the combined models.
estimate_ids <- c(component_ids, biomass = "biomass", total = "total")
estimate_label <- function(id) switch(id, biomass = "Plot biomass", total = "Total site biomass", label_of(id))

output_components <- list(
  wet = c("density", "size", "weight"),
  dry = c("density", "size", "weight", "wetdry"),
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

# A sheet: its rows and the result of its data check (kelpbio's for the model,
# or check_cover_sheet()), which either passes or aborts with a message naming
# the column. Warnings the check gives (implausible values) become the sheet's
# warnings, one per element, and its messages its note.
new_sheet <- function(id, rows, file, species) {
  warnings <- character()
  notes <- character()
  result <- tryCatch(
    withCallingHandlers(
      check_sheet(id, rows, species, x_name = sprintf("`%s`", components[[id]]$sheet)),
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
  list(
    component = id, name = components[[id]]$sheet, file = file, rows = rows,
    error = if (inherits(result, "error")) conditionMessage(result),
    warnings = if (length(warnings) > 0) unique(warnings),
    note = if (length(notes) > 0) paste(notes, collapse = " ")
  )
}

check_sheet <- function(id, rows, species, x_name) {
  if (id == "cover") check_cover_sheet(rows, species, x_name) else kelpbio_fn("check", id, species)(rows, x_name = x_name)
}

# Uploaded rows as a data frame with site and year as text: spreadsheets store
# years as numbers, and the kelpbio checks take site and year as text.
as_sheet_rows <- function(rows) {
  rows <- as.data.frame(rows)
  for (column in intersect(c("site", "year"), names(rows))) rows[[column]] <- as.character(rows[[column]])
  rows
}

# What each sheet column holds and the values it accepts, as kelpbio's
# kb_check_data_*() documents them. The cover sheet's columns are kelpbio's
# canopy_area_m2 and plot_area_m2 of each kind of survey (cover_columns).
column_info <- list(
  site = c("Site name, spelled the same way in every sheet", "Text, without commas or square brackets"),
  year = c("Survey year", "Year, e.g. 2024"),
  stipes = c("Stipes counted on the transect; sum counts recorded in bins along it", "Whole number, 0 or more"),
  plants = c("Plants counted on the transect", "Whole number, 0 or more"),
  area_m2 = c("Area of the transect surveyed (m\u00b2)", "Greater than 0"),
  diameter_mm = c("Maximum sub-bulb diameter of the plant (mm)", "Greater than 0"),
  fronds = c("Fronds reaching 1 m above the holdfast; leave out plants with none", "Whole number, greater than 0"),
  weight_kg = c("Wet weight of the plant (kg)", "Greater than 0"),
  wet_mass_g = c("Mass of the tissue sample before drying (g)", "Greater than 0"),
  dry_mass_g = c("Mass of the same sample after drying (g)", "Greater than 0, less than wet_mass_g"),
  sample_mass_mg = c("Mass of the dried sample analysed, as the lab reports it (mg)", "Greater than 0"),
  carbon_mass_ug = c("Carbon measured in the sample (\u00b5g)", "Greater than 0, less than the sample mass"),
  tide_height_m = c("Tide height at the drone survey, chart datum (m)", "Number"),
  plot_canopy_area_m2 = c(
    "Canopy area the drone imagery delineated within the plot (m\u00b2)",
    "0 or more (0 for no canopy), at most plot_boundary_area_m2; blank for a survey of a site only"
  ),
  plot_boundary_area_m2 = c("Area within the plot boundary (m\u00b2)", "Greater than 0; blank for a survey of a site only"),
  site_canopy_area_m2 = c(
    "Canopy area mapped within the site from the drone imagery (m\u00b2); the same on every row of the survey",
    "0 or more (0 for no canopy); blank for a survey of a plot only"
  )
)

# Whether a run needs a model's sheet: density, size and weight make plot
# biomass, and a pre-fit model stands in for a missing sheet.
sheet_need <- function(id) {
  prefit <- Filter(is_prefit, components[[id]]$sources)
  if (length(prefit) > 0) {
    return(sprintf("Optional: without it, the %s model is used", source_note(prefit[1])))
  }
  if (id %in% biomass_core_ids) "Required for biomass" else "Optional"
}

# The template's first sheet: each model sheet, whether it is needed, what a
# row is, and its columns with their accepted values.
template_readme <- function(species) {
  rows <- lapply(component_ids, function(id) {
    columns <- sheet_columns(id, species)
    data.frame(
      sheet = components[[id]]$sheet,
      needed = sheet_need(id),
      one_row_per = components[[id]]$row,
      column = columns,
      description = vapply(columns, function(x) column_info[[x]][1], ""),
      values = vapply(columns, function(x) column_info[[x]][2], "")
    )
  })
  all <- data.frame(
    sheet = "All sheets", needed = "", one_row_per = "", column = "Every column",
    description = "Fill every cell: a blank cell is an error, except where the cover sheet's columns allow it. Other columns are ignored.", values = ""
  )
  do.call(rbind, c(list(all), unname(rows)))
}

# The template workbook for a species: a README sheet, then one sheet per
# model, named as read_workbook() reads it, with the sheet's required columns
# as headers and no rows.
template_sheets <- function(species) {
  sheets <- lapply(component_ids, function(id) {
    columns <- sheet_columns(id, species)
    as.data.frame(stats::setNames(rep(list(character()), length(columns)), columns))
  })
  c(list(README = template_readme(species)), stats::setNames(sheets, vapply(component_ids, function(id) components[[id]]$sheet, "")))
}

template_file <- function(species) sprintf("kelpbio-template-%s.xlsx", species_info[[species]]$suffix)

# The rows of each sheet in an Excel workbook named after a model's sheet, by
# model; other sheets are ignored.
read_workbook <- function(path) {
  present <- readxl::excel_sheets(path)
  ids <- Filter(function(id) components[[id]]$sheet %in% present, component_ids)
  lapply(ids, function(id) as_sheet_rows(readxl::read_excel(path, sheet = components[[id]]$sheet)))
}

read_csv_rows <- function(path) as_sheet_rows(utils::read.csv(path, check.names = FALSE))

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
# row per entry, named as the parameter it applies to (as the diagnostics name
# it), with its family and hyperparameters (a: mean, log-scale mean or rate; b:
# SD or log-scale SD).

prior_labels <- c(
  intercept = "Intercept", logit_zero_inflation = "Zero inflation (logit)", dispersion = "Dispersion",
  shape = "Shape", diameter_power = "Diameter power", weight_floor = "Weight floor",
  density_slope = "Density slope", fronds_slope = "Fronds slope", precision = "Precision",
  cover_slope = "Cover slope", biomass_floor = "Biomass floor", tide_height_slope = "Tide height slope",
  error_scaling = "Error scaling", sd_site = "SD of site effect", sd_year = "SD of year effect",
  sd_site_year = "SD of site-year effect", sd_residual = "Residual SD"
)

prior_label <- function(id, name) {
  if (name == "intercept" && id %in% c("wetdry", "carbon")) {
    return("Intercept (logit)")
  }
  if (name %in% names(prior_labels)) prior_labels[[name]] else name
}

# Each prior family: its kelpbio class and hyperparameters (a, then b if any).
prior_families <- list(
  normal = list(class = "kb_prior_normal", args = c("mean", "sd")),
  lognormal = list(class = "kb_prior_lognormal", args = c("meanlog", "sdlog")),
  exponential = list(class = "kb_prior_exponential", args = "rate")
)

default_priors <- function(id, species = "nereo") {
  priors <- kelpbio_fn("priors", id, species)()
  rows <- lapply(names(priors), function(name) {
    prior <- priors[[name]]
    family <- names(prior_families)[vapply(prior_families, function(f) inherits(prior, f$class), logical(1))]
    args <- prior_families[[family]]$args
    data.frame(
      name = name, label = prior_label(id, name), family = family,
      a = prior[[args[1]]], b = if (length(args) > 1) prior[[args[2]]] else NA_real_
    )
  })
  do.call(rbind, rows)
}

# One row of the prior table as a kelpbio prior; errors on invalid hyperparameters.
as_prior <- function(row) {
  switch(row$family,
    normal = kb_prior_normal(row$a, row$b),
    lognormal = kb_prior_lognormal(row$a, row$b),
    exponential = kb_prior_exponential(row$a)
  )
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

# "Normal(0, 2)", "LogNormal(2, 1)", "Exponential(1)", as kb_model_describe() writes them.
format_prior <- function(prior) {
  name <- c(normal = "Normal", lognormal = "LogNormal", exponential = "Exponential")[prior$family]
  args <- ifelse(prior$family == "exponential", as.character(prior$a), paste0(prior$a, ", ", prior$b))
  sprintf("%s(%s)", name, args)
}

# The sampler settings, named as the kelpbio fit arguments.
default_sampler <- function() list(chains = 4, niters = 1000, nthin = 1)

# The message for each invalid sampler setting, by setting: each must be a
# positive whole number, and niters at least 2, checked as kelpbio checks its
# fit arguments.
sampler_errors <- function(sampler) {
  errors <- lapply(names(sampler), function(name) {
    tryCatch({
      x_name <- paste0("`", name, "`")
      chk::chk_whole_number(sampler[[name]], x_name = x_name)
      if (name == "niters") chk::chk_gte(sampler[[name]], value = 2, x_name = x_name) else chk::chk_gt(sampler[[name]], value = 0, x_name = x_name)
      NULL
    }, error = function(e) conditionMessage(e))
  })
  unlist(stats::setNames(errors, names(sampler)))
}

# The default of an argument of a kelpbio function, for help text that names a
# threshold: default_arg(kb_converged, "rhat").
default_arg <- function(fn, name) eval(formals(fn)[[name]])

# Predictions ---------------------------------------------------------------------

prediction_groupings <- c(
  population = "Overall", site = "By site", year = "By year", site_year = "By site and year",
  plant = "Individual plant"
)

# What each model predicts. `along` names the predictor of a curve model; its
# table, and its by-site-and-year figure, give the value at `at`, the predictor
# value (by species where it differs) passed to kb_new_data() as `value`.
prediction_info <- list(
  density = list(response = "density", table = "Density (stipes/m\u00b2)"),
  size = list(response = "mean sub-bulb diameter", table = "Mean sub-bulb diameter (mm)"),
  weight = list(
    response = "wet weight", along = "sub-bulb diameter", at = "a plant with a 50 mm sub-bulb diameter",
    value = list(nereo = list(diameter_mm = 50), macro = list(fronds = 10)),
    table = "Wet weight (kg) of a plant with a 50 mm sub-bulb diameter",
    plant_table = "Wet weight (kg) of each plant in the size sheet"
  ),
  wetdry = list(response = "ratio of dry to wet mass", table = "Ratio of dry to wet mass"),
  carbon = list(response = "carbon fraction of dry mass", table = "Carbon fraction of dry mass"),
  cover = list(
    response = "wet biomass per square metre", along = "tide-corrected cover", at = "a tide-corrected cover of 0.5",
    value = list(cover = 0.5), table = "Wet biomass (kg/m\u00b2) at a tide-corrected cover of 0.5"
  )
)

# The random effects a model has, from its prior entries.
has_effect <- function(id, effect, species = "nereo") {
  paste0("sd_", effect) %in% names(kelpbio_fn("priors", id, species)())
}

# The groupings a model's predictions can be shown at. A pre-fit model was
# fitted to other sites and years, so only its overall predictions
# apply. The weight model can also predict each plant in the size sheet
# (`plants`, from plant_rows()).
prediction_choices <- function(id, prefit = FALSE, species = "nereo", plants = FALSE) {
  groups <- if (prefit || !has_effect(id, "site", species)) {
    "population"
  } else if (has_effect(id, "year", species)) {
    c("population", "site", "year", "site_year")
  } else {
    c("population", "site")
  }
  if (id == "weight" && plants) groups <- c(groups, "plant")
  prediction_groupings[groups]
}

# The plants to predict the weight of: the rows of the size sheet, once it
# passes its check. For Nereocystis, each row also gets its site-year's observed
# stipe density from a density sheet that passes its check; site-years without
# one take the model's mean density.
plant_rows <- function(sheets, species) {
  size <- sheets$size
  if (is.null(size) || !is.null(size$error)) {
    return(NULL)
  }
  density <- sheets$density
  if (species == "nereo" && !is.null(density) && is.null(density$error)) kb_add_stipes_m2(size$rows, density$rows) else size$rows
}

prediction_by <- list(population = NULL, site = "site", year = "year", site_year = c("site", "year"))

# A model's predictions at a grouping: curves along the predictor for the figure,
# or, for the table (at = TRUE) and the by-site-and-year figure, the value at a
# reference predictor value: a 50 mm plant, a cover of 0.5. The rows to predict
# at come from kb_new_data(); the plant grouping predicts the weight of each row
# of `plants`.
model_predictions <- function(id, fit, grouping, species, at = FALSE, plants = NULL) {
  if (grouping == "plant") {
    return(kb_predict_weight(fit, new_data = plants))
  }
  if (id %in% c("wetdry", "carbon")) {
    return(if (id == "wetdry") kb_predict_wetdry(fit) else kb_predict_carbon(fit))
  }
  value <- prediction_info[[id]]$value
  if (id == "weight") value <- value[[species]]
  at <- at || grouping == "site_year"
  new_data <- do.call(kb_new_data, c(list(fit, by = prediction_by[[grouping]]), if (at) value))
  switch(id,
    density = kb_predict_density(fit, new_data),
    size = kb_predict_size(fit, new_data),
    weight = kb_predict_weight(fit, new_data),
    cover = kb_predict_cover_biomass(fit, new_data)
  )
}

# A predictions figure's height as a share of its width.
prediction_aspect <- function(id, grouping) {
  if (grouping == "plant") {
    return(4 / 8)
  }
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
  if (grouping == "plant") {
    return(sprintf("Predicted %s of each plant in the size sheet by %s, with 95%% compatibility intervals.", info$response, info$along))
  }
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
  sprintf("Predicted %s%s, with 95%% compatibility intervals.", what, by)
}
