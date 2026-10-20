# Workflow definitions. Every model result comes from kelpbio functions (the
# data figures are mocked in R/mock-kelpbio.R); this file holds the app's labels
# and choices.

# Pre-fit models, by the reference dataset they were fitted to. A model's pre-fit
# source is "prefit_" and the reference, e.g. "prefit_hakai"; the fit is
# kelpbio's reference fit of that name (fit_weight_hakai_nereo).
prefit_info <- list(
  coastwide = list(label = "Pre-fit Coastwide", data = "data compiled from surveys along the coast"),
  hakai = list(label = "Pre-fit Hakai Institute", data = "Hakai Institute survey data")
)

prefit_source <- function(reference) if (length(reference) > 0) paste0("prefit_", reference) else character()
is_prefit <- function(source) startsWith(source, "prefit_")
prefit_reference <- function(source) sub("^prefit_", "", source)

# kelpbio's reference fit of a model to a reference dataset.
prefit_fit <- function(id, species, reference) {
  getExportedValue("kelpbio", sprintf("fit_%s_%s_%s", model_name(id), reference, species_info[[species]]$suffix))
}

species_info <- list(
  nereo = list(latin = "Nereocystis luetkeana", common = "bull kelp", suffix = "nereo"),
  macro = list(latin = "Macrocystis pyrifera", common = "giant kelp", suffix = "macro")
)

# Sheets ----------------------------------------------------------------------------
# A workbook holds one sheet per kind of data, named by its id. `columns` are the
# sheet's columns, by species where they differ, optional ones and those of
# either kind of size data included (sheet_columns() picks them), and `row` is
# what one row of the sheet holds. Each sheet but the canopy sheet is
# the data of one model (components); the survey sheet is the cover
# calibration's.

sheet_ids <- c("weight", "size", "survey", "canopy", "wetdry", "carbon")
names(sheet_ids) <- sheet_ids

sheet_info <- list(
  weight = list(
    label = "Weight", row = "Harvested plant",
    columns = list(nereo = c("site", "year", "diameter_mm", "weight_kg"), macro = c("site", "year", "fronds", "weight_kg"))
  ),
  size = list(
    label = "Size", row = "Measured plant; in a census, every counted plant",
    columns = list(nereo = c("site", "year", "survey_id", "diameter_mm", "partial"), macro = c("survey_id", "fronds", "partial"))
  ),
  survey = list(
    label = "Survey", row = "In situ survey",
    columns = list(
      nereo = c("survey_id", "site", "year", "survey_area_m2", "plants", "plot_area_m2", "canopy_area_m2", "tide_height_m"),
      macro = c("survey_id", "site", "year", "survey_area_m2", "plot_area_m2", "canopy_area_m2", "tide_height_m")
    )
  ),
  canopy = list(
    label = "Canopy", row = "Drone map of a site",
    columns = c("site", "year", "canopy_area_m2", "tide_height_m", "classified_area_m2")
  ),
  wetdry = list(label = "Wet:dry", row = "Tissue sample", columns = c("wet_mass_g", "dry_mass_g")),
  carbon = list(label = "Carbon", row = "Dried tissue sample", columns = c("sample_mass_mg", "carbon_mass_ug"))
)

# A sheet's columns for a species and kind of size data: a census has no
# plants tally on its surveys, and its size sheet joins each plant to its
# survey by survey_id (which gives its site and year) and marks partial plants.
sheet_columns <- function(id, species, census = is_census(species)) {
  columns <- sheet_info[[id]]$columns
  if (is.list(columns)) columns <- columns[[species]]
  drop <- if (census) "plants" else "partial"
  if (id == "size") drop <- c(drop, if (census) c("site", "year") else "survey_id")
  setdiff(columns, drop)
}

sheet_label <- function(id) sheet_info[[id]]$label

# Census ------------------------------------------------------------------------------
# The kind of size data, chosen on the Data step: a measured sample of plants,
# to which a size model is fitted while the surveys tally their plants, or a
# census, which measures every counted plant, so the size sheet holds the
# plants of each survey and survey biomass weighs each plant. Macrocystis
# surveys are always a census, as kelpbio has no Macrocystis size model.
size_data_info <- list(
  sample = list(label = "Sample of plants measured", detail = "Size model fitted"),
  census = list(label = "All plants measured", detail = "A census; no size model fitted")
)

is_census <- function(species, size_data = "sample") species == "macro" || size_data == "census"

# The survey rows as kelpbio takes them: in a census, without a blank plants
# column (one with counts is left in, so the check flags it), as the counts
# then come from the plant rows.
survey_rows <- function(rows, census) {
  if (census && "plants" %in% names(rows) && all(is.na(rows$plants))) rows[setdiff(names(rows), "plants")] else rows
}

# A plant table with no rows, to check the survey sheet of a census before
# its size sheet passes its check.
empty_plants <- function(species) {
  plants <- data.frame(survey_id = character(), size = numeric())
  names(plants)[2] <- if (species == "macro") "fronds" else "diameter_mm"
  plants
}

# Models ------------------------------------------------------------------------------

component_ids <- c("weight", "size", "wetdry", "carbon", "cover")
names(component_ids) <- component_ids

# A model's sources: "user" (your data), its pre-fit sources, "census" (the size
# model only: the size sheet's plants are weighed directly) and "none" (not
# used). Your data is the default when there is a sheet; otherwise the first
# pre-fit source, or none. No model is required: a run can, for example, use
# only a size sheet and a pre-fit weight model to predict plant weights.
# `sheet` is the sheet the model is fitted to, and `prefit` the references of
# kelpbio's reference fits, by species. kelpbio names the cover model
# cover_calibration (`model`).
components <- list(
  weight = list(
    label = "Weight", detail = "Wet weight by plant size", sheet = "weight",
    sources = c("user", "prefit"), prefit = list(nereo = c("coastwide", "hakai"), macro = "hakai")
  ),
  size = list(
    label = "Size", detail = "Distribution of plant size by site-year", sheet = "size",
    sources = c("user", "prefit", "none"), prefit = list(nereo = "hakai")
  ),
  wetdry = list(
    label = "Wet:dry", detail = "Ratio of dry to wet mass", sheet = "wetdry",
    sources = c("user", "prefit", "none"), prefit = list(nereo = "hakai", macro = "hakai")
  ),
  carbon = list(
    label = "Carbon", detail = "Carbon fraction of dry mass", sheet = "carbon",
    sources = c("user", "prefit", "none"), prefit = list(nereo = "hakai", macro = "hakai")
  ),
  # Optional: scales survey biomass up to the canopy mapped by drone. It is
  # fitted to the survey biomass of the surveys with a drone flight, once the
  # weight and size models are ready.
  cover = list(
    label = "Cover calibration", detail = "Survey biomass by drone canopy cover",
    sheet = "survey", sources = c("user", "none"), model = "cover_calibration"
  )
)

# The models whose results survey biomass combines; the cover calibration is
# fitted to it, so a change to either makes the calibration out of date.
survey_ids <- c("weight", "size")

# The models by what they are for, in the order the Models and Estimates steps
# list them; the optional groups add to survey biomass.
model_groups <- list(
  survey = list(label = "For survey biomass", ids = survey_ids),
  canopy = list(label = "For canopy biomass", optional = TRUE, detail = "fitted once the weight and size models are ready", ids = "cover"),
  measure = list(label = "For dry and carbon biomass", optional = TRUE, ids = c("wetdry", "carbon"))
)
model_order <- unlist(lapply(model_groups, `[[`, "ids"), use.names = FALSE)

# The model fitted to a sheet, or NULL for the canopy sheet.
sheet_component <- function(id) {
  ids <- Filter(function(cid) components[[cid]]$sheet == id, component_ids)
  if (length(ids) > 0) ids[[1]]
}

# The source of the model fitted to a sheet, which says whether the sheet is in
# use. The survey sheet makes survey biomass whether or not the cover
# calibration is fitted to it, and the canopy sheet is fitted by no model, so
# both are always in use.
sheet_source <- function(id, sources) {
  model <- sheet_component(id)
  if (is.null(model) || id == "survey") "user" else sources[[model]]
}

# The sheets in use that fail their check.
sheet_errors <- function(sheets, sources) {
  Filter(function(id) !is.null(sheets[[id]]$error) && sheet_source(id, sources) %in% c("user", "census"), names(sheets))
}

# The models and estimates a run shows: a census has no size model, so no size
# predictions.
shown_models <- function(census) if (census) setdiff(component_ids, "size") else component_ids
shown_estimates <- function(census) if (census) setdiff(estimate_ids, "size") else estimate_ids

# kelpbio has no Macrocystis size model: Macrocystis surveys are a census.
has_model <- function(id, species) !(id == "size" && species == "macro")

# The sources a model offers for a species, its pre-fit models in place of
# "prefit": a census has only the census.
model_sources <- function(id, census = FALSE, species = "nereo") {
  if (id == "size" && census) {
    return("census")
  }
  sources <- as.list(components[[id]]$sources)
  sources[sources == "prefit"] <- list(prefit_source(components[[id]]$prefit[[species]]))
  unlist(sources)
}

# The kelpbio name of a model (as in kb_fit_<name>_<species>()).
model_name <- function(id) components[[id]]$model %||% id

# The kelpbio function for a model and species: kelpbio_fn("fit", "cover", "nereo")
# is kb_fit_cover_calibration_nereo(). In the package these resolve to the functions
# imported from kelpbio.
kelpbio_verbs <- c(check = "check_data", plot_data = "plot_data", priors = "priors", fit = "fit")
kelpbio_fn <- function(verb, id, species) {
  get(sprintf("kb_%s_%s_%s", kelpbio_verbs[[verb]], model_name(id), species), mode = "function")
}

label_of <- function(id) components[[id]]$label
lower_label <- function(id) tolower(label_of(id))

source_label <- function(source) {
  if (is_prefit(source)) {
    return(prefit_info[[prefit_reference(source)]]$label)
  }
  switch(source,
    user = "Your data",
    census = "Census",
    none = "Not used"
  )
}

source_labels <- function(sources) vapply(sources, source_label, character(1))

# For use mid-sentence: "pre-fit Hakai Institute", "not used".
source_note <- function(source) {
  label <- source_label(source)
  paste0(tolower(substr(label, 1, 1)), substring(label, 2))
}

default_source <- function(id, has_sheet, census = FALSE, species = "nereo") {
  options <- model_sources(id, census, species)
  if (has_sheet && "user" %in% options) {
    return("user")
  }
  prefit <- options[is_prefit(options)]
  if (length(prefit) > 0) {
    return(prefit[[1]])
  }
  options[[length(options)]]
}

# The cover calibration has data once some surveys have a drone flight.
model_has_data <- function(id, sheets) {
  sheet <- sheets[[components[[id]]$sheet]]
  !is.null(sheet) && (id != "cover" || nrow(drone_surveys(sheet$rows)) > 0)
}

default_sources <- function(sheets, census = FALSE, species = "nereo") {
  vapply(component_ids, function(id) default_source(id, model_has_data(id, sheets), census, species), character(1))
}

# The sources kept where the species and kind of size data offer them, and
# otherwise back to their default: the size source is census while it is a
# census, and a pre-fit model the species has no reference fit for is dropped.
valid_sources <- function(sources, census, sheets, species = "nereo") {
  for (id in component_ids) {
    if (!sources[[id]] %in% model_sources(id, census, species)) {
      sources[[id]] <- default_source(id, model_has_data(id, sheets), census, species)
    }
  }
  sources
}

# Each sheet's data figure (kb_plot_data_<model>_<species>(), or the survey and
# canopy figures shared by both species): its caption, by species where it
# differs, and its height as a share of its width in one panel.
data_plot_info <- list(
  weight = list(caption = c(nereo = "Wet weight of each plant by diameter.", macro = "Wet weight of each plant by frond count."), aspect = 4 / 8),
  size = list(caption = c(nereo = "Distribution of plant diameter.", macro = "Distribution of frond counts."), aspect = 4 / 8),
  survey = list(caption = "Canopy cover fraction of each plot flown by drone, by the plants counted in its survey.", aspect = 4 / 8),
  canopy = list(caption = "Canopy area mapped at each site, by year.", aspect = 4 / 8),
  wetdry = list(caption = "Distribution of the ratio of dry to wet mass of the tissue samples.", aspect = 4 / 8),
  carbon = list(caption = "Distribution of the carbon fraction of dry mass of the tissue samples.", aspect = 4 / 8)
)

data_plot_caption <- function(id, species) {
  caption <- data_plot_info[[id]]$caption
  if (length(caption) > 1) caption[[species]] else caption
}

# Whether a sheet has a figure: the survey figure shows the surveys flown by drone.
sheet_has_plot <- function(id, rows) id != "survey" || nrow(drone_surveys(rows)) > 0

# The panels a sheet's figure can be split into, by kelpbio's `facet`: a site
# or year column. The canopy figure has year on its x-axis.
sheet_facets <- function(id, rows) {
  c(
    site = if ("site" %in% names(rows)) "Site",
    year = if (id != "canopy" && "year" %in% names(rows)) "Year"
  )
}

# A sheet's data figure, in one panel or a panel per `facet`. In a census the
# survey figure counts each survey's plants in the size sheet (`size_census`).
sheet_plot <- function(id, rows, species, facet = NULL, size_census = NULL) {
  switch(id,
    survey = kb_plot_data_survey(rows, size_census = size_census, facet = facet),
    canopy = kb_plot_data_canopy(rows, facet = facet),
    kelpbio_fn("plot_data", id, species)(rows, facet = facet)
  )
}

# Each biomass measure (kelpbio's `measure`), with its unit per m\u00b2 (area)
# and in total.
output_info <- list(
  wet = list(label = "Wet biomass", unit = c(area = "kg/m\u00b2", total = "kg")),
  dry = list(label = "Dry biomass", unit = c(area = "kg/m\u00b2", total = "kg")),
  carbon = list(label = "Carbon", unit = c(area = "g C/m\u00b2", total = "kg C"))
)

# The models a measure needs besides those of the biomass itself.
output_components <- list(wet = character(), dry = "wetdry", carbon = c("wetdry", "carbon"))

# The estimates on the Estimates step: each model's predictions, then survey
# biomass and canopy biomass from the combined models.
estimate_ids <- c(component_ids, survey = "survey", canopy = "canopy")
# Survey biomass first, as canopy biomass builds on it.
derived_ids <- c("survey", "canopy")
estimate_label <- function(id) {
  switch(id,
    survey = "Survey biomass",
    canopy = "Canopy biomass",
    label_of(id)
  )
}

# The models each derived estimate uses, whose warnings show on its page.
derived_components <- list(survey = survey_ids, canopy = c(survey_ids, "cover"))

# Sheets --------------------------------------------------------------------------------

# A condition's message as one line of text: kelpbio's messages are formatted
# by cli, wrapped to the console width and with a bullet before each line
# after the first.
condition_text <- function(cnd) {
  message <- withr::with_options(list(cli.width = Inf, cli.num_colors = 1), conditionMessage(cnd))
  lines <- trimws(strsplit(message, "\n", fixed = TRUE)[[1]])
  lines[-1] <- sub("^(\u2139|\u2716|\u2714|\u2022|!|i|x|v|\\*) ", "", lines[-1])
  paste(lines[lines != ""], collapse = " ")
}

# A sheet: its rows and the result of its data check, which either passes or
# aborts with a message naming the column. Warnings the check gives
# (implausible values) become the sheet's warnings, one per element, and its
# messages its note. `check` runs the check.
new_sheet <- function(id, rows, file, check) {
  warnings <- character()
  notes <- character()
  result <- tryCatch(
    withCallingHandlers(
      check(),
      warning = function(w) {
        warnings <<- c(warnings, condition_text(w))
        invokeRestart("muffleWarning")
      },
      message = function(m) {
        notes <<- c(notes, condition_text(m))
        invokeRestart("muffleMessage")
      }
    ),
    error = function(e) e
  )
  list(
    component = id, name = id, file = file, rows = rows,
    error = if (inherits(result, "error")) condition_text(result),
    warnings = if (length(warnings) > 0) unique(warnings),
    note = if (length(notes) > 0) paste(notes, collapse = " ")
  )
}

# The kelpbio check of a sheet. In a census the size sheet is checked as a
# size census and the survey sheet with it (`size`, its rows once they pass
# their check), so plants that match no survey show on the survey sheet.
check_sheet <- function(id, rows, species, census, size = NULL) {
  x_name <- sprintf("`%s`", id)
  switch(id,
    size = if (!census) {
      kb_check_data_size_sample_nereo(rows, x_name = x_name)
    } else if (species == "macro") {
      kb_check_data_size_census_macro(rows, x_name = x_name)
    } else {
      kb_check_data_size_census_nereo(rows, x_name = x_name)
    },
    survey = if (census) kb_check_data_survey(survey_rows(rows, TRUE), size_census = size, x_name = x_name) else kb_check_data_survey(rows, x_name = x_name),
    canopy = kb_check_data_canopy(rows, x_name = x_name),
    kelpbio_fn("check", id, species)(rows, x_name = x_name)
  )
}

# Every sheet checked, from the rows and file of each, by sheet id. The survey
# sheet of a census is checked with the size sheet's plants, so a change to any
# sheet checks them all again. A survey sheet that does not match the kind of
# size data says how to change it.
check_sheets <- function(rows, files, species, census = is_census(species)) {
  sheets <- list()
  for (id in intersect(sheet_ids, names(rows))) {
    size <- if (!is.null(sheets$size) && is.null(sheets$size$error)) rows$size else empty_plants(species)
    sheets[[id]] <- new_sheet(id, rows[[id]], files[[id]], function() check_sheet(id, rows[[id]], species, census, size))
  }
  survey <- sheets$survey
  if (!is.null(survey$error) && species == "nereo") {
    tally <- "plants" %in% names(survey$rows) && !all(is.na(survey$rows$plants))
    if (!census && !tally) {
      sheets$survey$error <- paste(survey$error, sprintf("If the size sheet holds every counted plant, choose %s under Size data.", size_data_info$census$label))
    } else if (census && tally) {
      sheets$survey$error <- paste(survey$error, sprintf("If the size sheet holds a sample of the plants, choose %s under Size data.", size_data_info$sample$label))
    }
  }
  sheets
}

# Uploaded rows as a data frame with site, year and survey_id as text:
# spreadsheets store years and survey numbers as numbers, which kelpbio uses as
# text.
as_sheet_rows <- function(rows) {
  rows <- as.data.frame(rows)
  for (column in intersect(c("site", "year", "survey_id"), names(rows))) rows[[column]] <- as.character(rows[[column]])
  rows
}

# What each sheet column holds and the values it accepts, as kelpbio's
# kb_check_data_*() documents them; `sheet_column_info` holds the columns whose
# meaning differs by sheet.
column_info <- list(
  site = c("Site name, spelled the same way in every sheet", "Text, without commas or square brackets"),
  year = c("Year of the survey or harvest, in the peak season", "Year, e.g. 2024"),
  survey_id = c("Survey identifier, unique to each survey, also of the same site on the same day", "Text or number"),
  diameter_mm = c(
    "Diameter of the plant (mm): the maximum sub-bulb diameter, or another measure such as bulb diameter, the same in the weight and size sheets",
    "Greater than 0"
  ),
  fronds = c("Fronds reaching 1 m above the holdfast; leave out plants with none", "Whole number, greater than 0"),
  weight_kg = c("Wet weight of the plant (kg)", "Greater than 0"),
  partial = c("Whether the plant counts as half, such as on the edge of a transect", "TRUE or FALSE; optional"),
  survey_area_m2 = c("Area surveyed, such as the transects of a dive (m\u00b2)", "Greater than 0, at most plot_area_m2"),
  plants = c("Plants counted in the survey", "Whole number, 0 or more"),
  plot_area_m2 = c("Area of the plot polygon the drone canopy is measured over (m\u00b2)", "Greater than 0; blank for a survey without a drone flight"),
  canopy_area_m2 = c(
    "Canopy area the drone imagery delineated within the plot (m\u00b2)",
    "0 or more (0 for no canopy), at most plot_area_m2; blank for a survey without a drone flight"
  ),
  tide_height_m = c("Tide height at the drone flight, chart datum (m)", "Number; blank for a survey without a drone flight"),
  classified_area_m2 = c(
    "Water area the canopy classification covered within the site, including water where no canopy was found (m\u00b2)",
    "Greater than 0, at least canopy_area_m2"
  ),
  wet_mass_g = c("Mass of the tissue sample before drying (g)", "Greater than 0"),
  dry_mass_g = c("Mass of the same sample after drying (g)", "Greater than 0, less than wet_mass_g"),
  sample_mass_mg = c("Mass of the dried sample analysed, as the lab reports it (mg)", "Greater than 0"),
  carbon_mass_ug = c("Carbon measured in the sample (\u00b5g)", "Greater than 0, less than the sample mass")
)

sheet_column_info <- list(
  size = list(survey_id = c("Survey the plant was counted in, as in the survey sheet", "Text or number")),
  canopy = list(
    year = c("Year of the drone map", "Year, e.g. 2024"),
    canopy_area_m2 = c("Canopy area mapped within the site from the drone imagery (m\u00b2)", "0 or more (0 for no canopy)"),
    tide_height_m = c("Tide height at the flight, chart datum (m)", "Number")
  )
)

column_text <- function(sheet, column) sheet_column_info[[sheet]][[column]] %||% column_info[[column]]

# Whether a run needs a sheet: weight, size and survey make survey biomass, a
# pre-fit model stands in for a missing model sheet, and a canopy sheet adds
# canopy biomass.
sheet_need <- function(id, species = "nereo") {
  if (id == "survey") {
    return("Required for survey biomass and the cover calibration")
  }
  if (id == "canopy") {
    return("Optional: for canopy biomass")
  }
  prefit <- Filter(is_prefit, model_sources(id, species = species))
  need <- if (length(prefit) > 0) sprintf("Optional: without it, the %s model is used", source_note(prefit[1])) else "Optional"
  if (id == "size") need <- paste0(need, "; required for a census")
  need
}

# The template's first sheet: each sheet, whether it is needed, what a row is,
# and its columns with their accepted values, for a species and kind of size
# data.
template_readme <- function(species, census = is_census(species)) {
  rows <- lapply(sheet_ids, function(id) {
    columns <- sheet_columns(id, species, census)
    data.frame(
      sheet = id,
      needed = sheet_need(id, species),
      one_row_per = sheet_info[[id]]$row,
      column = columns,
      description = vapply(columns, function(x) column_text(id, x)[1], ""),
      values = vapply(columns, function(x) column_text(id, x)[2], "")
    )
  })
  all <- data.frame(
    sheet = "All sheets", needed = "", one_row_per = "", column = "Every column",
    description = paste(
      "Fill every cell: a blank cell is an error, except in an optional column and the drone columns of a survey without a flight.",
      "Other columns are ignored, except text columns of the canopy sheet, such as region or habitat, which canopy biomass can be totalled over by year."
    ),
    values = ""
  )
  do.call(rbind, c(list(all), unname(rows)))
}

# The template workbook for a species: a README sheet, then one sheet per
# kind of data, named as read_workbook() reads it, with the sheet's columns as
# headers and no rows.
template_sheets <- function(species, census = is_census(species)) {
  sheets <- lapply(sheet_ids, function(id) {
    columns <- sheet_columns(id, species, census)
    as.data.frame(stats::setNames(rep(list(character()), length(columns)), columns))
  })
  c(list(README = template_readme(species, census)), sheets)
}

template_file <- function(species, census = is_census(species)) {
  sprintf("kelpbio-template-%s%s.xlsx", species_info[[species]]$suffix, if (census && species == "nereo") "-census" else "")
}

# The rows of each sheet in an Excel workbook named after a sheet id, by id;
# other sheets are ignored.
read_workbook <- function(path) {
  present <- readxl::excel_sheets(path)
  ids <- Filter(function(id) id %in% present, sheet_ids)
  lapply(ids, function(id) as_sheet_rows(readxl::read_excel(path, sheet = id)))
}

read_csv_rows <- function(path) as_sheet_rows(utils::read.csv(path, check.names = FALSE))

site_years <- function(rows) {
  if (!all(c("site", "year") %in% names(rows))) {
    return(character())
  }
  keep <- !is.na(rows$year) & !is.na(rows$site)
  unique(paste(rows$site[keep], rows$year[keep], sep = "|"))
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

# The surveys with a drone flight, which the cover calibration is fitted to.
drone_surveys <- function(rows) {
  if (!all(c("plot_area_m2", "canopy_area_m2", "tide_height_m") %in% names(rows))) {
    return(rows[0, ])
  }
  rows[!is.na(rows$canopy_area_m2), ]
}

# The canopy sheet's other text columns, such as a region, which canopy
# biomass can be summed over.
canopy_groups <- function(rows) {
  text <- vapply(rows, function(x) is.character(x) || is.factor(x), logical(1))
  setdiff(names(rows)[text], c("site", "year"))
}

# Priors and sampler ----------------------------------------------------------------
# The prior editor works on a table built from the model's kb_priors_*() list: one
# row per entry, named as the parameter it applies to (as the diagnostics name
# it), with its family and hyperparameters (a: mean, log-scale mean or rate; b:
# SD or log-scale SD).

prior_labels <- c(
  intercept = "Intercept", shape = "Shape", diameter_power = "Diameter power", weight_floor = "Weight floor",
  fronds_slope = "Fronds slope", precision = "Precision", cover_slope = "Cover slope", biomass_floor = "Biomass floor",
  tide_height_slope = "Tide height slope", cv_residual = "Residual coefficient of variation",
  sd_site = "SD of site effect", sd_year = "SD of year effect", sd_site_year = "SD of site-year effect",
  sd_residual = "Residual SD"
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

# A model with no kelpbio model for the species (the Macrocystis size model)
# has no priors.
default_priors <- function(id, species = "nereo") {
  if (!has_model(id, species)) {
    return(data.frame(name = character(), label = character(), family = character(), a = numeric(), b = numeric()))
  }
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
    }, error = function(e) condition_text(e))
  })
  unlist(stats::setNames(errors, rows$name))
}

# "Normal(0, 2)", "LogNormal(2, 1)", "Exponential(1)", as kb_model_describe() writes them.
format_prior <- function(prior) {
  name <- c(normal = "Normal", lognormal = "LogNormal", exponential = "Exponential")[prior$family]
  args <- ifelse(prior$family == "exponential", as.character(prior$a), paste0(prior$a, ", ", prior$b))
  sprintf("%s(%s)", name, args)
}

# Every fit keeps the same number of draws, those of kelpbio's reference fits,
# as the derived estimates combine fits draw by draw.
fit_draws <- list(chains = 4L, niters = 1000L)

# The sampler settings that can be changed: the thinning (kelpbio's `nthin`)
# and the sampler's target acceptance rate (`adapt_delta`, passed in
# `control`), at kelpbio's defaults.
default_sampler <- function() list(nthin = 1, adapt_delta = 0.95)

# The message for each invalid sampler setting, by setting: nthin must be a
# positive whole number, as kelpbio checks it, and adapt_delta a number from
# 0.5 to 0.999.
sampler_errors <- function(sampler) {
  errors <- lapply(names(sampler), function(name) {
    tryCatch({
      x_name <- paste0("`", name, "`")
      if (name == "nthin") {
        chk::chk_whole_number(sampler[[name]], x_name = x_name)
        chk::chk_gt(sampler[[name]], value = 0, x_name = x_name)
      } else {
        chk::chk_number(sampler[[name]], x_name = x_name)
        chk::chk_range(sampler[[name]], range = c(0.5, 0.999), x_name = x_name)
      }
      NULL
    }, error = function(e) condition_text(e))
  })
  unlist(stats::setNames(errors, names(sampler)))
}

# The default of an argument of a kelpbio function, for help text that names a
# threshold: default_arg(kb_converged, "rhat").
default_arg <- function(fn, name) eval(formals(fn)[[name]])

# Predictions ---------------------------------------------------------------------

prediction_groupings <- c(
  population = "Overall", site = "Site", year = "Year", site_year = "Site and year",
  plant = "Individual plant"
)

# What each model predicts. `along` names the predictor of a curve model; its
# table, and its by-site-and-year figure, give the value at `at`, the predictor
# value (by species where it differs) passed to kb_new_data() as `value`.
prediction_info <- list(
  size = list(response = "mean diameter", table = "Mean diameter (mm)"),
  weight = list(
    response = "wet weight", along = "diameter", at = "a plant with a 50 mm diameter",
    value = list(nereo = list(diameter_mm = 50), macro = list(fronds = 10)),
    table = "Wet weight (kg) of a plant with a 50 mm diameter",
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
  if (!has_model(id, species)) {
    return(rep(FALSE, length(effect)))
  }
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
# passes its check.
plant_rows <- function(sheets) {
  size <- sheets$size
  if (is.null(size) || !is.null(size$error)) {
    return(NULL)
  }
  size$rows
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
    size = kb_predict_size(fit, new_data),
    weight = kb_predict_weight(fit, new_data),
    cover = kb_predict_cover_calibration(fit, new_data)
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

prediction_caption <- function(id, grouping, species = "nereo") {
  info <- prediction_info[[id]]
  if (grouping == "plant") {
    return(sprintf("Predicted %s of each plant in the size sheet by %s, with 95%% compatibility intervals.", info$response, info$along))
  }
  site <- has_effect(id, "site", species)
  year <- has_effect(id, "year", species)
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

# Derived estimates ----------------------------------------------------------------
# The rows survey and canopy biomass are shown in: each choice's label, a note
# where the label does not say what the rows are, and its kelpbio pool_by value
# (NULL keeps one row per survey or drone map). Biomass is a standing stock in
# the peak season, so sums stay within a year. kelpbio pools by any grouping;
# the app offers the ones a report uses.

survey_sums <- list(
  site_year = list(
    label = "By site and year", pool_by = c("site", "year"),
    detail = list(area = "The surveys of each site-year pooled: their summed biomass over their summed area.", total = "")
  ),
  survey = list(label = "Each survey", pool_by = NULL)
)

# The canopy sheet's own grouping columns (canopy_groups()), such as a region
# or habitat, each give totals by group and year.
canopy_sums <- function(groups = character()) {
  c(
    list(
      site_year = list(label = "Total by drone map", pool_by = NULL),
      year = list(label = "Total by year", pool_by = "year")
    ),
    stats::setNames(lapply(groups, function(g) {
      list(
        label = sprintf("Total by %s and year", g), pool_by = c(g, "year")
      )
    }), groups)
  )
}

# The example workbook of a species: kelpbio's simulated data, one sheet each,
# with a measured sample of Nereocystis plants and a census of Macrocystis
# plants.
example_rows <- function(species) {
  suffix <- species_info[[species]]$suffix
  name <- function(id) if (id == "size") paste0("size_", if (species == "macro") "census" else "sample") else id
  lapply(sheet_ids, function(id) {
    rows <- as.data.frame(getExportedValue("kelpbio", sprintf("data_%s_sim_%s", name(id), suffix)))
    rows[] <- lapply(rows, function(x) if (is.factor(x)) as.character(x) else x)
    rows
  })
}

# The arguments of kb_predict_biomass_survey() for the run: the weight fit, the
# size fit or, in a census, the size sheet's plants, and the surveys.
survey_biomass_args <- function(weight, size, sheets, census) {
  list(weight = weight, size = if (census) sheets$size$rows else size, survey = survey_rows(sheets$survey$rows, census))
}

# The cover calibration's fit, run in the background: kelpbio fits it to the
# wet survey biomass per m2 (with its draws), so the survey biomass is
# predicted first, in the same call.
fit_cover_calibration <- function(species, weight, size, survey, ...) {
  biomass <- kb_predict_biomass_survey(weight, size, survey, progress = "none")
  kelpbio_fn("fit", "cover", species)(data = biomass, ...)
}

# A predictions table without its column of draws (the response, in kelpbio's
# derived predictions), which a table or a spreadsheet cannot show.
summary_columns <- function(rows) {
  response <- attr(rows, "kb_response")
  if (!is.null(response) && response %in% names(rows) && !is.numeric(rows[[response]])) rows[[response]] <- NULL
  rows
}
