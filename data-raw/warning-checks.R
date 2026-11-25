# Writes warning-checks/ (gitignored and build-ignored): one upload per
# warning or error the app shows, each named for what it should show, to check
# them by hand. Each is the Nereocystis example workbook (example_rows()) with
# one change, unless noted. Names start with the step where the message shows:
#   upload-  a notification when the file is uploaded
#   data-    the Data step's sheet checks (kelpbio's kb_check_data_*())
#   models-  the Models step, before fitting
#   fit-     a model's warnings once fitted (runs MCMC)
#   estimate- the Estimates step's biomass (kelpbio's kb_predict_biomass_*())
# CSV cases are in csv/<case>/, as a CSV file's name sets its sheet.
# warning-checks/README.md lists each file and what it should show. Divergent
# transitions and unconverged parameters are in
# data-raw/workbooks/kelpbio-2-demo-seed2-nereo.xlsx. Run from the package root:
#   Rscript data-raw/warning-checks.R

pkgload::load_all(quiet = TRUE)

out <- "warning-checks"
unlink(out, recursive = TRUE)
dir.create(file.path(out, "csv"), recursive = TRUE)

set.seed(1)
base <- example_rows("nereo")
cases <- list()

# A case: `sheets` by sheet id (or a raw file with `file`), where it shows and
# what it should show.
add <- function(name, step, expect, sheets = NULL, census = FALSE, file = NULL, csv = NULL) {
  cases[[name]] <<- list(name = name, step = step, expect = expect, sheets = sheets, census = census, file = file, csv = csv)
}

change <- function(id, f, rows = base) {
  rows[[id]] <- f(rows[[id]])
  rows
}

first_rows <- function(x, n = 5) seq_len(min(n, nrow(x)))

# Upload -----------------------------------------------------------------------

add("upload-error-no-known-sheet", "Upload", "Error: the workbook has no sheet kelpbio reads, naming the sheet names to use.",
  file = function(path) writexl::write_xlsx(list(Sheet1 = base$weight), path))
add("upload-error-unreadable", "Upload", "Error: the file could not be read (a text file named .xlsx).",
  file = function(path) writeLines("not a workbook", path))
add("upload-error-unknown-csv-name", "Upload", "Error: weights.csv matches no sheet, naming the CSV file names to use.",
  csv = list(weights = base$weight))
add("upload-note-size-census-sheet", "Upload", "Notice: Size data set to All plants measured, from the size_census sheet.",
  sheets = local({
    rows <- base[c("weight", "survey")]
    rows$survey$plants <- NULL
    rows$size <- data.frame(survey_id = rep(rows$survey$survey_id, each = 3), diameter_mm = sample(base$size$diameter_mm, 3 * nrow(rows$survey)))
    rows
  }), census = TRUE)

# Data step: kelpbio check warnings ---------------------------------------------

add("data-warn-diameter-in-cm", "Data (weight)", "Warning: diameter_mm has a median unusually small for mm.",
  change("weight", function(x) transform(x, diameter_mm = diameter_mm / 10)))
add("data-warn-weight-in-g", "Data (weight)", "Warning: weight_kg has a median unusually large for kg.",
  change("weight", function(x) transform(x, weight_kg = weight_kg * 1000)))
add("data-warn-survey-area-in-ha", "Data (survey_in_situ)", "Warning: survey_area_m2 has a median unusually small for m\u00b2.",
  change("survey", function(x) transform(x, survey_area_m2 = survey_area_m2 / 10000)))
add("data-warn-survey-area-over-plot", "Data (survey_in_situ)", "Warning: 3 surveys have survey_area_m2 > plot_area_m2.",
  change("survey", function(x) {
    i <- which(!is.na(x$plot_area_m2))[1:3]
    x$survey_area_m2[i] <- round(x$plot_area_m2[i] * 1.2, 1)
    x
  }))
add("data-warn-plot-area-in-cm2", "Data (survey_in_situ)", "Warning: plot_area_m2 has a median unusually large for m\u00b2.",
  change("survey", function(x) transform(x, plot_area_m2 = plot_area_m2 * 10000, canopy_area_m2 = canopy_area_m2 * 10000)))
add("data-warn-tide-height-unusual", "Data (survey_in_situ)", "Warning: canopy_tide_height_m has a median unusually large for m.",
  change("survey", function(x) transform(x, canopy_tide_height_m = canopy_tide_height_m + 10)))
add("data-warn-classified-area-in-km2", "Data (survey_canopy)", "Warning: classified_area_m2 has a median unusually small for m\u00b2.",
  change("canopy", function(x) transform(x, classified_area_m2 = classified_area_m2 / 1e6, canopy_area_m2 = canopy_area_m2 / 1e6)))
add("data-warn-wet-mass-in-mg", "Data (wetdry)", "Warning: wet_mass_g has a median unusually large for g.",
  change("wetdry", function(x) transform(x, wet_mass_g = wet_mass_g * 1000, dry_mass_g = dry_mass_g * 1000)))
add("data-warn-dry-wet-ratio", "Data (wetdry)", "Warning: 5 samples have a dry:wet mass ratio outside 0.02 to 0.50.",
  change("wetdry", function(x) {
    i <- first_rows(x)
    x$dry_mass_g[i] <- x$wet_mass_g[i] * 0.6
    x
  }))
add("data-warn-carbon-fraction", "Data (carbon)", "Warning: 5 samples have a carbon fraction outside 0.10 to 0.50.",
  change("carbon", function(x) {
    i <- first_rows(x)
    x$carbon_mass_ug[i] <- x$carbon_mass_ug[i] / 10
    x
  }))
add("data-warn-nitrogen-fraction", "Data (nitrogen)", "Warning: 5 samples have a nitrogen fraction outside 0.002 to 0.05.",
  change("nitrogen", function(x) {
    i <- first_rows(x)
    x$nitrogen_mass_ug[i] <- x$nitrogen_mass_ug[i] * 5
    x
  }))

# Data step: app warnings and errors --------------------------------------------

add("data-warn-site-names-differ", "Data and Models", "Warning: site names differ across sheets only in case; they are treated as different sites.",
  change("weight", function(x) {
    x$site[x$site == x$site[1]] <- toupper(x$site[1])
    x
  }))
add("data-error-negative-weight", "Data (weight)", "Error: weight_kg must be greater than 0.",
  change("weight", function(x) {
    x$weight_kg[1] <- -1
    x
  }))
add("data-error-missing-column", "Data (weight)", "Error: the weight sheet has no weight_kg column.",
  change("weight", function(x) x[setdiff(names(x), "weight_kg")]))
add("data-error-no-plants-tally", "Data (survey_in_situ)", "Error: plants is missing, with the advice to choose All plants measured if the size sheet holds every counted plant.",
  change("survey", function(x) x[setdiff(names(x), "plants")]))

# Models step ------------------------------------------------------------------

add("models-warn-overlap-hakai", "Models (weight)", "Warning once the weight source is Your data + Hakai Institute: the weight sheet and the Hakai Institute data share site-years.",
  list(weight = as.data.frame(utils::head(kelpbiodata::data_weight_hakai_nereo, 200))))

# Fit warnings (run MCMC) --------------------------------------------------------

add("fit-warn-weight-site-year-aliased", "Models (weight, after Fit)", "Warning: the site:year effect cannot be separated from the site effect, as no site was sampled in more than one year.",
  change("weight", function(x) {
    first_year <- tapply(x$year, x$site, min)
    x[x$year == first_year[x$site], ]
  }))
add("fit-warn-cover-high-tide", "Models (cover calibration, after Fit)", "Warning: 3 surveys have canopy imaged at a canopy_tide_height_m above 2 m.",
  change("survey", function(x) {
    i <- which(!is.na(x$canopy_area_m2) & x$canopy_area_m2 > 0)[1:3]
    x$canopy_tide_height_m[i] <- 2.5
    x
  }))
add("fit-warn-cover-presence-separation", "Models (cover calibration, after Fit)", "Warning: every survey with no plants has a percent canopy cover at or below every survey with plants.",
  change("survey", function(x) {
    i <- which(!is.na(x$canopy_area_m2))[1:2]
    x$plants[i] <- 0
    x$canopy_area_m2[x$plants == 0 & !is.na(x$canopy_area_m2)] <- 0
    x
  }))

# Estimates step ---------------------------------------------------------------

add("estimate-warn-canopy-high-tide", "Estimates (canopy biomass)", "Warning: 3 rows have canopy imaged at a canopy_tide_height_m above 2 m.",
  change("canopy", function(x) {
    x$canopy_tide_height_m[1:3] <- 2.5
    x
  }))
add("estimate-warn-canopy-maps-overlap", "Estimates (canopy biomass, Total by year)", "Warning: some pool_by groups hold more than one row for a site-year, so their totals overlap.",
  change("canopy", function(x) rbind(x, x[1, ])))
add("estimate-warn-plants-beyond-weight-data", "Estimates (in situ survey biomass; census)", "Warning: 3 values of diameter_mm lie far outside the fitted range of the weight data.",
  local({
    rows <- base[c("weight", "survey", "wetdry", "carbon", "nitrogen")]
    rows$survey$plants <- NULL
    rows$survey <- rows$survey[setdiff(names(rows$survey), c("plot_area_m2", "canopy_area_m2", "canopy_tide_height_m"))]
    rows$size <- data.frame(survey_id = rep(rows$survey$survey_id, each = 3), diameter_mm = sample(base$size$diameter_mm, 3 * nrow(rows$survey)))
    rows$size$diameter_mm[1:3] <- 200
    rows
  }), census = TRUE)

# Write ------------------------------------------------------------------------

write_sheets <- function(sheets, path, census) {
  for (id in names(sheets)) {
    if ("year" %in% names(sheets[[id]])) sheets[[id]]$year <- as.integer(as.character(sheets[[id]]$year))
  }
  names(sheets) <- vapply(names(sheets), sheet_name, "", census = census)
  writexl::write_xlsx(sheets, path)
}

for (case in cases) {
  if (!is.null(case$csv)) {
    dir <- file.path(out, "csv", case$name)
    dir.create(dir)
    for (file in names(case$csv)) utils::write.csv(case$csv[[file]], file.path(dir, paste0(file, ".csv")), row.names = FALSE)
  } else if (!is.null(case$file)) {
    case$file(file.path(out, paste0(case$name, ".xlsx")))
  } else {
    write_sheets(case$sheets, file.path(out, paste0(case$name, ".xlsx")), case$census)
  }
}

# A CSV version of two data checks, for the CSV upload.
for (name in c("data-warn-diameter-in-cm", "data-warn-carbon-fraction")) {
  dir <- file.path(out, "csv", name)
  dir.create(dir)
  id <- if (grepl("carbon", name)) "carbon" else "weight"
  utils::write.csv(cases[[name]]$sheets[[id]], file.path(dir, paste0(id, ".csv")), row.names = FALSE)
}

readme <- c(
  "# Warning checks",
  "",
  "Uploads that each show one warning or error, written by `data-raw/warning-checks.R` (not committed).",
  "Each is the Nereocystis example workbook with one change, unless noted; upload it and look where Step says.",
  "fit- files need the model fitted (MCMC); estimate- files need the models fitted, then the estimate opened.",
  "CSV cases are in `csv/<case>/`; upload the files inside, as a CSV file's name sets its sheet.",
  "Divergent transitions and unconverged parameters are in `data-raw/workbooks/kelpbio-2-demo-seed2-nereo.xlsx`.",
  "",
  "| File | Step | Should show |",
  "|------|------|-------------|",
  vapply(cases, function(case) sprintf("| %s | %s | %s |", case$name, case$step, case$expect), "")
)
writeLines(readme, file.path(out, "README.md"))
cat(length(cases), "cases written to", out, "\n")
