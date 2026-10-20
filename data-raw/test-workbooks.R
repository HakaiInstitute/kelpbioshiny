# Writes test workbooks built from kelpbio's simulated data to
# data-raw/workbooks/, one per kind of user and species, for uploading in a
# demo or a test. The app's "Use example workbook" button loads the first
# (example_rows()). Years are written as numbers, as a spreadsheet stores them.
# Run from the package root:
#   Rscript data-raw/test-workbooks.R
#
# 1-full: every sheet, so each model is fitted to the data through to canopy
#   biomass. Nereocystis has a measured sample of plants, Macrocystis a census.
# 2-prefit: size, survey and canopy sheets but no harvests or tissue samples,
#   so weight, wet:dry and carbon are pre-fit.
# 3-census (Nereocystis only): every counted plant measured, each survey's
#   plants drawn from the measured sample of its site-year, and a twentieth
#   of them partial. Choose "All plants measured" under Size data before
#   uploading.
# 4-bad-data: every sheet, with a weight sheet missing two years (a data
#   error) and a survey sheet with three surveys whose area surveyed exceeds
#   their plot (a warning).

pkgload::load_all(quiet = TRUE)

write_workbook <- function(sheets, name, species) {
  for (id in names(sheets)) {
    if ("year" %in% names(sheets[[id]])) sheets[[id]]$year <- as.integer(sheets[[id]]$year)
  }
  writexl::write_xlsx(sheets, file.path("data-raw/workbooks", sprintf("kelpbio-%s-%s.xlsx", name, species)))
}

# A census of the surveys of a measured sample: each survey's plants drawn
# from the diameters measured in its site-year.
census_rows <- function(survey, size) {
  plants <- lapply(seq_len(nrow(survey)), function(i) {
    pool <- size$diameter_mm[size$site == survey$site[i] & size$year == survey$year[i]]
    diameter <- if (length(pool) > 0) pool[sample.int(length(pool), survey$plants[i], replace = TRUE)] else numeric()
    data.frame(survey_id = rep(survey$survey_id[i], length(diameter)), diameter_mm = diameter)
  })
  plants <- do.call(rbind, plants)
  plants$partial <- stats::runif(nrow(plants)) < 0.05
  plants
}

unlink("data-raw/workbooks", recursive = TRUE)
dir.create("data-raw/workbooks")
set.seed(2024)
for (species in names(species_info)) {
  full <- example_rows(species)
  write_workbook(full, "1-full", species)
  write_workbook(full[c("size", "survey", "canopy")], "2-prefit", species)
  if (species == "nereo") {
    census <- full
    census$size <- census_rows(full$survey, full$size)
    census$survey$plants <- NULL
    write_workbook(census, "3-census", species)
  }
  bad <- full
  bad$weight$year[c(3, 17)] <- NA
  flown <- which(!is.na(bad$survey$plot_area_m2))[1:3]
  bad$survey$survey_area_m2[flown] <- bad$survey$plot_area_m2[flown] * 1.5
  write_workbook(bad, "4-bad-data", species)
}
