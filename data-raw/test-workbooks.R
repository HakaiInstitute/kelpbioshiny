# Writes the example workbooks to data-raw/workbooks/, one per kind of run and
# species, for uploading in a demo or a test. They are built from kelpbio's
# simulated data and, for the canopy-only run, the sites of kelpbiodata's
# reference cover calibration. The app's "Use example workbook" button loads
# the first (example_rows()). Years are written as numbers, as a spreadsheet
# stores them, and the size sheet is named for its kind of size data
# (size_sample or size_census), which sets Size data on upload. Run from the
# package root:
#   Rscript data-raw/test-workbooks.R
#
# Use cases:
# 1-full: every sheet, so each model is fitted to the data through to canopy
#   biomass. Nereocystis has a measured sample of plants, Macrocystis a census.
# 2-prefit: size, survey and canopy sheets but no harvests or tissue samples,
#   so weight, wet:dry and carbon are pre-fit.
# 3-census (Nereocystis only): weight, size and survey sheets, for surveys that
#   cover the whole area of interest, so survey biomass in total over the area
#   surveyed is the estimate. Every counted plant is measured, each survey's
#   plants drawn from the measured sample of its site-year (census_rows()).
#   There are no drone columns or canopy sheet, so no cover calibration or
#   canopy biomass; wet:dry and carbon are pre-fit.
# 4-canopy-prefit: a canopy sheet alone, with drone maps of the sites of the
#   pre-fit Hakai Institute cover calibration flown again in a year after its
#   data, so canopy biomass takes each site's effect but treats the year as new,
#   and one map of a new site with the same areas as the first site's, whose
#   limits are wider as the site is new too.
#
# Errors:
# 5-bad-data: every sheet, with a weight sheet missing two years (a data
#   error) and a survey sheet with three surveys whose area surveyed exceeds
#   their plot (a warning).
# 6-refit (Nereocystis only): a weight sheet of many plants from a few sites
#   whose weights differ widely (refit_rows()), whose fit at the default
#   sampler settings usually has divergent transitions; raising adapt_delta to
#   0.99, as the warning advises, fixes it.

pkgload::load_all(quiet = TRUE)

write_workbook <- function(sheets, name, species, census = is_census(species)) {
  for (id in names(sheets)) {
    if ("year" %in% names(sheets[[id]])) sheets[[id]]$year <- as.integer(sheets[[id]]$year)
  }
  names(sheets) <- vapply(names(sheets), sheet_name, "", census = census)
  writexl::write_xlsx(sheets, file.path("data-raw/workbooks", sprintf("kelpbio-%s-%s.xlsx", name, species)))
}

# A census of the surveys of a measured sample: each survey's plants drawn
# from the diameters measured in its site-year or, where none were measured,
# at its site in other years, or else anywhere.
census_rows <- function(survey, size) {
  plants <- lapply(seq_len(nrow(survey)), function(i) {
    pool <- size$diameter_mm[size$site == survey$site[i] & size$year == survey$year[i]]
    if (length(pool) == 0) pool <- size$diameter_mm[size$site == survey$site[i]]
    if (length(pool) == 0) pool <- size$diameter_mm
    diameter <- pool[sample.int(length(pool), survey$plants[i], replace = TRUE)]
    data.frame(survey_id = rep(survey$survey_id[i], length(diameter)), diameter_mm = diameter)
  })
  do.call(rbind, plants)
}

# Drone maps of each site of the pre-fit cover calibration in the year after
# its data, with canopy, tide and classified areas from the simulated maps,
# and a map of a site the calibration has no data for ("New site") with the
# areas and tide of the first site's map, so the two can be compared. The
# Macrocystis maps leave out Manley and Triquetta, which also have
# Nereocystis, and Golden and Womanley, for a smaller set of sites of
# Macrocystis alone.
canopy_prefit_rows <- function(species, canopy) {
  fit <- prefit_fit("cover", species, "hakai")
  sites <- fit_sites(fit)
  maps <- canopy[sample.int(nrow(canopy), length(sites)), c("canopy_area_m2", "tide_height_m", "classified_area_m2")]
  rows <- data.frame(site = sites, year = max(as.integer(fit$meta$year_levels)) + 1L, maps, row.names = NULL)
  if (species == "macro") rows <- rows[!rows$site %in% c("Manley", "Triquetta", "Golden", "Womanley"), ]
  rbind(rows, transform(rows[1, ], site = "New site"))
}

# Weights of 150 plants in each site-year of three sites over two years,
# resampled from the simulated weights with the sites' weights spread apart
# (exp(-1.2) to exp(1.2) times): much data per group with large differences
# between groups, which the default sampler settings fit with divergent
# transitions. Seeded on its own, so the data stay the ones tested.
refit_rows <- function(weight) {
  withr::with_seed(7, {
    weight <- weight[weight$site %in% unique(weight$site)[1:3] & weight$year %in% unique(weight$year)[1:2], ]
    weight <- do.call(rbind, lapply(split(weight, interaction(weight$site, weight$year, drop = TRUE)), function(g) {
      g <- g[sample.int(nrow(g), 150, replace = TRUE), ]
      g$diameter_mm <- round(g$diameter_mm * exp(stats::rnorm(150, 0, 0.05)), 1)
      g
    }))
    shift <- stats::setNames(seq(-1.2, 1.2, length.out = 3), unique(weight$site))
    weight$weight_kg <- round(weight$weight_kg * exp(shift[weight$site]), 3)
    weight
  })
}

unlink("data-raw/workbooks", recursive = TRUE)
dir.create("data-raw/workbooks")
set.seed(2024)
for (species in names(species_info)) {
  full <- example_rows(species)
  write_workbook(full, "1-full", species)
  write_workbook(full[c("size", "survey", "canopy")], "2-prefit", species)
  if (species == "nereo") {
    census <- full[c("weight", "size", "survey")]
    census$size <- census_rows(full$survey, full$size)
    census$survey <- census$survey[setdiff(names(census$survey), c("plants", "plot_area_m2", "canopy_area_m2", "tide_height_m"))]
    write_workbook(census, "3-census", species, census = TRUE)
  }
  write_workbook(list(canopy = canopy_prefit_rows(species, full$canopy)), "4-canopy-prefit", species)
  bad <- full
  bad$weight$year[c(3, 17)] <- NA
  flown <- which(!is.na(bad$survey$plot_area_m2))[1:3]
  bad$survey$survey_area_m2[flown] <- bad$survey$plot_area_m2[flown] * 1.5
  write_workbook(bad, "5-bad-data", species)
  if (species == "nereo") write_workbook(list(weight = refit_rows(full$weight)), "6-refit", species)
}
