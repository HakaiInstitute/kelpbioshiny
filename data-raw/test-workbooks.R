# Writes the example workbooks to data-raw/workbooks/, one per kind of run,
# for uploading in a demo or a test. All are Nereocystis but 1-full-macro.
# They are built from kelpbio's simulated data and, for the canopy-only run,
# the sites of kelpbiodata's reference cover calibration. The app's "Use example workbook" button loads
# 1-full (example_rows()), and the R script of such a run downloads it from
# GitHub (example_url()), so commit it whenever it is rebuilt. Years are written as numbers, as a spreadsheet
# stores them, and the size sheet is named for its kind of size data
# (size_sample or size_census), which sets Size data on upload. Run from the
# package root:
#   Rscript data-raw/test-workbooks.R
#
# Use cases:
# 1-full: every sheet, so each model is fitted to the data through to canopy
#   biomass. Nereocystis has a measured sample of plants, Macrocystis a census.
# 2-demo-seed2: weight, size_sample, survey_in_situ and survey_canopy sheets,
#   with harvests from two sites in two years (demo_weight_rows()) and no
#   tissue samples, so wet:dry, carbon and nitrogen are pre-fit. With the
#   weight model's seed set to 2 on its Settings tab, as the file name
#   recalls, and the other sampler settings at their defaults:
#   - fitted to these harvests alone, the weight model has divergent
#     transitions; raising adapt_delta to 0.99, as the warning advises, fixes
#     them;
#   - fitted to these harvests appended to the Hakai Institute reference data,
#     some parameters do not converge; raising nthin to 2, as the warning
#     advises, fixes it.
#   The weight model can also be pre-fit. Without seed 2, most fits show the
#   same warnings. otter_cove 2020 has three in situ surveys and three canopy
#   maps (repeated_rows()), to show how estimates pool several surveys and
#   several maps of one site-year.
# 3-census: weight, size and survey_in_situ sheets, for surveys that
#   cover the whole area of interest, so survey biomass in total over the area
#   surveyed is the estimate. Every counted plant is measured, each survey's
#   plants drawn from the measured sample of its site-year (census_rows()).
#   There are no canopy imagery columns or survey_canopy sheet, so no cover calibration or
#   canopy biomass; wet:dry, carbon and nitrogen are pre-fit.
# 4-canopy-prefit: a survey_canopy sheet alone, with canopy maps of the sites of the
#   pre-fit Hakai Institute cover calibration mapped again in a year after its
#   data, so canopy biomass takes each site's effect but treats the year as new,
#   and one map of a new site with the same areas as the first site's, whose
#   limits are wider as the site is new too.
#
# Errors:
# 5-bad-data: every sheet, with a weight sheet missing two years (a data
#   error) and a survey_in_situ sheet with three surveys whose area surveyed exceeds
#   their plot (a warning).

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

# Canopy maps of each site of the pre-fit cover calibration in the year after
# its data, with canopy, tide and classified areas from the simulated maps,
# and a map of a site the calibration has no data for ("New site") with the
# areas and tide of the first site's map, so the two can be compared.
canopy_prefit_rows <- function(species, canopy) {
  fit <- prefit_fit("cover", species, "hakai")
  sites <- fit_sites(fit)
  maps <- canopy[sample.int(nrow(canopy), length(sites)), c("canopy_area_m2", "canopy_tide_height_m", "classified_area_m2")]
  rows <- data.frame(site = sites, year = max(as.integer(fit$meta$year_levels)) + 1L, maps, row.names = NULL)
  rbind(rows, transform(rows[1, ], site = "New site"))
}

# Weights of 100 plants in each site-year of two sites over two years,
# resampled from the simulated weights with their diameters jittered: much
# data per group from few groups, which the default sampler settings fit with
# divergent transitions. Seeded on its own, so the data stay the ones tested.
demo_weight_rows <- function(weight) {
  withr::with_seed(7, {
    weight <- weight[weight$site %in% c("cedar_bay", "otter_cove") & weight$year %in% c("2020", "2021"), ]
    do.call(rbind, lapply(split(weight, interaction(weight$site, weight$year, drop = TRUE)), function(g) {
      g <- g[sample.int(nrow(g), 100, replace = TRUE), ]
      g$diameter_mm <- round(g$diameter_mm * exp(stats::rnorm(100, 0, 0.05)), 1)
      g
    }))
  })
}

# Three in situ surveys and three canopy maps of otter_cove 2020: a third survey
# of a sparse plot beside its two denser ones, and two more maps of the whole
# site, one flown at a lower tide and one at a higher tide than its map, with
# the classified area of its map, so the replicates differ visibly.
repeated_rows <- function(survey, canopy) {
  key <- survey$site == "otter_cove" & survey$year == 2020
  third <- survey[which(key)[1], ]
  third$survey_id <- "otter_cove-2020-3"
  third$survey_area_m2 <- 100
  third$plants <- 160
  third$plot_area_m2 <- 230
  third$canopy_area_m2 <- 30.5
  third$canopy_tide_height_m <- 0.9
  survey <- rbind(survey[seq_len(max(which(key))), ], third, survey[-seq_len(max(which(key))), ])

  key <- canopy$site == "otter_cove" & canopy$year == 2020
  maps <- canopy[rep(which(key), 2), ]
  maps$canopy_area_m2 <- c(9600, 4100)
  maps$canopy_tide_height_m <- c(0.3, 1.6)
  canopy <- rbind(canopy[seq_len(max(which(key))), ], maps, canopy[-seq_len(max(which(key))), ])
  list(survey = survey, canopy = canopy)
}

unlink("data-raw/workbooks", recursive = TRUE)
dir.create("data-raw/workbooks")
set.seed(2024)
write_workbook(example_rows("macro"), "1-full", "macro")
full <- example_rows("nereo")
write_workbook(full, "1-full", "nereo")
demo <- full[c("weight", "size", "survey", "canopy")]
demo$weight <- demo_weight_rows(full$weight)
demo[c("survey", "canopy")] <- repeated_rows(demo$survey, demo$canopy)
write_workbook(demo, "2-demo-seed2", "nereo")
census <- full[c("weight", "size", "survey")]
census$size <- census_rows(full$survey, full$size)
census$survey <- census$survey[setdiff(names(census$survey), c("plants", "plot_area_m2", "canopy_area_m2", "canopy_tide_height_m"))]
write_workbook(census, "3-census", "nereo", census = TRUE)
write_workbook(list(canopy = canopy_prefit_rows("nereo", full$canopy)), "4-canopy-prefit", "nereo")
bad <- full
bad$weight$year[c(3, 17)] <- NA
flown <- which(!is.na(bad$survey$plot_area_m2))[1:3]
bad$survey$survey_area_m2[flown] <- bad$survey$plot_area_m2[flown] * 1.5
write_workbook(bad, "5-bad-data", "nereo")
