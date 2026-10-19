# Mock example data ----------------------------------------------------------------
#
# The example workbooks the app loads, and the fake survey data behind the test
# workbooks in data-raw/. Delete this file with R/mock-kelpbio.R. The real example
# data should come from kelpbio's bundled simulated datasets (data_weight_sim_nereo,
# data_size_sim_nereo, data_survey_sim_nereo, data_canopy_sim_nereo,
# data_plants_sim_macro and their twins), through an accessor like
# kb_example_data().

# Site-years left out of the fake survey data, so the examples have the gaps of
# a real monitoring program: surveys not at every site in every year, size
# missing from some surveyed site-years, and weight sampled at a few sites in a
# few years.
.mock_example_key <- function(site, year) paste(site, year)
.mock_survey_gaps <- .mock_example_key(c("site9", "site9", "site4", "site10", "site10"), c(2019, 2020, 2022, 2024, 2025))
.mock_size_gaps <- .mock_example_key(
  c("site9", "site3", "site8", "site6", "site6", "site2", "site10", "site10"),
  c(2019, 2021, 2021, 2023, 2024, 2025, 2024, 2025)
)
# Site-years with drone cover of plots but no map of the whole site.
.mock_unmapped <- .mock_example_key(c("site2", "site3"), c(2020, 2021))
.mock_weight_samples <- c(
  .mock_example_key(rep(c("site1", "site3", "site5", "site7"), each = 3), rep(c(2019, 2021, 2023), times = 4)),
  .mock_example_key("site2", 2024)
)

# Nereocystis allometry from bulb diameter (mm), for the census example: log
# wet weight (kg) on log bulb diameter, and log bulb diameter on log maximum
# sub-bulb diameter, as fitted to the Hakai Institute harvests (1,388 plants
# at 29 sites). Bulb diameter predicts weight less well than sub-bulb
# diameter, so its residual SD is larger. Residuals are capped at 2 SD, so the
# simulated weights stay within the range harvested.
.mock_bulb_weight <- c(intercept = -15.755, slope = 3.704, sdlog = 0.927)
.mock_bulb_subbulb <- c(intercept = 3.450, slope = 0.239, sdlog = 0.138)

# The day each site was surveyed in a year, in the peak season.
.mock_survey_date <- function(site, year) as.Date(sprintf("%s-08-%02d", year, 9 + as.integer(gsub("\\D", "", site))))

# The plots of each surveyed site-year, each surveyed once, with its fake
# density of plants per m2: the quadrats of the fake density data. One plot
# per site-year, as the Hakai Institute surveys have, or two or three
# (`several`), the third plot left out of about half the site-years.
.mock_example_plots <- function(several = FALSE) {
  density <- .mock_json("fake-density")
  density <- density[!.mock_example_key(density$site, density$year) %in% .mock_survey_gaps, ]
  keep <- if (several) {
    third <- .mock_with_seed(29, {
      keys <- unique(.mock_example_key(density$site, density$year))
      keys[stats::runif(length(keys)) < 0.5]
    })
    density$quadrat < 3 | .mock_example_key(density$site, density$year) %in% third
  } else {
    density$quadrat == 1
  }
  density <- density[keep, ]
  data.frame(
    site = density$site, year = as.character(density$year), plot_id = as.character(density$quadrat),
    date = .mock_survey_date(density$site, density$year), density = density$density
  )
}

# A plant table of every plant counted in each survey (`counts`), each with a
# size from `draw(site, year, n)`. A few plants on the edge of a transect are
# counted as half.
.mock_example_plants <- function(survey, counts, size_column, draw, seed) {
  rows <- .mock_with_seed(seed, lapply(seq_len(nrow(survey)), function(i) {
    if (counts[i] == 0) {
      return(NULL)
    }
    data.frame(
      site = survey$site[i], plot_id = survey$plot_id[i], date = survey$date[i],
      size = draw(survey$site[i], format(survey$date[i], "%Y"), counts[i]),
      partial = stats::runif(counts[i]) < 0.05
    )
  }))
  plants <- do.call(rbind, rows)
  names(plants)[names(plants) == "size"] <- size_column
  plants
}

# n fake sizes from a site-year's fake sub-bulb diameters (all of them where it
# has none).
.mock_draw_diameter <- function(site, year, n) {
  sizes <- .mock_json("fake-size")
  pool <- sizes$diameter[sizes$site == site & as.character(sizes$year) == year]
  if (length(pool) == 0) pool <- sizes$diameter
  pool[sample.int(length(pool), n, replace = TRUE)]
}
.mock_draw_fronds <- function(site, year, n) pmax(1, round(.mock_draw_diameter(site, year, n) / 4))

# n bulb diameters (mm), from fake sub-bulb diameters through the Hakai relationship.
.mock_draw_bulb <- function(site, year, n) {
  b <- .mock_bulb_subbulb
  round(exp(b[["intercept"]] + b[["slope"]] * log(.mock_draw_diameter(site, year, n)) + stats::rnorm(n, 0, b[["sdlog"]])), 1)
}

# The surveys of `plots`, each surveying transects within its plot, the plants
# tallied (Nereocystis with a measured sample) or not (a census), and the plot
# polygon, canopy and tide height of the plots flown by drone: the plots of the
# fake cover data with a percent cover, at one tide height per site-year.
.mock_example_survey <- function(plots, species, census) {
  area <- if (species == "macro") 60 else 120
  counts <- round(plots$density * area * if (species == "macro") 0.25 else 1)
  flights <- .mock_json("fake-cover")
  i <- match(paste(plots$site, plots$year, plots$plot_id), paste(flights$site, flights$year, flights$plot))
  flown <- !is.na(i) & !is.na(flights$plot_percent_cover[i])
  boundary <- .mock_with_seed(17, round(stats::runif(nrow(plots), 180, 280), 1))
  survey <- data.frame(site = plots$site, plot_id = plots$plot_id, date = plots$date, survey_area_m2 = area)
  if (!census) survey$plants <- counts
  survey$plot_area_m2 <- ifelse(flown, boundary, NA)
  survey$canopy_area_m2 <- ifelse(flown, round(boundary * flights$plot_percent_cover[i] / 100, 2), NA)
  survey$tide_height_m <- ifelse(flown, unname(.mock_tides()[.mock_example_key(plots$site, plots$year)]), NA)
  list(survey = survey, counts = counts)
}

# One tide height per site-year flown by drone.
.mock_tides <- function() {
  flights <- .mock_json("fake-cover")
  keys <- unique(.mock_example_key(flights$site, flights$year))
  .mock_with_seed(13, stats::setNames(round(stats::runif(length(keys), 0, 1.5), 2), keys))
}

# The canopy mapped over each site in a drone survey, in two regions.
.mock_example_canopy <- function() {
  flights <- .mock_json("fake-cover")
  mapped <- unique(flights[!is.na(flights$canopy_area), c("site", "year", "canopy_area")])
  mapped <- mapped[!.mock_example_key(mapped$site, mapped$year) %in% .mock_unmapped, ]
  data.frame(
    site = mapped$site, year = mapped$year, canopy_area_m2 = mapped$canopy_area,
    tide_height_m = unname(.mock_tides()[.mock_example_key(mapped$site, mapped$year)]),
    region = ifelse(as.integer(gsub("\\D", "", mapped$site)) <= 5, "north", "south")
  )
}

# Harvested plants: a sub-bulb diameter (Nereocystis) or frond count
# (Macrocystis) and wet weight, at a few site-years.
.mock_example_weight <- function(species) {
  weight <- .mock_json("fake-weight")
  weight <- weight[.mock_example_key(weight$site, weight$year) %in% .mock_weight_samples, ]
  date <- .mock_survey_date(weight$site, weight$year)
  if (species == "macro") {
    data.frame(site = weight$site, date = date, fronds = pmax(1, round(weight$diameter / 4)), weight_kg = weight$weight)
  } else {
    data.frame(site = weight$site, date = date, diameter_mm = weight$diameter, weight_kg = weight$weight)
  }
}

# Tissue samples: a wet and a dry mass per sample, and the sample and carbon
# mass of each dried sample analysed for carbon.
.mock_example_tissue <- function() {
  .mock_with_seed(11, {
    wet <- round(stats::runif(150, 50, 400), 1)
    wetdry <- data.frame(wet_mass_g = wet, dry_mass_g = round(wet * stats::plogis(stats::rnorm(150, stats::qlogis(0.1), 0.2)), 1))
    sample <- round(stats::runif(60, 2, 3), 2)
    carbon <- data.frame(sample_mass_mg = sample, carbon_mass_ug = round(sample * 1000 * stats::plogis(stats::rnorm(60, stats::qlogis(0.26), 0.15))))
  })
  list(wetdry = wetdry, carbon = carbon)
}

# Every sheet for 10 sites over 2019-2025, with the gaps above, in kelpbio's
# column names: one plot per site-year, or two or three (`several`). In
# Nereocystis surveys the plants are tallied and the size sheet holds a
# measured sample; Macrocystis surveys are a census, with the size sheet
# holding every counted plant.
.mock_example_sheets <- function(species, several = FALSE) {
  census <- species == "macro"
  plots <- .mock_example_plots(several)
  surveyed <- .mock_example_survey(plots, species, census)
  size <- if (census) {
    .mock_example_plants(surveyed$survey, surveyed$counts, "fronds", .mock_draw_fronds, seed = 19)
  } else {
    sizes <- .mock_json("fake-size")
    sizes <- sizes[!.mock_example_key(sizes$site, sizes$year) %in% .mock_size_gaps, ]
    data.frame(site = sizes$site, plot_id = "1", date = .mock_survey_date(sizes$site, sizes$year), diameter_mm = sizes$diameter)
  }
  c(
    list(weight = .mock_example_weight(species), size = size, survey = surveyed$survey, canopy = .mock_example_canopy()),
    .mock_example_tissue()
  )
}

# A census of whole sites, each a single plot surveyed in full, at 6 sites over
# 2021-2024, with no drone flights. Nereocystis plants have their bulb
# diameter measured (in diameter_mm), and the harvests are weighed against
# bulb diameter, simulated from the Hakai relationships above; Macrocystis
# plants have their fronds counted.
.mock_example_census <- function(species) {
  macro <- species == "macro"
  keys <- expand.grid(site = paste0("site", 1:6), year = as.character(2021:2024), stringsAsFactors = FALSE)
  .mock_with_seed(23, {
    area <- round(stats::runif(nrow(keys), 250, 600))
    counts <- round(area * stats::runif(nrow(keys), 0.3, 0.8) * if (macro) 0.5 else 1)
  })
  survey <- data.frame(site = keys$site, plot_id = "1", date = .mock_survey_date(keys$site, keys$year), survey_area_m2 = area)
  size <- .mock_example_plants(
    survey, counts, if (macro) "fronds" else "diameter_mm", if (macro) .mock_draw_fronds else .mock_draw_bulb,
    seed = 31
  )
  weight <- if (macro) {
    .mock_example_weight("macro")
  } else {
    harvested <- keys[keys$site %in% c("site1", "site3", "site5") & keys$year %in% c("2021", "2023"), ]
    .mock_with_seed(37, {
      rows <- lapply(seq_len(nrow(harvested)), function(i) {
        bulb <- .mock_draw_bulb(harvested$site[i], harvested$year[i], 25)
        w <- .mock_bulb_weight
        data.frame(
          site = harvested$site[i], date = .mock_survey_date(harvested$site[i], harvested$year[i]), diameter_mm = bulb,
          weight_kg = round(exp(w[["intercept"]] + w[["slope"]] * log(bulb) + pmin(pmax(stats::rnorm(length(bulb)), -2), 2) * w[["sdlog"]]), 3)
        )
      })
    })
    do.call(rbind, rows)
  }
  list(weight = weight, size = size, survey = survey)
}

# Planned; the name and examples are a proposal. An example workbook for a
# species: a named list of data frames, one per sheet, in kelpbio's column names.
# The examples are the workbooks of different kinds of user:
# - "full": every sheet, one plot per site-year as the Hakai Institute surveys
#   have, so each model can be fitted to the data, through to canopy biomass;
# - "survey": two or three plots per site-year, with size, survey and canopy
#   data but no harvests, so weight (pre-fit Coastwide), wetdry and carbon are
#   pre-fit;
# - "census": every plant measured on whole sites, each one plot, with no
#   drone flights, so survey biomass weighs each plant; Nereocystis size and
#   weight use bulb diameter;
# - "bad_weight": the full example with a weight sheet missing two dates, so
#   its data check fails, and a carbon sheet with three samples outside the
#   plausible carbon fractions, so its check passes with a warning and its fit
#   flags them as influential observations.
# data-raw/test-workbooks.R writes each example as an Excel workbook.
kb_example_data <- function(species = c("nereo", "macro"), example = c("survey", "full", "census", "bad_weight")) {
  species <- rlang::arg_match(species)
  example <- rlang::arg_match(example)
  switch(example,
    full = .mock_example_sheets(species),
    survey = .mock_example_sheets(species, several = TRUE)[c("size", "survey", "canopy")],
    census = .mock_example_census(species),
    bad_weight = {
      sheets <- .mock_example_sheets(species)
      sheets$weight$date[c(42, 218)] <- NA
      carbon <- sheets$carbon
      carbon$carbon_mass_ug[c(5, 23, 47)] <- round(carbon$sample_mass_mg[c(5, 23, 47)] * c(600, 80, 650))
      sheets$carbon <- carbon
      sheets
    }
  )
}
