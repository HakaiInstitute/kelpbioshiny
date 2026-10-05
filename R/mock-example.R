# Mock example data ----------------------------------------------------------------
#
# The example workbook the app loads, and the fake survey data behind the test
# workbooks in data-raw/. Delete this file with R/mock-kelpbio.R. The real example
# data should come from kelpbio's bundled simulated datasets (data_density_sim_nereo,
# data_size_sim_nereo, data_weight_sim_nereo and their macro twins), through an
# accessor like kb_example_data().

# Site-years left out of the fake survey data, so the example has the gaps of a
# real monitoring program: density not at every site in every year, size missing
# from some surveyed site-years, and weight sampled at a few sites in a few years.
.mock_example_key <- function(site, year) paste(site, year)
.mock_density_gaps <- .mock_example_key(c("site9", "site9", "site4", "site10", "site10"), c(2019, 2020, 2022, 2024, 2025))
.mock_size_gaps <- .mock_example_key(
  c("site9", "site3", "site8", "site6", "site6", "site2", "site10", "site10"),
  c(2019, 2021, 2021, 2023, 2024, 2025, 2024, 2025)
)
.mock_weight_samples <- c(
  .mock_example_key(rep(c("site1", "site3", "site5", "site7"), each = 3), rep(c(2019, 2021, 2023), times = 4)),
  .mock_example_key("site2", 2024)
)

# Every sheet the fake survey data provide, in kelpbio's column names: density,
# size, weight, wetdry, carbon and cover, for 10 sites over 2019-2025, with the
# gaps above.
.mock_example_sheets <- function(species = c("nereo", "macro")) {
  species <- rlang::arg_match(species)
  macro <- species == "macro"

  density <- .mock_json("fake-density")
  # Quadrats of 4 m2, so the fake densities become whole stipe counts.
  density <- data.frame(
    site = density$site, year = as.character(density$year), transect = density$quadrat,
    count = round(density$density * 4), area_m2 = 4
  )
  names(density)[names(density) == "count"] <- if (macro) "plants" else "stipes"
  density <- density[!.mock_example_key(density$site, density$year) %in% .mock_density_gaps, ]

  size <- .mock_json("fake-size")
  size <- size[!.mock_example_key(size$site, size$year) %in% .mock_size_gaps, ]
  weight <- .mock_json("fake-weight")
  weight <- weight[.mock_example_key(weight$site, weight$year) %in% .mock_weight_samples, ]
  if (macro) {
    size <- data.frame(site = size$site, year = size$year, fronds = pmax(1, round(size$diameter / 4)))
    weight <- data.frame(site = weight$site, year = weight$year, fronds = pmax(1, round(weight$diameter / 4)), weight_kg = weight$weight)
  } else {
    size <- data.frame(site = size$site, year = size$year, diameter_mm = size$diameter)
    weight <- data.frame(site = weight$site, year = weight$year, diameter_mm = weight$diameter, weight_kg = weight$weight)
  }

  # Tissue samples: a wet and a dry mass per sample, and the sample and carbon
  # mass of each dried sample analysed for carbon.
  .mock_with_seed(11, {
    wet <- round(stats::runif(150, 50, 400), 1)
    wetdry <- data.frame(wet_mass_g = wet, dry_mass_g = round(wet * stats::plogis(stats::rnorm(150, stats::qlogis(0.1), 0.2)), 1))
    sample <- round(stats::runif(60, 2, 3), 2)
    carbon <- data.frame(sample_mass_mg = sample, carbon_mass_ug = round(sample * 1000 * stats::plogis(stats::rnorm(60, stats::qlogis(0.26), 0.15))))
  })

  # Drone surveys of plots in a subset of site-years: the canopy area delineated
  # in each plot polygon, the polygon area and the tide height at the survey.
  plots <- .mock_json("fake-cover")
  plots <- plots[!is.na(plots$plot_percent_cover), ]
  cover <- .mock_with_seed(13, {
    polygon <- round(stats::runif(nrow(plots), 180, 280), 1)
    data.frame(
      site = plots$site, year = plots$year, canopy_m2 = round(polygon * plots$plot_percent_cover / 100, 2),
      polygon_m2 = polygon, tide_height_m = round(stats::runif(nrow(plots), 0, 1.5), 2)
    )
  })

  list(density = density, size = size, weight = weight, wetdry = wetdry, carbon = carbon, cover = cover)
}

# Planned; the name and examples are a proposal. An example workbook for a
# species: a named list of data frames, one per sheet, in kelpbio's column names.
# The examples are the workbooks of different kinds of user:
# - "full": density, size, weight, wetdry, carbon and cover, so those models
#   can be fitted to the data, through to total biomass;
# - "density_size": the density and size data a typical monitoring program
#   collects, so the other models are pre-fit;
# - "bad_weight": density and size, and a weight sheet with two rows missing
#   their year, so its data check fails;
# - "size_only": plant diameters only, to predict the weight of each plant.
# data-raw/test-workbooks.R writes each example as an Excel workbook.
kb_example_data <- function(species = c("nereo", "macro"), example = c("density_size", "full", "bad_weight", "size_only")) {
  species <- rlang::arg_match(species)
  example <- rlang::arg_match(example)
  sheets <- .mock_example_sheets(species)
  switch(example,
    full = sheets[c("density", "size", "weight", "wetdry", "carbon", "cover")],
    density_size = sheets[c("density", "size")],
    bad_weight = {
      weight <- sheets$weight
      weight$year[c(42, 218)] <- NA
      c(sheets[c("density", "size")], list(weight = weight))
    },
    size_only = sheets["size"]
  )
}
