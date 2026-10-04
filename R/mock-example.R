# Mock example data ----------------------------------------------------------------
#
# The example workbook the app loads, and the fake survey data behind the test
# workbooks in data-raw/. Delete this file with R/mock-kelpbio.R. The real example
# data should come from kelpbio's bundled simulated datasets (data_density_sim_nereo,
# data_size_sim_nereo, data_weight_sim_nereo and their macro twins), through an
# accessor like kb_example_data().

# Every sheet the fake survey data provide, in kelpbio's column names: density,
# size, weight, wetdry, carbon and cover. Size and weight data run 2019-2025, with
# no size data for one site-year.
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

  size <- .mock_json("fake-size")
  size <- size[!(size$site == "site8" & size$year == "2021"), ]
  weight <- .mock_json("fake-weight")
  if (macro) {
    size <- data.frame(site = size$site, year = size$year, fronds = pmax(1, round(size$diameter / 4)))
    weight <- data.frame(site = weight$site, year = weight$year, fronds = pmax(1, round(weight$diameter / 4)), weight_kg = weight$weight)
  } else {
    size <- data.frame(site = size$site, year = size$year, diameter_mm = size$diameter)
    weight <- data.frame(site = weight$site, year = weight$year, diameter_mm = weight$diameter, weight_kg = weight$weight)
  }

  # Tissue samples: a wet and a dry mass per sample, and the dry and carbon mass
  # of three samples per site-year.
  .mock_with_seed(11, {
    wet <- round(stats::runif(150, 50, 400), 1)
    wetdry <- data.frame(wet_mass_g = wet, dry_mass_g = round(wet * stats::plogis(stats::rnorm(150, stats::qlogis(0.1), 0.2)), 1))
    site_years <- unique(density[c("site", "year")])
    carbon <- site_years[rep(seq_len(nrow(site_years)), each = 3), ]
    carbon$dry_mass_g <- round(stats::runif(nrow(carbon), 5, 20), 2)
    carbon$carbon_mass_g <- round(carbon$dry_mass_g * stats::plogis(stats::rnorm(nrow(carbon), stats::qlogis(0.3), 0.15)), 2)
    rownames(carbon) <- NULL
  })

  # Drone canopy area and plot percent cover for a subset of site-years.
  cover <- .mock_json("fake-cover")
  names(cover)[names(cover) == "canopy_area"] <- "canopy_area_m2"

  list(density = density, size = size, weight = weight, wetdry = wetdry, carbon = carbon, cover = cover)
}

# Planned; the name and examples are a proposal. An example workbook for a
# species: a named list of data frames, one per sheet, in kelpbio's column names.
# The examples are the workbooks of different kinds of user:
# - "full": density, size, weight, wetdry and carbon, so every model can be
#   fitted to the data;
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
    full = sheets[c("density", "size", "weight", "wetdry", "carbon")],
    density_size = sheets[c("density", "size")],
    bad_weight = {
      weight <- sheets$weight
      weight$year[c(42, 318)] <- NA
      c(sheets[c("density", "size")], list(weight = weight))
    },
    size_only = sheets["size"]
  )
}
