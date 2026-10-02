# Mock example data ----------------------------------------------------------------
#
# The example workbook the app loads in place of an upload. Delete this file with
# R/mock-kelpbio.R. The real example data should come from kelpbio's bundled
# simulated datasets (data_density_sim_nereo, data_size_sim_nereo,
# data_weight_sim_nereo and their macro twins), through an accessor like this one.

# Planned; the name is a proposal. The example workbook for a species: a named list
# of data frames, one per sheet (density, size, weight and cover), in kelpbio's
# column names. The weight sheet has two rows with a missing year, so its data
# check fails, as a sheet with a data error would.
kb_example_data <- function(species = c("nereo", "macro")) {
  species <- rlang::arg_match(species)
  macro <- species == "macro"

  density <- .mock_json("fake-density")
  # Quadrats of 4 m2, so the fake densities become whole stipe counts.
  density <- data.frame(
    site = density$site, year = as.character(density$year), transect = density$quadrat,
    count = round(density$density * 4), area_m2 = 4
  )
  names(density)[names(density) == "count"] <- if (macro) "plants" else "stipes"

  # Size and weight data run 2019-2025, with no size data for one site-year.
  size <- .mock_json("fake-size")
  size <- size[!(size$site == "site8" & size$year == "2021"), ]
  weight <- .mock_json("fake-weight")
  weight$year[c(42, 318)] <- NA
  if (macro) {
    size <- data.frame(site = size$site, year = size$year, fronds = pmax(1, round(size$diameter / 4)))
    weight <- data.frame(site = weight$site, year = weight$year, fronds = pmax(1, round(weight$diameter / 4)), weight_kg = weight$weight)
  } else {
    size <- data.frame(site = size$site, year = size$year, diameter_mm = size$diameter)
    weight <- data.frame(site = weight$site, year = weight$year, diameter_mm = weight$diameter, weight_kg = weight$weight)
  }

  # Drone canopy area and plot percent cover for a subset of site-years.
  cover <- .mock_json("fake-cover")
  names(cover)[names(cover) == "canopy_area"] <- "canopy_area_m2"

  list(density = density, size = size, weight = weight, cover = cover)
}
