# Writes the example workbooks of kb_example_data() (R/mock-example.R) to
# data-raw/workbooks/, one per kind of user and species, for uploading in a
# demo or a test. The app's "Use example workbook" button loads the first.
# Years and plot numbers are written as numbers, as a spreadsheet stores them.
# Run from the package root:
#   Rscript data-raw/test-workbooks.R
#
# 1-full: every sheet, one plot per site-year as the Hakai Institute surveys
#   have, so each model is fitted to the data through to canopy biomass.
# 2-several-plots: two or three plots per site-year with size, survey and
#   canopy sheets but no harvests, so weight (pre-fit Coastwide), wet:dry and
#   carbon are pre-fit.
# 3-census: every plant measured on whole sites, each one plot, with no drone
#   flights, so only survey biomass. For Nereocystis, choose "All plants
#   measured" under Size data before uploading; size and weight use bulb
#   diameter.
# 4-bad-weight: every sheet, with a weight sheet missing two dates (a data
#   error) and a carbon sheet with three implausible samples (a warning).

pkgload::load_all(quiet = TRUE)

files <- c(full = "1-full", survey = "2-several-plots", census = "3-census", bad_weight = "4-bad-weight")

dir.create("data-raw/workbooks", showWarnings = FALSE)
for (species in names(species_info)) {
  for (example in names(files)) {
    sheets <- kb_example_data(species, example)
    for (id in names(sheets)) {
      for (column in intersect(c("year", "plot_id"), names(sheets[[id]]))) sheets[[id]][[column]] <- as.integer(sheets[[id]][[column]])
    }
    writexl::write_xlsx(sheets, file.path("data-raw/workbooks", sprintf("kelpbio-%s-%s.xlsx", files[[example]], species)))
  }
}
