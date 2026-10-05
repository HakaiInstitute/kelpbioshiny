# Writes the example workbooks of kb_example_data() (R/mock-example.R) to
# data-raw/workbooks/, for testing uploads: one per kind of user. The app's
# "Use example workbook" menu loads the same sheets. Years are written as
# numbers, as a spreadsheet stores them. Run from the package root:
#   Rscript data-raw/test-workbooks.R

pkgload::load_all(quiet = TRUE)

files <- c(full = "1-full", density_size = "2-density-size", bad_weight = "3-bad-weight", size_only = "4-size-only")

dir.create("data-raw/workbooks", showWarnings = FALSE)
for (example in names(files)) {
  sheets <- kb_example_data("nereo", example)
  for (id in names(sheets)) {
    if ("year" %in% names(sheets[[id]])) sheets[[id]]$year <- as.integer(sheets[[id]]$year)
  }
  writexl::write_xlsx(sheets, file.path("data-raw/workbooks", sprintf("kelpbio-%s-nereo.xlsx", files[[example]])))
}
