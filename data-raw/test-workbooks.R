# Writes the example workbooks of kb_example_data() (R/mock-example.R) to
# data-raw/workbooks/, for testing uploads: one per kind of user. The app's
# "Use example workbook" menu loads the same sheets. Years and plot numbers are
# written as numbers, as a spreadsheet stores them. Run from the package root:
#   Rscript data-raw/test-workbooks.R

pkgload::load_all(quiet = TRUE)

files <- c(full = "1-full", survey = "2-several-plots", census = "3-census", bad_weight = "4-bad-weight")

dir.create("data-raw/workbooks", showWarnings = FALSE)
for (example in names(files)) {
  sheets <- kb_example_data("nereo", example)
  for (id in names(sheets)) {
    for (column in intersect(c("year", "plot_id"), names(sheets[[id]]))) sheets[[id]][[column]] <- as.integer(sheets[[id]][[column]])
  }
  writexl::write_xlsx(sheets, file.path("data-raw/workbooks", sprintf("kelpbio-%s-nereo.xlsx", files[[example]])))
}
