# Dev build / QC script for kelpbioshiny.
#
#   Rscript scripts/build.R          install, document, test
#   Rscript scripts/build.R --check  + R CMD check
#
# In Positron (or VS Code), the same runs are tasks: Command Palette >
# "Tasks: Run Task" > "kelpbioshiny: ..." (.vscode/tasks.json).
#
#   * The install comes first so the shinytest2 tests, which run the installed
#     app in a separate R process, test the current source.
#   * Linting is handled by jarl in CI (.github/workflows/lint-with-jarl.yaml,
#     configured by jarl.toml); this script does not lint.

usage <- paste(
  "Usage: Rscript scripts/build.R [--check]",
  "  --check  also run R CMD check",
  sep = "\n"
)
args <- commandArgs(trailingOnly = TRUE)
if (any(c("--help", "-h") %in% args)) {
  cat(usage, "\n", sep = "")
  quit(status = 0)
}
unknown <- setdiff(args, "--check")
if (length(unknown)) {
  cat("Unknown option: ", paste(unknown, collapse = " "), "\n\n", usage, "\n", sep = "")
  quit(status = 1)
}

full_check <- "--check" %in% args
message("Build: install, document, test", if (full_check) " + R CMD check")

devtools::install(build = FALSE, quick = TRUE, upgrade = FALSE)

if (requireNamespace("roxygen2md", quietly = TRUE)) {
  roxygen2md::roxygen2md()
}
devtools::document()

devtools::test()

if (full_check) {
  devtools::check()
}
