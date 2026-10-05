#' @keywords internal
"_PACKAGE"

# bayesplot, jsonlite, scales and tibble are in Imports for the mocks only
# (R/mock-kelpbio.R, R/mock-example.R), as is ggplot2 apart from the app's
# geom_point() layer on the cover predictions and its wrap_dims() call for the
# biomass figure height. Drop them from DESCRIPTION when kelpbio is imported and
# the mocks are deleted, checking first that no app code still uses them.

## usethis namespace: start
## usethis namespace: end
NULL
