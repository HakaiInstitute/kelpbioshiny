#' Run the kelpbio Shiny app
#'
#' Run the kelpbio Shiny app.
#'
#' @details
#' The app steps through a biomass run: load survey data, fit or choose a
#' pre-fit model for each sub-model, view biomass by site-year, and export the
#' results.
#'
#' @return A Shiny app object. Printing it, as happens when `run_app()` is
#'   called at the console, launches the app.
#' @export
#' @examples
#' if (interactive()) {
#'   run_app()
#' }
run_app <- function() {
  shiny::shinyAppDir(system.file("app", package = "kelpbioshiny"))
}
