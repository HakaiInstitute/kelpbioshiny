# The four steps of a run: the navbar labels, the page subtitles, the welcome
# card and the user guide all read their names and descriptions from here.

# What the app is for, in one sentence: the welcome card and the about page.
app_purpose <- "Get annual estimates of kelp biomass by site, or the weight of each plant, from survey data, using Bayesian methods in the kelpbio R package."

app_steps <- list(
  data = list(label = "Data", description = "Load the survey data, from a workbook or a CSV file per model."),
  models = list(label = "Models", description = "Fit each model to the data or choose a pre-fit model, then check the diagnostics."),
  estimates = list(label = "Estimates", description = "Review the estimates from each model, and biomass by site and year from the combined models."),
  export = list(label = "Export", description = "Save the estimates, or rerun the whole analysis in R.")
)

steps <- vapply(app_steps, `[[`, "", "label")

step_description <- function(value) app_steps[[value]]$description
