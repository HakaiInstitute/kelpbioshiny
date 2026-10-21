# The four steps of a run: the navbar labels, the page subtitles, the welcome
# card and the user guide all read their names and descriptions from here.

# What the app is for, in one sentence: the welcome card and the about page.
app_purpose <- "Get annual estimates of kelp biomass by site from survey data, using Bayesian methods in the kelpbio R package."

app_steps <- list(
  data = list(label = "Data", description = "Load your survey data."),
  models = list(label = "Models", description = "Fit the models, or use pre-fit ones."),
  estimates = list(label = "Estimates", description = "Review biomass and model estimates."),
  export = list(label = "Export", description = "Save the estimates, or rerun the whole analysis in R.")
)

steps <- vapply(app_steps, `[[`, "", "label")

step_description <- function(value) app_steps[[value]]$description
