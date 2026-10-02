# Help for readers new to Bayesian models: a muted help icon that opens a
# popover with a short explanation, and a dotted-underline term with a one-line
# tooltip. Popover text lives in help_topics so the wording stays consistent.

# kelpbio owns the statistics; help text links to its articles rather than
# restating them. Every link is kelpbio_url plus one of kelpbio_articles.
kelpbio_url <- "https://hakaiinstitute.github.io/kelpbio/"
kelpbio_articles <- c(
  diagnostics = "articles/diagnostics.html",
  results = "articles/results.html",
  prefit = "articles/pre-fit-models.html",
  models = "articles/models.html",
  total_biomass = "articles/total-biomass.html",
  priors = "articles/priors.html"
)

help_topics <- list(
  rhat = list(
    title = "R-hat",
    body = paste(
      "R-hat compares the chains with each other. Values close to 1 mean the chains agree on the same answer.",
      "A value above 1.05 means the model needs more sampling before its estimates can be relied on."
    ),
    link = kelpbio_articles[["diagnostics"]]
  ),
  ess = list(
    title = "Effective sample size (ESS)",
    body = paste(
      "Draws that follow each other in a chain are similar, so together they hold less information than independent draws.",
      "ESS is the number of independent draws they are worth. Below 10% of the draws, estimates and their limits are less reliable."
    ),
    link = kelpbio_articles[["diagnostics"]]
  ),
  ppc = list(
    title = "Posterior predictive check",
    body = paste(
      "A posterior predictive check simulates new datasets from the fitted model and compares them with your data.",
      "If the model fits well, the dark line for your data sits within the band of light lines for the simulated data.",
      "A systematic difference, such as a shifted peak or heavier tails, means the model misses a feature of the data."
    ),
    link = kelpbio_articles[["diagnostics"]]
  ),
  sensitivity = list(
    title = "Prior sensitivity",
    body = paste(
      "Prior sensitivity measures how far each estimate moves when the priors, and then the data, are given slightly more or less weight.",
      "A weak prior means the priors are not driving the estimate, and strong data means the data are informative about it.",
      "A parameter without both depends on the priors as much as on the data."
    ),
    link = kelpbio_articles[["diagnostics"]]
  ),
  prediction_groups = list(
    title = "Population-level and group predictions",
    body = paste(
      "Population-level predictions are for a typical site in a typical year. They leave out the differences between sites and between years, so they suit sites and years that were not sampled.",
      "Predictions by site or by year add the estimated difference for each sampled site or year, so they describe those sites and years. Predictions by site and year include both."
    ),
    link = kelpbio_articles[["results"]]
  ),
  nthin = list(
    title = "Thinning (nthin)",
    body = paste(
      "Keeps every nth draw from each chain. The sampler runs n times as many iterations to keep the same number of draws,",
      "so the kept draws are less alike and ESS goes up. Fitting takes about n times as long."
    ),
    link = kelpbio_articles[["diagnostics"]]
  ),
  interval = list(
    title = "Estimates and limits",
    body = paste(
      "Each estimate is the median of the posterior distribution. The lower and upper limits bound the 95% compatibility interval:",
      "the range of values most compatible with the data and the model."
    ),
    link = kelpbio_articles[["results"]]
  ),
  prefit = list(
    title = "Pre-fit models",
    body = paste(
      sprintf(
        "%s models were fitted in advance to %s, and %s models to %s.",
        prefit_info$coastwide$label, prefit_info$coastwide$data, prefit_info$hakai$label, prefit_info$hakai$data
      ),
      "They need no data or fitting here.",
      "Sites in the reference data get their own estimates; other sites use population-level estimates."
    ),
    link = kelpbio_articles[["prefit"]]
  ),
  population = list(
    title = "Population-level estimate",
    body = paste(
      "When a site-year has no data for a model, the typical value across all sites and years is used instead of a value for that site-year.",
      "Biomass is still estimated, but it does not reflect conditions specific to that site-year."
    ),
    link = kelpbio_articles[["results"]]
  ),
  weight_density = list(
    title = "Density in the weight model",
    body = paste(
      "Plants of the same diameter weigh more or less depending on how crowded they are,",
      "so the weight model uses the density estimates as a predictor.",
      "A weight model fitted to your data is fitted after the density model."
    ),
    link = kelpbio_articles[["models"]]
  ),
  # Cover and totals: the exact formulation is a placeholder until the
  # biomass:cover model is designed in kelpbio.
  canopy_area = list(
    title = "Canopy area",
    body = paste(
      "The total area of kelp canopy at a site in a year, in square metres, measured from drone imagery.",
      "It is the same on every row of that site-year in the cover sheet."
    ),
    link = kelpbio_articles[["total_biomass"]]
  ),
  percent_cover = list(
    title = "Percent cover",
    body = paste(
      "The percentage of a plot covered by kelp canopy in the drone imagery.",
      "It links the biomass measured in plots to what the drone sees."
    ),
    link = kelpbio_articles[["total_biomass"]]
  ),
  total_biomass = list(
    title = "Total biomass",
    body = paste(
      "Biomass per unit area is predicted from percent cover with the biomass:cover model, then scaled up by the canopy area.",
      "The uncertainty in every model is carried through to the limits.",
      "Site-years with canopy area but no plot cover use the population-level cover relationship."
    ),
    link = kelpbio_articles[["total_biomass"]]
  ),
  priors = list(
    title = "Priors",
    body = paste(
      "A prior states which parameter values are plausible before the data are seen.",
      "The defaults rule out implausible values but leave the data to decide the estimates, so most analyses keep them."
    ),
    link = kelpbio_articles[["priors"]]
  )
)

help_icon <- function(title, body, link = NULL) {
  bslib::popover(
    tags$button(
      type = "button", class = "kb-help btn btn-link p-0 border-0 align-baseline text-body-secondary",
      `aria-label` = paste("About", tolower(title)),
      lucide("circle-help")
    ),
    div(class = "d-flex flex-column gap-2", body, if (!is.null(link)) {
      tags$a(
        href = link, target = "_blank", rel = "noopener", class = "d-inline-flex align-items-center gap-1",
        "Learn more", lucide("external-link")
      )
    }),
    title = title
  )
}

help_url <- function(key) paste0(kelpbio_url, help_topics[[key]]$link)

help_topic <- function(key) {
  topic <- help_topics[[key]]
  help_icon(topic$title, topic$body, help_url(key))
}

help_term <- function(text, tip) {
  bslib::tooltip(span(class = "kb-term", tabindex = "0", text), tip)
}

# A label followed by its help icon, kept on one line.
with_help <- function(label, key) {
  span(class = "d-inline-flex align-items-center gap-1", label, help_topic(key))
}
