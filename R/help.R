# Help for readers new to Bayesian models: a muted help icon that opens a
# popover with a short explanation. Popover text lives in help_topics so the
# wording stays consistent.

# kelpbio owns the statistics; help text links to its articles rather than
# restating them. Every link is kelpbio_url plus the path of one of
# kelpbio_articles, which the Methods page shows as tiles, in this order.
# TODO: match the titles, summaries and paths to kelpbio's articles once they
# are written.
kelpbio_url <- "https://hakaiinstitute.github.io/kelpbio/"
kelpbio_articles <- list(
  models = list(
    title = "The models",
    summary = "What each model estimates, and how they combine into survey and canopy biomass.",
    path = "articles/models.html"
  ),
  priors = list(
    title = "Priors",
    summary = "The default priors of each model, and how to choose others.",
    path = "articles/priors.html"
  ),
  prefit = list(
    title = "Pre-fit models",
    summary = "The reference data behind each pre-fit model, and when to use one.",
    path = "articles/pre-fit-models.html"
  ),
  diagnostics = list(
    title = "Checking a fit",
    summary = "Convergence, prior sensitivity, influential observations and posterior predictive checks.",
    path = "articles/diagnostics.html"
  ),
  results = list(
    title = "Interpreting the estimates",
    summary = "Overall and group predictions, and their compatibility intervals.",
    path = "articles/results.html"
  ),
  canopy_biomass = list(
    title = "Canopy biomass",
    summary = "Calibrating drone canopy cover on survey biomass, and scaling it to the canopy mapped over whole sites.",
    path = "articles/canopy-biomass.html"
  )
)

help_topics <- list(
  rhat = list(
    title = "R-hat",
    body = function() {
      paste(
        "R-hat compares the chains with each other. Values close to 1 mean the chains agree on the same answer.",
        sprintf("A value above %s means the model needs more sampling before its estimates can be relied on.", default_arg(kb_converged, "rhat"))
      )
    },
    link = kelpbio_articles$diagnostics$path
  ),
  ess = list(
    title = "Effective sample size (ESS)",
    body = function() {
      paste(
        "Draws that follow each other in a chain are similar, so together they hold less information than independent draws.",
        sprintf(
          "ESS is the number of independent draws they are worth, for the middle (bulk) and the tails of the distribution. Below %s per chain, estimates and their limits are less reliable.",
          default_arg(kb_converged, "ess")
        )
      )
    },
    link = kelpbio_articles$diagnostics$path
  ),
  ppc = list(
    title = "Posterior predictive check",
    body = paste(
      "A posterior predictive check simulates new datasets from the fitted model and compares them with your data.",
      "If the model fits well, the dark line for your data sits within the band of light lines for the simulated data.",
      "A systematic difference, such as a shifted peak or heavier tails, means the model misses a feature of the data."
    ),
    link = kelpbio_articles$diagnostics$path
  ),
  sensitivity = list(
    title = "Prior sensitivity",
    body = paste(
      "Prior sensitivity measures how far each estimate moves when the priors, and then the data, are given slightly more or less weight.",
      "A weak prior means the priors are not driving the estimate, and strong data means the data are informative about it.",
      "A parameter without both depends on the priors as much as on the data."
    ),
    link = kelpbio_articles$diagnostics$path
  ),
  influence = list(
    title = "Influential observations",
    body = function() {
      paste(
        "Pareto k measures how much the fit changes when an observation is left out.",
        sprintf("Above %s, the observation strongly influences the estimates.", default_arg(kb_influence, "threshold")),
        "That can be a recording error, or a valid observation in a site-year with few others."
      )
    },
    link = kelpbio_articles$diagnostics$path
  ),
  prediction_groups = list(
    title = "Overall and group predictions",
    body = paste(
      "Overall predictions are for a typical site in a typical year. They leave out the differences between sites and between years, so they suit sites and years that were not sampled.",
      "Predictions by site or by year add the estimated difference for each sampled site or year, so they describe those sites and years. Predictions by site and year include both."
    ),
    link = kelpbio_articles$results$path
  ),
  chains = list(
    title = "Chains",
    body = "Independent runs of the sampler. Comparing the chains with each other shows whether the sampler has converged.",
    link = kelpbio_articles$diagnostics$path
  ),
  niters = list(
    title = "Iterations (niters)",
    body = "The number of draws kept from each chain. More draws give more reliable estimates, but fitting takes longer.",
    link = kelpbio_articles$diagnostics$path
  ),
  nthin = list(
    title = "Thinning (nthin)",
    body = paste(
      "Keeps every nth draw from each chain. The sampler runs n times as many iterations to keep the same number of draws,",
      "so the kept draws are less alike and ESS goes up. Fitting takes about n times as long."
    ),
    link = kelpbio_articles$diagnostics$path
  ),
  interval = list(
    title = "Estimates and limits",
    body = paste(
      "Each estimate is the median of the posterior distribution. The lower and upper limits bound the 95% compatibility interval:",
      "the range of values most compatible with the data and the model."
    ),
    link = kelpbio_articles$results$path
  ),
  prefit = list(
    title = "Pre-fit models",
    body = paste(
      sprintf(
        "%s models were fitted in advance to %s, and %s models to %s.",
        prefit_info$coastwide$label, prefit_info$coastwide$data, prefit_info$hakai$label, prefit_info$hakai$data
      ),
      "They need no data or fitting here.",
      "Sites in the reference data get their own estimates."
    ),
    link = kelpbio_articles$prefit$path
  ),
  coverage = list(
    title = "Site-years without data",
    body = paste(
      "When a model has no data for a site-year, its estimate there uses the site's estimate from other years and the year's estimate from other sites.",
      "What is specific to that site-year is drawn from the variation the model estimated between site-years, so its compatibility interval is wider.",
      "With neither the site nor the year in the data, the estimate is for a typical site and year."
    ),
    link = kelpbio_articles$results$path
  ),
  census = list(
    title = "Census",
    body = paste(
      "With a sample of plants measured, the surveys tally their plants and a size model is fitted to the plants measured.",
      "With all plants measured (a census), the size sheet holds the plants of each survey, joined to it by site, plot_id and date, and the survey sheet has no plants column.",
      "Survey biomass then sums the weights of the plants, and no size model is fitted. Macrocystis surveys are always a census."
    ),
    link = kelpbio_articles$models$path
  ),
  mean_weight = list(
    title = "Mean plant weight",
    body = paste(
      "The expected weight of a plant at a site-year: the weight model's curve averaged over the site-year's size distribution from the size model.",
      "Survey biomass is the plants counted in a survey times this weight."
    ),
    link = kelpbio_articles$models$path
  ),
  survey_biomass = list(
    title = "Survey biomass",
    body = paste(
      "The biomass of the plants counted in an in situ survey, per square metre of the area searched.",
      "By site and year, the surveys of a site-year are pooled: their summed biomass over their summed area."
    ),
    link = kelpbio_articles$models$path
  ),
  canopy_area = list(
    title = "Canopy area",
    body = paste(
      "The area of kelp canopy the drone imagery delineated within a plot polygon, or mapped over a whole site, in square metres.",
      "A flight with no canopy has a canopy area of 0."
    ),
    link = kelpbio_articles$canopy_biomass$path
  ),
  tide_cover = list(
    title = "Tide-corrected cover",
    body = paste(
      "Less of the canopy shows at the surface at higher tides, so the cover calibration corrects the canopy area for the tide height at the flight.",
      "Cover is the corrected canopy area as a proportion of the plot area, from 0 to 1."
    ),
    link = kelpbio_articles$canopy_biomass$path
  ),
  canopy_biomass = list(
    title = "Canopy biomass",
    body = paste(
      "The cover calibration relates the wet biomass of each survey to the tide-corrected drone canopy cover of its plot.",
      "Canopy biomass applies it to the canopy mapped over each site in a drone map.",
      "The uncertainty in every model is carried through to the limits."
    ),
    link = kelpbio_articles$canopy_biomass$path
  ),
  priors = list(
    title = "Priors",
    body = paste(
      "A prior states which parameter values are plausible before the data are seen.",
      "The defaults rule out implausible values but leave the data to decide the estimates, so most analyses keep them."
    ),
    link = kelpbio_articles$priors$path
  )
)

# A topic's text. Text that names a kelpbio default is a function, so it reads
# the default when shown.
topic_body <- function(topic) if (is.function(topic$body)) topic$body() else topic$body

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
  help_icon(topic$title, topic_body(topic), help_url(key))
}

# A label followed by its help icon, which follows the label's last word also
# when the label wraps.
with_help <- function(label, key) {
  span(class = "kb-with-help", label, help_topic(key))
}
