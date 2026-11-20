# Help for readers new to Bayesian models: a muted help icon that opens a
# popover with a short explanation. Popover text lives in help_topics so the
# wording stays consistent.

# kelpbio owns the statistics; help text links to its articles rather than
# restating them. Every link is kelpbio_url plus one of kelpbio_links, a page
# of kelpbio's site and the heading it opens at.
kelpbio_url <- "https://hakaiinstitute.github.io/kelpbio/"
kelpbio_links <- list(
  methods = "articles/methods.html",
  data_format = "articles/data-format.html",
  glossary = "articles/kelpbio.html#glossary",
  prefit = "articles/kelpbio.html#pre-fit-models",
  census = "articles/methods.html#site-wide-census",
  hierarchical = "articles/methods.html#hierarchical-effects",
  survey_biomass = "articles/methods.html#survey-biomass",
  cover_calibration = "articles/methods.html#cover-calibration",
  total_biomass = "articles/methods.html#total-biomass",
  estimates = "articles/methods.html#estimates-and-convergence",
  priors = "articles/advanced.html#priors",
  new_sites = "articles/advanced.html#new-sites-and-years",
  diagnostics = "articles/advanced.html#diagnostics",
  ppc = "articles/advanced.html#posterior-predictive-check",
  sensitivity = "articles/advanced.html#prior-sensitivity"
)
kelpbio_link <- function(key) paste0(kelpbio_url, kelpbio_links[[key]])

# A kelpbio vignette as installed with the package (its doc/ folder), as the
# list of its sections (id, title, and body without the title), for the Help
# page to show in place. NULL when this copy of kelpbio was installed without
# its vignettes, as from a local checkout; the page then links to the article
# online. Ids gain `prefix`, so they cannot clash with the app's, and links
# within the article follow; other relative links point to kelpbio's site.
kelpbio_vignette <- function(name, prefix) {
  path <- system.file("doc", paste0(name, ".html"), package = "kelpbio")
  if (!nzchar(path)) {
    return(NULL)
  }
  doc <- xml2::read_html(path)
  body <- xml2::xml_find_first(doc, "//body")
  for (node in xml2::xml_find_all(body, ".//*[@id]")) {
    xml2::xml_attr(node, "id") <- paste0(prefix, xml2::xml_attr(node, "id"))
  }
  for (node in xml2::xml_find_all(body, ".//a[@href]")) {
    href <- xml2::xml_attr(node, "href")
    if (startsWith(href, "#")) {
      xml2::xml_attr(node, "href") <- paste0("#", prefix, substring(href, 2))
    } else {
      if (!grepl("^[a-z]+:", href)) xml2::xml_attr(node, "href") <- paste0(kelpbio_url, "articles/", href)
      xml2::xml_attr(node, "target") <- "_blank"
      xml2::xml_attr(node, "rel") <- "noopener"
    }
  }
  sections <- xml2::xml_find_all(body, "./div[contains(concat(' ', @class, ' '), ' level2 ')]")
  lapply(sections, function(section) {
    heading <- xml2::xml_find_first(section, "./h2")
    title <- xml2::xml_text(heading)
    xml2::xml_remove(heading)
    # The page gives the section's card its id.
    id <- xml2::xml_attr(section, "id")
    xml2::xml_set_attr(section, "id", NULL)
    list(id = id, title = title, html = HTML(as.character(section)))
  })
}

help_topics <- list(
  rhat = list(
    title = "R-hat",
    body = function() {
      paste(
        "R-hat compares the chains with each other. Values close to 1 mean the chains agree on the same answer.",
        sprintf("A value above %s means the model needs more sampling before its estimates can be relied on.", default_arg(kb_converged, "rhat"))
      )
    },
    link = "estimates"
  ),
  ess = list(
    title = "Effective sample size (ESS)",
    body = function() {
      paste(
        "Draws that follow each other in a chain are similar, so together they hold less information than independent draws.",
        sprintf(
          "ESS is the number of independent draws they are worth, for the middle (bulk) and the tails of the distribution. Below %s per chain (%s over the %s chains), estimates and their limits are less reliable.",
          default_arg(kb_converged, "ess"), default_arg(kb_converged, "ess") * fit_draws$chains, fit_draws$chains
        )
      )
    },
    link = "estimates"
  ),
  ppc = list(
    title = "Posterior predictive check",
    body = paste(
      "A posterior predictive check simulates new datasets from the fitted model and compares them with your data.",
      "A systematic difference, such as a shifted peak or heavier tails, means the model misses a feature of the data."
    ),
    link = "ppc"
  ),
  sensitivity = list(
    title = "Prior sensitivity",
    body = paste(
      "Prior sensitivity measures how far each estimate moves when the priors, and then the data, are given slightly more or less weight.",
      "A weak prior means the priors are not driving the estimate, and strong data means the data are informative about it.",
      "A parameter without both depends on the priors as much as on the data."
    ),
    link = "sensitivity"
  ),
  prediction_groups = list(
    title = "Overall and group predictions",
    body = paste(
      "Overall predictions are for a typical site in a typical year. They leave out the differences between sites and between years, so they suit sites and years that were not sampled.",
      "Predictions by site or by year add the estimated difference for each sampled site or year, so they describe those sites and years. Predictions by site and year include both."
    ),
    link = "hierarchical"
  ),

  sampler = list(
    title = "Sampler settings",
    body = function() {
      sprintf(
        "Each fit keeps %s draws from each of %s chains, as the pre-fit models do, so that their estimates can be combined.",
        format(fit_draws$niters, big.mark = ","), fit_draws$chains
      )
    },
    link = "diagnostics"
  ),
  nthin = list(
    title = "Thinning (nthin)",
    body = paste(
      "Keeps every nth draw from each chain. The sampler runs n times as many iterations to keep the same number of draws,",
      "so the kept draws are less alike and ESS goes up. Fitting takes about n times as long."
    ),
    link = "diagnostics"
  ),
  adapt_delta = list(
    title = "Target acceptance (adapt_delta)",
    body = paste(
      "The acceptance rate the sampler tunes its step size to. A higher value takes smaller steps,",
      "which removes divergent transitions but makes fitting slower."
    ),
    link = "diagnostics"
  ),
  seed = list(
    title = "Seed",
    body = paste(
      "The starting point of the sampler's random numbers, drawn at random when the app starts.",
      "Fitting the same data with the same settings and seed gives the same estimates; the R script and settings.csv record it."
    )
  ),
  interval = list(
    title = "Estimates and limits",
    body = paste(
      "Each estimate is the median of the posterior distribution. The lower and upper limits bound the 95% compatibility interval:",
      "the range of values most compatible with the data and the model."
    ),
    link = "estimates"
  ),
  prefit = list(
    title = "Pre-fit models",
    body = paste(
      "A pre-fit model was fitted in advance to reference data, so it needs no data or fitting here.",
      "A Your data + reference source fits the model here to your data together with the reference data."
    ),
    link = "prefit"
  ),
  coverage = list(
    title = "Site-years without data",
    body = paste(
      "The models are hierarchical, so a site-year without data still gets an estimate.",
      "It uses the site effect, estimated from the site's other years, and the year effect, estimated from the year's other sites, with a wider compatibility interval.",
      "With neither the site nor the year in the data, the estimate is for a typical site-year."
    ),
    link = "hierarchical"
  ),
  census = list(
    title = "Census",
    body = paste(
      "In a census, every plant counted in an in situ survey is measured, so in situ survey biomass sums the weights of the plants and no size model is fitted.",
      "Otherwise, a sample of plants is measured and a size model is fitted to them.",
      "Macrocystis in situ surveys are always a census."
    ),
    link = "census"
  ),
  survey_biomass = list(
    title = "In situ survey biomass",
    body = paste(
      "The biomass of the plants counted in an in situ survey, per square metre of the area surveyed or in total over it.",
      "By site and year, the in situ surveys of a site-year are pooled: their summed biomass, divided by their summed area for biomass per square metre."
    ),
    link = "survey_biomass"
  ),
  canopy_area = list(
    title = "Canopy area",
    body = paste(
      "The area of kelp canopy the canopy imagery delineated within a plot polygon, or mapped over a whole site, in square metres.",
      "Imagery with no canopy gives a canopy area of 0."
    ),
    link = "cover_calibration"
  ),
  tide_cover = list(
    title = "Tide-corrected cover",
    body = paste(
      "Less of the canopy shows at the surface at higher tides, so the cover calibration corrects the canopy area for the tide height when the imagery was taken.",
      "Cover is the corrected canopy area as a proportion of the plot area, from 0 to 1."
    ),
    link = "cover_calibration"
  ),
  canopy_biomass = list(
    title = "Canopy biomass",
    body = paste(
      "The cover calibration relates the wet biomass of each in situ survey to the tide-corrected canopy cover of its plot.",
      "Canopy biomass applies it to the canopy mapped over each site in a canopy map.",
      "The uncertainty in every model is carried through to the limits."
    ),
    link = "total_biomass"
  ),
  stand_ins = list(
    title = "Representative site",
    body = paste(
      "A site that a model has no data for is estimated as a new site, drawn from the spread between sites, so its compatibility interval is wide.",
      "To narrow it, optionally choose one or more sites in the model's data with similar conditions, such as nearby sites; if more than one site is selected, their effects are averaged.",
      "The Data support column of the estimates names the representative sites used."
    ),
    link = "new_sites"
  ),
  priors = list(
    title = "Priors",
    body = paste(
      "A prior states which parameter values are plausible before the data are seen.",
      "The defaults rule out implausible values but leave the data to decide the estimates, so most analyses keep them."
    ),
    link = "priors"
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

# A topic's kelpbio article, or NULL for a topic about the app alone.
help_url <- function(key) if (!is.null(help_topics[[key]]$link)) kelpbio_link(help_topics[[key]]$link)

help_topic <- function(key) {
  topic <- help_topics[[key]]
  help_icon(topic$title, topic_body(topic), help_url(key))
}

# A label followed by its help icon, which follows the label's last word also
# when the label wraps.
with_help <- function(label, key) {
  span(class = "kb-with-help", label, help_topic(key))
}
