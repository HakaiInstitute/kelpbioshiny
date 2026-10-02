# the user guide text

    Code
      writeLines(guide_text())
    Output
      User guide
      How to use the app, step by step.
      Steps
      A run moves through four steps in the navbar. A step's marker shows a check mark once it is complete.
      Data
      Load the survey data, from a workbook or a CSV file per model.
      The workbook has one sheet per model, with these columns. The template workbook on the Data step has them all.
      Sheet
      Model
      Columns
      Sheet needed
      density
      Density
      site, year, quadrat, density
      Required
      size
      Size
      site, year, diameter
      Optional
      weight
      Weight
      site, year, diameter, weight, density
      Optional
      blade
      Blade fraction
      site, year, blade_weight, total_weight
      Optional
      wetdry
      Wet:dry
      site, year, wet_weight, dry_weight
      Optional
      carbon
      Carbon
      site, year, dry_weight, carbon
      Optional
      cover
      Biomass:cover
      site, year, canopy_area, plot, plot_percent_cover
      Optional
      Models
      Fit each model to the data or choose a pre-fit model, then check the diagnostics.
      Each model uses one source: your data, a pre-fit model, or none.
      Model
      Sources
      Density
      Your data
      Size
      Your data, Pre-fit coastwide
      Weight
      Your data, Pre-fit coastwide
      Blade fraction
      Your data, Not used
      Wet:dry
      Your data, Pre-fit Hakai Institute, Not used
      Carbon
      Your data, Pre-fit Hakai Institute, Not used
      Biomass:cover
      Your data, Not used
      Biomass
      Review biomass estimates by site-year from the combined models.
      Export
      Save the results, or rerun the whole analysis in R.
      Glossary
      The terms behind the help icons in the app.
      R-hat
      R-hat compares the chains with each other. Values close to 1 mean the chains agree on the same answer. A value above 1.05 means the model needs more sampling before its estimates can be relied on.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/diagnostics.html)
      Effective sample size (ESS)
      Draws that follow each other in a chain are similar, so together they hold less information than independent draws. ESS is the number of independent draws they are worth. Below 400, estimates and their limits are less reliable.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/diagnostics.html)
      Posterior predictive check
      A posterior predictive check simulates new datasets from the fitted model and compares them with your data. If the model describes the data well, the dark line for your data sits within the band of light lines for the simulated data. A systematic difference, such as a higher or shifted peak or heavier tails, means the model misses a feature of the data. The deviance residuals overlay makes the same comparison on the residual scale, so the shapes should match there too. Small wiggles are expected.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/diagnostics.html)
      Prior sensitivity
      Power-scaling makes the priors, and then the likelihood, slightly stronger and weaker, and measures how far each parameter's estimate moves. A weak prior means the priors are not driving the results. Strong data means the data are informative about the parameter. A parameter without both depends on the priors as much as on the data. If a prior is not weak and this is unintended, make it less informative in prior settings (a larger SD for a normal prior, a smaller rate for an exponential prior) and refit.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/diagnostics.html)
      Population-level and group predictions
      Population-level predictions are for a typical site in a typical year. They leave out the differences between sites and between years, so they suit sites and years that were not sampled. Predictions by site or by year add the estimated difference for each sampled site or year, so they describe those sites and years. Predictions by site and year include both.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/results.html)
      Thinning (nthin)
      Keeps every nth draw from each chain. The sampler runs n times as many iterations to keep the same number of draws, so the kept draws are less alike and ESS goes up. Fitting takes about n times as long.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/diagnostics.html)
      Estimates and limits
      Each estimate is the median of the posterior distribution. The lower and upper limits bound the 95% compatibility interval: the range of values most compatible with the data and the model.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/results.html)
      Pre-fit models
      A pre-fit model was fitted in advance to a larger reference dataset, so it needs no data or fitting here. Pre-fit coastwide models use data compiled from surveys along the coast; Pre-fit Hakai Institute models use Hakai Institute survey data. Your sites are not in that data, so population-level estimates are used.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/pre-fit-models.html)
      Population-level estimate
      When a site-year has no data for a model, the typical value across all sites and years is used instead of a value for that site-year. Biomass is still estimated, but it does not reflect conditions specific to that site-year.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/results.html)
      Density in the weight model
      Plants of the same diameter weigh more or less depending on how crowded they are, so the weight model uses the density estimates as a predictor. A weight model fitted to your data is fitted after the density model.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/models.html)
      Canopy area
      The total area of kelp canopy at a site in a year, in square metres, measured from drone imagery. It is the same on every row of that site-year in the cover sheet.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/total-biomass.html)
      Percent cover
      The percentage of a plot covered by kelp canopy in the drone imagery. It links the biomass measured in plots to what the drone sees.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/total-biomass.html)
      Total biomass
      Biomass per unit area is predicted from percent cover with the biomass:cover model, then scaled up by the canopy area. The uncertainty in every model is carried through to the limits. Site-years with canopy area but no plot cover use the population-level cover relationship.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/total-biomass.html)
      Priors
      A prior states which parameter values are plausible before the data are seen. The defaults rule out implausible values but leave the data to decide the estimates, so most analyses keep them.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/priors.html)
      Statistical details, including the models, priors and diagnostics, are in the
      kelpbio documentation (https://hakaiinstitute.github.io/kelpbio/)
      .

