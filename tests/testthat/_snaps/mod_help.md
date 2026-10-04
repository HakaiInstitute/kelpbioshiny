# the user guide text

    Code
      writeLines(guide_text())
    Output
      User guide
      How to use the app, step by step.
      Steps
      A run moves through four steps in the navbar. A step's marker shows a check mark once it is complete.
      1
      Data
      Load the survey data, from a workbook or a CSV file per model.
      The columns of each sheet. Other columns are ignored.
      Sheet
      Model
      Columns
      Sheet needed
      density
      Density
      Nereocystis luetkeana
      :
      site, year, stipes, area_m2
      Macrocystis pyrifera
      :
      site, year, plants, area_m2
      Required
      size
      Size
      Nereocystis luetkeana
      :
      site, year, diameter_mm
      Macrocystis pyrifera
      :
      site, year, fronds
      Optional
      weight
      Weight
      Nereocystis luetkeana
      :
      site, year, diameter_mm, weight_kg
      Macrocystis pyrifera
      :
      site, year, fronds, weight_kg
      Optional
      blade
      Blade fraction
      site, year, blade_weight_kg, total_weight_kg
      Optional
      wetdry
      Wet:dry
      wet_mass_g, dry_mass_g
      Optional
      carbon
      Carbon
      site, year, dry_mass_g, carbon_mass_g
      Optional
      cover
      Cover
      site, year, canopy_area_m2, plot, plot_percent_cover
      Optional
      2
      Models
      Fit each model to the data or choose a pre-fit model, then check the diagnostics.
      Each model uses one source: your data, a pre-fit model, or none.
      Model
      Sources
      Density
      Your data
      Size
      Your data, Pre-fit Hakai Institute
      Weight
      Your data, Pre-fit coastwide, Pre-fit Hakai Institute
      Blade fraction
      Your data, Not used
      Wet:dry
      Your data, Pre-fit Hakai Institute, Not used
      Carbon
      Your data, Pre-fit Hakai Institute, Not used
      Cover
      Your data, Not used
      3
      Biomass
      Review the annual biomass estimates for each site from the combined models.
      4
      Export
      Save the results, or rerun the whole analysis in R.
      Warnings and how to fix them
      The warnings a model or sheet can show.
      Convergence warning
      Some parameters have not converged. Increase thinning (nthin) in Sampler settings, for example to 2, and refit.
      Prior sensitivity warning
      If the flagged prior was not chosen on purpose, make it less informative on the Settings tab and refit.
      The fit failed
      Refit the model to see its results.
      The data check failed
      Correct the sheet and upload it again.
      Site names differ across sheets
      Rename the sites in the workbook so the names match exactly, then upload again.
      Statistical details
      The models, priors and diagnostics are explained in the kelpbio documentation.
      Open the kelpbio documentation
      Glossary
      The terms behind the help icons in the app.
      R-hat
      R-hat compares the chains with each other. Values close to 1 mean the chains agree on the same answer. A value above 1.01 means the model needs more sampling before its estimates can be relied on.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/diagnostics.html)
      Effective sample size (ESS)
      Draws that follow each other in a chain are similar, so together they hold less information than independent draws. ESS is the number of independent draws they are worth. Below 10% of the draws, estimates and their limits are less reliable.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/diagnostics.html)
      Posterior predictive check
      A posterior predictive check simulates new datasets from the fitted model and compares them with your data. If the model fits well, the dark line for your data sits within the band of light lines for the simulated data. A systematic difference, such as a shifted peak or heavier tails, means the model misses a feature of the data.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/diagnostics.html)
      Prior sensitivity
      Prior sensitivity measures how far each estimate moves when the priors, and then the data, are given slightly more or less weight. A weak prior means the priors are not driving the estimate, and strong data means the data are informative about it. A parameter without both depends on the priors as much as on the data.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/diagnostics.html)
      Population-level and group predictions
      Population-level predictions are for a typical site in a typical year. They leave out the differences between sites and between years, so they suit sites and years that were not sampled. Predictions by site or by year add the estimated difference for each sampled site or year, so they describe those sites and years. Predictions by site and year include both.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/results.html)
      Chains
      Independent runs of the sampler. Comparing the chains with each other shows whether the sampler has converged.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/diagnostics.html)
      Iterations (niters)
      The number of draws kept from each chain. More draws give more reliable estimates, but fitting takes longer.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/diagnostics.html)
      Thinning (nthin)
      Keeps every nth draw from each chain. The sampler runs n times as many iterations to keep the same number of draws, so the kept draws are less alike and ESS goes up. Fitting takes about n times as long.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/diagnostics.html)
      Estimates and limits
      Each estimate is the median of the posterior distribution. The lower and upper limits bound the 95% compatibility interval: the range of values most compatible with the data and the model.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/results.html)
      Pre-fit models
      Pre-fit coastwide models were fitted in advance to data compiled from surveys along the coast, and Pre-fit Hakai Institute models to Hakai Institute survey data. They need no data or fitting here. Sites in the reference data get their own estimates; other sites use population-level estimates.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/pre-fit-models.html)
      Population-level estimate
      When a site-year has no data for a model, the typical value across all sites and years is used instead of a value for that site-year. Biomass is still estimated, but it does not reflect conditions specific to that site-year.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/results.html)
      Density in the weight model
      Bull kelp plants of the same diameter weigh more or less depending on how crowded they are, so the weight model uses stipe density as a predictor: the stipes counted over the area surveyed in each site-year of the density data. Site-years without density data take the mean density.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/models.html)
      Canopy area
      The total area of kelp canopy at a site in a year, in square metres, measured from drone imagery. It is the same on every row of that site-year in the cover sheet.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/total-biomass.html)
      Percent cover
      The percentage of a plot covered by kelp canopy in the drone imagery. It links the biomass measured in plots to what the drone sees.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/total-biomass.html)
      Total biomass
      Biomass per unit area is predicted from percent cover with the cover model, then scaled up by the canopy area. The uncertainty in every model is carried through to the limits. Site-years with canopy area but no plot cover use the population-level cover relationship.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/total-biomass.html)
      Priors
      A prior states which parameter values are plausible before the data are seen. The defaults rule out implausible values but leave the data to decide the estimates, so most analyses keep them.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/priors.html)

