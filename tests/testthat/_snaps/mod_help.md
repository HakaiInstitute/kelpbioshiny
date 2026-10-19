# the user guide text

    Code
      writeLines(guide_text())
    Output
      User guide
      How to use the app, step by step.
      Steps
      A run moves through four steps in the navbar. A step's marker shows a check mark once the step is complete: data loaded, models ready, or estimates saved.
      1
      Data
      Load the survey data, from a workbook or a CSV file per sheet.
      The columns of each sheet. Every sheet is optional, and other columns are ignored. For Nereocystis, choose under Size data whether a sample of plants was measured or all of them. With all plants measured, a census, the size sheet holds every counted plant and the survey sheet has no plants column. Macrocystis surveys are always a census.
      Sheet
      One row per
      Columns
      weight
      Harvested plant
      Nereocystis luetkeana
      :
      site, date, diameter_mm, weight_kg
      Macrocystis pyrifera
      :
      site, date, fronds, weight_kg
      size
      Measured plant; in a census, every counted plant
      Nereocystis luetkeana
      :
      site, plot_id, date, diameter_mm
      Nereocystis luetkeana
      ,
      all plants measured
      :
      site, plot_id, date, diameter_mm, partial
      Macrocystis pyrifera
      :
      site, plot_id, date, fronds, partial
      survey
      In situ survey of a plot
      Nereocystis luetkeana
      :
      site, plot_id, date, survey_area_m2, plants, plot_area_m2, canopy_area_m2, tide_height_m
      Nereocystis luetkeana
      ,
      all plants measured
      :
      site, plot_id, date, survey_area_m2, plot_area_m2, canopy_area_m2, tide_height_m
      Macrocystis pyrifera
      :
      site, plot_id, date, survey_area_m2, plot_area_m2, canopy_area_m2, tide_height_m
      canopy
      Drone map of a site in a year
      site, year, canopy_area_m2, tide_height_m, site_area_m2
      wetdry
      Tissue sample
      wet_mass_g, dry_mass_g
      carbon
      Dried tissue sample
      sample_mass_mg, carbon_mass_ug
      2
      Models
      Fit each model to your data or use a pre-fit model; open a model to adjust settings and view diagnostics.
      Each model uses one source: your data, a pre-fit model, or none. With all plants measured (a census) there is no size model.
      Model
      Sources
      Weight
      Your data, Pre-fit Coastwide, Pre-fit Hakai Institute
      Size
      Your data, Pre-fit Hakai Institute, Not used
      Wet:dry
      Your data, Pre-fit Hakai Institute, Not used
      Carbon
      Your data, Pre-fit Hakai Institute, Not used
      Cover calibration
      Your data, Not used
      3
      Estimates
      Review the estimates from each model, and survey and canopy biomass from the combined models.
      4
      Export
      Save the estimates, or rerun the whole analysis in R.
      The app can also predict the wet weight of each plant, without biomass. Load a size sheet, use the weight model (fitted to your data or pre-fit), and open Weight on the Estimates step with Group by set to Individual plant.
      Warnings
      The warnings a model or sheet can show, and how to fix them.
      Convergence warning
      Some parameters have not converged. Increase thinning (nthin) on the Settings tab, for example to 2, and refit.
      Prior sensitivity warning
      If the flagged prior was not chosen on purpose, make it less informative on the Settings tab and refit.
      Influential observations
      Check the flagged observations, listed on the Diagnostics tab, for recording errors. Correct any error in the sheet, upload it again and refit; keep observations that check out.
      Settings changed since the fit
      The results and the R script use the settings the model was fitted with. Refit the model to use the new settings.
      The fit failed
      Refit the model to see its results.
      The data check failed
      Correct the sheet and upload it again.
      Site names differ across sheets
      Rename the sites in the workbook so the names match exactly, then upload again.
      Glossary
      The terms behind the help icons in the app.
      R-hat
      R-hat compares the chains with each other. Values close to 1 mean the chains agree on the same answer. A value above 1.01 means the model needs more sampling before its estimates can be relied on.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/diagnostics.html)
      Effective sample size (ESS)
      Draws that follow each other in a chain are similar, so together they hold less information than independent draws. ESS is the number of independent draws they are worth, for the middle (bulk) and the tails of the distribution. Below 100 per chain, estimates and their limits are less reliable.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/diagnostics.html)
      Posterior predictive check
      A posterior predictive check simulates new datasets from the fitted model and compares them with your data. If the model fits well, the dark line for your data sits within the band of light lines for the simulated data. A systematic difference, such as a shifted peak or heavier tails, means the model misses a feature of the data.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/diagnostics.html)
      Prior sensitivity
      Prior sensitivity measures how far each estimate moves when the priors, and then the data, are given slightly more or less weight. A weak prior means the priors are not driving the estimate, and strong data means the data are informative about it. A parameter without both depends on the priors as much as on the data.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/diagnostics.html)
      Influential observations
      Pareto k measures how much the fit changes when an observation is left out. Above 0.7, the observation strongly influences the estimates. That can be a recording error, or a valid observation in a site-year with few others.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/diagnostics.html)
      Overall and group predictions
      Overall predictions are for a typical site in a typical year. They leave out the differences between sites and between years, so they suit sites and years that were not sampled. Predictions by site or by year add the estimated difference for each sampled site or year, so they describe those sites and years. Predictions by site and year include both.
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
      Pre-fit Coastwide models were fitted in advance to data compiled from surveys along the coast, and Pre-fit Hakai Institute models to Hakai Institute survey data. They need no data or fitting here. Sites in the reference data get their own estimates.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/pre-fit-models.html)
      Site-years without data
      When a model has no data for a site-year, its estimate there uses the site's estimate from other years and the year's estimate from other sites. What is specific to that site-year is drawn from the variation the model estimated between site-years, so its compatibility interval is wider. With neither the site nor the year in the data, the estimate is for a typical site and year.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/results.html)
      Census
      With a sample of plants measured, the surveys tally their plants and a size model is fitted to the plants measured. With all plants measured (a census), the size sheet holds the plants of each survey, joined to it by site, plot_id and date, and the survey sheet has no plants column. Survey biomass then sums the weights of the plants, and no size model is fitted. Macrocystis surveys are always a census.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/models.html)
      Mean plant weight
      The expected weight of a plant at a site-year: the weight model's curve averaged over the site-year's size distribution from the size model. Survey biomass is the plants counted in a survey times this weight.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/models.html)
      Survey biomass
      The biomass of the plants counted in an in situ survey, per square metre of the area searched. By site and year, the surveys of a site-year are pooled: their summed biomass over their summed area.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/models.html)
      Canopy area
      The area of kelp canopy the drone imagery delineated within a plot polygon, or mapped over a whole site, in square metres. A flight with no canopy has a canopy area of 0.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/canopy-biomass.html)
      Tide-corrected cover
      Less of the canopy shows at the surface at higher tides, so the cover calibration corrects the canopy area for the tide height at the flight. Cover is the corrected canopy area as a proportion of the plot area, from 0 to 1.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/canopy-biomass.html)
      Canopy biomass
      The cover calibration relates the wet biomass of each survey to the tide-corrected drone canopy cover of its plot. Canopy biomass applies it to the canopy mapped over each site in a drone map. The uncertainty in every model is carried through to the limits.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/canopy-biomass.html)
      Priors
      A prior states which parameter values are plausible before the data are seen. The defaults rule out implausible values but leave the data to decide the estimates, so most analyses keep them.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/priors.html)

