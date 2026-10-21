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
      Load your survey data.
      The columns of each sheet. Every sheet is optional, and other columns are ignored. For Nereocystis, choose under Size data whether a sample of plants was measured or all of them. With all plants measured, a census, the size sheet holds every counted plant and the survey sheet has no plants column. Macrocystis surveys are always a census.
      Sheet
      One row per
      Columns
      weight
      Harvested plant
      Nereocystis luetkeana
      :
      site, year, diameter_mm, weight_kg
      Macrocystis pyrifera
      :
      site, year, fronds, weight_kg
      size
      Measured plant; in a census, every counted plant
      Nereocystis luetkeana
      :
      site, year, diameter_mm
      Nereocystis luetkeana
      ,
      all plants measured
      :
      survey_id, diameter_mm, partial
      Macrocystis pyrifera
      :
      survey_id, fronds, partial
      survey
      In situ survey
      Nereocystis luetkeana
      :
      survey_id, site, year, survey_area_m2, plants, plot_area_m2, canopy_area_m2, tide_height_m
      Nereocystis luetkeana
      ,
      all plants measured
      :
      survey_id, site, year, survey_area_m2, plot_area_m2, canopy_area_m2, tide_height_m
      Macrocystis pyrifera
      :
      survey_id, site, year, survey_area_m2, plot_area_m2, canopy_area_m2, tide_height_m
      canopy
      Drone map of a site
      site, year, canopy_area_m2, tide_height_m, classified_area_m2
      wetdry
      Tissue sample
      wet_mass_g, dry_mass_g
      carbon
      Dried tissue sample
      sample_mass_mg, carbon_mass_ug
      2
      Models
      Fit the models, or use pre-fit ones.
      Each model uses one source: your data, a pre-fit model, or none. With all plants measured (a census) there is no size model.
      Model
      Sources
      Weight
      Your data, Pre-fit Coastwide (
      Nereocystis luetkeana
      only), Pre-fit Hakai Institute
      Size
      Your data, Pre-fit Hakai Institute (
      Nereocystis luetkeana
      only), Not used
      Wet:dry
      Your data, Pre-fit Hakai Institute, Not used
      Carbon
      Your data, Pre-fit Hakai Institute, Not used
      Cover calibration
      Your data, Pre-fit Hakai Institute, Not used
      3
      Estimates
      Review biomass and model estimates.
      4
      Export
      Save the estimates, or rerun the whole analysis in R.
      The results workbook, the figures and the report hold every available estimate, at the choices made on its page on the Estimates step (such as Group by, Scale and Show), in each measure available. To resume a run later, download its fit bundle and add it under Resume a run on the Data step.
      Results workbook (Excel: every available estimate, with the settings and sources)
      Figures (ZIP of PNG and PDF figures)
      Fit bundle (RDS: add it on the Data step to resume this run)
      Report (PDF with methods, diagnostics and results)
      The app can also predict the wet weight of each plant, without biomass. Load a size sheet, use the weight model (fitted to your data or pre-fit), and open Weight on the Estimates step with Group by set to Individual plant.
      Warnings and notes
      The warnings and notes a model or sheet can show, and what to do about them.
      Warnings
      The fit or data cannot be relied on until they are fixed.
      Convergence warning
      Some parameters have not converged. Increase thinning (nthin) on the Settings tab, for example to 2, and refit.
      Divergent transitions
      The sampler could not explore part of the posterior. Raise adapt_delta on the Settings tab, for example to 0.99, and refit.
      Settings changed since the fit
      The results and the R script use the settings the model was fitted with. Refit the model to use the new settings.
      The fit failed
      Refit the model to see its results.
      The data check failed
      Correct the sheet and upload it again.
      Site names differ across sheets
      Rename the sites in the workbook so the names match exactly, then upload again.
      Notes
      The fit is valid; check what is flagged.
      Prior sensitivity
      If the flagged prior was not chosen on purpose, make it less informative on the Settings tab and refit.
      Influential observations
      Check the flagged observations, listed on the Diagnostics tab, for recording errors. Correct any error in the sheet, upload it again and refit; keep observations that check out.
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
      Pareto k measures how much the fit changes when an observation is left out. Above a threshold that loo sets from the number of draws, the observation strongly influences the estimates. That can be a recording error, or a valid observation in a site-year with few others.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/diagnostics.html)
      Overall and group predictions
      Overall predictions are for a typical site in a typical year. They leave out the differences between sites and between years, so they suit sites and years that were not sampled. Predictions by site or by year add the estimated difference for each sampled site or year, so they describe those sites and years. Predictions by site and year include both.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/results.html)
      Thinning (nthin)
      Keeps every nth draw from each chain. The sampler runs n times as many iterations to keep the same number of draws, so the kept draws are less alike and ESS goes up. Fitting takes about n times as long.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/diagnostics.html)
      Target acceptance (adapt_delta)
      The acceptance rate the sampler tunes its step size to. A higher value takes smaller steps, which removes divergent transitions but makes fitting slower.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/diagnostics.html)
      Estimates and limits
      Each estimate is the median of the posterior distribution. The lower and upper limits bound the 95% compatibility interval: the range of values most compatible with the data and the model.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/results.html)
      Pre-fit models
      Pre-fit Coastwide models were fitted in advance to data compiled from surveys along the coast, and Pre-fit Hakai Institute models to Hakai Institute survey data. They need no data or fitting here. Sites in the reference data get their own estimates.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/pre-fit-models.html)
      Site-years without data
      The models are hierarchical, so a site-year without data still gets an estimate. It uses the site's estimate from other years and the year's estimate from other sites, with a wider compatibility interval. With neither the site nor the year in the data, the estimate is for a typical site and year.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/results.html)
      Census
      With a sample of plants measured, the surveys tally their plants and a size model is fitted to the plants measured. With all plants measured (a census), the size sheet holds the plants of each survey, joined to it by survey_id, and the survey sheet has no plants column. Survey biomass then sums the weights of the plants, and no size model is fitted. Macrocystis surveys are always a census.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/models.html)
      Survey biomass
      The biomass of the plants counted in an in situ survey, per square metre of the area surveyed or in total over it. By site and year, the surveys of a site-year are pooled: their summed biomass, divided by their summed area for biomass per square metre.
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
      Stand-in sites
      A site that a model has no data for gets the effect of a new site, drawn from the spread between sites, so its compatibility interval is wide. Sites in the model's data, such as nearby sites with similar conditions, can stand in for it instead; several are averaged. The estimate flags name the stand-ins used.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/results.html)
      Priors
      A prior states which parameter values are plausible before the data are seen. The defaults rule out implausible values but leave the data to decide the estimates, so most analyses keep them.
      Learn more (https://hakaiinstitute.github.io/kelpbio/articles/priors.html)

