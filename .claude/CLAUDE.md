# kelpbioshiny

kelpbioshiny is an R package that holds a Shiny app for [kelpbio](https://github.com/HakaiInstitute/kelpbio), a package for Bayesian estimation of kelp biomass.
Its one export is `run_app()`, and a run moves through four steps: Data, Models, Estimates and Export.
`.claude/STYLE.md` sets the wording of the text the app shows; kelpbio's `.claude/CLAUDE.md` sets the shared code style.

## Related packages

- kelpbio fits the models and computes every estimate, table and figure.
- kelpbiodata holds the Hakai Institute reference data and fits offered as pre-fit models (CC BY 4.0, with an attribution statement the app shows).
- Both install from their `main` branches on GitHub through `Remotes:`.

## Workflow

- Commit to `dev`; changes reach `main` through a pull request.
- CI runs R CMD check and jarl on pushes to `dev` and `main`.
- Before pushing, run `Rscript scripts/build.R` (install, document, test) and `jarl check .`.
- `scripts/deploy-azure.R` deploys the Docker image to Azure; `scripts/deploy.R` deploys a preview to shinyapps.io.

## Design rules

- The app computes no numbers and draws no figures of its own: these come from kelpbio (`test-architecture.R` checks this). The two exceptions, the Data step's figures and the census plant counts, are in `R/mock-kelpbio.R`.
- Fits run in the background on mirai daemons, one per session at a time, with 4 chains of 1,000 draws; only `nthin`, `adapt_delta` and `seed` can be changed.
- Each fact has one home in `R/` (steps in `app_steps`, sheets and models in `components`, glossary in `help_topics`), reused by the UI and the User guide.
- Styling comes from `R/theme.R` and bslib; custom CSS targets only `.kb-*` classes. No custom JavaScript files.
- R code is ASCII only and calls no `library()`.

## Tests and example workbooks

- Fits are stubbed (`local_stub_fits()`), so the tests run no MCMC; browser tests never click Fit.
- `data-raw/test-workbooks.R` builds the example workbooks in `data-raw/workbooks/` from kelpbio's simulated data; its header says what each shows. Rebuild and commit them when kelpbio's simulated data change.
