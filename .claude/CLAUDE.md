# kelpbioshiny

kelpbioshiny is an R package that holds a Shiny app for [kelpbio](https://github.com/HakaiInstitute/kelpbio), a package for Bayesian estimation of kelp biomass.
Its one export is `run_app()`.
A run moves through four steps: Data, Models, Estimates and Export.

This file is the guide for maintaining and developing the app.
`.claude/STYLE.md` sets the wording of every text the app shows and of the README.
kelpbio's `.claude/CLAUDE.md` sets the shared code style and test conventions; this file lists what is specific to the app.
Both files are committed; the rest of `.claude/` is not.

## Related packages

- kelpbio fits the models and computes every estimate, table and figure.
- kelpbiodata holds the reference fits offered as pre-fit models.
- Both are in Imports and are not pinned: the app uses the versions installed.

## Repositories and workflow

- Code lives in `HakaiInstitute/kelpbioshiny`; issues are tracked in the public fork `poissonconsulting/kelpbioshiny`.
- Work is committed to `dev`, one themed change per commit, and reaches `main` through a pull request that lists the issues it closes (`Closes poissonconsulting/kelpbioshiny#<n>`).
- CI runs R CMD check and jarl on pull requests to `main`, installing kelpbio and kelpbiodata from GitHub.
- Before pushing, run `Rscript scripts/build.R` and `jarl check .`; keep the local jarl as recent as CI's, and never assign inside a call (`out <- f()` outside `expect_*()`).
- Planned work goes in an issue on the fork.
- With Claude Code, load the posit-dev skills `shiny:shiny-bslib` and `shiny:shiny-bslib-theming` before working on the app.

## Commands

| Task | Command |
|------|---------|
| Run the app from source | `pkgload::load_all(); run_app()` |
| Run the tests | `devtools::test()` |
| Install, document and test | `Rscript scripts/build.R` |
| Add R CMD check | `Rscript scripts/build.R --check` |
| Rebuild the example workbooks | `Rscript data-raw/test-workbooks.R` |

## Layout

| Path | Contents |
|------|----------|
| `R/mod_<step>.R` | One module per step (`data`, `models`, `estimates`, `export`); `mod_model.R` is a model's page and `mod_help.R` the Help tab |
| `R/state.R` | `new_store()`: the session's reactive state, actions and fit queue, and the fit status rules |
| `R/data.R` | Sheet and model definitions, sheet checks, the census rule, and prior and sampler settings |
| `R/steps.R`, `R/help.R` | Step names and descriptions; help popovers and glossary (`help_topics`) |
| `R/functions-ui.R`, `R/theme.R`, `R/icons.R`, `R/kelp-art.R` | UI building blocks, theme, icons and the navbar's kelp drawing |
| `R/mock-kelpbio.R` | Data figures (`kb_plot_data_*()`) planned for kelpbio |
| `inst/app/` | `ui.R`, `server.R`, `global.R` (upload limit, daemon shutdown) and `www/` (CSS, logo, favicon) |
| `data-raw/` | `test-workbooks.R` and the example workbooks it writes; build-ignored |
| `scripts/` | `build.R` and `deploy.R` |

## Design rules

- The app computes no numbers and draws no custom figures: every estimate, table and figure comes from a `kb_*()` function or a kelpbio generic (`tidy()`, `glance()`, `summary()`); the app labels, arranges and formats. `test-architecture.R` checks this.
- kelpbio owns the statistics: model forms, priors, diagnostics and their interpretation are documented there, and the app links to kelpbio's articles rather than restating them.
- Fits run in the background, one `kb_fit_*()` call at a time per session, on mirai daemons through an `ExtendedTask`; the store polls `kb_progress()`. The daemons start with the first fit (option `kelpbioshiny.daemons` sets their number).
- Every fit keeps 4 chains of 1,000 draws (`fit_draws`), as the reference fits do, since the derived estimates combine fits draw by draw. Only `nthin`, `adapt_delta` and `seed` can be changed; the seed is drawn once per session, and the R script and `settings.csv` record it.
- Pre-fit models are kelpbiodata reference fits (`prefit_fit()`), offered only where the installed kelpbiodata holds one; a combined source ("Your data + Hakai Institute") appends the user's data to the reference data and fits the model in the app.
- Each fact has one home in R (step text in `app_steps`, sheets and sources in `components`, glossary text in `help_topics`) and is reused by the UI, popovers and User guide; a change to steps, sheets, sources, warnings or help topics updates the guide in the same change.
- Styling comes from the Bootstrap variables in `R/theme.R` and bslib components. Custom CSS targets only `.kb-*` classes and reads colours from `var(--bs-*)`. There are no custom JavaScript files.
- Buttons use `button()`'s variants: primary for the run's next step (at most one per screen), soft for a step still to do among several, outline for other actions, ghost for small adjustments inside a panel.
- R code is ASCII only (write `\u` escapes), calls no `library()`, and imports shiny and bslib whole and other packages through `@importFrom` or `pkg::fun()`. Internal functions carry plain comments, not roxygen.
- Before removing a function or UI element, search for every caller, including mocks and code run on the daemons.

## Sheets, models and the census

- Sheets (`sheet_ids`) and models (`component_ids`) are defined apart: each model is fitted to one sheet, the cover calibration to the in situ survey sheet, and the canopy sheet feeds only canopy biomass. The sheets are checked together (`check_sheets()`).
- In a census every counted plant is measured: the size sheet holds each survey's plants, joined by `survey_id`, and no size model is fitted. *Macrocystis* surveys are always a census. The size sheet is named `size_sample` or `size_census` in workbooks and CSV files, which sets the choice on upload.
- The derived estimates offer what a report uses: in situ survey biomass per m² or in total, by site and year or by survey, and canopy biomass by canopy map, by year, or by year within a text column of the canopy sheet. Sums never cross years.

## Testing

The suite is kept small: it shows when a kelpbio release or a change to the app breaks a run, not every screen.
Add a test for a new kelpbio call path or for a bug a user found.

| Test | File |
|------|------|
| Status rules and the census rule; Fit all, settings and seed; a *Macrocystis* run | `test-state.R` |
| Example workbook upload; a sheet that fails its check | `test-mod_data.R` |
| Each fitted model's page | `test-mod_model.R` |
| Results ZIP and fit bundle, and resuming a run | `test-mod_export.R` |
| The citation file | `test-mod_help.R` |
| No statistics in app code; every kelpbio function used exists | `test-architecture.R` |
| The installed app starts and loads the example (shinytest2) | `test-shinytest2.R` |

- Fits are stubbed at the kelpbio boundary: `local_stub_fits()` returns kelpbio's fits to its simulated data, and `new_store(session, run_fit =)` takes a runner that fits in the session, so no MCMC runs.
- Browser tests never click Fit, which would run MCMC.
- App code holds no test-only outputs or `exportTestValues()`.

## Example workbooks

`data-raw/test-workbooks.R` builds them from kelpbio's simulated data; its header says what each shows.
Rebuild and check them whenever kelpbio's simulated data change.

| Workbook | Shows |
|----------|-------|
| `kelpbio-1-full-nereo`, `kelpbio-1-full-macro` | Every sheet, fitted through to canopy biomass |
| `kelpbio-2-demo-seed2-nereo` | Pre-fit models and appending data; with the weight model's seed set to 2, divergences (fixed by `adapt_delta = 0.99`) and, appended to the reference data, unconverged parameters (fixed by `nthin = 2`) |
| `kelpbio-3-census-nereo` | A census without canopy data |
| `kelpbio-4-canopy-prefit-nereo` | Canopy biomass from canopy maps alone, with the pre-fit cover calibration |
| `kelpbio-5-bad-data-nereo` | A data error and a data warning |

## Releases

The package is pre-release (`0.0.0.9000`): no NEWS entries or version bumps yet.
From the first release, follow the tidyverse release process: a NEWS bullet for each user-facing change, versions set with `usethis::use_version()`, and lifecycle for deprecations.
