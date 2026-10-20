# CLAUDE.md - kelpbioshiny

R package holding the Shiny app for kelpbio (Bayesian kelp biomass estimation). The one export is `run_app()`. The app walks a run through four steps: Data, Models, Estimates, Export. Why a Shiny app, and its scope: `~/Code/HakaiInstitute/kelpbio/decisions/webapp-decision.md`.

Follow the kelpbio `CLAUDE.md` (`~/Code/HakaiInstitute/kelpbio/CLAUDE.md`) for writing, code style, documentation and testing conventions; this file lists only what differs or is specific to the app.

## Common Commands

| Task | Command |
|------|---------|
| Routine build + QC | `Rscript scripts/build.R` (`install()` -> `roxygen2md()` -> `document()` -> `test()`) |
| Full check (slow) | `Rscript scripts/build.R --check` (adds `R CMD check`) |
| Run all tests | `devtools::test()` |
| Run the app from source | `pkgload::load_all(); run_app()` |

The build runs are also Positron/VS Code tasks ("kelpbioshiny: build", "kelpbioshiny: build + check"). Linting runs in CI via jarl (`jarl.toml`).

## Layout

| Path | Contents |
|------|----------|
| `R/run-app.R` | `run_app()`: `shinyAppDir()` on `inst/app` |
| `R/app-ui.R`, `R/app-server.R` | `app_ui()` (navbar, steps, footer) and `app_server()` (wires the modules to one store) |
| `R/mod_<step>.R` | One module per step: `mod_<step>_ui(id)` / `mod_<step>_server(id, store)` (`data`, `models`, `estimates`, `export`); `mod_model.R` is the per-model detail page (fit, settings, diagnostics, parameter estimates, description), and `mod_estimates.R` holds each model's predictions and the derived estimates (survey biomass, canopy biomass) |
| `R/mod_help.R` | The Help tab: `help_ui()` and `help_server()` (its sidebar) hold three pages, `help_guide_ui()` (User guide, built from the app's definitions), `help_methods_ui()` (a tile per kelpbio article, from `kelpbio_articles`) and `help_about_ui()` (with the citations) |
| `R/steps.R` | `app_purpose` and `app_steps`: the app's one-line purpose, and the step names and one-line descriptions |
| `R/state.R` | `new_store()`: the per-session store of reactive state and actions, the fit queue and its runner, plus the pure fit-record transitions and status logic |
| `R/data.R` | Sheet and model definitions (`sheet_info`, `components`), the census rule, labels, sheet checks, prior and sampler settings |
| `R/theme.R` | The Hakai `bs_theme()` and the reactable theme |
| `R/functions-ui.R`, `R/help.R`, `R/icons.R`, `R/kelp-art.R` | UI building blocks, help popovers, Lucide icons, the navbar's kelp drawings |
| `R/mock-kelpbio.R` | Mocks of the kelpbio data figures the app calls, not yet in kelpbio (see below) |
| `R/namespace.R` | `@import` / `@importFrom` directives |
| `inst/app/` | `ui.R`, `server.R` (call the internals), `global.R` (stops the mirai daemons with the app) and `www/` (CSS, logo, favicon) |
| `data-raw/` | `test-workbooks.R` and the test workbooks it writes (`workbooks/`), one per kind of user; build-ignored |
| `inst/CITATION` | The app's citation, shown on the About page |
| `app.R` | Deployment entry point (`load_all()` + `run_app()`), build-ignored |

## Key Rules

- **The app computes no numbers and draws no custom figures.** Every estimate, table and figure comes from an unqualified `kb_*()` call or a kelpbio generic (`tidy()`, `glance()`, `summary()`, `converged()`); the app only labels, arranges and formats. `test-architecture.R` scans the installed namespace with codetools to check that app code calls no summary or distribution functions.
- **Fits run in the background.** Each `kb_fit_*()` call runs on a mirai daemon through one `ExtendedTask` per session; the store polls `kb_progress()` while it runs. The cover calibration's job (`fit_cover_calibration()`) predicts the survey biomass it is fitted to in the same daemon call. `start_daemons()` starts the daemons with the first fit (one per R process, or the option `kelpbioshiny.daemons`) and, under `load_all()`, loads the package from source on them. The runner is injectable (`new_store(session, run_fit =)`), so tests run fits in the session.
- **kelpbio is in Imports** (`Remotes:` pins its branch until it is merged); its functions are imported in `R/namespace.R`. The only mocks are the data figures, `kb_plot_data_*()`, in `R/mock-kelpbio.R`, each marked Planned; app code calls no mock internals. To switch one to kelpbio: delete its mock and add `@importFrom kelpbio <fun>` to `R/namespace.R`. kelpbio's derived predictions carry their response as an `rvar` column of draws, which the app drops from tables with `summary_columns()`.
- **Pre-fit models and examples come from kelpbio's data.** A pre-fit source is kelpbio's reference fit (`prefit_fit()`: `fit_<model>_<reference>_<species>`), the references by species in `components[[id]]$prefit`. The example workbook is kelpbio's simulated data (`example_rows()`: `data_<sheet>_sim_<species>`); `data-raw/test-workbooks.R` builds the other test workbooks from it.
- **Every fit keeps 4 chains of 1,000 draws** (`fit_draws`), as the reference fits do, since the derived estimates need fits with the same number of draws. Only `nthin` and `adapt_delta` (passed in `control`) can be changed. A convergence warning advises more thinning; a divergence warning (`fit_convergence()`) a higher `adapt_delta`.
- **Styling** comes from the Hakai theme: Bootstrap Sass variables in `bs_theme()` and bslib components. Custom CSS (`inst/app/www/styles.css`) targets only `.kb-*` classes and reads colours from `var(--bs-*)`; it never targets Bootstrap, bslib, Shiny or reactable internals, except one rule that stops Shiny's busy indicators growing empty HTML outputs. No custom JavaScript files; the one script is the inline `onclick` of `copy_button()` (`R/functions-ui.R`), shared by every Copy button.
- **Layout**: every page is `step_layout()`, a sidebar beside the main column, under a `page_header()` (a model's page too). The sidebar navigates where a step has sub-pages (Models, Estimates, Help); on Data it holds the run's setup (species, size data, template, resume), above the main column on a phone (`step_layout(aside_first = TRUE)`), and on Export the run summary. A sheet's data are shown only on the Data step: each sheet is an accordion panel ("View data" in its title) with its figure (`sheet_plot()`, in one panel or faceted by site or year) and a searchable, filterable table; model pages have no data tab, and their data links open the sheet's panel (`store$view_sheet()`). A model's full warnings are on its page, with a one-line verdict per check at the top of its Diagnostics tab; Estimates and Export show one summary notice (`warnings_summary()`) linking to it. Models are listed by what they are for (`model_groups`, `model_order`): survey biomass, then the optional canopy and dry and carbon groups; the Estimates sidebar lists survey and canopy biomass first, then each model's predictions.
- **Buttons** use `button()`'s four variants. Primary (filled Hakai red) is the next step of the run, at most one per screen: Continue to models, Fit all, Continue to estimates (once nothing is left to fit), Fit model, View estimates (on a model page once that model is ready), Continue to export, and a dialog's confirming action. A step's next action sits at the right of its header (`page_header()`). Soft (red tint, `.kb-btn-soft`) is a step still to do among several, such as a model's Fit in the Models list. Outline is every other action: navigation, Refit, Cancel, downloads and Copy. Ghost (no border) is a small adjustment inside a panel: Clear, Reset to defaults, Show all.
- **No `library()` calls**; shiny and bslib are imported whole, everything else via `@importFrom` or `pkg::fun()`. Internal functions carry plain comments, not roxygen (no Rd).
- R code is ASCII only: write non-ASCII characters as `\u` escapes.

## Sheets, models and the census

Sheets (`sheet_ids`: weight, size, survey, canopy, wetdry, carbon) and models (`component_ids`: weight, size, wetdry, carbon, cover) are defined apart: each model is fitted to one sheet (`components[[id]]$sheet`; the cover calibration to the survey sheet), and the canopy sheet feeds only canopy biomass. The sheets are checked together (`check_sheets()`), as the survey sheet decides whether the size sheet is a census.

A census measures every counted plant: the size sheet then holds the plants of each survey, joined to it by `survey_id`, the survey sheet has no `plants` tally, and no size model is fitted (the size source is `"census"`). The user chooses it under Size data on the Data step (`store$size_data()`, `size_data_info`), and the template, sheet checks and column lists follow (`sheet_columns(id, species, census)`); a survey sheet that does not match the choice fails its check with advice. *Macrocystis* surveys are always a census (kelpbio has no *Macrocystis* size model; `is_census()`). A census run shows no size model on the Models, Estimates and Export steps (`shown_models()`, `shown_estimates()`).

The derived estimates offer only what a report uses (`survey_sums`, `canopy_sums()`): survey biomass per m² surveyed or in total over the area surveyed (the default in a census), by site-year or by survey; canopy biomass in kg by drone map, by year, or by year within any text column of the canopy sheet (such as region or habitat). Sums never cross years, as biomass is a peak-season standing stock. kelpbio's other options (mean plant weight, canopy biomass per m², any `pool_by`) stay in the package.

## Documentation and help text

- kelpbio owns the statistics: model forms, priors, diagnostics theory, interpretation and any long-form explanation live in kelpbio's roxygen docs and vignettes/articles. kelpbioshiny never restates them; it links to the relevant kelpbio article or section (`kelpbio_url` and `kelpbio_articles` in `R/help.R`, each article's title, summary and path, which the Methods page lists).
- kelpbioshiny documents only how to use the app: what each step is for, what to click, which sheets and columns to provide, and what to do about a warning.
- One fact, one home inside the app: step names and descriptions (`app_steps`), sheet and column lists and model sources (`components`, `prefit_info`), and glossary text (`help_topics`) are each defined once in R and reused by the UI, the welcome card, the help popovers and the User guide. Never duplicate them as hand-written prose.
- Help popovers: one to three plain-language sentences, then a "Learn more" link to kelpbio. Short text that points elsewhere is preferred over long explanations.
- No screenshots in the guide (if ever needed, generate them with shinytest2).
- Definition of done: any change to steps, sheets, sources, warnings or help topics updates the guide in the same PR; the guide snapshot test (`test-mod_help.R`) shows the change for review. Statistical content changes go to kelpbio, not here.
- Writing follows the kelpbio `CLAUDE.md` writing rules (cold-start reader, no session context, no em-dashes).

## Testing

The suite is deliberately minimal for the prototype: one example of each test type, as the pattern to follow. A full suite replaces it once the app moves past the prototype.

| Type | File |
|------|------|
| Unit test of a pure function (a status rule) | `test-state.R` |
| Store test: `testServer()` on `new_store()`, fits stubbed, run by `test_runner()` | `test-state.R` |
| Module test: `testServer()` on a step's server, driven by `session$setInputs()` | `test-mod_data.R` |
| Snapshot of the User guide text | `test-mod_help.R` |
| Architecture: codetools on the installed namespace | `test-architecture.R` |
| Browser smoke test (shinytest2 `AppDriver` on the installed app; install first) | `test-shinytest2.R` |

- Fits are stubbed at the kelpbio boundary: `local_stub_fits()` replaces the `kb_fit_*()` functions with `local_mocked_bindings()`, each returning kelpbio's fit to its simulated data (`fit_<model>_sim_<species>`), so no MCMC runs. The runner is injected with `new_store(session, run_fit =)`.
- Browser tests never click Fit: with kelpbio in place it would run real MCMC. They `skip_on_cran()` and use no screenshot snapshots.
- Never add `exportTestValues()` or test-only outputs to app code.

Shared helpers: `tests/testthat/helpers.R` (`store_app()`) and `tests/testthat/helper-fits.R` (`local_stub_fits()`, `test_runner()`, `finish_fits()`).
