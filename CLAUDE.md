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
| `R/mod_<step>.R` | One module per step: `mod_<step>_ui(id)` / `mod_<step>_server(id, store)` (`data`, `models`, `estimates`, `export`); `mod_model.R` is the per-model detail page (fit, settings, diagnostics), and `mod_estimates.R` holds each model's predictions and the biomass estimates |
| `R/mod_help.R` | The Help tab: `help_ui()` and `help_server()` (its sidebar) hold two pages, `help_guide_ui()` (User guide, built from the app's definitions) and `help_about_ui()` (with the citations) |
| `R/steps.R` | `app_purpose` and `app_steps`: the app's one-line purpose, and the step names and one-line descriptions |
| `R/state.R` | `new_store()`: the per-session store of reactive state and actions, the fit queue and its runner, plus the pure fit-record transitions and status logic |
| `R/data.R` | Model definitions, labels, sheets, prior and sampler settings |
| `R/theme.R` | The Hakai `bs_theme()` and the reactable theme |
| `R/functions-ui.R`, `R/help.R`, `R/icons.R`, `R/kelp-art.R` | UI building blocks, help popovers, Lucide icons, the navbar's kelp drawings |
| `R/mock-kelpbio.R`, `R/mock-example.R` | Mocks of the kelpbio functions the app calls, and the example data (see below) |
| `R/namespace.R` | `@import` / `@importFrom` directives |
| `inst/app/` | `ui.R`, `server.R` (call the internals), `global.R` (starts the mirai daemons that run the fits) and `www/` (CSS, logo, favicon) |
| `inst/extdata/` | Fake JSON: the example data and the mocks' results |
| `data-raw/` | `test-workbooks.R` and the test workbooks it writes (`workbooks/`), one per kind of user; build-ignored |
| `inst/CITATION` | The app's citation, shown on the About page |
| `app.R` | Deployment entry point (`load_all()` + `run_app()`), build-ignored |

## Key Rules

- **The app computes no numbers and draws no custom figures.** Every estimate, table and figure comes from an unqualified `kb_*()` call or a kelpbio generic (`tidy()`, `glance()`, `summary()`, `converged()`); the app only labels, arranges and formats. `test-architecture.R` scans the installed namespace with codetools to check that app code calls no summary or distribution functions.
- **Fits run in the background.** Each `kb_fit_*()` call runs on a mirai daemon through one `ExtendedTask` per session; the store polls `kb_fit_progress()` while it runs. The runner is injectable (`new_store(session, run_fit =)`), so tests run fits in the session.
- **Mocks live only in `R/mock-kelpbio.R` and `R/mock-example.R`.** App code calls no mock internals and reads fake data only through `kb_example_data()`. Each mock is marked Exists (exported by kelpbio's sim-weight-site-sd branch, not yet merged into main; matches its signature and checks) or Planned. kelpbio is not yet in Imports. To switch a function to the real one: delete its mock, add kelpbio to Imports in `DESCRIPTION` (once), and add `@importFrom kelpbio <fun>` to `R/namespace.R`. The mock's `kb_fit` method for `tidy` is registered with `@exportS3Method`; delete it once kelpbio is imported, since kelpbio provides it.
- **Styling** comes from the Hakai theme: Bootstrap Sass variables in `bs_theme()` and bslib components. Custom CSS (`inst/app/www/styles.css`) targets only `.kb-*` classes and reads colours from `var(--bs-*)`; it never targets Bootstrap, bslib, Shiny or reactable internals. No custom JavaScript files; the one script is the inline `onclick` of `copy_button()` (`R/functions-ui.R`), shared by every Copy button.
- **Layout**: every page is `step_layout()`, a sidebar beside the main column, under a `page_header()` (a model's page too). The sidebar navigates where a step has sub-pages (Models, Estimates, Help); on Data it holds the run's setup (species, template, resume) and on Export the run summary. A model's full warnings are on its page; Estimates and Export show one summary notice (`warnings_summary()`) linking to it.
- **Buttons** use `button()`'s four variants. Primary (filled Hakai red) is the next step of the run, at most one per screen: Continue to models, Fit all, Continue to estimates (once nothing is left to fit), Fit model, View estimates (on a model page once that model is ready), Continue to export, and a dialog's confirming action. A step's next action sits at the right of its header (`page_header()`). Soft (red tint, `.kb-btn-soft`) is a step still to do among several, such as a model's Fit in the Models list. Outline is every other action: navigation, Refit, Cancel, downloads and Copy. Ghost (no border) is a small adjustment inside a panel: Clear, Reset to defaults, Show all.
- **No `library()` calls**; shiny and bslib are imported whole, everything else via `@importFrom` or `pkg::fun()`. Internal functions carry plain comments, not roxygen (no Rd).
- R code is ASCII only: write non-ASCII characters as `\u` escapes.

## Documentation and help text

- kelpbio owns the statistics: model forms, priors, diagnostics theory, interpretation and any long-form explanation live in kelpbio's roxygen docs and vignettes/articles. kelpbioshiny never restates them; it links to the relevant kelpbio article or section (`kelpbio_url` and `kelpbio_articles` in `R/help.R`).
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

- Fits are stubbed at the kelpbio boundary: `local_stub_fits()` replaces the `kb_fit_*()` functions with `local_mocked_bindings()`, so the tests survive the swap from the mocks to kelpbio. The runner is injected with `new_store(session, run_fit =)`.
- Browser tests never click Fit: with kelpbio in place it would run real MCMC. They `skip_on_cran()` and use no screenshot snapshots.
- Never add `exportTestValues()` or test-only outputs to app code.

Shared helpers: `tests/testthat/helpers.R` (`store_app()`) and `tests/testthat/helper-fits.R` (`local_stub_fits()`, `test_runner()`, `finish_fits()`).
