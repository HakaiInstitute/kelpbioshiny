# Decision: the web app is a Shiny + bslib companion package

Status: accepted (2026-10)

## Context

The app lets marine ecologists with no R experience estimate kelp biomass with
kelpbio: upload survey data, fit or choose sub-models, review diagnostics, and
download estimates, figures, fitted models, and a script that reproduces the run
in R. Users are researchers who will publish results, so convergence diagnostics,
compatibility intervals, and reproducibility are requirements, not extras.

Facts that shape the choice:

- The models are R/Stan (`rstan`, `decisions/engine-choice.md`). Any app needs an
  R process on the server; a non-R frontend means R behind an API.
- Custom fitting is the main use, not the exception. Only the weight model is
  expected to be used pre-fit in most runs. Fits take minutes and must run in the
  background with progress and cancel.
- Concurrency is low: a handful of users at a time.
- The app code will mostly be written by Claude Code and validated through use
  and tests; the reviewer's attention goes to kelpbio, where correctness lives.
- shinyapps.io is being retired into Posit Connect Cloud (active until
  2026-12-31). Hosting is therefore Posit Connect, Connect Cloud, or a container
  on Hakai infrastructure.

## Options considered

UI prototypes of the same workflow with the same fake data were built in Shiny +
bslib and in React (Vite, TypeScript, Tailwind, shadcn/ui), the latter assuming a
plumber API backend.

- **React + plumber.** The most polished interaction and the best frontend test
  tooling (Vitest, Playwright). Costs: two runtimes, a hand-built fit job system
  (submit, poll, cancel, store, clean up) and per-user state keyed by job ID, and
  a TypeScript codebase nobody on the project reviews.
- **Shiny + bslib, default styling.** Simplest, but visibly dated next to the
  React prototype.
- **Shiny + bslib restyled to match shadcn.** Nearly indistinguishable from
  React, but needed about 500 lines of CSS overriding Bootstrap, bslib, and
  reactable internals plus custom JavaScript: brittle across package upgrades.
- **Shiny + bslib through supported APIs only.** Theme via `bs_theme()` Sass
  variables, bslib components, and `reactableTheme()`; custom CSS (about 120
  lines) only on the app's own classes; no custom JavaScript. Close to the React
  look, with a few simpler interactions (plain radio choices, no per-option
  disabling in selects).
- Python frontends (Streamlit, Shiny for Python) and a port of shadcn to Shiny
  (`shiny.shadcn`, a two-component proof of concept) were ruled out early.

## Decision

Build the app as a Shiny + bslib companion package, styled only through
supported APIs.

- **One runtime.** Server code calls `kb_*` functions directly; uploaded data and
  fits live in the session.
- **Fits run in the background** with `ExtendedTask` (mirai), polling
  `kb_fit_progress()` for progress. One queue per session runs fits in turn.
- **The app computes no numbers and draws no custom figures.** Every estimate,
  table, and plot comes from a kelpbio function, so kelpbio's test suite covers
  the results and the app is a thin layer over it.
- **Styling rules.** Theme through `bs_theme()` and Sass variables; custom CSS
  only on `.kb-*` classes using `var(--bs-*)`; no CSS that targets Bootstrap,
  bslib, shiny, or reactable internals; no custom JavaScript files.

## Workflow

Four steps, free to move between, each showing whether it is complete:

1. **Data.** Species, then one Excel workbook with a sheet per sub-model (or a
   CSV per sub-model). Each sheet is checked with `kb_check_data_*()`, and a
   coverage grid shows which site-years have data for which sub-models, flagging
   gaps (filled by population-level estimates) and mismatched site names.
2. **Models.** One row per sub-model with its source: your data, a pre-fit model
   (coastwide for size and weight, Hakai Institute for wet:dry and carbon), or
   not used. Density always uses the user's data. Defaults follow the workbook;
   "Fit all" queues the custom fits. Each model has a page with tabs for data,
   settings (priors and sampler), diagnostics, predictions, and description.
3. **Biomass.** Unlocks when every sub-model in use is ready. Wet biomass needs
   density, size, and weight; dry adds wet:dry; carbon adds carbon. Estimates by
   site-year with compatibility intervals, flagged where pre-fit or
   population-level components were used and where a fit has convergence
   warnings.
4. **Export.** Results workbook (one sheet per table plus the settings and
   sources used), figures, a re-uploadable fit bundle, an R script that
   reproduces the run with `kb_*` calls, and a report.

Plain-language help (popovers on technical terms such as R-hat, ESS, thinning,
and compatibility intervals) is part of the design, since most users will not be
familiar with Bayesian analysis.

## Consequences

- The app's state is per session. A fit is lost if the browser closes mid-fit;
  the fit bundle download lets users resume.
- On Posit Connect, run the app with a long idle timeout so background fits are
  not ended with an idle session.
- Tests follow shinyssdtools: `testServer()` per module with kelpbio's fixture
  fits standing in for MCMC, and a few shinytest2 end-to-end workflows.
- Some interactions are simpler than a React build would allow. If they become a
  real limitation, the "no numbers in the app" rule keeps a later move to a
  plumber API possible: the API would wrap the same `kb_*` calls.
