# Decision: model logic moves from the app to kelpbio

Status: accepted, not yet implemented (2026-10)

## Context

The app is meant to present kelpbio results and compute nothing itself. Some
model facts and bookkeeping still live in the app, so they can drift from what
kelpbio does and every new sub-model needs app changes.

## Decision

Each item below moves to kelpbio when the related sub-model or feature is
built there. The app keeps its current code (or a mock in `R/mock-kelpbio.R`)
until then, and deletes it once the kelpbio function exists.

| Proposed kelpbio function | Replaces in the app |
|---|---|
| `kb_models(species = NULL)`: labels, descriptions, required columns, required or optional, pre-fit references, dependencies, and the check/priors/fit/prefit functions per model | `components`, `kelpbio_fn()` name building, the cover id mapping, `kb_model_dependencies()` |
| `kb_coverage(density, size, weight, cover)` and `kb_check_site_names(...)` | `coverage()`, site-year bookkeeping and site-name matching in `R/mod_data.R` |
| A helper that adds site-year `stipes_m2` (from the density data, corrected for area) to weight data; name to be decided | The planned mock used before a weight fit |
| `kb_convergence(fit, ..., rhat = 1.01, esr = 0.1)`: per-term flags | Per-term convergence flagging |
| `kb_sensitivity(fit, ..., prior_cjs = 0.1, lik_cjs = 0.05)` with the matching prior entry and, optionally, a suggested wider prior | The prior advice computed in the app |
| `format.kb_prior()` and a labelled priors table | `prior_labels`, `format_prior()`, `default_priors()`; one name per parameter across diagnostics and the prior editor |
| `kb_group_vars(fit)` and `kb_predictor_ref(fit)` | `has_year_effect()` and the hard-coded prediction reference values |
| A `data` argument to `kb_plot_predictions()` for observed points | The app's own plot layer on the cover predictions |
| Units on `kb_predict_biomass()` output and `kb_biomass_models(type, total)` | `output_info` units and output availability rules |
| Sampler defaults from the fit functions' formals or `kb_sampler_defaults()` | Defaults repeated in the app |

Two smaller additions the app already mocks: each `kb_priors_*()` entry names
the model term it sets (so warnings and the prior editor use one name), and
`kb_model_describe(fit, priors = FALSE)` leaves the priors out (the app shows
priors only in its prior editor).

The export script stays in the app but should be built from the same call
objects that run each fit, so the script always matches what ran.

## Rationale

kelpbio is the reviewed, tested home for model behaviour. Keeping model facts
there means one source of truth, fewer app changes per sub-model, and an app
that a single maintainer can keep in step with the package.

## kelpbio articles the app links to

The app's help links point to these kelpbio articles (`kelpbio_articles` in
`R/help.R`), each still to be written in kelpbio:

- `articles/diagnostics.html`: R-hat, effective sample size, thinning,
  posterior predictive checks and prior sensitivity.
- `articles/results.html`: estimates and compatibility intervals,
  population-level and group predictions, and population-level estimates for
  site-years without data.
- `articles/pre-fit-models.html`: the pre-fit models and their reference data.
- `articles/models.html`: the sub-models, including density in the weight model.
- `articles/total-biomass.html`: canopy area, percent cover and the cover model
  that scales biomass per unit area up to total biomass.
- `articles/priors.html`: the default priors and when to change them.
