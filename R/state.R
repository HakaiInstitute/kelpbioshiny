# App state and actions.
#
# One store per session. Steps and modules read its reactive values and call its
# actions; nothing else mutates state.

TICK_MS <- 200

# Fit records -------------------------------------------------------------------
# Each model's fit to your data is a record: its status ("idle", "queued",
# "fitting", "fitted" or "failed"), the kelpbio fit once fitted, the error
# message once failed, and the settings it was fitted with once it starts. The
# transitions below are pure, so they can be tested without a session.

new_record <- function(status = "idle", fit = NULL, error = NULL, settings = NULL) {
  list(status = status, fit = fit, error = error, settings = settings)
}

# A model's prior and sampler settings, as recorded with its fit.
fit_settings <- function(priors, sampler) list(priors = priors, sampler = sampler)

# Whether the settings now differ from those a fit used. Values are compared as
# shown, so a setting edited and then put back is unchanged.
settings_changed <- function(fitted, current) {
  shown <- function(settings) {
    c(paste(settings$priors$name, format_prior(settings$priors)), vapply(settings$sampler, format, ""))
  }
  !identical(shown(fitted), shown(current))
}

idle_records <- function() lapply(component_ids, function(id) new_record())

record_status <- function(records) vapply(records, `[[`, "", "status")

# A change to a model in survey biomass makes the cover calibration fit, which
# is fitted to the survey biomass, out of date. A queued cover fit stays queued:
# it runs after the models it needs.
invalidate_cover <- function(records, ids) {
  if (any(ids %in% survey_ids) && records$cover$status %in% c("fitting", "fitted", "failed")) {
    records$cover <- new_record()
  }
  records
}

queue_records <- function(records, ids) {
  records <- invalidate_cover(records, ids)
  records[ids] <- list(new_record("queued"))
  records
}

reset_records <- function(records, ids) {
  records <- invalidate_cover(records, ids)
  records[ids] <- list(new_record())
  records
}

# The fitted and running models a reset of `ids` discards: the ids and, for a
# model in survey biomass, the cover calibration.
reset_ids <- function(records, ids) {
  affected <- unique(c(ids, if (any(ids %in% survey_ids)) "cover"))
  affected[record_status(records[affected]) %in% c("fitted", "fitting")]
}

# Whether any model's priors differ from the defaults for `species`.
priors_edited <- function(priors, species) {
  any(vapply(component_ids, function(id) {
    any(format_prior(priors[[id]]) != format_prior(default_priors(id, species)))
  }, logical(1)))
}

cancel_records <- function(records) {
  pending <- names(records)[record_status(records) %in% c("queued", "fitting")]
  records[pending] <- list(new_record())
  records
}

# Cancels one model's queued or running fit, and a queued cover fit, which waits
# on the models in survey biomass.
cancel_record <- function(records, id) {
  ids <- c(id, if (id %in% survey_ids) "cover")
  pending <- ids[record_status(records[ids]) %in% c("queued", "fitting")]
  records[pending] <- list(new_record())
  records
}

start_record <- function(records, id, settings = NULL) {
  records[[id]] <- new_record("fitting", settings = settings)
  records
}

# A result applies only while its record is still fitting: a fit that was
# cancelled or reset in the meantime is discarded.
finish_record <- function(records, id, fit) {
  if (records[[id]]$status == "fitting") records[[id]] <- new_record("fitted", fit = fit, settings = records[[id]]$settings)
  records
}

fail_record <- function(records, id, message) {
  if (records[[id]]$status %in% c("queued", "fitting")) records[[id]] <- new_record("failed", error = message)
  records
}

# Fits run one at a time in model order, so the cover fit comes last.
next_queued <- function(records) {
  queued <- names(records)[record_status(records) == "queued"]
  if (length(queued) > 0) queued[[1]]
}

fitting_id <- function(records) {
  fitting <- names(records)[record_status(records) == "fitting"]
  if (length(fitting) > 0) fitting[[1]]
}

# Statuses ----------------------------------------------------------------------
# What the app shows for each model, from its source, sheet, fit record and, for
# a pre-fit source, the pre-fit model (list(fit, error)). Kinds: "not-used",
# "no-data", "data-error", "not-fitted", "queued", "fitting", "ready" (pre-fit,
# a census whose size sheet passes its check, or fitted) and "failed". A fitted
# model carries two warnings: `convergence` when parameters did not converge
# and `divergence` when the sampler had too many divergent transitions (both
# from fit_convergence()); two notes: `prior` (prior_warning, from its prior
# sensitivity) when a prior is influencing an estimate, and `influence`
# (influence_warning, from kb_influence()) when observations strongly
# influence the fit; and `outdated` when its current `settings` differ from
# those it was fitted with.
component_status <- function(source, sheet, record, prefit = NULL, cover_blocked = FALSE, prior_warning = FALSE,
                             influence_warning = FALSE, settings = NULL) {
  if (source == "none") {
    return(list(kind = "not-used", source = source))
  }
  if (is_prefit(source)) {
    if (!is.null(prefit$error)) {
      return(list(kind = "failed", source = source, message = prefit$error))
    }
    return(list(kind = "ready", source = source))
  }
  if (is.null(sheet)) {
    return(list(kind = "no-data", source = source))
  }
  if (!is.null(sheet$error)) {
    return(list(kind = "data-error", source = source, message = sheet$error))
  }
  if (source == "census") {
    return(list(kind = "ready", source = source))
  }
  if (record$status == "fitted") {
    convergence <- fit_convergence(record$fit)
  }
  switch(record$status,
    queued = list(kind = "queued", source = source),
    fitting = list(kind = "fitting", source = source),
    fitted = list(
      kind = "ready", source = source, convergence = convergence$convergence,
      divergence = convergence$divergence, prior = prior_warning,
      influence = influence_warning,
      outdated = !is.null(record$settings) && !is.null(settings) && settings_changed(record$settings, settings)
    ),
    failed = list(kind = "failed", source = source, message = record$error),
    list(kind = "not-fitted", source = source, blocked = cover_blocked)
  )
}

is_ready_status <- function(status) status$kind %in% c("ready", "not-used")
is_pending_status <- function(status) status$kind %in% c("queued", "fitting")
# Only a fit that did not converge has a warning: prior sensitivity and
# influential observations are notes to check a valid fit.
has_warning <- function(status) isTRUE(status$convergence) || isTRUE(status$divergence)

# Why a fit did not converge, by kelpbio's kb_converged() verdict:
# `divergence` when its divergent transitions exceed kelpbio's limit, which a
# higher adapt_delta fixes, and `convergence` when it fails for another reason,
# such as a parameter whose R-hat or ESS is flagged, which more thinning fixes.
fit_convergence <- function(fit) {
  fit_summary <- summary(fit)
  divergence <- fit_summary$perc_divergent > default_arg(kb_converged, "max_perc_divergent")
  unconverged <- !all(fit_summary$coefficients$converged)
  list(
    convergence = !fit_summary$converged && (unconverged || !divergence),
    divergence = !fit_summary$converged && divergence
  )
}

# The parameters kb_sensitivity() rows flag: `influencing`, whose prior is not
# weak, and `weak_data`, whose prior is weak and data not strong. A prior
# kelpbio marks as informative by design (informative_prior), such as the cover
# calibration's tide height slope, is flagged by neither.
sensitivity_flags <- function(rows) {
  checked <- !rows$informative_prior
  list(
    influencing = rows$term[checked & !rows$weak_prior],
    weak_data = rows$term[checked & rows$weak_prior & !rows$strong_data]
  )
}

# kb_sensitivity() rows flag a prior warning when a prior is influencing an
# estimate.
prior_flagged <- function(rows) !is.null(rows) && length(sensitivity_flags(rows)$influencing) > 0

# kb_influence() rows flag an influence warning when any observation is influential.
influence_flagged <- function(rows) !is.null(rows) && any(rows$influential)

# Survey biomass needs the weight and size models, each in use.
survey_possible <- function(sources) all(sources[survey_ids] != "none")

# The models fitted to their own sheets first; the cover calibration can be
# fitted once survey biomass can be predicted, and its data are the surveys with
# a drone flight (none counts as no data). prior_warnings flags, by model, a fit
# whose priors influence its estimates, influence_warnings a fit with
# influential observations, and settings each model's current settings.
component_statuses <- function(sources, sheets, records, prefits = list(), prior_warnings = list(),
                               influence_warnings = list(), settings = list()) {
  status <- function(id, sheet = sheets[[components[[id]]$sheet]], ...) {
    component_status(
      sources[[id]], sheet, records[[id]], ...,
      prior_warning = isTRUE(prior_warnings[[id]]), influence_warning = isTRUE(influence_warnings[[id]]),
      settings = settings[[id]]
    )
  }
  ids <- setdiff(component_ids, "cover")
  statuses <- lapply(stats::setNames(ids, ids), function(id) status(id, prefit = prefits[[id]]))
  ready <- all(vapply(statuses[survey_ids], is_ready_status, logical(1))) && survey_possible(sources)
  survey <- sheets$survey
  flown <- if (!is.null(survey) && is.null(survey$error) && nrow(drone_surveys(survey$rows)) == 0) NULL else survey
  c(statuses, list(cover = status("cover", flown, prefit = prefits$cover, cover_blocked = !ready)))
}

# A model fitted to your data can be fitted now: it has checked data and, for the
# cover calibration, the models in survey biomass are ready.
can_fit <- function(status) {
  status$source == "user" && (status$kind %in% c("failed", "ready") || (status$kind == "not-fitted" && !status$blocked))
}

# The models Fit all queues: those not yet fitted, failed, or fitted with
# settings since changed, less those with an invalid setting (`skipped`). The
# cover calibration joins when the weight and size models are in use and each
# is ready, already pending, or in the same batch.
fit_all_plan <- function(statuses, invalid = character()) {
  candidates <- Filter(function(id) {
    status <- statuses[[id]]
    status$source == "user" && (status$kind %in% c("not-fitted", "failed") || isTRUE(status$outdated))
  }, component_ids)
  skipped <- intersect(candidates, invalid)
  ids <- setdiff(candidates, invalid)
  if ("cover" %in% ids) {
    on_track <- function(id) {
      statuses[[id]]$source != "none" && (is_ready_status(statuses[[id]]) || is_pending_status(statuses[[id]]) || id %in% ids)
    }
    if (!all(vapply(survey_ids, on_track, logical(1)))) ids <- setdiff(ids, "cover")
  }
  list(ids = unname(ids), skipped = unname(skipped))
}

# Whether an estimate on the Estimates step can be shown, with the reason when
# not: a model's predictions need the model ready (fitted to your data or
# pre-fit); survey biomass needs the weight and size models ready and a survey
# sheet; canopy biomass needs a fitted cover calibration and a canopy sheet.
estimate_availability <- function(id, statuses, sources, sheets, mismatches) {
  unavailable <- function(reason) list(available = FALSE, reason = reason)
  if (id %in% component_ids) {
    status <- statuses[[id]]
    model <- lower_label(id)
    if (status$source == "census" && status$kind == "ready") {
      return(unavailable("The size sheet is a census of every counted plant, so no size model is fitted: survey biomass weighs each plant."))
    }
    return(switch(status$kind,
      "ready" = list(available = TRUE),
      "not-used" = unavailable(sprintf("The %s model is not used in this run.", model)),
      "no-data" = unavailable(if (id == "cover") {
        "The cover calibration needs surveys with a drone flight: add plot_area_m2, canopy_area_m2 and tide_height_m to the survey sheet."
      } else {
        sprintf("The %s model has no data. Add a %s sheet on the Data step.", model, components[[id]]$sheet)
      }),
      "data-error" = unavailable(sprintf("The %s sheet failed its data check.", components[[id]]$sheet)),
      "queued" = ,
      "fitting" = unavailable(sprintf("The %s model is fitting.", model)),
      "failed" = unavailable(sprintf("The %s model failed to fit.", model)),
      unavailable(sprintf("The %s model is not fitted yet.", model))
    ))
  }
  if (length(mismatches) > 0) {
    return(unavailable(sprintf("Site names differ across sheets: %s. %s", and_list(sprintf("\"%s\"", mismatches)), mismatch_advice)))
  }
  if (id == "canopy") {
    canopy <- sheets$canopy
    cover <- statuses$cover
    reason <- if (is.null(canopy)) {
      "Canopy biomass needs a canopy sheet of drone maps of whole sites. Add one on the Data step."
    } else if (!is.null(canopy$error)) {
      "The canopy sheet failed its data check."
    } else if (cover$kind == "not-used") {
      "Canopy biomass needs the cover calibration. Choose a source for it on the Models step."
    } else if (cover$kind != "ready") {
      "Canopy biomass is available once the cover calibration is fitted."
    }
    return(if (is.null(reason)) list(available = TRUE) else unavailable(reason))
  }
  if (!survey_possible(sources)) {
    return(unavailable(sprintf("%s needs the weight and size models, each fitted to your data or pre-fit.", estimate_label(id))))
  }
  if (is.null(sheets$survey)) {
    return(unavailable("Survey biomass needs a survey sheet. Add one on the Data step."))
  }
  if (!is.null(sheets$survey$error)) {
    return(unavailable("The survey sheet failed its data check."))
  }
  blocking <- survey_ids[!vapply(statuses[survey_ids], is_ready_status, logical(1))]
  if (length(blocking) > 0) {
    return(unavailable(sprintf(
      "Every model in use needs to be ready first. %s.",
      paste(sprintf("%s: %s", vapply(blocking, label_of, ""), vapply(statuses[blocking], block_reason, "")), collapse = "; ")
    )))
  }
  list(available = TRUE)
}

# What the loaded data can estimate once the models are fitted, for the Data
# step: each biomass estimate, list(possible, reason, prefit), `prefit` naming
# the pre-fit models a possible estimate relies on (the conversion models for
# dry biomass and carbon). A model can be used when it is pre-fit, or its sheet
# passes its check.
data_estimable <- function(sheets, sources, census, mismatches = character()) {
  valid <- function(id) !is.null(sheets[[id]]) && is.null(sheets[[id]]$error)
  usable <- function(id) is_prefit(sources[[id]]) || (sources[[id]] %in% c("user", "census") && valid(components[[id]]$sheet))
  result <- function(reason, ids) {
    if (!is.null(reason)) {
      return(list(possible = FALSE, reason = reason))
    }
    list(possible = TRUE, prefit = Filter(function(id) is_prefit(sources[[id]]), ids))
  }
  survey <- if (!valid("survey")) {
    if (is.null(sheets$survey)) "needs a survey sheet" else "the survey sheet fails its check"
  } else if (!usable("weight")) {
    "needs a weight sheet that passes its check, or a pre-fit weight model"
  } else if (!usable("size")) {
    if (census) "needs a size sheet of every counted plant" else "needs a size sheet that passes its check, or a pre-fit size model"
  } else if (length(mismatches) > 0) {
    "site names differ across sheets"
  }
  # A pre-fit cover calibration needs only the canopy sheet; one fitted to your
  # data needs survey biomass and drone flights on the surveys.
  canopy <- if (!valid("canopy")) {
    if (is.null(sheets$canopy)) "needs a canopy sheet of drone maps" else "the canopy sheet fails its check"
  } else if (is_prefit(sources[["cover"]])) {
    NULL
  } else if (!is.null(survey)) {
    "needs survey biomass"
  } else if (sources[["cover"]] == "none" || nrow(drone_surveys(sheets$survey$rows)) == 0) {
    "needs drone flights on the surveys"
  }
  dry <- if (!usable("wetdry")) "needs a wetdry sheet or a pre-fit wet:dry model"
  carbon <- dry %||% if (!usable("carbon")) "needs a carbon sheet or a pre-fit carbon model"
  list(
    survey = result(survey, survey_ids), canopy = result(canopy, if (is_prefit(sources[["cover"]])) "cover" else survey_ids),
    dry = result(dry, "wetdry"), carbon = result(carbon, c("wetdry", "carbon"))
  )
}

output_availability <- function(sources) {
  available <- function() list(available = TRUE)
  unavailable <- function(reason) list(available = FALSE, reason = reason)
  list(
    wet = available(),
    dry = if (sources[["wetdry"]] == "none") {
      unavailable("Dry biomass unavailable: no wet:dry model")
    } else {
      available()
    },
    carbon = if (sources[["carbon"]] == "none") {
      unavailable("Carbon unavailable: no carbon model")
    } else if (sources[["wetdry"]] == "none") {
      unavailable("Carbon unavailable: carbon needs the wet:dry model")
    } else {
      available()
    }
  )
}

# The kb_fit_*() call for a model fitted to your data, as list(fn, args). The
# cover calibration is fitted to the survey biomass from the weight and size
# models in use (`fits`) and the surveys (fit_cover_calibration()).
fit_call <- function(id, species, sheets, priors, sampler, progress_dir, fits = list(), census = FALSE) {
  settings <- list(
    priors = prior_list(priors), chains = fit_draws$chains, niters = fit_draws$niters, nthin = sampler$nthin,
    control = list(adapt_delta = sampler$adapt_delta), progress = "none", progress_dir = progress_dir
  )
  if (id == "cover") {
    return(list(fn = fit_cover_calibration, args = c(list(species = species), survey_biomass_args(fits$weight, fits$size, sheets, census), settings)))
  }
  list(fn = kelpbio_fn("fit", id, species), args = c(list(data = sheets[[components[[id]]$sheet]]$rows), settings))
}

# Fit runners -------------------------------------------------------------------
# A runner runs one kb_fit_*() call at a time in the background: invoke(fn, args)
# starts it, status() is "initial", "running", "success" or "error", result() is
# list(fit) or list(error), and cancel() stops it. The default runs each fit on a
# mirai daemon through an ExtendedTask. Tests pass a runner that runs in the
# session.

# The daemons start with the first fit, so a session that never fits costs no
# daemon. They are per R process and shared by its sessions; each session fits
# one model at a time, so one daemon serves one user without waiting. The
# option kelpbioshiny.daemons sets their number for a deployment.
start_daemons <- function() {
  if (mirai::daemons_set()) {
    return(invisible())
  }
  n <- getOption("kelpbioshiny.daemons", 1L)
  mirai::daemons(n)
  # A fit's function is sent as a reference to its namespace, which a daemon
  # loads: from source when the app was loaded with load_all() (as app.R does),
  # so the daemons run the same code as the app; otherwise installed.
  source_path <- if (requireNamespace("pkgload", quietly = TRUE) && pkgload::is_dev_package("kelpbioshiny")) {
    pkgload::pkg_path(system.file(package = "kelpbioshiny"))
  }
  mirai::everywhere(
    if (is.null(source_path)) loadNamespace("kelpbioshiny") else pkgload::load_all(source_path, quiet = TRUE),
    source_path = source_path,
    .min = n
  )
  invisible()
}

mirai_fit_runner <- function() {
  current <- NULL
  task <- ExtendedTask$new(function(fn, args) {
    start_daemons()
    current <<- mirai::mirai(
      tryCatch(list(fit = do.call(fn, args)), error = function(e) list(error = condition_text(e))),
      .args = list(fn = fn, args = args, condition_text = condition_text)
    )
    current
  })
  list(
    invoke = function(fn, args) task$invoke(fn, args),
    status = function() task$status(),
    result = function() task$result(),
    cancel = function() if (!is.null(current)) mirai::stop_mirai(current)
  )
}

# The store ---------------------------------------------------------------------
# A reactiveVal only invalidates when its value changes, so routing a reactive
# through one stops downstream outputs re-rendering on unrelated state changes.
dedupe <- function(r) {
  value <- reactiveVal()
  observe(value(r()), priority = 100)
  value
}


new_store <- function(session, run_fit = mirai_fit_runner) {
  s <- new.env()
  runner <- run_fit()

  s$session <- session
  s$species <- reactiveVal("nereo")
  s$workbook <- reactiveVal(NULL)
  s$sheets <- reactiveVal(list())
  s$sources <- reactiveVal(default_sources(list()))
  s$priors <- reactiveVal(lapply(component_ids, default_priors, species = "nereo"))
  s$samplers <- reactiveVal(lapply(component_ids, function(id) default_sampler()))
  s$records <- reactiveVal(idle_records())
  # The kind of Nereocystis size data (size_data_info), and whether the run is a
  # census, so the size sheet holds every counted plant and no size model is
  # fitted.
  s$size_data <- reactiveVal("sample")
  s$census <- reactive(is_census(s$species(), s$size_data()))
  # The completed share of the running fit, in percent.
  s$progress <- reactiveVal(0)
  s$open <- reactiveVal("hub")
  s$tab_request <- reactiveVal(NULL)

  # Pre-fit models, loaded once per model, species and reference; a load that
  # fails gives its error message.
  prefits <- new.env()
  s$prefit <- function(id) {
    source <- s$sources()[[id]]
    key <- paste(id, s$species(), source)
    if (is.null(prefits[[key]])) {
      prefits[[key]] <- tryCatch(
        list(fit = prefit_fit(id, s$species(), prefit_reference(source))),
        error = function(e) list(error = condition_text(e))
      )
    }
    prefits[[key]]
  }

  # Derived state --------------------------------------------------------------

  s$statuses <- reactive({
    sources <- s$sources()
    prefit_ids <- Filter(function(id) is_prefit(sources[[id]]), component_ids)
    prior_warnings <- lapply(component_ids, function(id) prior_flagged(s$sensitivity[[id]]()))
    influence_warnings <- lapply(component_ids, function(id) influence_flagged(s$influence[[id]]()))
    component_statuses(
      sources, s$sheets(), s$records(), lapply(prefit_ids, s$prefit), prior_warnings, influence_warnings,
      s$settings()
    )
  })
  s$fitting <- reactive(fitting_id(s$records()))

  # Each model's current prior and sampler settings.
  s$settings <- reactive({
    priors <- s$priors()
    samplers <- s$samplers()
    lapply(component_ids, function(id) fit_settings(priors[[id]], samplers[[id]]))
  })
  # The settings behind each model's results: those it was fitted with, or the
  # current settings while it has no fit.
  s$run_settings <- reactive({
    records <- s$records()
    current <- s$settings()
    lapply(component_ids, function(id) if (records[[id]]$status == "fitted") records[[id]]$settings else current[[id]])
  })

  # The fit a model contributes: its pre-fit model, or its fit to your data once
  # fitted; NULL otherwise.
  s$fit_of <- function(id) {
    source <- s$sources()[[id]]
    if (is_prefit(source)) {
      return(s$prefit(id)$fit)
    }
    if (source == "user") s$records()[[id]]$fit
  }

  # Each kelpbio result below is a reactive on the fits it uses, routed through
  # dedupe(), so it is computed once per change of those fits and shared by
  # every output that shows it.

  # Prior sensitivity of each model's fit to your data; NULL until fitted. The
  # statuses read it, so it is computed as soon as a model is fitted.
  s$sensitivity <- lapply(component_ids, function(id) {
    fit <- dedupe(reactive(s$records()[[id]]$fit))
    reactive(if (!is.null(fit())) kb_sensitivity(fit()))
  })
  # Influential observations of each model's fit to your data; NULL until fitted.
  s$influence <- lapply(component_ids, function(id) {
    fit <- dedupe(reactive(s$records()[[id]]$fit))
    reactive(if (!is.null(fit())) kb_influence(fit()))
  })

  # The derived estimates, from the fits in use and the sheets, for the
  # Estimates step: the biomass of each survey, and biomass over the canopy
  # mapped in each drone map of a site. Each is
  # called from a reactive, so it reruns when its inputs change.
  fits <- dedupe(reactive(lapply(component_ids, s$fit_of)))
  survey_inputs <- dedupe(reactive({
    f <- fits()
    c(survey_biomass_args(f$weight, f$size, s$sheets(), s$census()), list(wetdry = f$wetdry, carbon = f$carbon))
  }))
  canopy_inputs <- dedupe(reactive({
    f <- fits()
    list(fit = f$cover, canopy = s$sheets()$canopy$rows, wetdry = f$wetdry, carbon = f$carbon)
  }))
  # The sites chosen to stand in for sites a model has no data for, by model
  # (stand_in_sites()), and those of them that are sites of the fits in use.
  s$stand_ins_chosen <- reactiveVal(list())
  s$stand_ins <- dedupe(reactive(stand_in_sites(s$stand_ins_chosen(), fits())))
  s$set_stand_ins <- function(id, sites) {
    chosen <- isolate(s$stand_ins_chosen())
    if (identical(chosen[[id]], sites)) {
      return()
    }
    chosen[[id]] <- sites
    s$stand_ins_chosen(chosen)
  }
  # kelpbio's representative_site for a derived estimate: a list by fit for
  # survey biomass, the cover calibration's sites for canopy biomass, or NULL.
  s$representative_site <- function(eid) {
    sites <- s$stand_ins()[intersect(stand_in_ids[[eid]], names(s$stand_ins()))]
    if (length(sites) == 0) {
      return(NULL)
    }
    if (eid == "canopy") sites$cover else sites
  }

  s$survey_biomass <- function(measure, per_area, pool_by) {
    a <- survey_inputs()
    kb_predict_biomass_survey(
      a$weight, a$size, a$survey, a$wetdry, a$carbon,
      measure = measure, per_area = per_area, pool_by = pool_by,
      representative_site = s$representative_site("survey"), progress = "none"
    )
  }
  s$canopy_biomass <- function(measure, pool_by) {
    a <- canopy_inputs()
    kb_predict_biomass_canopy(
      a$fit, a$canopy, a$wetdry, a$carbon,
      measure = measure, pool_by = pool_by, representative_site = s$representative_site("canopy")
    )
  }

  # Invalid prior and sampler settings, by model: list(priors, sampler), each a
  # named character vector of error messages.
  s$setting_errors <- reactive({
    priors <- s$priors()
    samplers <- s$samplers()
    lapply(component_ids, function(id) list(priors = prior_errors(priors[[id]]), sampler = sampler_errors(samplers[[id]])))
  })
  s$invalid <- reactive({
    errors <- s$setting_errors()
    component_ids[vapply(errors, function(e) length(e$priors) + length(e$sampler) > 0, logical(1))]
  })
  s$fit_plan <- reactive(fit_all_plan(s$statuses(), s$invalid()))

  s$has_data <- reactive(length(s$sheets()) > 0)
  s$mismatches <- reactive(site_mismatches(s$sheets()))
  # Survey biomass can be predicted, so the cover calibration can be fitted.
  s$survey_ready <- reactive(isTRUE(s$estimates()$survey$available))
  s$outputs <- reactive(output_availability(s$sources()))
  # Each estimate on the Estimates step: list(available, reason).
  s$estimates <- reactive({
    lapply(estimate_ids, estimate_availability,
      statuses = s$statuses(), sources = s$sources(), sheets = s$sheets(), mismatches = s$mismatches()
    )
  })
  # A run has an estimate of its own to save once it has a biomass estimate or
  # a model fitted to its data; pre-fit models alone do not count.
  s$own_estimates <- reactive({
    estimates <- s$estimates()
    statuses <- s$statuses()
    own <- Filter(function(id) statuses[[id]]$source == "user", component_ids)
    ids <- c(derived_ids, own)
    ids[vapply(estimates[ids], `[[`, logical(1), "available")]
  })
  s$any_estimate <- reactive(s$has_data() && length(s$own_estimates()) > 0)

  # The models and estimates this run shows (no size model in a census).
  s$shown_models <- reactive(shown_models(s$census()))
  s$shown_estimates <- reactive(shown_estimates(s$census()))

  # Actions ----------------------------------------------------------------------

  # An error is announced at once (role alert); other notices wait for the
  # screen reader to finish (role status).
  s$notify <- function(text, type = "message", title = NULL, duration = 4) {
    ui <- div(role = if (type == "error") "alert" else "status", if (is.null(title)) text else tagList(div(class = "fw-semibold", title), div(text)))
    showNotification(ui, type = type, duration = duration, session = session)
  }

  # Runs `action`, a change that resets the fits of `ids` and, with
  # `resets_priors` (a species change), every model's priors, after asking first
  # when it would discard a fit or an edited prior. Cancel runs `cancel`, which
  # puts the input that asked for the change back.
  pending_reset <- NULL
  s$confirm_reset <- function(ids, action, cancel = function() NULL, resets_priors = FALSE) {
    fitted <- reset_ids(isolate(s$records()), ids)
    edited <- resets_priors && priors_edited(isolate(s$priors()), isolate(s$species()))
    if (length(fitted) == 0 && !edited) {
      return(action())
    }
    pending_reset <<- list(action = action, cancel = cancel)
    showModal(
      modalDialog(
        title = span(id = "reset_title", if (length(fitted) > 0) "Discard fitted models?" else "Reset priors?"),
        div(
          class = "d-flex flex-column gap-2",
          if (length(fitted) > 0) {
            div(sprintf(
              "This discards the %s of %s. %s to be fitted again.",
              if (length(fitted) == 1) "fit" else "fits", and_list(vapply(fitted, label_of, "")),
              if (length(fitted) == 1) "It needs" else "They need"
            ))
          },
          if (edited) div("Edited priors return to their defaults for the new species.")
        ),
        footer = tagList(
          button("reset_cancel", "Cancel", variant = "outline"),
          button("reset_continue", if (length(fitted) > 0) "Discard fits" else "Reset priors")
        ),
        size = "s"
      ) |>
        tagAppendAttributes(`aria-labelledby` = "reset_title"),
      session = session
    )
  }
  resolve_reset <- function(choice) {
    removeModal(session)
    pending <- pending_reset
    pending_reset <<- NULL
    if (!is.null(pending)) pending[[choice]]()
  }
  observeEvent(session$input$reset_continue, resolve_reset("action"))
  observeEvent(session$input$reset_cancel, resolve_reset("cancel"))

  # The fit that is running: its model and progress directory.
  running <- NULL
  stop_running <- function(cancel = FALSE) {
    if (!is.null(running)) {
      if (cancel) runner$cancel()
      unlink(running$dir, recursive = TRUE)
    }
    running <<- NULL
    s$progress(0)
  }

  # Every change of fit records goes through here, so a reset of the running
  # model also stops its fit.
  set_records <- function(records) {
    s$records(records)
    if (!is.null(running) && records[[running$id]]$status != "fitting") stop_running(cancel = TRUE)
  }
  update_records <- function(transition, ...) set_records(transition(isolate(s$records()), ...))

  s$reset_fit <- function(id) update_records(reset_records, id)

  # New data start a new run: fits are discarded and the Estimates step opens
  # on its default estimate again.
  apply_sheets <- function(sheets, file) {
    update_records(function(records) idle_records())
    s$estimate(NULL)
    s$stand_ins_chosen(list())
    s$sheets(sheets)
    s$workbook(file)
    census <- isolate(s$census())
    species <- isolate(s$species())
    s$sources(valid_sources(default_sources(sheets, census, species), census, sheets, species))
  }

  # Every sheet of `rows` from the one file, checked.
  checked_sheets <- function(rows, file) {
    check_sheets(rows, lapply(rows, function(x) file), isolate(s$species()), isolate(s$census()))
  }

  # The sheets checked again, after a change of species or kind of size data.
  recheck_sheets <- function() {
    sheets <- isolate(s$sheets())
    sheets <- check_sheets(lapply(sheets, `[[`, "rows"), lapply(sheets, `[[`, "file"), isolate(s$species()), isolate(s$census()))
    s$sheets(sheets)
    s$sources(valid_sources(isolate(s$sources()), isolate(s$census()), sheets, isolate(s$species())))
  }

  # The example workbook of the species (example_rows()), with every sheet.
  s$load_example <- function() {
    s$size_data("sample")
    species <- isolate(s$species())
    file <- sprintf("example-%s.xlsx", species)
    apply_sheets(checked_sheets(example_rows(species), file), file)
  }

  # A workbook that cannot be read, or has no sheet named after a model, leaves
  # the data as they were.
  s$load_workbook <- function(file, path) {
    rows <- tryCatch(read_workbook(path), error = function(e) e)
    if (inherits(rows, "error")) {
      s$notify(condition_text(rows), type = "error", title = sprintf("%s could not be read", file), duration = 10)
      return()
    }
    if (length(rows) == 0) {
      s$notify(
        sprintf("Name each sheet after its data: %s.", and_list(unname(sheet_ids))),
        type = "error", title = sprintf("%s has no sheet kelpbio reads", file), duration = 10
      )
      return()
    }
    apply_sheets(checked_sheets(rows, file), file)
  }

  # One sheet from a CSV. The sheets are checked again together, as the survey
  # sheet of a census is checked with the size sheet. The model fitted to the
  # sheet, if any, is reset and set to Your data.
  s$load_csv <- function(id, file, path) {
    rows <- tryCatch(read_csv_rows(path), error = function(e) e)
    if (inherits(rows, "error")) {
      s$notify(condition_text(rows), type = "error", title = sprintf("%s could not be read", file), duration = 10)
      return()
    }
    cid <- sheet_component(id)
    if (!is.null(cid)) s$reset_fit(cid)
    sheets <- isolate(s$sheets())
    all_rows <- lapply(sheets, `[[`, "rows")
    files <- lapply(sheets, `[[`, "file")
    all_rows[[id]] <- rows
    files[[id]] <- file
    sheets <- check_sheets(all_rows, files, isolate(s$species()), isolate(s$census()))
    s$sheets(sheets)
    sources <- isolate(s$sources())
    if (!is.null(cid) && "user" %in% components[[cid]]$sources && model_has_data(cid, sheets)) sources[[cid]] <- "user"
    s$sources(valid_sources(sources, isolate(s$census()), sheets, isolate(s$species())))
  }

  s$clear_data <- function() apply_sheets(list(), NULL)

  # Restores a run from a fit bundle (run_bundle()): its data, sources,
  # settings, fits and stand-in sites. A file that is not a fit bundle leaves
  # the run as it was.
  s$resume <- function(file, path) {
    bundle <- tryCatch(read_bundle(path), error = function(e) e)
    if (inherits(bundle, "error")) {
      s$notify(condition_text(bundle), type = "error", title = sprintf("%s could not be resumed", file), duration = 10)
      return()
    }
    s$species(bundle$species)
    s$size_data(bundle$size_data)
    census <- isolate(s$census())
    sheets <- check_sheets(lapply(bundle$sheets, `[[`, "rows"), lapply(bundle$sheets, `[[`, "file"), bundle$species, census)
    s$sheets(sheets)
    s$workbook(bundle$workbook)
    s$sources(valid_sources(bundle$sources, census, sheets, bundle$species))
    s$priors(bundle$priors)
    s$samplers(bundle$samplers)
    records <- idle_records()
    for (id in names(bundle$fits)) records[[id]] <- new_record("fitted", fit = bundle$fits[[id]]$fit, settings = bundle$fits[[id]]$settings)
    update_records(function(old) records)
    s$stand_ins_chosen(bundle$stand_ins)
    s$estimate_choices(bundle$estimate_choices)
    s$estimate(NULL)
    s$notify(sprintf("Resumed the run saved in %s.", file))
  }

  # The data checks depend on the species, so a species change checks the sheets again.
  s$set_species <- function(value) {
    if (identical(value, isolate(s$species()))) {
      return()
    }
    s$species(value)
    s$priors(lapply(component_ids, default_priors, species = value))
    update_records(function(records) idle_records())
    s$estimate(NULL)
    recheck_sheets()
  }

  # A change of the kind of size data changes what the size sheet holds, so
  # the size fit (and the cover calibration on it) is discarded and the sheets
  # are checked again.
  s$set_size_data <- function(value) {
    if (identical(value, isolate(s$size_data()))) {
      return()
    }
    update_records(reset_records, "size")
    s$size_data(value)
    recheck_sheets()
  }

  s$set_source <- function(id, value) {
    sources <- isolate(s$sources())
    if (identical(sources[[id]], value)) {
      return()
    }
    s$reset_fit(id)
    sources[[id]] <- value
    s$sources(sources)
  }

  # Edits are stored as typed, invalid ones included, so the editor can show
  # kelpbio's message for them; an invalid setting blocks fitting.
  s$update_prior <- function(id, name, field, value) {
    priors <- isolate(s$priors())
    row <- priors[[id]]$name == name
    if (is.null(value) || identical(priors[[id]][row, field], value)) {
      return()
    }
    priors[[id]][row, field] <- value
    s$priors(priors)
  }

  s$reset_priors <- function(id) {
    priors <- isolate(s$priors())
    priors[[id]] <- default_priors(id, isolate(s$species()))
    s$priors(priors)
  }

  s$update_sampler <- function(id, field, value) {
    samplers <- isolate(s$samplers())
    if (is.null(value) || identical(samplers[[id]][[field]], value)) {
      return()
    }
    samplers[[id]][[field]] <- value
    s$samplers(samplers)
  }

  s$queue_fits <- function(ids) {
    ids <- setdiff(ids, isolate(s$invalid()))
    if (length(ids) > 0) update_records(queue_records, ids)
  }

  s$fit_all <- function() s$queue_fits(isolate(s$fit_plan())$ids)

  s$cancel_fits <- function() {
    update_records(cancel_records)
    s$notify("Fits cancelled.")
  }

  s$cancel_fit <- function(id) update_records(cancel_record, id)

  # Navigation: the Models step shows the hub unless a model page was asked for.
  pending_model <- NULL
  s$go_to <- function(step, model = NULL) {
    if (step == "models") pending_model <<- model %||% "hub"
    if (identical(isolate(session$input$step), step)) {
      if (step == "models") s$open(pending_model)
      return()
    }
    nav_select("step", step, session = session)
  }

  # The estimate the Estimates step shows (an estimate_ids value), or NULL for
  # the first one available; reset when the data are replaced or the species
  # changes.
  s$estimate <- reactiveVal(NULL)
  s$open_estimate <- function(id) {
    s$estimate(id)
    s$go_to("estimates")
  }

  # The choices made on each estimate's page, by estimate id: a model's
  # `grouping`, a derived estimate's `scale` and `sum`. The Export step's
  # downloads use them; a choice no longer offered falls back to the default.
  s$estimate_choices <- reactiveVal(list())
  s$set_estimate_choice <- function(id, name, value) {
    choices <- isolate(s$estimate_choices())
    if (identical(choices[[id]][[name]], value)) {
      return()
    }
    choices[[id]][[name]] <- value
    s$estimate_choices(choices)
  }

  # Opens a sheet's panel on the Data step, with its figure and table.
  s$view_sheet <- function(id) {
    s$go_to("data")
    accordion_panel_open("data-sheet_list", id, session = session)
  }

  # Opens one page of the Help tab: "guide" or "about".
  s$open_help <- function(page) {
    s$go_to("help")
    nav_select("help_page", page, session = session)
  }

  # Opens one tab of a model's detail page, e.g. its diagnostics from a warning.
  tab_requests <- 0
  s$open_tab <- function(id, tab) {
    tab_requests <<- tab_requests + 1
    s$go_to("models", id)
    s$tab_request(list(id = id, tab = tab, n = tab_requests))
  }

  observeEvent(session$input$step, {
    if (session$input$step == "models") {
      s$open(pending_model %||% "hub")
      pending_model <<- NULL
    }
  })

  # Whether the current results have been exported (a download, or the R script
  # copied), for the navbar marker. A change of fits or sources changes the
  # results, so it resets.
  s$exported <- reactiveVal(FALSE)
  observeEvent(list(s$records(), s$sources()), s$exported(FALSE), ignoreInit = TRUE)

  # Fit queue ------------------------------------------------------------------
  # Fits run one at a time in the background, so the session stays responsive
  # and fits continue whichever step is shown. The next queued fit starts once
  # the runner is free; a fit whose call cannot be built (e.g. the cover model
  # without biomass) fails at once.
  #
  # The cover calibration's job predicts the survey biomass it is fitted to
  # (fit_cover_calibration()), so that prediction runs in the background too.
  #
  # TODO: once the real kelpbio predictions replace the mocks, run the slow
  # ones in this queue too, so all waiting stays on the Models step:
  # - Order: fit the weight and size models -> predict survey biomass -> fit
  #   the cover calibration to it -> predict canopy biomass, automatically,
  #   and keep the survey biomass for the Estimates step rather than
  #   predicting it again there. Wet, dry and carbon in one job, as they
  #   share draws.
  # - Each model's own predictions stay on demand on the Estimates step, cached
  #   with bindCache(), unless timing with kelpbio shows them slow; then run
  #   them in the same daemon call as the fit. The cache is shared by every
  #   session in the R process, so the key must identify the fit itself (for
  #   example a hash of it), never a summary such as a rounded estimate, or one
  #   user can be shown another's results.
  # - Statuses gain "predicting"; the navbar badge and Models banner show it,
  #   and Continue to estimates waits for the predictions.
  # - A refit, source change or new data marks the dependent predictions stale
  #   and queues them again.
  # - kb_predict_biomass_survey() records progress
  #   in a progress_dir for kb_progress(), as the fits do. Needs from kelpbio:
  #   pre-fit models shipped with their predictions.

  start_next <- function(id) {
    progress_dir <- tempfile("kb-fit-")
    dir.create(progress_dir)
    call <- tryCatch(
      {
        if (id == "cover" && !s$survey_ready()) {
          stop("Survey biomass is not available: the weight and size models must be ready first.", call. = FALSE)
        }
        fit_call(
          id, s$species(), s$sheets(), s$priors()[[id]], s$samplers()[[id]], progress_dir,
          fits = if (id == "cover") list(weight = s$fit_of("weight"), size = s$fit_of("size")), census = s$census()
        )
      },
      error = function(e) e
    )
    set_records(start_record(s$records(), id, s$settings()[[id]]))
    if (inherits(call, "error")) {
      unlink(progress_dir, recursive = TRUE)
      fit_failed(id, condition_text(call))
      return()
    }
    running <<- list(id = id, dir = progress_dir)
    s$progress(0)
    runner$invoke(call$fn, call$args)
  }

  # A failure is the one fit result shown as a notification, as it can happen
  # while another step is open. A finished fit shows only in the statuses and
  # the model page notices.
  fit_failed <- function(id, message) {
    set_records(fail_record(s$records(), id, message))
    s$notify(message, type = "error", title = sprintf("%s model failed", label_of(id)), duration = 10)
  }

  fit_done <- function(id, fit) set_records(finish_record(s$records(), id, fit))

  observe({
    records <- s$records()
    if (!is.null(running) || runner$status() == "running") {
      return()
    }
    id <- next_queued(records)
    if (!is.null(id)) isolate(start_next(id))
  })

  # A cancelled fit settles as an error after running was cleared, so only the
  # running fit's result is used.
  observe({
    status <- runner$status()
    if (is.null(running) || !status %in% c("success", "error")) {
      return()
    }
    isolate({
      id <- running$id
      result <- tryCatch(runner$result(), error = function(e) list(error = condition_text(e)))
      stop_running()
      if (is.null(result$error)) fit_done(id, result$fit) else fit_failed(id, result$error)
    })
  })

  observe({
    if (is.null(s$fitting())) {
      return()
    }
    invalidateLater(TICK_MS)
    if (!is.null(running)) s$progress(100 * kb_progress(running$dir))
  })

  session$onSessionEnded(function() stop_running(cancel = TRUE))

  s
}
