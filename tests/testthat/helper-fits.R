# Fitting in tests: kb_fit_*() stubs that record their calls and return a fake
# fit at once, and a fit runner that runs each call in the session when told to.

# Stubs every kb_fit_<model>_<species>() for the calling test. Each records its
# arguments and returns the mock's fake fit, or errors for a model in `fail`.
# Returns an environment whose `calls` lists list(model, species, args) in order.
local_stub_fits <- function(fail = character(), env = parent.frame()) {
  log <- new.env()
  log$calls <- list()
  stub <- function(model, species) {
    force(model)
    force(species)
    function(...) {
      args <- list(...)
      log$calls <- c(log$calls, list(list(model = model, species = species, args = args)))
      if (model %in% fail) stop(sprintf("Stub %s fit failed.", model), call. = FALSE)
      fit <- .mock_new_fit(model, species, args$data, args$priors, args$chains, args$niters, args$nthin)
      if (model == "biomass_cover") fit$data <- tibble::as_tibble(.mock_cover_response(args$data))
      fit
    }
  }
  stubs <- list()
  for (id in component_ids) {
    for (species in names(species_info)) {
      stubs[[sprintf("kb_fit_%s_%s", fn_of(id), species)]] <- stub(fn_of(id), species)
    }
  }
  testthat::local_mocked_bindings(!!!stubs, .env = env)
  log
}

# The models fitted, in order, from local_stub_fits()'s log.
fitted_models <- function(log) vapply(log$calls, `[[`, "", "model")

# A fit runner that runs nothing until complete() is called, which runs the
# invoked call in the session; the mirai runner's interface otherwise.
test_runner <- function() {
  status <- reactiveVal("initial")
  call <- NULL
  value <- NULL
  list(
    invoke = function(fn, args) {
      call <<- list(fn = fn, args = args)
      status("running")
    },
    status = function() status(),
    result = function() value,
    cancel = function() {
      value <<- NULL
      status("error")
    },
    complete = function() {
      value <<- tryCatch(list(fit = do.call(call$fn, call$args)), error = function(e) list(error = conditionMessage(e)))
      status("success")
    },
    running = function() isolate(status()) == "running"
  )
}

# Runs the queued fits to the end: each in turn completes and the session reacts.
finish_fits <- function(session, runner) {
  session$flushReact()
  while (runner$running()) {
    runner$complete()
    session$flushReact()
  }
}
