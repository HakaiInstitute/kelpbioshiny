# Policy checks on the installed namespace: the app's own functions (everything
# but the mocks, whose names start with kb_ or .mock, and the mocks' kb_fit
# methods) call no statistics and use only kelpbio functions that exist.

app_functions <- function() {
  ns <- asNamespace("kelpbioshiny")
  names <- ls(ns, all.names = TRUE)
  names <- names[vapply(names, function(name) is.function(get(name, envir = ns)), logical(1))]
  mocks <- startsWith(names, "kb_") | startsWith(names, ".mock") | endsWith(names, ".kb_fit")
  lapply(stats::setNames(nm = names[!mocks]), get, envir = ns)
}

# The functions a function calls, including pkg::fun() calls, and the other
# symbols it uses, found by walking its code with codetools.
used_names <- function(fn) {
  found <- character()
  walker <- codetools::makeCodeWalker(
    call = function(e, w) {
      head <- e[[1]]
      if (is.call(head) && identical(head[[1]], as.name("::"))) found <<- c(found, as.character(head[[3]]))
      # In x$name, name is a list element, not a symbol in use.
      parts <- if (identical(head, as.name("$"))) as.list(e)[1:2] else as.list(e)
      for (part in parts) if (!missing(part)) codetools::walkCode(part, w)
    },
    leaf = function(e, w) {
      if (is.name(e)) found <<- c(found, as.character(e))
    }
  )
  codetools::walkCode(body(fn), walker)
  unique(found)
}

test_that("the app code calls no summary or distribution functions", {
  skip_if_not_installed("codetools")
  stats_functions <- c(
    "median", "mean", "quantile", "sd", "var", "signif", "exp", "log", "qlogis", "plogis",
    "rnorm", "rlnorm", "runif", "rgamma", "rbeta", "dbeta"
  )
  used <- lapply(app_functions(), function(fn) intersect(used_names(fn), stats_functions))
  expect_identical(unname(unlist(used)), character())
})

test_that("every kelpbio function the app uses exists, and no mock internals are used", {
  skip_if_not_installed("codetools")
  used <- unique(unlist(lapply(app_functions(), used_names)))
  kelpbio <- used[startsWith(used, "kb_")]
  expect_gt(length(kelpbio), 10)
  ns <- asNamespace("kelpbioshiny")
  missing <- kelpbio[!vapply(kelpbio, exists, logical(1), envir = ns, mode = "function")]
  expect_identical(missing, character())
  expect_identical(used[startsWith(used, ".mock")], character())
})

test_that("every kelpbio function name the app builds exists", {
  built <- unlist(lapply(names(species_info), function(species) {
    lapply(component_ids, function(id) {
      verbs <- c("check", "priors", "fit", if (any(is_prefit(components[[id]]$sources))) "prefit")
      sprintf("kb_%s_%s_%s", kelpbio_verbs[verbs], fn_of(id), species)
    })
  }))
  ns <- asNamespace("kelpbioshiny")
  missing <- built[!vapply(built, exists, logical(1), envir = ns, mode = "function")]
  expect_identical(unname(missing), character())
})

test_that("the example data pass the data checks, except the weight sheet's missing years", {
  for (species in names(species_info)) {
    rows <- kb_example_data(species)
    for (id in c("density", "size", "cover")) {
      expect_no_error(suppressMessages(kelpbio_fn("check", id, species)(rows[[id]])))
      expect_true(all(sheet_columns(id, species) %in% names(rows[[id]])), info = paste(id, species))
    }
    expect_error(kelpbio_fn("check", "weight", species)(rows$weight), "must not have any missing values")
  }
})

test_that("each pre-fit accessor takes the references its model offers", {
  for (species in names(species_info)) {
    for (id in component_ids) {
      sources <- components[[id]]$sources
      references <- prefit_reference(sources[is_prefit(sources)])
      if (length(references) == 0) next
      accessor <- kelpbio_fn("prefit", id, species)
      expect_identical(eval(formals(accessor)$reference), references, info = paste(id, species))
      for (reference in references) {
        expect_identical(accessor(reference = reference)$meta$reference, reference, info = paste(id, species))
      }
    }
  }
})

test_that("the mock fit writes progress that kb_fit_progress() reads", {
  dir <- withr::local_tempdir()
  expect_identical(kb_fit_progress(dir), 0)
  rows <- kb_example_data("nereo")$size
  fit <- kb_fit_size_nereo(rows, niters = 10L, progress = "none", progress_dir = dir)
  expect_s3_class(fit, "kb_fit_size_nereo")
  expect_identical(kb_fit_progress(dir), 1)
  expect_error(kb_fit_size_nereo(rows, nthin = 0), "`nthin` must be greater than 0")
})
