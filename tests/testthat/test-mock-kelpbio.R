# The source checks read R/ and so run from the source tree only (not in
# R CMD check, which tests the installed package).
source_dir <- function() test_path("..", "..", "R")

parsed_tokens <- function(files) {
  data <- do.call(rbind, lapply(files, function(file) {
    data <- utils::getParseData(parse(file, keep.source = TRUE))
    data[data$terminal, c("token", "text")]
  }))
  data
}

top_level_assignments <- function(file) {
  exprs <- parse(file)
  names <- vapply(exprs, function(e) {
    if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]])) as.character(e[[2]]) else NA_character_
  }, character(1))
  names[!is.na(names)]
}

app_source_files <- function() {
  files <- list.files(source_dir(), pattern = "\\.R$", full.names = TRUE)
  files[basename(files) != "mock-kelpbio.R"]
}

test_that("every kelpbio function the app calls is defined in the mock file", {
  skip_if_not(dir.exists(source_dir()))
  files <- app_source_files()
  tokens <- parsed_tokens(files)
  app_defined <- unlist(lapply(files, top_level_assignments))
  mocked <- top_level_assignments(file.path(source_dir(), "mock-kelpbio.R"))

  kb_symbols <- unique(tokens$text[tokens$token %in% c("SYMBOL_FUNCTION_CALL", "SYMBOL") & startsWith(tokens$text, "kb_")])
  kelpbio_calls <- setdiff(kb_symbols, app_defined)
  expect_gt(length(kelpbio_calls), 10)
  expect_identical(setdiff(kelpbio_calls, mocked), character())
})

test_that("the app code calls no summary or distribution functions", {
  skip_if_not(dir.exists(source_dir()))
  tokens <- parsed_tokens(app_source_files())
  calls <- tokens$text[tokens$token == "SYMBOL_FUNCTION_CALL"]
  stats_calls <- c(
    "median", "mean", "quantile", "sd", "var", "signif", "exp", "log", "qlogis", "plogis",
    "rnorm", "rlnorm", "runif"
  )
  expect_identical(intersect(calls, stats_calls), character())
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

test_that("the kb_fit methods are registered for the imported generics", {
  fit <- kb_prefit_weight_nereo()
  expect_s3_class(tidy(fit), "tbl_df")
  expect_s3_class(glance(fit), "tbl_df")
  expect_true(converged(fit, rhat = RHAT_MAX, esr = ESR_MIN))
  expect_named(rhat(fit), tidy(fit)$term)
  expect_s3_class(summary(fit), "summary_kb_fit")
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

test_that("a pre-fit accessor rejects a reference it does not have", {
  expect_identical(kb_prefit_weight_nereo()$meta$reference, "coastwide")
  expect_identical(kb_prefit_size_nereo()$meta$reference, "hakai")
  expect_error(kb_prefit_size_nereo(reference = "coastwide"), "must be one of")
  expect_error(kb_prefit_weight_nereo(reference = "regional"), "must be one of")
})
