# Architecture test: codetools walks the installed namespace (so it also runs
# under R CMD check) to check the app's own functions (everything but the mocks,
# whose names start with kb_ or .mock, and the mocks' kb_fit methods) call no
# statistics and use only kelpbio functions that exist.

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
