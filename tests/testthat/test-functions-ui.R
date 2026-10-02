sensitivity_rows <- function(parameter, prior, weak_prior) {
  tibble::tibble(parameter = parameter, prior = prior, weak_prior = weak_prior, strong_data = TRUE)
}

notice_text <- function(x) paste(as.character(x), collapse = "")

test_that("prior advice follows the flagged prior's family and current setting", {
  expect_identical(
    prior_advice(kb_prior_exponential(rate = 1)),
    "For this exponential prior, try reducing the rate (for example from 1 to 0.5)."
  )
  expect_identical(
    prior_advice(kb_prior_normal(mean = 0, sd = 2)),
    "For this normal prior, try increasing the SD (for example from 2 to 4)."
  )
  expect_identical(
    prior_advice(kb_prior_normal(mean = 0, sd = 1), "bPower"),
    "For bPower (normal prior), try increasing the SD (for example from 1 to 2)."
  )
  expect_identical(prior_advice(structure(list(), class = "kb_prior")), "Try a wider prior.")
  expect_identical(prior_advice(NULL, "sYear"), "For sYear, try a wider prior.")
})

test_that("the prior notice gives advice for each flagged prior", {
  priors <- list(sd_year = kb_prior_exponential(rate = 1), power = kb_prior_normal(mean = 2, sd = 1))
  rows <- sensitivity_rows(c("sYear", "bPower", "sSite"), c("sd_year", "power", "sd_site"), c(FALSE, FALSE, TRUE))
  text <- notice_text(prior_notice(rows, priors, "open"))
  expect_match(text, "The priors for sYear and bPower are influencing the estimates.", fixed = TRUE)
  expect_match(text, "For sYear (exponential prior), try reducing the rate (for example from 1 to 0.5).", fixed = TRUE)
  expect_match(text, "For bPower (normal prior), try increasing the SD (for example from 1 to 2).", fixed = TRUE)
  expect_null(prior_notice(sensitivity_rows("sSite", "sd_site", TRUE), priors, "open"))
})

test_that("each status has one label", {
  label <- function(kind, ...) gsub("<[^>]+>", "", notice_text(status_badge(list(kind = kind, ...))))
  expect_identical(
    trimws(c(
      label("not-used"), label("no-data"), label("data-error"), label("not-fitted"), label("queued"),
      label("fitting"), label("ready"), label("ready", warning = TRUE), label("failed")
    )),
    c("Not used", "Needs data", "Data error", "Not fitted", "Queued", "Fitting 0%", "Ready", "Ready, convergence warning", "Failed")
  )
})
