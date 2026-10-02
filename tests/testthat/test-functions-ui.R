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
  text <- notice_text(prior_notice(rows, priors, "open", "dismiss", compact = TRUE))
  expect_match(text, "The priors for sYear and bPower are influencing the estimates.", fixed = TRUE)
  expect_match(text, "If this is unintended, try making the priors less informative.", fixed = TRUE)
  expect_match(text, "For sYear (exponential prior), try reducing the rate (for example from 1 to 0.5).", fixed = TRUE)
  expect_match(text, "For bPower (normal prior), try increasing the SD (for example from 1 to 2).", fixed = TRUE)
  expect_match(text, "Open prior settings", fixed = TRUE)
  expect_match(text, "I understand, dismiss", fixed = TRUE)

  expect_null(prior_notice(sensitivity_rows("sSite", "sd_site", TRUE), priors, "open"))
})

test_that("a dismissed prior notice is left out of the sensitivity notices", {
  priors <- list(sd_year = kb_prior_exponential(rate = 1))
  rows <- sensitivity_rows("sYear", "sd_year", FALSE)
  expect_match(notice_text(sensitivity_notices(rows, priors, "open", "dismiss")), "influencing", fixed = TRUE)
  expect_no_match(notice_text(sensitivity_notices(rows, priors, "open", "dismiss", dismissed = TRUE)), "influencing", fixed = TRUE)
})

test_that("a dismissed convergence warning shows as a quieter status", {
  status <- list(kind = "fitted", converged = FALSE, dismissed = "convergence")
  expect_false(shows_convergence_warning(status))
  expect_match(notice_text(status_badge(status)), "Fitted, warning dismissed", fixed = TRUE)
  expect_true(shows_convergence_warning(list(kind = "fitted", converged = FALSE, dismissed = character())))
})
