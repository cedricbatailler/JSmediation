test_that("check_assumptions_plot", {
  # it seems that check models produce some error which are not relevent for
  # the test.
  plot_assumptions_without_warn <- function(x, ..., seed = 123) {
    withr::local_options(list(warn = -1))
    withr::local_seed(seed)

    plot_assumptions(x, ...)
  }

  # run mediation model
  data(ho_et_al)
  ho_et_al$condition_c <- build_contrast(ho_et_al$condition,
                                         "Low discrimination",
                                         "High discrimination")
  mediation_fit <-
    mdt_simple(data = ho_et_al,
               IV = condition_c,
               DV = hypodescent,
               M = linkedfate)

  # Internal tests run error

  expect_error(plot_assumptions_without_warn(mediation_fit, tests = NULL),
               "must contains at least one element.")
  expect_error(plot_assumptions_without_warn(mediation_fit, tests = c(1L, 2L)),
               "argument must be a character vector.")
  expect_error(plot_assumptions(mediation_fit, tests = c("foo", "bar")),
               "unsupported checks")
  expect_warning(plot_assumptions(mediation_fit, tests = c("foo", "normality")),
                 "unsupported checks")

  # Run smoothly
  expect_error(
    plot_assumptions_without_warn(mediation_fit),
    regexp = NA # Expect no error
  )

  skip_on_ci() # Test printing locally

  # HELP WANTED:
  # Uses vdiffr to check the result of `plot_assumptions`. Because
  # `plot_assumptions` produces three plot automatically and because vdiffr
  # automatically captures the last plot, the only plot testes is the last one.
  vdiffr::expect_doppelganger(title = "Last plot plot_assumptions produces",
                              fig = plot_assumptions_without_warn(mediation_fit))

})
