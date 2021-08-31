test_that("check_assumptions_plot", {
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

  plot_assumptions_without_warn <- function(x, seed = 123) {
    withr::local_options(list(warn = -1))
    withr::local_seed(seed)

    plot_assumptions(x)
  }

  skip_on_ci() # Test locally

  # HELP WANTED:
  # Uses vdiffr to check the result of `plot_assumptions`. Because
  # `plot_assumptions` produces three plot automatically and because vdiffr
  # automatically captures the last plot, the only plot testes is the last one.
  vdiffr::expect_doppelganger(title = "Last plot plot_assumptions produces",
                              fig = plot_assumptions_without_warn(mediation_fit))

})
