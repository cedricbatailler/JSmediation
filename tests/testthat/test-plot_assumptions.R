test_that("check_assumptions_plot", {
  # set seed
  withr::local_seed(123)

  # run mediation model
  data(ho_et_al)
  ho_et_al$condition_c <- build_contrast(ho_et_al$condition,
                                         "Low discrimination",
                                         "High discrimination")
  my_model <-
    mdt_simple(data = ho_et_al,
               IV = condition_c,
               DV = hypodescent,
               M = linkedfate)

  # number of models to test
  number_of_models <- purrr::chuck(my_model, "js_models") %>% length()

  # prepare plotting
  plot_tempdir <- tempdir()
  plot_path <- glue::glue("{plot_tempdir}/assumption-plot-%02d.png")

  # save plots in tempdir -----------------------------------------------------
  # use warn=-1 because performance::check_models sometimes generates error as
  # a normal behavior
  png(plot_path)
  withr::with_options(list(warn = -1), { plot_assumptions(my_model) })
  dev.off()

  # compare the models --------------------------------------------------------
  sprintf(plot_path, 1:number_of_models) %>% 
    purrr::map(~ expect_snapshot_file(.x, cran = FALSE))

})
