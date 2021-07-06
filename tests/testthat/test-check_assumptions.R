test_that("check_assumptions run without error", {
  ho_et_al$condition_c <- build_contrast(ho_et_al$condition,
                                         "Low discrimination",
                                         "High discrimination")

  my_model <-
    mdt_simple(data = ho_et_al,
               IV = condition_c,
               DV = hypodescent,
               M = linkedfate)

  expect_error(check_assumptions(my_model), NA)
})

test_that("check_assumptions test assumptions for each models", {
  ho_et_al$condition_c <- build_contrast(ho_et_al$condition,
                                         "Low discrimination",
                                         "High discrimination")

  my_model <-
    mdt_simple(data = ho_et_al,
               IV = condition_c,
               DV = hypodescent,
               M = linkedfate)

  purrr::pluck(my_model, "js_models") %>%
    names() %>%
    stringr::str_replace("\\+", "\\\\+") %>%
    purrr::map(~ expect_output(check_assumptions(my_model), .x))
})

test_that("check_assumptions's `tests` argument ", {
  ho_et_al$condition_c <- build_contrast(ho_et_al$condition,
                                         "Low discrimination",
                                         "High discrimination")

  my_model <-
    mdt_simple(data = ho_et_al,
               IV = condition_c,
               DV = hypodescent,
               M = linkedfate)

  expect_error(check_assumptions(my_model, tests = c()))
  expect_error(check_assumptions(my_model, tests = c(1L, 2L)))
  expect_warning(check_assumptions(my_model, tests = c("foo", "bar")))
  expect_warning(check_assumptions(my_model, tests = c("normality", "foo")))
  expect_output(check_assumptions(my_model, c("normality", "heteroscedasticity", "outliers")))
})

test_that("check_assumptions_plot", {
  skip("test is not functional")
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

  # plot
  png(plot_path)
  withr::with_options(list(warn = -1), { check_assumptions_plot(my_model) })
  dev.off()

  # compare the models
  sprintf(plot_path, 1:number_of_models) %>% 
    purrr::map(~ expect_snapshot_file(.x))

  })
