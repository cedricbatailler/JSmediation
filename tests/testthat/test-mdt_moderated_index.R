test_that("moderated mediation index produce the intended intervals", {
  withr::with_seed(123, {
    moderated_model <-
      ho_et_al %>%
      dplyr::mutate(condition_c = build_contrast(condition,
                                                 "Low discrimination",
                                                 "High discrimination"),
                    dplyr::across(c(linkedfate, sdo),
                                  ~ scale(., center = TRUE, scale = FALSE))) %>%
      mdt_moderated(condition_c,
                    hypodescent,
                    linkedfate,
                    sdo)

    model_with_stage_1_index <-
      add_index(moderated_model, stage = "1", times = 5e6) %>%
      purrr::pluck("indirect_index_infos", "CI")

    model_with_stage_2_index <-
      add_index(moderated_model, stage = "2", times = 5e6) %>%
      purrr::pluck("indirect_index_infos", "CI")

    model_with_total_index <-
      add_index(moderated_model, stage = "total", times = 5e6) %>%
      purrr::pluck("indirect_index_infos", "CI")

    #CIs indicate significant results when they are supposed to
    # stage 1 index is significant
    expect_lt(model_with_stage_1_index[2],
              expected = 0)

    # stage 2 index contains 0
    expect_lt(model_with_stage_2_index[1],
              expected = 0)
    expect_gt(model_with_stage_2_index[2],
              expected = 0)

    # total index is significant
    expect_lt(model_with_total_index[2],
              expected = 0)

    # CIs produce the right values
    expect_equal(model_with_stage_1_index[[1]], -0.0733,
                 tolerance = 1e-3)
    expect_equal(model_with_stage_1_index[[2]], -0.0157,
                 tolerance = 1e-3)

    expect_equal(model_with_stage_2_index[[1]], -0.0820,
                 tolerance = 1e-3)
    expect_equal(model_with_stage_2_index[[2]],  0.0088,
                 tolerance = 1e-3)

    expect_equal(model_with_total_index[[1]], -0.1320,
                 tolerance = 1e-3)
    expect_equal(model_with_total_index[[2]], -0.0244,
                 tolerance = 1e-3)

  })
})
