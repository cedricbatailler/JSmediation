test_that("M arg accepts single numeric value", {
  moderated_mediation_model <-
    ho_et_al %>%
    dplyr::mutate(condition_c =
                    build_contrast(condition,
                                   "High discrimination",
                                   "Low discrimination"),
                  linkedfate_c =
                    scale(linkedfate, scale = FALSE),
                  sdo_c =
                    scale(sdo, scale = FALSE)) %>%
    mdt_moderated(
      dataset,
      condition_c,
      hypodescent,
      linkedfate_c,
      sdo_c
    )

  expect_warning(
    moderated_mediation_model %>% compute_indirect_effect_for(M = "foo")
  )

  expect_warning(
    moderated_mediation_model %>% compute_indirect_effect_for(M = c(1, 2))
  )

  expect_warning(
    moderated_mediation_model %>% compute_indirect_effect_for(M = 0),
    NA
  )

})
