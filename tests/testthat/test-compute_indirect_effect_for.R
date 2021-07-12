test_that("Mod arg accepts single numeric value", {
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
      condition_c,
      hypodescent,
      linkedfate_c,
      sdo_c
    )

  expect_error(
    moderated_mediation_model %>% compute_indirect_effect_for(Mod = "foo")
  )

  expect_error(
    moderated_mediation_model %>% compute_indirect_effect_for(Mod = c(1, 2))
  )

  expect_error(
    moderated_mediation_model %>% compute_indirect_effect_for(Mod = 0),
    NA
  )

})

test_that("JSmediation approach is consistent with the {mediation} approach (Mod = 1)", {
  # note that because the mediation::mediate is computationnaly intensive, we
  # compute the conditionnal indirect effect for a single moderator value.

  # set seed
  withr::local_seed(123)
  # data set
  dataset <-
    ho_et_al %>%
    dplyr::mutate(condition_c = build_contrast(condition,
                                               "Low discrimination",
                                               "High discrimination")) %>%
    dplyr::mutate(dplyr::across(c(linkedfate, sdo),
                                ~ as.numeric(scale(.))))

  # mediation approach
  model_1 <- lm(linkedfate ~ condition_c * sdo, dataset)
  model_2 <- lm(hypodescent ~ (condition_c + linkedfate) * sdo, dataset)

  mediation_model <-
    mediation::mediate(model_1, model_2,
                       covariates = list(sdo = 1),
                       boot = TRUE,
                       boot.ci.type = "perc",
                       sims = 5000,
                       treat    = "condition_c",
                       mediator = "linkedfate")

  JSmediation_model <-
    dataset %>%
    mdt_moderated(DV = hypodescent,
                  IV = condition_c,
                  M = linkedfate,
                  Mod = sdo) %>%
    compute_indirect_effect_for(Mod = 1)

  #checks
  JSmediation_estimate <- purrr::chuck(JSmediation_model, "estimate")
  mediation_estimate   <- purrr::chuck(mediation_model, "d0")

  expect_equal(JSmediation_estimate, mediation_estimate, tolerance = 5e-2)
})

test_that("JSmediation approach is consistent with the {processR} approach (Mod = -1, 0, 1)", {
  # set seed
  withr::local_seed("123")

  # data set
  dataset <-
    ho_et_al %>%
    dplyr::mutate(condition_c = build_contrast(condition,
                                               "Low discrimination",
                                               "High discrimination")) %>%
    dplyr::mutate(dplyr::across(c(linkedfate, sdo),
                                ~ as.numeric(scale(.))))

  # processR approach
  # gen eqn (model 59)
  moderated_mediation_eqn <-
    processR::tripleEquation(X = "condition_c",
                             M = "linkedfate",
                             Y = "hypodescent",
                             moderator = list(name = "sdo",
                                              site = list(c("a", "b", "c"))))

  moderated_mediation_fit <-
    withr::with_options(list(warn = -1),
                        lavaan::sem(moderated_mediation_eqn,
                                    dataset))

  moderated_mediation_indirect_indices <-
    processR::modmedSummary(moderated_mediation_fit)

  # JSmediation approach
  JSmediation_model <-
    dataset %>%
    mdt_moderated(condition_c,
                  hypodescent,
                  linkedfate,
                  sdo)

  # check
  processR_estimates <-
    purrr::chuck(moderated_mediation_indirect_indices, "indirect")

  JSmediation_estimate <-
    purrr::chuck(moderated_mediation_indirect_indices, "values") %>%
    purrr::map_dbl(~ JSmediation_model %>%
                     compute_indirect_effect_for(.x) %>%
                     purrr::chuck("estimate"))

  expect_equal(processR_estimates, JSmediation_estimate,
               tolerance = 5e-2)
})
