context("test-mdt_moderated.R")

test_that("used models are correctly specified", {
  data <-
    ho_et_al %>%
    dplyr::mutate(condition_c =
                    build_contrast(condition,
                                   "High discrimination",
                                   "Low discrimination"))
  
  js_model <-
    data %>%
    mdt_moderated(condition_c, hypodescent, linkedfate, sdo)
  
  models <-
    list(formula(hypodescent ~ condition_c * sdo),
         formula(linkedfate ~ condition_c * sdo),
         formula(hypodescent ~ (condition_c + linkedfate) * sdo)) %>%
    purrr::map(~lm(.x, data))
  
  models_js <- purrr::pluck(js_model, "js_models")
  
  expect_equivalent(models_js, models)
})

test_that("mdt_moderated does not throw error", {
  dataset <-
    ho_et_al %>%
    dplyr::mutate(condition_c =
                    build_contrast(condition,
                                   "High discrimination",
                                   "Low discrimination"),
                  linkedfate_c =
                    scale(linkedfate, scale = FALSE),
                  sdo_c =
                    scale(sdo, scale = FALSE))
  
  expect_silent(
    mdt_moderated(dataset,
                  condition_c,
                  hypodescent,
                  linkedfate_c,
                  sdo_c)
  )
})

test_that("print method for mdt_moderated does not throw error", {
  dataset <-
    ho_et_al %>%
    dplyr::mutate(condition_c =
                    build_contrast(condition,
                                   "High discrimination",
                                   "Low discrimination"),
                  linkedfate_c =
                    scale(linkedfate, scale = FALSE),
                  sdo_c =
                    scale(sdo, scale = FALSE))
  
  model <- 
    mdt_moderated(dataset,
                  condition_c,
                  hypodescent,
                  linkedfate_c,
                  sdo_c)
  
  expect_output(print(model))
  
})

test_that("print method for mdt_moderated throws message when a variable is not contrast-coded/centered", {
  dataset <-
    ho_et_al %>%
    dplyr::mutate(condition_c =
                    build_contrast(condition,
                                   "High discrimination",
                                   "Low discrimination"),
                  linkedfate_c =
                    scale(linkedfate, scale = FALSE))
  
  model <-
    mdt_moderated(dataset,
                  condition_c,
                  hypodescent,
                  linkedfate_c,
                  sdo)
    
  expect_message(print(model))
})

test_that("add_index method for mdt_moderated does not throw error when used as intented", {
  dataset <-
    ho_et_al %>%
    dplyr::mutate(condition_c =
                    build_contrast(condition,
                                   "High discrimination",
                                   "Low discrimination"),
                  linkedfate_c =
                    scale(linkedfate, scale = FALSE),
                  sdo_c =
                    scale(sdo, scale = FALSE))
  
  model <- 
    mdt_moderated(dataset,
                  condition_c,
                  hypodescent,
                  linkedfate_c,
                  sdo_c)
  
  expect_error(add_index(model))
  expect_silent(add_index(model, stage = 1))
  expect_silent(add_index(model, stage = 2))
  expect_silent(add_index(model, stage = "total"))
  
})
