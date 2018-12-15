context("test-extract_models")

test_that("extract_model throws an error when `step` = NULL", {
  data(ho_et_al)
  
  ho_et_al$condition_c <- build_contrast(ho_et_al$condition,
                                         "Low discrimination",
                                         "High discrimination")
  model <- mdt_simple(data = ho_et_al,
                      IV   = condition_c,
                      DV   = hypodescent,
                      M    = linkedfate)
  
  expect_error(extract_model(model))
})

test_that("extract_model does not throw an error when used as intented", {
  data(ho_et_al)
  
  ho_et_al$condition_c <- build_contrast(ho_et_al$condition,
                                         "Low discrimination",
                                         "High discrimination")
  model <- mdt_simple(data = ho_et_al,
                      IV   = condition_c,
                      DV   = hypodescent,
                      M    = linkedfate)
  
  expect_silent(extract_model(model, 1))
})

test_that("extract_models does not throw an error when used as intented", {
  data(ho_et_al)
  
  ho_et_al$condition_c <- build_contrast(ho_et_al$condition,
                                         "Low discrimination",
                                         "High discrimination")
  model <- mdt_simple(data = ho_et_al,
                      IV   = condition_c,
                      DV   = hypodescent,
                      M    = linkedfate)
  
  expect_silent(extract_models(model))
})

test_that("extract_models does not throw an error when used as intented", {
  data(ho_et_al)
  
  ho_et_al$condition_c <- build_contrast(ho_et_al$condition,
                                         "Low discrimination",
                                         "High discrimination")
  model <- mdt_simple(data = ho_et_al,
                      IV   = condition_c,
                      DV   = hypodescent,
                      M    = linkedfate)
  
  expect_silent(extract_tidy_models(model))
})
