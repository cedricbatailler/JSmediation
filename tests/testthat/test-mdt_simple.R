context("test-mdt_simple")

test_that("mdt_simple does not throw an error", {
  data(ho_et_al)
  ho_et_al$condition_c <- build_contrast(ho_et_al$condition,
                                         "Low discrimination",
                                         "High discrimination")
  expect_silent(
    mdt_simple(data = ho_et_al,
               IV   = condition_c,
               DV   = hypodescent,
               M    = linkedfate)
  )
})

test_that("mdt_simple print method does not throw an error", {
  data(ho_et_al)
  ho_et_al$condition_c <- build_contrast(ho_et_al$condition,
                                         "Low discrimination",
                                         "High discrimination")
  model <- mdt_simple(data = ho_et_al,
                      IV   = condition_c,
                      DV   = hypodescent,
                      M    = linkedfate)
  
  expect_output(print(model))
  expect_output(print(add_index(model)))
})
