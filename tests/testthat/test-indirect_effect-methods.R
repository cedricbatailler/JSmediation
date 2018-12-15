context("test-indirect_effect-methods")

test_that("print method does not throw error", {
  data(ho_et_al)
  ho_et_al$condition_c <- build_contrast(ho_et_al$condition,
                                         "Low discrimination",
                                         "High discrimination")
  mediation <-
    mdt_simple(data = ho_et_al,
             IV = condition_c,
             DV = hypodescent,
             M = linkedfate)
  
  mediation <- add_index(mediation)
  
  expect_output(print(mediation$indirect_index_infos))
  })
