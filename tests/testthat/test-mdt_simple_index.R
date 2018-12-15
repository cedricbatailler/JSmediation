context("test-mdt_simple_index")

test_that("add_index method does not throw error", {
  data(ho_et_al)
  ho_et_al$condition_c <- build_contrast(ho_et_al$condition,
                                         "Low discrimination",
                                         "High discrimination")
  
  simple_mediation <- 
    mdt_simple(data = ho_et_al,
               IV   = condition_c,
               DV   = hypodescent,
               M    = linkedfate)
  
  expect_silent(add_index(simple_mediation))
})
