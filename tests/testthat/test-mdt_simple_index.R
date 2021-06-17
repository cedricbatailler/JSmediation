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

test_that("simple mediation index produce the intended intervals", {
  withr::with_seed(123, 
  {
    simple_model <- 
      ho_et_al %>% 
      dplyr::mutate(condition_c = build_contrast(condition,
                                                 "Low discrimination",
                                                 "High discrimination")) %>% 
      mdt_simple(condition_c,
                 hypodescent, 
                 linkedfate)
    
    model_index <-
      add_index(simple_model, times = 5e6) %>% 
      purrr::pluck("indirect_index_infos", "CI")
    
    # CIs produce the right values
    expect_equal(model_index[[1]], 0.0894, 
                 tolerance = 1e-3)
    expect_equal(model_index[[2]], 0.2060, 
                 tolerance = 1e-3)
    
  })
})
