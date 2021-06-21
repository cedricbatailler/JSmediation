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
    purrr::map(~expect_output(check_assumptions(my_model), .x))
  
})
