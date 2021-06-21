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
