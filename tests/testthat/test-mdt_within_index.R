-test_that("simple mediation index produce the intended intervals", {
  withr::with_seed(123, {
    within_model <-
      mdt_within(dohle_siegrist,
                 name,
                 willingness,
                 hazardousness,
                 participant)

    model_index <-
      add_index(within_model, times = 5e6) %>%
      purrr::pluck("indirect_index_infos", "CI")

    # CIs produce the right values
    expect_equal(model_index[[1]], 0.1600,
                 tolerance = 1e-3)
    expect_equal(model_index[[2]], 0.8684,
                 tolerance = 1e-3)

  })
})
