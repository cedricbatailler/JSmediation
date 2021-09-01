test_that("mdt_within does not throw error", {
  expect_silent(mdt_within(dohle_siegrist,
                           name,
                           willingness,
                           hazardousness,
                           participant))
})

test_that("add_index method for mdt_within does not throw error", {
  model <-
    mdt_within(dohle_siegrist,
               name,
               willingness,
               hazardousness,
               participant)

  expect_silent(add_index(model))
})

test_that("print method for mdt_within does not throw error", {
  model <-
    mdt_within(dohle_siegrist,
               name,
               willingness,
               hazardousness,
               participant)

  expect_output(print(model))
  expect_output(print(add_index(model)))
})

test_that("Type checks work as intended in mdt_within", {
  data("dohle_siegrist")
  dohle_siegrist$foo <- "foo"
  dohle_siegrist$bar <- 1

  expect_error(
    mdt_within(dohle_siegrist,
               bar,
               willingness,
               hazardousness,
               participant)
  )

  expect_error(
    mdt_within(dohle_siegrist,
               name,
               foo,
               hazardousness,
               participant)
  )

  expect_error(
    mdt_within(dohle_siegrist,
               name,
               willingness,
               foo,
               participant)
  )
})

test_that("mdt_within reverse the coding when it is needed", {
  data("dohle_siegrist")
  dohle_siegrist$willingness_reversed <- (- dohle_siegrist$willingness)

  expect_error(mdt_within(dohle_siegrist,
                          name,
                          willingness_reversed,
                          hazardousness,
                          participant),
               NA) # code produces no error

  mediation_coding_1 <-
    mdt_within(dohle_siegrist,
               name,
               willingness_reversed,
               hazardousness,
               participant) %>%
    purrr::chuck("params", "IV") %>%
    as.character()

  mediation_coding_2 <-
    mdt_within(dohle_siegrist,
               name,
               willingness,
               hazardousness,
               participant) %>%
    purrr::chuck("params", "IV") %>%
    as.character()

  expect_equal(mediation_coding_1, "name (difference: complex - simple)")
  expect_equal(mediation_coding_2, "name (difference: simple - complex)")
})
