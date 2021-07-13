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
  data(dohle_siegrist)
  dohle_siegrist$foo <- "foo"

  expect_error(
    mdt_within(dohle_siegrist,
               foo,
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
