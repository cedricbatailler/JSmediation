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
