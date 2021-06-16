test_that("mdt_within_wide does not throw an error when used as intended", {
  expect_silent(
    mdt_within_wide(dohle_siegrist_wide,
                    willingness_c,
                    willingness_s,
                    hazardousness_c,
                    hazardousness_s)
  )
})

test_that("mdt_within_wide throws error when the variable type is not the one expected", {
  dataset <- 
    data.frame(
      1:30,
      DVA = c("a", "b"),
      DVB = c("a", "b"),
      MA  = c("a", "b"),
      MB  = c("a", "b")
    )
  
  expect_error(
    mdt_within_wide(dataset,
                    DVA,
                    DVB,
                    MA,
                    MB)
  )
})
