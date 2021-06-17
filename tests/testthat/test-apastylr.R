test_that("apastylr does not throw an error when used as intended", {
  data(ho_et_al)
  test <- lm(hypodescent ~ linkedfate, ho_et_al)
  expect_silent(apastylr(test, "linkedfate"))
})

test_that("Correctly reports p < .001", {
  data(ho_et_al)
  test <- lm(hypodescent ~ linkedfate, ho_et_al)
  expect_output(print(apastylr(test, "linkedfate")), "< .001")
})

test_that("apastylr throws a warning when `term` des not exists", {
  data(ho_et_al)
  test <- lm(hypodescent ~ linkedfate, ho_et_al)
  expect_error(apastylr(test, "somethingelse"))
})
