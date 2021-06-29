test_that("standardize_variables works on data frames", {

  expect_error(standardize_variables("foo"))
  expect_error(standardize_variables(mtcars, cyl), NA)

})

test_that("standardize_variables actually standardizes variables", {

  standardized_variable <- standardize_variables(mtcars, cyl)$cyl

  expect_equal(mean(standardized_variable), 0)
  expect_equal(sd(standardized_variable), 1)

  standardized_variables <- standardize_variables(mtcars, c(cyl, disp))

  expect_equal(mean(standardized_variables$cyl), 0)
  expect_equal(mean(standardized_variables$disp), 0)
  expect_equal(sd(standardized_variables$cyl), 1)
  expect_equal(sd(standardized_variables$disp), 1)

})

test_that("standardize_variables throws error for non-numeric variables", {

  expect_error(standardize_variables(ho_et_al, condition))

})
