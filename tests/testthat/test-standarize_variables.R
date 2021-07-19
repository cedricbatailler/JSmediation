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

test_that("suffix argument work as intended", {

  scaled_dataset <-
    mtcars %>%
    standardize_variables(cyl, suffix = "foo")

  expect_true("cyl_foo" %in% names(scaled_dataset))

  scaled_dataset <-
    mtcars %>%
    standardize_variables(c(cyl, disp),
                          suffix = "foo")

  expect_true("cyl_foo" %in% names(scaled_dataset))
  expect_true("disp_foo" %in% names(scaled_dataset))

})

test_that("standardize_variables keeps grouping", {
  dataset_grouped <-
    dplyr::group_by(mtcars, cyl)

  dataset_standardized <-
    dataset_grouped %>%
    standardize_variables(mpg)

  expect_identical(dplyr::groups(dataset_standardized),
                   dplyr::groups(dataset_grouped))
})

test_that("standardize_variables is insensitive to grouping", {
  dataset_grouped <-
    dplyr::group_by(mtcars, cyl)

  dataset_grouped_standardized <-
    dataset_grouped %>%
    standardize_variables(mpg) %>%
    dplyr::ungroup()

  dataset_notgrouped_standardized <-
    mtcars %>%
    standardize_variables(mpg)

  expect_equal(dataset_grouped_standardized,
               dataset_notgrouped_standardized)
})
