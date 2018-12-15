context("test-build_contrast.R")

test_that("returns double", {
  vector <- c("a", "b", "a", "a")

  contrasts <- build_contrast(vector, "a", "b")

  expect_type(contrasts, "double")
})

test_that("returns two codes only", {
  vector <- c("a", "b", "a", "a")

  contrasts <- build_contrast(vector, "a", "b")
  expect_length(unique(contrasts), 2L)
})

test_that("does not throw an error", {
  vector <- c("a", "b", "a", "a")
  expect_silent(build_contrast(vector, "a", "b"))
})

test_that("default method does not throw an error", {
  vector <- c(1, 1, 2)
  expect_silent(build_contrast(vector, "1", "2"))
})
