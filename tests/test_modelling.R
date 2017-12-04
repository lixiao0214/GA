library(testthat)
source('../modelling.R')

context("Modelling tests")

test_that("unittest: check_input does its work correctly", {
  expect_error(check_input(input = 42))
  expect_error(check_input(input = list()))
  expect_error(check_input(input = list(1, 2)))
  expect_error(check_input(input = list(1, data.frame())))
})

test_that("oracle test: get_goodness_of_fit behaves correctly on known dataset", {
  expect_equal(get_goodness_of_fit(df = airquality, regression_target = "Ozone"), 997.2188,
               tolerance = .001)
})
