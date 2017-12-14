library(testthat)

context("Modelling tests")

test_that("unittest: check_input does its work correctly", {
  expect_error(check_input(input = 42))
  expect_error(check_input(input = list()))
  expect_error(check_input(input = list(1, 2)))
  expect_error(check_input(input = list(1, data.frame())))
})

test_that("unittest: test get_goodness_of_fit input types", {
  expect_error(get_goodness_of_fit(df = 1,
                                   regression_target = 'good_input',
                                   x_names = c('good_input', 'good_input'),
                                   criterion = 'good_input'))
  expect_error(get_goodness_of_fit(df = data.frame(),
                                   regression_target = 1,
                                   x_names = c('good_input', 'good_input'),
                                   criterion = 'good_input'))
  expect_error(get_goodness_of_fit(df = data.frame(),
                                   regression_target = 'good_input',
                                   x_names = 1,
                                   criterion = 'AIC'))
  expect_error(get_goodness_of_fit(df = data.frame(),
                                   regression_target = 'good_input',
                                   x_names = c('good_input', 'good_input'),
                                   criterion = AIC))
})

test_that("unittest: test compute_population_goodness_of_fit input types", {
  expect_error(compute_population_goodness_of_fit(data = 1,
                                                  population = data.frame(),
                                                  regression_target = "good_input",
                                                  criterion = "good_input"))
  expect_error(compute_population_goodness_of_fit(data = data.frame(),
                                                  population = 1,
                                                  regression_target = "good_input",
                                                  criterion = "good_input"))
  expect_error(compute_population_goodness_of_fit(data = data.frame(),
                                                  population = data.frame(),
                                                  regression_target = 1,
                                                  criterion = "good_input"))
  expect_error(compute_population_goodness_of_fit(data = data.frame(),
                                                  population = data.frame(),
                                                  regression_target = "good_input",
                                                  criterion = 1))
  }
  )


# test_that("oracle test: get_goodness_of_fit behaves correctly on known dataset", {
#   expect_equal(get_goodness_of_fit(df = airquality, regression_target = "Ozone"), 997.2188,
#                tolerance = .001)
# })
