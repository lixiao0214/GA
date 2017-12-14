library(testthat)

# Generate random dataset
regression_target <- "V1"
toy_datasets <- generate_toy_dataset(regression_target = regression_target)
main_dataset <- toy_datasets$main_dataset
population <- as.matrix(toy_datasets$population)
after_mutation <- generate_mutation(population, 0.01, main_dataset,regression_target)

test_that("test generate_mutation workds",{
  expect_true(is.matrix(after_mutation))
  expect_true(nrow(after_mutation)==50)
  expect_true(ncol(after_mutation)==7)
  })
