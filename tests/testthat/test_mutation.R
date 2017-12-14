library(testthat)
generate_toy_dataset <- function(n_cols = 7, n_rows = 100, n_population = 50, regression_target = "V1") {

  stopifnot(is.character(regression_target))

  main_dataset <- as.data.frame(matrix(runif(n = n_rows * n_cols), nrow = n_rows, ncol = n_cols))

  population <- as.data.frame(matrix(rbinom(n = n_population * n_cols, prob = .5, size = 1), ncol = n_cols))

  if (any(rowSums(population) <= 1)) {
    # list of covariates
    col_names <- names(population)
    col_names <- col_names[col_names != regression_target]
    # Put a one in the first covariate
    population[, col_names][rowSums(population[, col_names]) == 0, 1] = 1
  }

  return(list(population = population[, col_names],
              main_dataset = main_dataset))
}


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
