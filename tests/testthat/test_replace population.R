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
population <- toy_datasets$population
population$"goodness_of_fit_values" <- runif(50,0,100)
offsprings <- population[sample(1:50,20),]

# Get next population using different methods
next_population1 <- get_next_population(population,offsprings,scheme = "re-rank")
next_population2 <- get_next_population(population,offsprings,scheme = "proportion")

# Test if re-rank works
test_that("test get_next_population using re-rank works",{
  expect_true(is.data.frame(next_population1))
  expect_true(nrow(population)==nrow(next_population1))
  expect_true(ncol(population)==ncol(next_population1))

  # test whether the values of goodness of fit is in ascending order
  expect_true(sum(order(next_population1$goodness_of_fit_values)==1:50)==50)
})

# Test if proportion works
test_that("test get_next_population using proportion works",{
  expect_true(is.data.frame(next_population1))
  expect_true(nrow(population)==nrow(next_population1))
  expect_true(ncol(population)==ncol(next_population1))

  # test whether the unreplaced values of goodness of fit is in ascending order
  expect_true(sum(order(next_population1$goodness_of_fit_values)==1:30)==30)
})
