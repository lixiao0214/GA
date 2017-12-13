generate_mutation <- function(input,
                              mutation_rate,
                              main_dataset,
                              regression_target){
  #' Generate offsprings after mutation
  #'
  #' @description generate_mutation is used to generate offsprings after mutations based on particular mutation rate.
  #' And it will give the values of goodness of fit for each offspring.
  #'
  #' @author Xiao Li
  #'
  #' @usage generate_mutation(input, mutation_rate, main_dataset)
  #'
  #' @param input A matrix contains all the populations which are required to mutate.
  #' The number in each cell should be either 0 or 1. Each row represents an individual.
  #' Each column represents the variable.
  #' @param mutation_rate The chance for each variable in each individual to change the value.
  #' @param main_dataset The dataset used to run the regression model.
  #' And its model help to get the corresponding value of goodness of fit.
  #' @param regression_target character: column of df regressed on other covariates in df
  #'
  #' @examples # Generate random input and choose a specific dataset in R
  #' regression_target <- "V1"
  #' toy_datasets <- generate_toy_dataset(regression_target = regression_target)
  #' main_dataset <- toy_datasets$main_dataset
  #' population <- toy_datasets$population
  #' generate_mutation(population, 0.01, main_dataset)

  # if (!is.matrix(input)) {
  #   stop("input should be a matrix.")
  # }

  if (mutation_rate < 0 || mutation_rate > 1) {
    stop("mutation_rate should be a number between 0 and 1.")
  }

  if (!is.data.frame(main_dataset)) {
    stop("main_dataset should be a data frame.")
  }

  # Generate a copy of the input without regression_target
  col_names <- names(main_dataset)
  col_names <- col_names[col_names != regression_target]
  after_mutation <- input[, col_names]

  # The logic of this for loop:
  # To make a value change based on a value, I will do following simulation:
  # Generate a random number from Unif(0,1)
  # Compare the random number with mutation rate
  # If the random number is smaller than the mutation number
  # The corresponding gene will mutate
  # Otherwise, the value will stay the same
  for (i in length(input)) {
    u <- runif(1)
    if (u <= mutation_rate) {
      after_mutation[i] <- abs(after_mutation[i] - 1)
    }
  }

  # Create df_mutation data.frame
  df_mutation <- after_mutation

  df_mutation[regression_target] = 1
  offsprings <- compute_population_goodness_of_fit(data = main_dataset,
                                                   population = df_mutation,
                                                   regression_target = regression_target)
  return(offsprings)
}

# Testing
# Parameters
regression_target <- "V1"
n_cols = 8
n_rows = 100
n_population = 300

# Generate init dataset
toy_datasets <- generate_toy_dataset(n_cols = n_cols,
                                     n_rows = n_rows,
                                     n_population = n_population,
                                     regression_target = regression_target)
main_dataset <- toy_datasets$main_dataset
population <- toy_datasets$population

generate_mutation(input = population, mutation_rate = 0.2,
                  main_dataset = main_dataset, regression_target = "V1")

