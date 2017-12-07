# Set the initial mutation rate = 0.01
mutation_rate <- 0.01

generate_mutation <- function(input,
                              mutation_rate,
                              main_dataset){

  #' test
  #' @author Xiao Li
  # Generate a copy of the input
  after_mutation <- input

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
  df_mutation <- as.data.frame(after_mutation)

  names(df_mutation) <- c('col_1', 'col_2', 'col_3', 'col_4', 'col_5','col_6', 'col_7', 'col_8', 'col_9', 'col_10')
  regression_target <- "col_1"
  df_mutation[regression_target] = 1
  browser()
  offsprings <- compute_population_goodness_of_fit(data = main_dataset,
                                                   population = df_mutation,
                                                   regression_target = 'col_1')
  return(offsprings)
}


# Test
m <- matrix(rbinom(n = 300, prob = .5, size = 1), ncol = 10)
generate_mutation(m, 0.01)
system.time(generate_mutation(m, 0.01))
