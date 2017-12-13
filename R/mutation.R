generate_mutation <- function(input,
                              mutation_rate,
                              regression_target,
                              df){
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
  #'
  #' @examples # Generate random input and choose a specific dataset in R
  #' population <- as.data.frame(matrix(rbinom(n = 300, prob = .5, size = 1), ncol = 10))
  #' main_dataset <- as.data.frame.matrix(crimtab[, 1:10])
  #' generate_mutation(population, 0.01,main_dataset)
  #'

  if(!is.matrix(input)){
    stop("input should be a matrix.")
  }

  if(mutation_rate < 0 || mutation_rate > 1){
    stop("mutation_rate should be a number between 0 and 1.")
  }

  if(!is.data.frame(main_dataset)){
    stop("main_dataset should be a data frame.")
  }

  # Generate a copy of the input
  regression_index <- which(colnames(df)==regression_target)
  after_mutation <- input

  # The logic of this for loop:
  # To make a value change based on a value, I will do following simulation:
  # Generate a random number from Unif(0,1)
  # Compare the random number with mutation rate
  # If the random number is smaller than the mutation number
  # The corresponding gene will mutate
  # Otherwise, the value will stay the same
  rand <- runif(length(after_mutation))
  index <- which(rand < mutation_rate)
  after_mutation[index] <- abs(after_mutation[index]-1)
  after_mutation[,regression_index] <- 1

  offsprings <- as.data.frame(after_mutation)
  colnames(offsprings) <- colnames(df)

  return(offsprings)
}


