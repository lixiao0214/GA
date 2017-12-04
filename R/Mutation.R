# Set the initial mutation rate = 0.01
mutation_rate <- 0.01

Mutation <- function(input, mutation_rate){
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

  return(after_mutation)
}


# test
m <- matrix(rep(c(0,1), 30000), ncol = 30)
system.time(Mutation(m, 0.01))
