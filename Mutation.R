# Set the initial mutation rate = 0.01
mutation_rate <- 0.01

# I assume the input will be in vector form first
# Later I will modify it to other data type
Mutation <- function(input,mutation_rate){
  # Generate an empty vector to store gene after mutation
  after_mutation <- c()
  
  # Generate an empty vector for further use of random sample
  rand <- c()
  
  # The logic of this for loop:
  # To make a value change based on a value, I will do following simulation:
  # Create size (1/desired prob) vector, with (1/desired prob)-1 values are the same
  # One of the values is different from all other values
  # Then we random sample one value from this new vector
  # The for-loop is working to such algorithm to every element in the vector
  for(i in 1:length(input)){
    if(t1[i]==0) {
      rand <- c(rep(input[i],round(1/mutation_rate)-1),1)
    } else {
      rand <- c(rep(input[i],round(1/mutation_rate)-1),0)
    }
    after_mutation[i] <- sample(rand,1)
  }
  
  return(after_mutation)
}


# test
set.seed(123)
t1 <- sample(c(0,1),10,replace = T)
t1
Mutation(t1,0.01)

