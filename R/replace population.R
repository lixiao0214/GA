get_next_population <- function(parents, offsprings, scheme){
  #' Generate next population
  #'
  #' @author Xiao Li
  #'
  #' @description get_next_population generate next population to use based on two methods.
  #' The first method is to replace the worst certain percent of original population by offsprings.
  #' The second method is to combine all parents and offsprings, and then choose the top individuals.
  #'
  #' @param parents a data frame contains the original population
  #' @param offsprings a data frame contains all offsprings
  #' @param scheme "proportion" returns the first method and "re-rank" returns the second method
  #'
  #' @example #Generate random input and choose a specific dataset in R
  #' regression_target <- "V1"
  #' toy_datasets <- generate_toy_dataset(regression_target = regression_target)
  #' main_dataset <- toy_datasets$main_dataset
  #' population <- toy_datasets$population
  #' population$"goodness_of_fit_values" <- runif(50,0,100)
  #' offsprings <- population[sample(1:50,20),]
  #' get_next_population(population,offsprings,scheme = "re-rank")

  next_population <- c()

  # Perform first method
  if(scheme=="proportion"){
    pct <- nrow(offsprings)/nrow(parents)
    goodness_order <- order(parents[,dim(parents)[2]],decreasing = F)
    index <- goodness_order[1:round(length(goodness_order)*(1-pct))]
    next_population <- rbind(parents[index,],offsprings)
  }

  # Perform second method
  if(scheme=="re-rank"){
    total_population <- rbind(parents,offsprings)
    index <- order(total_population$goodness_of_fit,decreasing = F)
    next_index <- index[1:nrow(parents)]
    next_population <- total_population[next_index,]
  }

  return(next_population)
}


