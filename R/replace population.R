get_next_population <- function(parents, offsprings, scheme, pct=NULL){
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
  #' @param pct it is used to first method to determine the proportion of original population to be replaced
  #'

  if(!is.data.frame(parents)){
    stop("parents should be a data frame")
  }

  if(!is.data.frame(offsprings)){
    stop("offsprings should be a data frame")
  }

  next_population <- c()
  if(scheme=="proportion"){
    goodness_order <- order(parents$goodness_of_fit,decreasing = F)
    index <- goodness_order[1:(length(goodness_order)*(1-pct))]
    next_population <- rbind(parents[index,],offsprings)
  }

  if(scheme=="re-rank"){
    total_population <- rbind(parents,offsprings)
    index <- order(total_population$goodness_of_fit,decreasing = F)
    next_index <- index[1:nrow(parents)]
    next_population <- total_population[next_index,]
  }
  return(next_population)
}


