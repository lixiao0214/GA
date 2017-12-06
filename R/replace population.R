replace_population1 <- function(parents,     # Generation t
                               offsprings,   # Generation t+1
                               pct           # percentage of generation t will be 
                                             # replaced by generation t+1
                               ){
  goodness_order <- order(parents$goodness_of_fit,decreasing = T)
  index <- goodness_order[1:length(goodness_order)*(1-pct)]
  next_generation <- rbind(parents[index,],offsprings)
  return(next_generation)
}


replace_population2 <- function(parents,      # Generation t
                                offsprings    # Generation t+1
                                ){
  total_population <- rbind(parents,offsprings)
  index <- order(total_population$goodness_of_fit,decreasing = T)
  next_index <- index[1:length(parents)]
  next_generation <- total_population[next_index,]
  return(next_generation)
}


