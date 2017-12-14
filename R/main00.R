# Importing files
source("R/modelling.R")
source("R/mutation.R")
# source("selectparent0.R")
source("R/selectparent.R")
source("R/cross_p_split.R")
source("R/replace population.R")

# make one ieteration
one_iteration<-function(data,
                        population,
                        method,
                        p,
                        mutation_rate,
                        regression_target,
                        scheme){

  ###add fitness to population
  population = compute_population_goodness_of_fit(data = data,
                                                  population = population,
                                                  regression_target = regression_target)
  ###select parents
  selected_parents <-selectparents(originalparents = as.matrix(population)[, (1:(dim(population)[2]-1))],
                                   couplenum = 10,
                                   method = method,
                                   fitness = population[,dim(population)[2]],
                                   subsetnum = 4)
  ###get new population with fitness
  new_generation <- crossover_p_split(parents = selected_parents, p = p)
  mutated_offsprings <- generate_mutation(input = new_generation, mutation_rate = mutation_rate,
                                         main_dataset = data, regression_target = regression_target)

  new_population<-get_next_population (population, mutated_offsprings, scheme=scheme)
  #replace
  return(new_population)
}

##### final function
main<-function(data,
               method,
               p,
               mutation_rate,
               regression_target,
               scheme){
  #' Main function
  #'
  #' Returns the optimal x variable and every generation's AIC values
  #'
  #' @author Jinhui Xu, Xiao Li
  #' @param data data.frame: regression_target and covariates
  #' @param method the method to select the parents, "twopropselection", "onepropselection" or "tournament"
  #' @param regression_target character: Y variable
  #' @param p integer: splits number in crossover
  #' @param mutation_rate numeric: the rate of mutation in population
  #' @param scheme method of replacing population, "proportion" or "re-rank"
  #' @example main(data= mtcars, regression_target = 'mpg', p=2, mutation_rate=0.01)
  #' @return optimal x variable and every generation's AIC values



  ###set original population
  ncols<-dim(data)[2]-1
  population <- matrix(rbinom(n = 100* ncols, prob = 0.5, size = 1), ncol = ncols)


  AIC<-list()
  iteration<-1
  error <- 1

  while(iteration<=30){

    if(iteration==1){
      old_AIC<-rep(0,dim(population)[1])
    }
    else old_AIC<-population_withfit[,dim(population_withfit)[2]]


    population_withfit<-one_iteration(data = data,
                                      population= population,
                                      method = method,
                                      p=p,
                                      mutation_rate=mutation_rate,
                                      regression_target=regression_target,
                                      scheme = scheme)

    population<-population_withfit[,1:(dim(population_withfit)[2]-1)]

    error<-crossprod(sort(population_withfit[,dim(population_withfit)[2]])-sort(old_AIC))

    AIC[[iteration]]<-population_withfit[,dim(population_withfit)[2]]

    iteration=iteration+1
  }

  ####calculate the final selected x varible and store in list
  selected_columns <- as.logical(population[1, ])
  y_variable<-split_data(dataset = data, regression_target)[[1]]
  x_variable<-split_data(dataset = data, regression_target)[[2]]
  x_selected<-x_variable[selected_columns]

  return(list(AIC,as.data.frame(cbind(y_variable,x_selected))))
}

# m <- main(data=main_dataset,
#           method='tournament',
#           p=2,
#           mutation_rate = 0.01,
#           regression_target = 'V2',
#           scheme = "re-rank")

# mm <- matrix(unlist(m),nrow = 6)
# pts <- data.frame()
# for(i in 1:nrow(mm)){
#   pts <- rbind(pts,data.frame(x=rep(i,length(mm[i,])),y=mm[i,]))
# }
# pts

#  library(ggplot2)
#  ggplot(data = pts, aes(x=x,y=y)) +
#  geom_point() +
#  geom_smooth()

####  test
xxx<-main(data=mtcars,
          method = "onepropselection",
          p=2,
          mutation_rate = 0.01,
          regression_target = 'mpg',
          scheme = "re-rank")

