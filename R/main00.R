# Importing files
source("R/modelling.R")
source("R/mutation.R")
# source("selectparent0.R")
source("R/selectparent.R")
source("R/cross_p_split.R")
source("R/replace population.R")

# Generate init dataset
main_dataset <- as.data.frame.matrix(crimtab[, 1:10])
names(main_dataset) <- c('col_1', 'col_2', 'col_3', 'col_4', 'col_5', 'col_6', 'col_7', 'col_8', 'col_9', 'col_10')

population <- as.data.frame(matrix(rbinom(n = 300, prob = .5, size = 1), ncol = 10))
names(population) <- c('col_1', 'col_2', 'col_3', 'col_4', 'col_5', 'col_6', 'col_7', 'col_8', 'col_9', 'col_10')

# Parameters
regression_target <- 'col_1'

# Make sure we always have the regression_target column
population[regression_target] = 1

# make one ieteration
one_ieteration<-function(data, population, p, mutation_rate){
  population = compute_population_goodness_of_fit(data = main_dataset,
                                                  population = population,
                                                  regression_target = 'col_1')
  selected_parents <-selectparents(originalparents = as.matrix(population)[, 1:10],
                                   couplenum = 10,
                                   method = 'tournament',
                                   fitness = population[,"goodness_of_fit"],
                                   subsetnum = 4
  )

  new_generation <- crossover_p_split(parents = selected_parents, p = p)
  mutated_offspring <- generate_mutation(input = new_generation, mutation_rate = mutation_rate,
                                         main_dataset = main_dataset)
  return(mutated_offspring)
}

##### final function
main<-function(data, population, p, mutation_rate){
  AIC<-list()
  iteration<-1
  error <- 1
  while(iteration<=1000 && error>0.0001){
#    old_AIC<-mutated_offspring[,dim(mutated_offspring)[2]]
#    population<-mutated_offspring[,1:(dim(mutated_offspring)[2]-1)]
    if(iteration==1){
      old_AIC<-rep(0,dim(mutated_offspring)[1])
    }
    else old_AIC<-mutated_offspring[,dim(mutated_offspring)[2]]


    mutated_offspring<-one_ieteration(data = data,population= population, p=p, mutation_rate=mutation_rate)

    population<-mutated_offspring[,1:(dim(mutated_offspring)[2]-1)]

    error<-crossprod(sort(mutated_offspring[,dim(mutated_offspring)[2]])-sort(old_AIC))

    AIC[[iteration]]<-mutated_offspring[,dim(mutated_offspring)[2]]

    iteration=iteration+1
  }
  return(AIC)
}

m <- main(data=main_dataset, population =population, p=2, mutation_rate = 0.01)
mm <- matrix(unlist(m),nrow = 6)
pts <- data.frame()
for(i in 1:nrow(mm)){
  pts <- rbind(pts,data.frame(x=rep(i,length(mm[i,])),y=mm[i,]))
}
pts

library(ggplot2)
ggplot(data = pts, aes(x=x,y=y)) +
  geom_point() +
  geom_smooth()
