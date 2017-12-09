# Importing files
source("R/modelling.R")
source("R/mutation.R")
# source("selectparent0.R")
source("R/selectparent1.R")
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

run_one_iteration <- function(main_dataset,
                              population,
                              regression_target,
                              criterion,
                              couplenum,
                              method,
                              subsetnum,
                              p,
                              mutation_rate,
                              scheme){
  #' Genetic Algorithm - One iteration
  #'
  #' Performs one iteration of our genetic algorithm
  #'
  #' @author Louis Rémus
  #' @param main_dataset data.frame: regression_target and covariates
  #' @param population data.frame: selected column per element of the population
  #' @param regression_target character: column of df regressed on other covariates in df
  #' @param criterion character: goodness of fit criterion
  #' @param couplenum number of couples selected
  #' @param method the method to select the parents, "twopropselection", "onepropselection" or "tournament"
  #' @param subsetnum In method "tournament", we partition the set of chromosomes in generation t into "subsetnum" disjoint subsets
  #' @param p Number of splits in the process of crossover.
  #' @param mutation_rate
  #' @example compute_population_goodness_of_fit(data = main_dataset,
  #' population = population,
  #' regression_target = 'col_1')
  #' @return mutated_offspring

  # Modelling
  population = compute_population_goodness_of_fit(data = main_dataset,
                                                  population = population,
                                                  regression_target = 'col_1')

  # Parent selection
  selected_parents <- selectparents(originalparents = as.matrix(population[, names(population) != "goodness_of_fit"]),
                                    couplenum = couplenum,
                                    method = method,
                                    fitness = population[,"goodness_of_fit"],
                                    subsetnum = subsetnum
  )

  # Sample selected parents
  # selected_parents <- list(as.data.frame(matrix(rbinom(n = 20, prob = .5, size = 1), ncol = 10, nrow = 2)),
                           # as.data.frame(matrix(rbinom(n = 20, prob = .5, size = 1), ncol = 10, nrow = 2)))

  # Crossover & Mutation
  new_generation <- crossover_p_split(parents = selected_parents,
                                      p = p)
  mutated_offspring <- generate_mutation(input = new_generation,
                                         mutation_rate = mutation_rate,
                                         main_dataset = main_dataset)
  next_population <- get_next_population(population_new,mutated_offspring,scheme="re-rank")
  return(next_population)
}

run_one_iteration(main_dataset = main_dataset,
                  population = population,
                  regression_target = "col_1",
                  criterion = "BIC",
                  couplenum = 10,
                  method = "tournament",
                  subsetnum = 4,
                  p = 2,
                  mutation_rate = 0.01,
                  scheme = "re-rank")

# Modelling
population = compute_population_goodness_of_fit(data = main_dataset,
                                                    population = population,
                                                    regression_target = 'col_1')


# Parent selection
selected_parents <- selectparents(originalparents = as.matrix(population)[, 1:10],
                                  couplenum = 10,
                                  method = 'tournament',
                                  fitness = population[,"goodness_of_fit"],
                                  subsetnum = 4
                                  )

# Sample selected parents
selected_parents <- list(as.data.frame(matrix(rbinom(n = 20, prob = .5, size = 1), ncol = 10, nrow = 2)),
                         as.data.frame(matrix(rbinom(n = 20, prob = .5, size = 1), ncol = 10, nrow = 2)))

# Crossover & Mutation
new_generation <- crossover_p_split(parents = selected_parents, p = 2)
mutated_offspring <- generate_mutation(input = new_generation, mutation_rate = .01,
                                       main_dataset = main_dataset)
next_population <- get_next_population(population_new,mutated_offspring,scheme="re-rank")



