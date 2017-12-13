# Importing files
source("R/modelling.R")
source("R/mutation.R")
source("R/selectparent.R")
source("R/cross_p_split.R")
source("R/replace population.R")


# Testing
# Parameters
regression_target <- "V1"
n_cols = 8
n_rows = 100
n_population = 300

# Generate init dataset
toy_datasets <- generate_toy_dataset(n_cols = n_cols,
                                     n_rows = n_rows,
                                     n_population = n_population,
                                     regression_target = regression_target)
main_dataset <- toy_datasets$main_dataset
population <- toy_datasets$population

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
                                                  regression_target = "V1")

  # Parent selection
  selected_parents <- selectparents(originalparents = as.matrix(population[, names(population) != "goodness_of_fit"]),
                                    couplenum = couplenum,
                                    method = method,
                                    fitness = population[, "goodness_of_fit"],
                                    subsetnum = subsetnum
  )
  # Crossover & Mutation
  new_generation <- crossover_p_split(parents = selected_parents,
                                      p = p)
  mutated_offspring <- generate_mutation(input = new_generation,
                                         mutation_rate = mutation_rate,
                                         main_dataset = main_dataset)
  next_population <- get_next_population(population_new,
                                         mutated_offspring,
                                         scheme = "re-rank")
  return(next_population)
}

run_one_iteration(main_dataset = main_dataset,
                  population = population,
                  regression_target = "V1",
                  criterion = "BIC",
                  couplenum = 10,
                  method = "tournament",
                  subsetnum = 4,
                  p = 2,
                  mutation_rate = 0.01,
                  scheme = "proportion")
