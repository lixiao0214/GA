# Importing files
source("R/modelling.R")
source("R/mutation.R")
# source("selectparent0.R")
source("R/selectparent1.R")
source("R/cross_p_split.R")


# Generate init dataset
main_dataset <- as.data.frame.matrix(crimtab[, 1:10])
names(main_dataset) <- c('col_1', 'col_2', 'col_3', 'col_4', 'col_5', 'col_6', 'col_7', 'col_8', 'col_9', 'col_10')

population <- as.data.frame(matrix(rbinom(n = 300, prob = .5, size = 1), ncol = 10))
names(population) <- c('col_1', 'col_2', 'col_3', 'col_4', 'col_5', 'col_6', 'col_7', 'col_8', 'col_9', 'col_10')

# Parameters
regression_target <- 'col_1'

# Make sure we always have the regression_target column
population[regression_target] = 1

# Modelling
population = compute_population_goodness_of_fit(data = main_dataset,
                                                    population = population,
                                                    regression_target = 'col_1')


# Parent selection
selectparents(originalparents = as.matrix(population)[, 1:10],
              couplenum = 10,
              method = 'onepropselection',
              fitness = population[,"goodness_of_fit"])

# Sample selected parents
selected_parents <- list(as.data.frame(matrix(rbinom(n = 20, prob = .5, size = 1), ncol = 10, nrow = 2)),
                         as.data.frame(matrix(rbinom(n = 20, prob = .5, size = 1), ncol = 10, nrow = 2)))

# Crossover & Mutation
new_generation <- crossover_p_split(parents = selected_parents, p = 2)
mutated_offspring <- generate_mutation(input = new_generation, mutation_rate = .01,
                                       main_dataset = main_dataset)
