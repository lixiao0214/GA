# Importing files
source("modelling.R")
source("mutation.R")
source("selectparent0.R")
source("selectparent1.R")
source("cross_p_split.R")


# Generate init dataset
main_dataset <- as.data.frame.matrix(crimtab[, 1:10])
names(main_dataset) <- c('col_1', 'col_2', 'col_3', 'col_4', 'col_5', 'col_6', 'col_7', 'col_8', 'col_9', 'col_10')

population <- as.data.frame(matrix(rbinom(n = 300, prob = .5, size = 1), ncol = 10))
names(population) <- c('col_1', 'col_2', 'col_3', 'col_4', 'col_5', 'col_6', 'col_7', 'col_8', 'col_9', 'col_10')

# Parameters
regression_target <- 'col_1'


# Modelling
for (i in 1:nrow(population)) {
  paste('Processing:', i)
  # Extract the columns for element i of the population
  main_dataset
  # Compute goodness_of_fit
}
get_goodness_of_fit(df = df, regression_target = regression_target)

AIC_values <- runif(nrow(population))
population['AIC'] <- AIC_values

# Parent selection
selectparents(originalparents = as.matrix(population)[, 1:10],
              couplenum = 10,
              method = 'twopropselection',
              fitness = population['AIC'])

# Sample selected parents
selected_parents <- list(as.data.frame(matrix(rbinom(n = 20, prob = .5, size = 1), ncol = 10, nrow = 2)),
                         as.data.frame(matrix(rbinom(n = 20, prob = .5, size = 1), ncol = 10, nrow = 2)))

# Crossover & Mutation
new_generation <- crossover_p_split(parents = selected_parents, p = 2)
mutated_offspring <- generate_mutation(input = new_generation, mutation_rate = .01)
