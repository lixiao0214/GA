#
# Modelling related functions
#
# Modelling receives the population data and outputs goodness-of-fit for each element of the population
# Input: list of (boolean data.frame with column 'features' and rows element of the population)
# Output: list of (boolean data.frame with column 'features' and rows element of the population, goodness-of-fit)


# Deprecated
check_input <- function(input) {
  #' Checking Modelling Input
  #'
  #' Checks whether the input is the appropriate format
  #' @author Louis Rémus
  #' @param input list of data.frames
  #' @examples
  #' check_input(input = list(data.frame(a=1), data.frame(b=2)))
  #' @return Nothing
  stopifnot(is.list(input))
  lapply(input, stopifnot(is.data.frame))
}

# Unit function
get_goodness_of_fit <- function(df, regression_target, criterion = "AIC") {
  #' Goodness of Fit
  #'
  #' Returns the goodness-of-fit for regression_target ~ df
  #'
  #' @author Louis Rémus
  #' @param df data.frame: regression_target and covariates
  #' @param regression_target character: column of df regressed on other covariates in df
  #' @param criterion character: goodness of fit criterion
  #' @example get_goodness_of_fit(df = subset_data, regression_target = regression_target, criterion = "AIC")
  #' @return Goodness of fit value for the given regression problem


  stopifnot(is.data.frame(df))
  stopifnot(is.character(regression_target))
  stopifnot(is.character(criterion))

  # Get the function
  criterion_function <- match.fun(criterion)

  # list of covariates
  col_names <- names(df)
  if (all(col_names != regression_target)) {
    stop('regression_target not in regression data.frame')
  }
  col_names <- col_names[col_names != regression_target]

  # Build the formula
  formula <- as.formula(paste(regression_target, "~", paste(col_names, collapse = " + ")))
  lm_fit <- lm(formula = formula, data = df)

  goodness_of_fit <- criterion_function(lm_fit)
  return(goodness_of_fit)
  }

compute_population_goodness_of_fit <- function(data, population, regression_target,
                                               criterion = "AIC", verbose = FALSE) {
  #' Compute Population Goodness of Fit
  #'
  #' Returns the goodness of fit for the given population
  #' @author Louis Rémus
  #' @param data data.frame: regression_target and covariates
  #' @param population data.frame: selected column per element of the population
  #' @param regression_target character: column of df regressed on other covariates in df
  #' @param criterion character: goodness of fit criterion
  #' @example
  #' population_new = compute_population_goodness_of_fit(data = main_dataset, population = population, regression_target = 'col_1')
  #' @return Returns the goodness of fit for the given population

  stopifnot(is.data.frame(data))
  stopifnot(is.data.frame(population))
  stopifnot(is.character(regression_target))
  stopifnot(is.character(criterion))

  # Make sure we always have the regression_target column
  stopifnot(all(population[regression_target] == 1))
  # Make sure there is more than one column
  stopifnot(dim(data)[2] > 1)

  col_names <- names(population)
  # Cast columns type to numeric
  population = as.data.frame(sapply(population, as.numeric))
  names(population) <- col_names
  stopifnot(is.data.frame(population))

  # Compute goodness of fit for each element of the population
  if (verbose) print('Computing goodness of fit for population')

  goodness_of_fit_values <- sapply(1:nrow(population), function(i){
    if (verbose) print(paste("Processing element:", i))
    selected_columns <- names(population)[which(population[i, ] == 0)]
    if (length(selected_columns) == 1) {
      # We have only one column
      stop("Only one column in regression data.frame")
    }
    subset_data <- main_dataset[, selected_columns]
    return(get_goodness_of_fit(df = subset_data, regression_target = regression_target, criterion = criterion))
  })
  population['goodness_of_fit'] <- goodness_of_fit_values
  return(population)
}

generate_toy_dataset <- function(n_cols = 7, n_rows = 100, n_population = 100, regression_target = "V1") {
  #' Generate a toy dataset
  #'
  #' Returns a toy dataset in the appropriate format
  #' Enforce 1s in the regression_target column, and sum of row for population > 1 (otherwise no regression)
  #'
  #' @author Louis Rémus
  #' @param n_cols : number of columns of the regression dataset
  #' @param n_rows : number of rows of the regression dataset
  #' @param n_population : number of rows of the population dataset
  #' @param regression_target character: column of df regressed on other covariates in df
  #' @example
  #' @return main_dataset, population_dataset

  stopifnot(is.character(regression_target))

  main_dataset <- as.data.frame(matrix(runif(n = n_rows * n_cols), nrow = n_rows, ncol = n_cols))

  population <- as.data.frame(matrix(rbinom(n = n_population * n_cols, prob = .5, size = 1), ncol = n_cols))

  # Make sure we always have the regression_target column in our regressions
  population[regression_target] = 1

  # Make sure we do not have rows with sum of bool = 1 (means no regression)
  if (any(rowSums(population) <= 1)) {
    # list of covariates
    col_names <- names(population)
    col_names <- col_names[col_names != regression_target]
    # Put a one in the first covariate
    population[, col_names][rowSums(population[, col_names]) == 0, 1] = 1
  }

  return(list(population = population[, col_names],
              main_dataset = main_dataset))
}


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


population_new <- compute_population_goodness_of_fit(data = main_dataset,
                                                    population = population,
                                                    regression_target = "V1")

