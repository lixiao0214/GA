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
  stopifnot(is.list(input))
  lapply(input, stopifnot(is.data.frame))
}

# Unit function
get_goodness_of_fit <- function(df, regression_target, criterion = "AIC") {
  #' Goodness of Fit
  #'
  #' Returns the goodness-of-fit for regression_target ~ df
  #' @author Louis Rémus
  #' @param df data.frame: regression_target and covariates
  #' @param regression_target character: column of df regressed on other covariates in df
  #' @param criterion character: goodness of fit criterion
  #' @example get_goodness_of_fit(df = subset_data, regression_target = regression_target, criterion = "AIC")

  stopifnot(is.data.frame(df))
  stopifnot(is.character(regression_target))
  stopifnot(is.character(criterion))

  # list of covariates
  col_names <- names(df)
  col_names <- col_names[col_names != regression_target]

  # Build the formula
  formula <- as.formula(paste(regression_target, "~", paste(col_names, collapse = " + ")))
  lm_fit <- lm(formula = formula, data = df)
  if (criterion == "AIC") {
    goodness_of_fit <- AIC(lm_fit)
    # Quick check
    stopifnot(all.equal(goodness_of_fit, AIC(logLik(lm_fit))))
  }
  else {
    stop("Unknown goodness of fit criterion")
  }
  return(goodness_of_fit)
}

goodness_of_fit <- function(data, population, regression_target,
                                               criterion = "AIC", verbose = FALSE) {
  #' Compute Population AIC
  #'
  #' Returns the goodness-of-fit for the given population
  #' @author Louis Rémus
  #' @param data data.frame: regression_target and covariates
  #' @param population data.frame: selected column per element of the population
  #' @param regression_target character: column of df regressed on other covariates in df
  #' @param criterion character: goodness of fit criterion
  #' @example
  #' population_new = compute_population_goodness_of_fit(data = main_dataset, population = population, regression_target = 'col_1')


  stopifnot(is.data.frame(data))
  stopifnot(is.data.frame(population))
  stopifnot(is.character(regression_target))
  stopifnot(is.character(criterion))
  # Make sure we always have the regression_target column
  stopifnot(all(population[regression_target] == 1))

  # Compute goodness of fit for each element of the population
  if (verbose) print('Computing goodness of fit for population')

  goodness_of_fit_values <- sapply(1:nrow(population), function(i){
    if (verbose) print(paste("Processing element:", i))
    selected_columns <- as.logical(population[i, ])
    subset_data <- main_dataset[, selected_columns]
    return(get_goodness_of_fit(df = subset_data, regression_target = regression_target, criterion = criterion))
  })
  population['goodness_of_fit'] <- goodness_of_fit_values
  return(population)
}

# Testing
# Generate init dataset
main_dataset <- as.data.frame.matrix(crimtab[, 1:10])
names(main_dataset) <- c('col_1', 'col_2', 'col_3', 'col_4', 'col_5', 'col_6', 'col_7', 'col_8', 'col_9', 'col_10')

population <- as.data.frame(matrix(rbinom(n = 300, prob = .5, size = 1), ncol = 10))
names(population) <- c('col_1', 'col_2', 'col_3', 'col_4', 'col_5', 'col_6', 'col_7', 'col_8', 'col_9', 'col_10')

# Parameters
regression_target <- "col_1"

# Make sure we always have the regression_target column in our regressions
population[regression_target] = 1

population_new = compute_population_goodness_of_fit(data = main_dataset,
                                                    population = population,
                                                    regression_target = 'col_1')

