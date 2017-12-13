##### the only data information we get from user is a dataset(dataframe) and target_names(character)

####get dataset and target_names, output Y_variable(one column), X_variable(feature matrix)
split_data<-function(dataset, target_name){
  y_variable<-dataset[target_name]
  x_variable<-setdiff(dataset,y_variable)
  return(list(y_variable,x_variable))
}

get_goodness_of_fit <- function(df, regression_target,x_names, criterion = "AIC") {

  stopifnot(is.data.frame(df))
  stopifnot(is.character(regression_target))
  stopifnot(is.character(criterion))

  # Get the function
  criterion_function <- match.fun(criterion)

  # Build the formula
  formula <- as.formula(paste(regression_target, "~", paste(x_names, collapse = " + ")))
  lm_fit <- lm(formula = formula, data = df)

  goodness_of_fit <- criterion_function(lm_fit)
  return(goodness_of_fit)
}

compute_population_goodness_of_fit <- function(data, population, regression_target,
                                               criterion = "AIC", verbose = FALSE) {

  stopifnot(is.data.frame(data))
  stopifnot(is.character(regression_target))
  stopifnot(is.character(criterion))

  # Make sure there is more than one column
  stopifnot(dim(data)[2] > 1)


  # Compute goodness of fit for each element of the population
  if (verbose) print('Computing goodness of fit for population')

  goodness_of_fit_values <- sapply(1:nrow(population), function(i){
    if (verbose) print(paste("Processing element:", i))
    selected_columns <- as.logical(population[i, ])
    if (sum(selected_columns) == 1) {
      # We have only one column
      print("Only one column in regression data.frame, exiting.")
      stop()
    }
    y_variable<-split_data(data, regression_target)[[1]]
    x_variable<-split_data(data, regression_target)[[2]]
    x_names<-names(x_variable[, selected_columns])
    subset_data<-cbind(y_variable,x_variable[, selected_columns])
    return(get_goodness_of_fit(df = subset_data, regression_target = regression_target,x_names, criterion = criterion))
  })
  population<-cbind(population,goodness_of_fit_values)
  return(population)
}
