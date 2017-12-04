#
# Modelling related functions
#
# Modelling receives the population data and outputs goodness-of-fit for each element of the population
# Input: list of (boolean data.frame with column 'features' and rows element of the population)
# Output: list of (boolean data.frame with column 'features' and rows element of the population, goodness-of-fit)


# To update
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

get_goodness_of_fit <- function(df, regression_target) {
  #' Goodness of Fit
  #'
  #' Returns the goodness-of-fit for regression_target ~ df
  #' @author Louis Rémus
  #' @param df data.frame: regression_target and covariates
  #' @param regression_target string: column of df regressed on other covariates in df

  stopifnot(is.data.frame(df))
  stopifnot(is.character(regression_target))

  # list of covariates
  col_names <- names(df)
  col_names[col_names != regression_target]
  # Build the formula

  formula <- as.formula(paste(regression_target, "~", paste(col_names, collapse = " + ")))
  lm_fit <- lm(formula = formula, data = df)
  goodness_of_fit <- AIC(lm_fit)
  stopifnot(all.equal(goodness_of_fit, AIC(logLik(lm_fit))))
  return(goodness_of_fit)
}

