#' Implements ridge regression with many predictors
#'
#' This function computes coefficients for ridge regression
#' All columns of the provided data frame are used as predictors, except the
#' one specified as a response.
#'
#' No interaction terms are included.
#'
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param lambda A vector of penalty terms to try
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#' @import data.table
#'
#' @export
ridge_regression <- function(dat, response, lambda) {

  dat <- scale(dat)

  dt <- data.table(dat)
  y <- dt %>%  pull({{response}})
  x <- dt %>% select(-{{response}})

  x <- cbind(1, x)
  x <- as.matrix(x)

  betas <- function(lambda){A <- t(solve(t(x) %*% x + lambda * diag(ncol(x)), t(x) %*% y))
  return(A)}

  A <- lapply(lambda, betas)
  A <- Reduce(function(...) merge(..., all = T), A)

  rownames(A)[1] <- 'Intercpet'

  A <- cbind(lambda, z)

  results <- data.table(t(A))


  return(A)

}

#' Determines the best penalty term from a set of options
#'
#' This function uses a randomly chosen test and training set
#'
#' No interaction terms are included.
#'
#'
#' @param train_dat A data frame to construct the model from
#' @param test_dat A data frame to test the model on
#' @param response The name of a response variable in the data frame (unquoted)
#' @param lambda A vector of penalty terms to try
#'
#' @return A data frame of penalty terms and resulting errors
#'
#' @import dplyr
#'
#' @export
find_best_lambda <- function(train_dat, test_dat, response, lambdas) {


  ### lambda_errors should be a data frame with two columns: "lambda" and "error"
  ### For each lambda, you should record the resulting Sum of Squared error
  ### (i.e., the predicted value minus the real value squared) from prediction
  ### on the test dataset.

  return(lambda_errors)
}
