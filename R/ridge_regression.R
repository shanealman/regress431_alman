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
#'
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

  colnames(A)[[1]] = 'Intercept'

  A <- cbind(A, lambda)

  results <- data.table(A)


  return(results)

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
#' @import data.table
#'
#' @export
find_best_lambda <- function(train_dat, test_dat, response, lambdas) {

  train_betas <- ridge_regression(train_dat, {{response}}, lambdas)

  test_dat <- scale(test_dat)

  fits <- test_dat %*% head(t(train_betas), -1)

  test_dat <- data.table(test_dat)

  y <- test_dat %>% pull({{response}})

  sse <- colSums((y - fits)^2)

  lambda_errors <- cbind(sse = sse, lambdas = rev(lambdas))

  return(lambda_errors)
}
