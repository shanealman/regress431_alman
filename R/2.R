#' Computes predicted values given coefficients
#'
#' This function takes a data frame of coefficients in the form outputted by
#' functions like \code{multiple_linear_regression} or \code{ridge_regression}.
#'
#' It calculates the predicted values of a response variable from these coefficients.
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param coefs A data frame of coefficient estimates
#'
#' @return A data frame of true and predicted values
#'
#' @import dplyr
#' @import data.table
#'
#' @export
predict_from_coefs <- function(dat, response, coefs){

  dt <- data.table(dat)
  y <- dt %>%  pull({{response}})
  x <- dt %>% select(-{{response}})

  x <- cbind(1, x)
  x <- as.matrix(x)

  predictions <- x %*% t(coefs)

  results <- data.frame(True_Values = y, Predicted_Values = predictions)

  return(results)
}

