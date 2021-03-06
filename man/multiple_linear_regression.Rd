% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/1.R, R/linear_regression.R
\name{multiple_linear_regression}
\alias{multiple_linear_regression}
\title{Implements linear regression with many predictors by hand}
\usage{
multiple_linear_regression(dat, response)

multiple_linear_regression(dat, response)
}
\arguments{
\item{dat}{A data frame}

\item{response}{The name of a response variable in the data frame (unquoted)}
}
\value{
A data frame of coefficients

A data frame of coefficients
}
\description{
This function computes coefficients for multiple regression by hand.
All columns of the provided data frame are used as predictors, except the
one specified as a response.

This function computes coefficients for multiple regression by hand.
All columns of the provided data frame are used as predictors, except the
one specified as a response.
}
\details{
No interaction terms are included.

No interaction terms are included.
}
