% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RESTK_validation.R
\name{RESTK_validation}
\alias{RESTK_validation}
\title{RESTK Validation}
\usage{
RESTK_validation(
  validation_data = NULL,
  maxk_line = NULL,
  probs_interest = NULL,
  bootstrap_size = NULL,
  bootstrap_validation_sims = NULL
)
}
\arguments{
\item{validation_data}{validation data}

\item{maxk_line}{maxk line obtained from RESTK_training}

\item{probs_interest}{Probabilities of interest to estimate}

\item{bootstrap_size}{size of bootstrap simulations on the validation data}

\item{bootstrap_validation_sims}{number of bootstrap simulations on the validation data}
}
\value{
Returns the estimated quantiles from the probabilities of interest
}
\description{
\code{RESTK_validation} main function for the validation of the RESTK methodology by using the maxk line
}
\examples{
validation_data <- rnorm(1e3, mean = 100, sd = 10)
probs_interest <- c(1-1e-6, 1-1e-7)
bootstrap_size <- 1000
bootstrap_validation_sims <- 100
maxk_line <- c(100, 125, 150)
estimated_quants <- RESTK_validation(validation_data = validation_data,
                                     maxk_line = maxk_line,
                                     probs_interest = probs_interest,
                                     bootstrap_size = bootstrap_size,
                                     bootstrap_validation_sims = bootstrap_validation_sims)
}
\keyword{RESTK}
