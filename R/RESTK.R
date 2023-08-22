#' RESTK
#'
#' \code{RESTK} function used to project the maxk line into the probabilities of interest
#' @param validation_data validation data
#' @param training_data training data
#' @param probs Probabilities where maxk was evaluated
#' @param probs_interest Probabilities of interest to estimate
#' @param bootstrap_size size of bootstrap simulations on the training data
#' @param bootstrap_training_sims number of bootstrap simulations on the training data
#' @param bootstrap_validation_sims number of bootstrap simulations on the validation data
#' @return Returns the maxk line for the probabilities of interest
#' @keywords RESTK
#' @export
#' @examples
#' training_data <- rnorm(1e3, mean = 100, sd = 10)
#' validation_data <- rnorm(1e3, mean = 100, sd = 10)
#' bootstrap_size <- 1000
#' bootstrap_training_sims <- 10
#' bootstrap_validation_sims <- 10
#' probs <- c(1-1e-1, 1-0.5e-1, 1-1e-2)
#' probs_interest <- c(1-1e-6, 1-1e-7)
#' maxk_line <- c(100, 125, 150)
#'
#' estimated_quants <- RESTK(training_data = training_data,
#'                           validation_data = validation_data,
#'                           probs = probs,
#'                           probs_interest = probs_interest,
#'                           bootstrap_size = bootstrap_size,
#'                           bootstrap_training_sims = bootstrap_training_sims,
#'                           bootstrap_validation_sims = bootstrap_validation_sims)

RESTK <- function(training_data = NULL,
                  validation_data = NULL,
                  probs = NULL,
                  probs_interest = NULL,
                  bootstrap_size = NULL,
                  bootstrap_training_sims = NULL,
                  bootstrap_validation_sims = NULL) {
  maxk_line <- RESTK::RESTK_training(
    training_data = training_data,
    probs = probs,
    probs_interest = probs_interest,
    bootstrap_size = bootstrap_size,
    bootstrap_training_sims = bootstrap_training_sims
  )

  estimated_quants <- RESTK::RESTK_validation(
    validation_data = validation_data,
    maxk_line = maxk_line,
    probs_interest = probs_interest,
    bootstrap_size = bootstrap_size,
    bootstrap_validation_sims = bootstrap_validation_sims
  )
  return(estimated_quants)
}
