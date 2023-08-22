#' RESTK Training
#'
#' \code{RESTK_training} function used to project the maxk line into the probabilities of interest
#' @param training_data training data
#' @param probs Probabilities where maxk was evaluated
#' @param probs_interest Probabilities of interest to estimate
#' @param bootstrap_size size of bootstrap simulations on the training data
#' @param bootstrap_training_sims number of bootstrap simulations on the training data
#' @return Returns the estimated maxk line from the probabilities of interest
#' @keywords RESTK
#' @export
#' @examples
#' training_data <- rnorm(1e3, mean = 100, sd = 10)
#' probs <- c(1-1e-1, 1-0.5e-1, 1-1e-2)
#' probs_interest <- c(1-1e-6, 1-1e-7)
#' bootstrap_size <- 1000
#' bootstrap_training_sims <- 100
#'
#' maxk_line <- RESTK_training(training_data = training_data,
#'                            probs = probs,
#'                            probs_interest = probs_interest,
#'                            bootstrap_size = bootstrap_size,
#'                            bootstrap_training_sims = bootstrap_training_sims)

RESTK_training <- function(training_data = NULL,
                           probs = NULL,
                           probs_interest = NULL,
                           bootstrap_size = NULL,
                           bootstrap_training_sims = NULL) {
  quants <- RESTK::sample_quantile_estimation(samp = training_data, probs = probs, bootstrap_sims = bootstrap_training_sims)

  estimated_max <- data.frame(matrix(ncol = length(probs), nrow = bootstrap_training_sims))
  for (s in 1:bootstrap_training_sims) {
    samp <- sample(training_data, bootstrap_size, replace = T)
    estim_maxk <- RESTK::compute_maxk(samp = samp, probs = probs, quants = quants)
    estimated_max[s, ] <- estim_maxk
  }
  colnames(estimated_max) <- as.character(probs)

  min_maxk <- apply(estimated_max, 2, min)
  maxk_line <- RESTK::linear_adjust(min_maxk = min_maxk, probs = probs, probs_interest = probs_interest)
  return(maxk_line)
}
