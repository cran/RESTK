#' Estimate Quantiles within the Sample
#'
#' \code{sample_quantile_estimation} returns the estimated quantiles for the chosen probabilities from the input sample.
#' This method uses the sample quantile method number 8 from the default quantile function.
#' @param samp Sample of data to model
#' @param probs Probabilities of interest to generate the max_k line
#' @param bootstrap_sims Number of bootstrap simulations to estimate the quantiles
#' @return Returns estimated quantiles for the chosen probabilities.
#' @keywords RESTK
#' @export
#' @examples
#' samp <- rnorm(1e3, mean = 100, sd = 10)
#' probs <- c(1-1e-1, 1-0.5e-1, 1-1e-2)
#' bootstrap_training_sims <- 100
#' estimated_quantiles <- sample_quantile_estimation(samp = samp,
#'                                                   probs = probs,
#'                                                   bootstrap_sims = bootstrap_training_sims)

sample_quantile_estimation <- function(samp = NULL, probs = NULL, bootstrap_sims = NULL) {
  cumsum <- rep(0, length(probs))
  n_samp <- length(samp)
  for (i in 1:bootstrap_sims) {
    temp <- stats::quantile(sample(samp, size = n_samp, replace = T), probs = probs, type = 8)
    cumsum <- temp / bootstrap_sims + cumsum
  }
  return(cumsum)
}
