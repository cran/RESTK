#' Compute the maximum k for a given sample
#'
#' \code{compute_maxk} returns the estimated quantiles for the chosen probabilities from the input sample.
#' This method uses the sample quantile method number 8 from the default quantile function.
#' @param samp Sample of data to model
#' @param probs Probabilities of interest to generate the max_k line
#' @param quants Estimated quantiles of interest to generate the max_k line
#' @param k_range Range of k values for the optimization function
#' @return Returns estimated maxk for the sample and quantiles given.
#' @keywords RESTK
#' @export
#' @examples
#' samp <- rnorm(1e3, mean = 100, sd = 10)
#' probs <- c(1-1e-1, 1-0.5e-1, 1-1e-2)
#' quants <- c(100, 125, 150)
#' estimated_max_k <- compute_maxk(samp = samp, probs = probs, quants = quants, k_range = c(1,100))
compute_maxk <- function(samp = NULL, probs = NULL, quants = NULL, k_range = c(1, 120)) {
  maxk_quants <- c()
  for (p in 1:length(probs)) {
    prob <- probs[p]
    quant <- quants[p]
    tightness_optim_k <- function(k = NULL) {
      return(RESTK::tightness(samp = samp, prob = prob, quant = quant, k = k))
    }
    maxk_quants[p] <- stats::optimize(tightness_optim_k, interval = k_range)$minimum
  }
  return(maxk_quants)
}
