#' Tightness function
#'
#' \code{tightness} function used to minimized the tightness as a function of the value of k
#' @param samp Sample of data to model
#' @param prob Probability of interest
#' @param quant Quantile of interest
#' @param k value of k to check tightness
#' @return Returns the squared difference between the tightness and 1
#' @keywords RESTK
#' @export
#' @examples
#' samp <- rnorm(1e3, mean = 100, sd = 10)
#' prob <- c(1-1e-2)
#' k <- 1:100
#' quant <- qnorm(p = prob, mean = 100, sd = 10)
#' tightness(samp = samp, prob = prob, quant = quant, k = k)

tightness <- function(samp = NULL, prob = NULL, quant = NULL, k = NULL) {
  e_k_est <- purrr::map_dbl(k, ~ mean(samp^(.x)))
  contour_quantile_est <- (e_k_est)^(1 / k) / (1 - prob)^(1 / k)
  tightness_est <- contour_quantile_est / quant
  return((tightness_est - 1)^2)
}
