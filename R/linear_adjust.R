#' Linear adjust
#'
#' \code{linear_adjust} function used to project the max_k line into the probabilities of interest
#' @param min_maxk minimum maxk found for each probability of interest
#' @param probs Probabilities where maxk was evaluated
#' @param probs_interest Probabilities of interest to estimate
#' @return Returns the maxk line for the probabilities of interest
#' @keywords RESTK
#' @export
#' @examples
#' linear_adjust(min_maxk = c(10, 15, 20),
#'               probs = c(1-1e-1, 1-1e-2, 1-1e-3),
#'               probs_interest = c(1-1e-6, 1-1e-7, 1-1e-8))
linear_adjust <- function(min_maxk = NULL, probs = NULL, probs_interest) {
  lm_data <- data.frame(x = -log10(1 - as.numeric(probs)), y = min_maxk)
  lm_fit <- stats::lm(data = lm_data, y ~ x)
  slope <- lm_fit$coefficients["x"]
  intercept <- lm_fit$coefficients["(Intercept)"]
  p_fit <- -log10(1 - as.numeric(probs_interest))
  maxk_line <- round(intercept + slope * p_fit)
  return(maxk_line)
}
