#' RESTK Validation
#'
#' \code{RESTK_validation} main function for the validation of the RESTK methodology by using the maxk line
#' @param validation_data validation data
#' @param maxk_line maxk line obtained from RESTK_training
#' @param probs_interest Probabilities of interest to estimate
#' @param bootstrap_size size of bootstrap simulations on the validation data
#' @param bootstrap_validation_sims number of bootstrap simulations on the validation data
#' @return Returns the estimated quantiles from the probabilities of interest
#' @keywords RESTK
#' @export
#' @examples
#' validation_data <- rnorm(1e3, mean = 100, sd = 10)
#' probs_interest <- c(1-1e-6, 1-1e-7)
#' bootstrap_size <- 1000
#' bootstrap_validation_sims <- 100
#' maxk_line <- c(100, 125, 150)
#' estimated_quants <- RESTK_validation(validation_data = validation_data,
#'                                      maxk_line = maxk_line,
#'                                      probs_interest = probs_interest,
#'                                      bootstrap_size = bootstrap_size,
#'                                      bootstrap_validation_sims = bootstrap_validation_sims)

RESTK_validation <- function(validation_data = NULL,
                             maxk_line = NULL,
                             probs_interest = NULL,
                             bootstrap_size = NULL,
                             bootstrap_validation_sims = NULL) {
  estimated_quants <- data.frame(matrix(ncol = length(probs_interest), nrow = bootstrap_validation_sims))

  for (s in 1:bootstrap_validation_sims) {
    samp <- sample(validation_data, bootstrap_size, replace = T)
    estim_quant <- RESTK::estimate_quantiles_maxk(samp = samp, maxk_line = maxk_line, probs_interest = probs_interest)
    estimated_quants[s,] <- estim_quant
  }
  colnames(estimated_quants) <- as.character(probs_interest)
  return(estimated_quants)
}
