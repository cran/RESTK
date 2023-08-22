#' Estimate Quantiles with Maxk
#'
#' \code{estimate_quantiles_maxk} use the maxk line obtained to estimate quantiles with MIK
#' @param samp sample
#' @param maxk_line  maxk line obtained for the probabilities of interest
#' @param probs_interest Probabilities of interest to estimate
#' @return Returns the estimation of the quantiles using the maxk line
#' @keywords RESTK
#' @export
#' @examples
#' linear_adjust(min_maxk = c(10, 15, 20),
#'               probs = c(1-1e-1, 1-1e-2, 1-1e-3),
#'               probs_interest = c(1-1e-6, 1-1e-7, 1-1e-8))
estimate_quantiles_maxk <- function(samp = NULL, maxk_line = NULL, probs_interest = NULL) {
  estimated_quantiles <- c()
  iter_1 <- 1
  n <- length(samp)
  for (i in 1:length(probs_interest)) {
    prob <- probs_interest[i]
    data_test_k_est <- c()
    ks <- 1:maxk_line[i]
    iter_2 <- 1
    for (k in ks) {
      e_k_est <- sum(samp^k) / n
      cota_k_est <- (e_k_est^(1 / k)) / (1 - prob)^(1 / k)
      names(cota_k_est) <- paste0("k", k)
      data_test_k_est[iter_2] <- cota_k_est
      iter_2 <- iter_2 + 1
    }

    best_k_est <- RESTK::get_min_maxk(data_test_k_est, ks)

    estimated_quantiles[iter_1] <- (sum(samp^best_k_est) / n)^(1 / best_k_est) / (1 - prob)^(1 / best_k_est)
    iter_1 <- iter_1 + 1
  }
  return(estimated_quantiles)
}
