#' Get the minimum maxk
#'
#' \code{get_min_maxk} get the minimum maxk from a set of maxks and tightness
#' @param samp_tightness tightness from a given sample and maxk
#' @param k_seq sequence of maxk to evaluate
#' @return Returns the minimum maxk
#' @keywords RESTK
#' @export
#' @examples
#' get_min_maxk(samp_tightness = c(1.5, 1.2, 0.98),
#'              k_seq = c(20, 30 , 40))
get_min_maxk <- function(samp_tightness = NULL, k_seq = NULL) {
  if (sum(samp_tightness < 1) != 0) {
    k_index <- which(samp_tightness < 1)[1] - 1
    max_k <- k_seq[k_index]
  } else {
    k_index <- which(samp_tightness == min(samp_tightness))[1]
    max_k <- k_seq[k_index]
  }
  return(max_k)
}
