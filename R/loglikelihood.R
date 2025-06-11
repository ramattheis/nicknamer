#' Compute the log-likelihood of observed counts under the garbling model
#'
#' @param p        Numeric vector of length K; true-name mixture weights.
#' @param D        Distance matrix (or sparseMatrix) K×K.
#' @param M        Binary mask matrix (or sparseMatrix) K×K.
#' @param delta    Numeric in [0,1]; garbling probability.
#' @param lambda   Numeric >0; decay rate for off-diagonals.
#' @param n_obs    Integer vector of length K; counts of observed labels.
#'
#' @return Single numeric: log-likelihood \(\sum_i n_{\rm obs,i}\log L_i\).
#'         Returns \(-\infty\) if any \(L_i\le0\)
#'
#' @export
loglikelihood <- function(p, D, M, delta, lambda, n_obs) {
  # build the full kernel P(obs=i | true=j)
  Kmat <- make_kernel(D, M, delta, lambda)

  # mixture probs L_i = sum_j p_j * Kmat[i,j]
  L <- as.numeric(Kmat %*% p)

  if (any(L <= 0)) {
    return(-Inf)
  }
  sum(n_obs * log(L))
}
