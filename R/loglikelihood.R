#' Compute log‐likelihood via matrix mulitplication
#'
#' @param p      Numeric vector of “true‐name” probabilities (length K).
#' @param D      K×K distance matrix (sparse or dense), with d_{ij} > 0 only
#'               for neighbor pairs (zeros elsewhere).
#' @param M      K×K mask matrix (sparse or dense) with 1’s for neighbor edges
#'               and 0’s elsewhere. Must be symmetric.
#' @param delta  Garbling probability in [0,1].
#' @param lambda Non‐negative rate for exponential‐decay weights.
#' @param n_obs  Integer or numeric counts vector (length K).
#'
#' @return The scalar log‐likelihood
#'   \(\sum_{i=1}^K n_i \log L_i\)
#'   where
#'   \(\mathbf L = (1-\delta)\,\mathbf p + \delta\,( \mathbf p\,E)\),
#'   \(E = \mathrm{diag}(1/Z)\,W\),
#'   \(W = M\circ e^{-\lambda D}\),
#'   \(Z = W\,\mathbf1\).
#' @importFrom Matrix Diagonal rowSums
#' @export
loglikelihood <- function(p, D, M, delta, lambda, n_obs) {
  if (!requireNamespace("Matrix", quietly = TRUE)) {
    stop("Package 'Matrix' is required for sparse‐matrix support.")
  }
  # 1) Build W = M ∘ exp(-λ D)
  if (inherits(D, "sparseMatrix")) {
    W <- D
    W@x <- exp(-lambda * D@x)         # only nonzero entries stored
  } else {
    W <- M * exp(-lambda * D)         # entrywise
  }

  # 2) Row‐sums Z
  Z <- Matrix::rowSums(W)
  invZ <- 1 / Z
  invZ[!is.finite(invZ)] <- 0        # handle rows with no neighbors

  # 3) Transition matrix E = diag(invZ) %*% W
  E <- Matrix::Diagonal(x = invZ) %*% W

  # 4) Mixture probs L = (1−δ)p + δ * (p %*% E)
  p    <- as.numeric(p)
  L    <- (1 - delta) * p + delta * as.numeric(p %*% E)

  # 5) Log‐likelihood
  if (any(L <= 0)) return(-Inf)
  sum(n_obs * log(L))
}
