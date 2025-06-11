#' Build the transition kernel matrix for the garbling model
#'
#' @description
#' Constructs the K×K kernel of P(obs = i | true = j) by
#' 1. Computing an unnormalized weight matrix W = M \%*\% exp(-lambda * D),
#' 2. Row-normalizing W to obtain mutation probabilities E, and
#' 3. Combining with the garbling probability \code{delta} to form Kmat = delta * t(E) + diag(1 - delta).
#'
#' @param D Matrix or sparseMatrix; K×K distance matrix between names.
#' @param M Matrix or sparseMatrix; K×K binary mask for allowed mutations (1 = neighbor, 0 otherwise).
#' @param delta Numeric in [0,1]; probability mass moved off the diagonal.
#' @param lambda Numeric > 0; exponential decay rate for off-diagonal mutations.
#'
#' @return K×K matrix; entry (i,j) = P(observed = i | true = j).
#'
#' @importFrom Matrix rowSums Diagonal t
#'
#' @examples
#' # assume D_mat and M_mat are defined
#' Kmat <- make_kernel(D_mat, M_mat, delta = 0.1, lambda = 1)
#'
#' @export
make_kernel <- function(D, M, delta, lambda) {
  # Build unnormalized kernel W
  if (inherits(D, "sparseMatrix")) {
    W <- D
    W@x <- exp(-lambda * D@x)
    W <- W * M
  } else {
    W <- M * exp(-lambda * D)
  }

  # Row-normalize to get E
  Z <- Matrix::rowSums(W)
  invZ <- 1 / Z
  invZ[!is.finite(invZ)] <- 0
  if (inherits(W, "Matrix")) {
    E <- Matrix::Diagonal(x = invZ) %*% W
  } else {
    E <- sweep(W, 1, Z, FUN = "/")
    E[!is.finite(E)] <- 0
  }

  # Build transition kernel P(obs=i | true=j)
  # E[j,i] stored at E[j,i], so transpose to align
  Kmat <- delta * Matrix::t(E)
  diag(Kmat) <- (1 - delta)
  Kmat
}
