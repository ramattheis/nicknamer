#' Sample from a Dirichlet Distribution
#'
#' @description
#' Draw a single random vector from a Dirichlet distribution with concentration parameters \code{alpha}.
#' Normalizing independent Gamma draws yields the Dirichlet law.
#'
#' @param alpha Numeric vector of positive concentration parameters (\eqn{\alpha_1,\dots,\alpha_K}).
#'
#' @return Numeric vector of length \code{length(alpha)}, summing to 1, representing a draw from \eqn{\mathrm{Dirichlet}(\alpha)}.
#'
#' @export
rdirichlet <- function(alpha) {
  y <- rgamma(length(alpha), shape = alpha, rate = 1)
  y / sum(y)
}
