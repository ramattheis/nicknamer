#' Generate a random draw from a Dirichlet distribution
#'
#' @description
#' Draws a single random vector from a Dirichlet distribution using the gamma trick.
#'
#' @param alpha Numeric vector; concentration parameters of the Dirichlet (length K).
#'
#' @return Numeric vector of length K that sums to 1, a single Dirichlet(α) draw.
#'
#' @examples
#' rdirichlet(c(1, 1, 1))  # uniform over 3-simplex
#'
#' @export
rdirichlet <- function(alpha) {
  x <- rgamma(length(alpha), shape = alpha, rate = 1)
  x / sum(x)
}
