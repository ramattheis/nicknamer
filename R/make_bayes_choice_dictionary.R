#' Construct Bayes-Optimal Choice Dictionary
#'
#' Builds a dictionary mapping each observed name to its Bayes-optimal standard choice,
#' combining self-posterior weight and neighbor contributions via a sparse distance matrix.
#'
#' @param names Character vector of observed names (length K).
#' @param D A sparse distance matrix (e.g. \code{dgCMatrix}) of size K × K.
#' @param p Numeric vector of prior probabilities for each name (length K).
#' @param delta Numeric scalar in [0,1]; weight on neighbor contributions.
#' @param lambda Numeric scalar; decay parameter for distance weighting.
#'
#' @return A \code{data.frame} with columns:
#' \describe{
#'   \item{observed}{Original observed names.}
#'   \item{standard}{Bayes-optimal standard name (or \code{NA} if no unique max).}
#'   \item{p_standard}{Frequency of the standardized name in the population.}
#' }
#'
#' @examples
#' \dontrun{
#' library(Matrix)
#' names <- c("Smith", "Smyth", "Smit")
#' # dummy distance matrix
#' D <- sparseMatrix(i = c(1,1,2), j = c(2,3,3), x = c(1,2,3), dims = c(3,3))
#' p <- c(0.5, 0.3, 0.2)
#' make_bayes_choice_dictionary(names, D, p, delta = 0.1, lambda = 2)
#' }
#'
#' @seealso \code{\link{make_bayes_choice_dictionary_cpp}}
#' @export
make_bayes_choice_dictionary <- function(names, D, p, delta, lambda) {
  # input checks
  if (!is.character(names)) stop("'names' must be a character vector")
  if (!inherits(D, "sparseMatrix")) stop("'D' must be a sparse matrix from the Matrix package")
  K <- length(names)
  if (length(p) != K) stop("length(p) must equal length(names)")
  if (!all(dim(D) == c(K, K))) stop("D must be a K×K matrix where K = length(names)")
  if (!is.numeric(delta) || length(delta) != 1) stop("'delta' must be a single numeric value")
  if (!is.numeric(lambda) || length(lambda) != 1) stop("'lambda' must be a single numeric value")

  # pass through to the C++ implementation
  make_bayes_choice_dictionary_cpp(names, D, p, delta, lambda)
}
