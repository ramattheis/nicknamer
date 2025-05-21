#' Return Bayes' choices for standard names
#'
#' #' @description
#'
#'
#' @param names A vector of unique character strings to standardize.
#'              Data frame with columns:
#'                      - string: unique identifiers
#'                      - count: integer counts
#' @param dictionary The output of `make_bayes_choice_dictionary()`.
#'               data.frame with two columns:
#'                      $observed: character vector of observed, noisy names
#'                      $standard: character vector of standardized names
#' @param ncores   Integer ≥ 1; number of parallel worker processes to launch.
#'                 Defaults to 1.
#'
#' @return A data.frame with two columns:
#'   - `observed`: string vector of observed names.
#'   - `standard`: string vector of standardized names.
#'
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom pbapply pblapply
#' @importFrom stringdist stringdist
#' @export
standardize_names <- function(
    data,
    neighbor_list,
    lambda,
    post,
    ncores  = 1
) {

  # Helper function for classifying individual surnames

  # Combining posterior information into one list




}
