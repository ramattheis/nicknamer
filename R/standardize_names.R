#' Return Bayes choices for standard names
#'
#' @description takes a vector of raw name strings and returns standardized names.
#' Relies on the output of `draw_gibbs()` and `make_bayes_choice_dictionary`.
#' Bayes' choice for a name \eqn{s_i} is \eqn{s_j} to maximize \eqn{Pr(\text{true surname} = s_j | \text{observed} = s_i, \lambda, \delta, s_j \in C)}
#' where \eqn{C} collects names included more than 99% of the time as true names in posterior draws.
#'
#' @param names A vector of character strings to standardize. It's assumed that
#'               name strings have already been "cleaned" in the same manner as
#'               input to `make_bayes_choice_dictionary()`, e.g. using
#'               `clean_surnames()` for English-language surnames.
#' @param dictionary The output of `make_bayes_choice_dictionary()`.
#'               data.frame with two columns:
#'                      $observed: character vector of observed, noisy names
#'                      $standard: character vector of standardized names
#' @param lambda   Should match the `data` argument in `draw_gibbs()`.
#'               Numeric; fixed scale parameter for error kernel (default 1)
#' @param post The output of `draw_gibbs()`, a list with three elementS:
#'                      - `delta_samples` the chain of draws for \eqn{\delta}
#'                      - `x_avg` the posterior mean for the probability of inclusion.
#'                      - `p_avg` the posterior mean frequency in the sample.
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
    names,
    dictionary,
    lambda = 1,
    post,
    ncores = 1
) {

  # Throw a warning if names and dictionary$observed don't overlap
  overlap = max(c(mean(names %in% dictionary$observed),
                mean(dictionary$observed %in% names)))
  if(overlap < 0.5){
    warning("Overlap between `names` and `dictionary$observed` is small...\n Are you sure you have the right dictionary?")
  }

  # Splitting off names missing from the dictionary
  missing_names <- names[ !(names %in% dictionary$observed)] |> unique()

  # Making a new dictionary for missing names
  new_dictionary <- standardize_names_helper(...)

  # Binding old and new dictionaries
  full_dictionary <- rbind(dictionary, new_dictionary)

  # look up the row in dictionary for each name
  idx <- match(names, dictionary$observed)

  # pull out the standard names
  dictionary$standard[idx]



}
