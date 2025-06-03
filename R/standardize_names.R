#' Return Bayes choices for standard names
#'
#' @description takes a vector of cleaned name strings and returns standardized names.
#' Relies on the output of `draw_gibbs()` and `make_bayes_choice_dictionary`.
#' Bayes' choice for a name \eqn{s_i} is \eqn{s_j} to maximize \eqn{Pr(\text{true surname} = s_j | \text{observed} = s_i, \lambda, \delta, s_j \in C)}
#' where \eqn{C} collects names included more than 99% of the time as true names in posterior draws.
#'
#' @param names A vector of character strings to standardize. It's assumed that
#'               name strings have already been "cleaned" in the same manner as
#'               input to `make_bayes_choice_dictionary()`, e.g. using
#'               `clean_surnames()` for English-language surnames.
#' @param dictionary The output of `make_bayes_choice_dictionary()`.
#'               data.frame with five columns:
#'                      $observed: character vector of observed, noisy names
#'                      $standard: character vector of standardized names
#'                      $posterior: numeric vector of posterior probabilities
#'                      $bayes_choice: integer indicating the posterior mode
#'                      $p_standard: numeric vector containing frequency of the standard name
#' @param method  method   Character scalar; distance metric passed to
#'                 `stringdist::stringdist()`.  Common choices: `"jw"`, `"lv"`
#'                 should match the method used in the original dictionary.
#'                 Default is "jw".
#' @param lambda   Should match the `lambda` argument in `draw_gibbs()`.
#'               Numeric; fixed scale parameter for error kernel (default 0.01)
#' @param delta Postive number equal to the posterior mean; i.e.`mean(post$delta_samples)`
#'              from the output of `draw_gibbs()`, defaults to 0.01.
#' @param ncores   Integer ≥ 1; number of parallel worker processes to launch.
#'                 Defaults to 1.
#'
#' @return A data.frame with two columns:
#'   - `observed`: string vector of observed names.
#'   - `standard`: string vector of standardized names.
#'
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom pbapply pbsapply
#' @importFrom stringdist stringdist
#' @export
standardize_names <- function(
    names,
    dictionary,
    method = "jw",
    lambda = 1e-2,
    delta = 1e-2,
    ncores = 1
) {

  # Throw a warning if names and dictionary$observed don't overlap
  overlap = max(c(mean(names %in% dictionary$observed),
                mean(dictionary$observed %in% names)))
  if(overlap < 0.5){
    warning("Overlap between `names` and `dictionary$observed` is small...\n Are you sure you have the right dictionary?")
  }

  # Creating a set of standard names from the dictionary
  standard_names <- subset(dictionary, bayes_choice == 1)
  standard_names <- subset(standard_names, !duplicated(standard))
  standard_names[,c("observed","bayes_choice","posterior")] <- NULL

  # Splitting off names missing from the dictionary
  missing_observed <- names[ !(names %in% dictionary$observed)] |> unique()

  if(length(missing_observed) > 0){

    # Starting up workers to handle missing_names
    cl <- parallel::makeCluster(ncores)

    # Sending shared objects to workers
    parallel::clusterExport(
      cl,
      varlist = c(
        "stringdist",
        "standard_names",
        "lambda",
        "delta",
        "method"
      ),
      envir = environment()
    )

    # Making a new dictionary for missing names
    missing_standard <- pbsapply(
      X = missing_observed,
      FUN = standardize_missing_name,
      cl = cl)

    # clean up
    parallel::stopCluster(cl)

    # Binding old and new dictionaries
    dictionary <- subset(dictionary, bayes_choice == 1)
    dictionary[,c("posterior","bayes_choice","p_standard")] <- NULL
    new_dictionary <- as.data.frame(cbind(missing_observed, missing_standard))
    colnames(new_dictionary) <- c("observed","standard")
    full_dictionary <- rbind(dictionary, new_dictionary)

  } else {
    # There are no terms missing from the dictionary, so jumping to the merge step
    dictionary <- subset(dictionary, bayes_choice == 1)
    dictionary[,c("posterior","bayes_choice","p_standard")] <- NULL
    full_dictionary <- dictionary

  }

  # look up the row in dictionary for each name
  idx <- match(names, full_dictionary$observed)

  # pull out the standard names
  dictionary$standard[idx]

}
