#' Build a dictionary of Bayes choices for standard choices from posterior draws
#'
#' @description
#' `make_bayes_choice_dictionary()` takes the input and output of `draw_gibbs()`
#' and returns a df with classified names. Bayes' choice for a name \eqn{s_i} is
#' \eqn{s_j} to maximize \eqn{Pr(\text{true name} = s_j | \text{observed} = s_i, \lambda, \delta, s_j \in C)}
#' where \eqn{C} collects names included more than 99% of the time as true names in posterior draws.
#'
#' @param data  Should match the `data` argument in `draw_gibbs()`.
#'              Data frame with columns:
#'                      - string: unique identifiers
#'                      - count: integer counts
#' @param neighbor_list Should match the `data` argument in `draw_gibbs()`.
#'               List of length K with elements:
#'                      $j: integer vector of neighbor indices
#'                      $d: numeric vector of edit distances
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
#'   - `standard`: string vector of standardized names (`NA` if name is ambiguous).
#'
#' @importFrom parallel makeCluster stopCluster
#' @importFrom pbapply pblapply
#' @export
make_bayes_choice_dictionary <- function(
    data,
    neighbor_list,
    lambda,
    post,
    ncores  = 1
) {

  # Re-packing data into a list of length K
  packed = pack_name_posterior_cpp(
    data$string,
    neighbor_list,
    post$x_avg,
    post$p_avg
  )

  # Setting up the cluster
  cl <- parallel::makeCluster(ncores)

  # export your globals and functions
  delta = mean(post$delta_samples)
  parallel::clusterExport(cl,
                varlist = c("delta", "lambda", "make_bayes_choice_dictionary_helper"),
                envir   = environment()
  )

  # Finding the bayes choice (index) for each name
  bayes_choices <- pbapply::pblapply(
    X   = packed,
    FUN = make_bayes_choice_dictionary_helper,
    cl  = cl
  )
  bayes_choices = do.call(rbind,bayes_choices)

  # clean up
  parallel::stopCluster(cl)

  # Returning standardized names
  bayes_choices$standard = data$string[as.numeric(bayes_choices$candidate_id)]
  bayes_choices$candidate_id = NULL
  bayes_choices = bayes_choices[, c(1,4,2,3)]
  colnames(bayes_choices) = c("observed","standard","posterior","bayes_choice")

  return(bayes_choices)
}
