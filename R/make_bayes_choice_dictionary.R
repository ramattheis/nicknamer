#' Build a dictionary of Bayes' choices for standard choices from posterior draws
#'
#' #' @description
#' `make_bayes_choice_dictionary()` takes the input and output of `draw_gibbs()`
#' and returns a df with classified names. Bayes' choice for a name \eqn{s_i} is
#' \eqn{s_j} to maximize \eqn{Pr(\text{true surname} = s_j | \text{observed} = s_i, \lambda, \delta, s_j \in C)}
#' where \eqn{C} collects names included more than 99% of the time as true names in posterior draws.
#'
#' @param all_names Character vector with the full set of unique names.
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
#'   - `standard`: string vector of standardized names.
#'
#' @importFrom parallel makeCluster stopCluster
#' @importFrom pbapply pblapply
#' @export
make_bayes_choice_dictionary <- function(
    all_names,
    data,
    neighbor_list,
    lambda,
    post,
    ncores  = 1
) {

  # Combining posterior information into one list
  packed = lapply(seq_along(data$string), function(k)
    list(
      name = data$string[k],
      nb = neighbor_list[[k]],
      id = k,
      x = post$x_avg[k],
      p = post$p_avg[k],
      xs = post$x_avg[neighbor_list[[k]]$j],
      ps = post$p_avg[neighbor_list[[k]]$j]
    )
  )

  # Helper function for classifying individual surnames
  bayes_choice_helper = function(pack){
    # unpacking
    observed = pack$name
    nbs = pack$nb
    id = pack$id
    x = as.numeric(pack$x > 0.99)
    p = pack$p
    xs = as.numeric(pack$xs > 0.99)
    ps = pack$ps

    # Computing posterior
    phi_self = (1-delta)*p*x

    # Computing off-diagonal posterior
    ws = xs*exp(-nbs$d/lambda)
    ws = if(max(ws) > 0){ws/sum(ws)} else {ws}
    phis = delta*ps*ws

    # Picking posterior mode
    if(max(c(phi_self, phis)) == 0){
      bayes_choice_id = NA
    } else {
      if(phi_self > max(phis)){
        bayes_choice_id = id
      } else {
        bayes_choice_id = nbs$j[which(phis == max(phis))][1]
      }
    }

    # Returning self + bayes choice
    as.data.frame(cbind(observed, bayes_choice_id))
  }

  # Setting up the cluster
  cl <- parallel::makeCluster(ncores)

  # export your globals and functions
  delta = mean(post$delta_samples)
  parallel::clusterExport(cl,
                varlist = c("delta", "lambda", "bayes_choice_helper"),
                envir   = environment()
  )

  # Finding the bayes choice (index) for each name
  bayes_choices <- pbapply::pblapply(
    X   = packed,
    FUN = bayes_choice_helper,
    cl  = cl
  )
  bayes_choices = do.call(rbind,bayes_choices)

  # 7) clean up
  parallel::stopCluster(cl)

  # Returning surnames
  bayes_choices$bayes_choice = all_surnames[as.numeric(bayes_choices$bayes_choice_id)]
  bayes_choices$bayes_choice_id = NULL
  colnames(bayes_choices) = c("observed","standard")

  return(bayes_choices)
}
