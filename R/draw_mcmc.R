#' Run a joint MH sampler for the garbling mixture model
#'
#' @description
#' Performs a single Metropolis-Hastings sampling step jointly updating
#' the garbling probability (delta) and the exponential-decay rate (lambda)
#' under a fixed true-name probability vector \code{p}, using sparse or dense
#' matrix arithmetic via \code{loglikelihood()}. This one-step proposal
#' can improve mixing when \code{delta} and \code{lambda} are correlated.
#'
#' @param p            Numeric vector of length K; fixed true-name probabilities.
#' @param D            K×K distance matrix (sparse or dense) with distances
#'                     for neighbor pairs and zeros elsewhere.
#' @param M            K×K binary mask matrix (sparse or dense) with 1’s
#'                     for neighbor edges; must be symmetric.
#' @param n_obs        Numeric or integer vector (length K) of observed counts.
#' @param n_iter       Integer; number of sampler iterations (default: 10000).
#' @param delta_init   Numeric in (0,1); initial value for delta (default: 0.1).
#' @param lambda_init  Numeric > 0; initial value for lambda (default: 1.0).
#' @param sd_logit     Numeric; proposal SD for logit(delta) random walk (default: 0.1).
#' @param sd_loglam    Numeric; proposal SD for log(lambda) (default: 0.1).
#' @param prior_delta  Function; log-density prior for delta (default: Beta(9,1)).
#' @param prior_lambda Function; log-density prior for lambda (default: Gamma(1,0.1)).
#'
#' @return A list with components:
#' \describe{
#'   \item{delta}{Numeric vector of sampled delta values (length \code{n_iter}).}
#'   \item{lambda}{Numeric vector of sampled lambda values.}
#'   \item{likelihood}{Numeric vector of log-likelihood values at each iteration.}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' draws <- draw_mcmc(
#'   p = p_vec,
#'   D = D_mat,
#'   M = M_mat,
#'   n_obs = counts,
#'   n_iter = 5000
#' )
#' }
#'
draw_mcmc <- function(
    p,
    D,
    M,
    n_obs,
    n_iter       = 10000,
    delta_init   = 0.1,
    lambda_init  = 1.0,
    sd_logit     = 0.1,
    sd_loglam    = 0.1,
    prior_delta  = function(d) dbeta(d, 9, 1, log = TRUE),
    prior_lambda = function(l) dgamma(l, 1, 0.1, log = TRUE)
) {
  # storage
  delta_chain  <- numeric(n_iter)
  lambda_chain <- numeric(n_iter)
  ll_chain     <- numeric(n_iter)

  # initialize
  delta_curr   <- delta_init
  lambda_curr  <- lambda_init
  ll_curr      <- loglikelihood(p, D, M, delta_curr, lambda_curr, n_obs)

  # progress bar
  pb <- utils::txtProgressBar(min = 0, max = n_iter, style = 3)

  # helper for logit and inverse-logit
  logit   <- function(x) log(x / (1 - x))
  ilogit  <- function(x) exp(x) / (1 + exp(x))

  for (t in seq_len(n_iter)) {

    #--------------------------------------------------
    # 1) propose jointly: logit(delta) and log(lambda)
    logit_prop <- rnorm(1, logit(delta_curr), sd_logit)
    d_prop     <- ilogit(logit_prop)
    logl_prop  <- rnorm(1, log(lambda_curr), sd_loglam)
    lam_prop   <- exp(logl_prop)

    # compute log-likelihood for proposed values
    ll_prop <- loglikelihood(p, D, M, d_prop, lam_prop, n_obs)

    # log-posterior for current and proposed
    log_post_curr <- ll_curr + prior_delta(delta_curr) + prior_lambda(lambda_curr)
    log_post_prop <- ll_prop  + prior_delta(d_prop)    + prior_lambda(lam_prop)

    # Jacobians: for delta (logit) and lambda (log)
    jacobian_delta <- log(d_prop * (1 - d_prop)) - log(delta_curr * (1 - delta_curr))
    jacobian_lambda <- log(lam_prop) - log(lambda_curr)

    # acceptance ratio
    log_r <- (log_post_prop - log_post_curr) + jacobian_delta + jacobian_lambda

    if (log(runif(1)) < log_r) {
      delta_curr  <- d_prop
      lambda_curr <- lam_prop
      ll_curr     <- ll_prop
    }

    #--------------------------------------------------
    # 2) store samples and update progress bar
    delta_chain[t]  <- delta_curr
    lambda_chain[t] <- lambda_curr
    ll_chain[t]     <- ll_curr
    utils::setTxtProgressBar(pb, t)
  }

  close(pb)

  list(
    delta      = delta_chain,
    lambda     = lambda_chain,
    likelihood = ll_chain
  )
}

