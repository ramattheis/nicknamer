#' Draw from the posterior of the name garbling model.
#'
#' @description
#' Performs Gibbs sampling for the garbling mixture model.
#' It updates the true-name probability vector \code{p} via a collapsed Gibbs step using expected counts,
#' and jointly updates the distortion probability \code{delta} and decay rate \code{lambda} via a Metropolis-Hastings random walk.
#' Only the first five entries of \code{p} are retained in the output.
#'
#' @param p_init Numeric vector of length K; initial true-name probabilities.
#' @param D Matrix or sparse matrix; K×K distance matrix with distances for neighbor pairs.
#' @param M Matrix or sparse matrix; K×K binary mask of neighbor edges (1 for neighbors, 0 otherwise).
#' @param n_obs Numeric or integer vector of length K; observed counts per name.
#' @param n_iter Integer; number of sampler iterations. Default \code{10000}.
#' @param delta_init Numeric in (0,1); initial value for \code{delta}. Default \code{0.1}.
#' @param lambda_init Numeric >0; initial value for \code{lambda}. Default \code{1.0}.
#' @param sd_logit Numeric; proposal SD for logit(\code{delta}). Default \code{0.1}.
#' @param sd_loglam Numeric; proposal SD for log(\code{lambda}). Default \code{0.1}.
#' @param alpha_dir Numeric; Dirichlet prior hyperparameter (scalar or length-K vector). Default \code{1}.
#' @param prior_delta Function; log-density prior for \code{delta}. Default Beta(9,1) on \code{[0,1]}.
#' @param prior_lambda Function; log-density prior for \code{lambda}. Default Gamma(1,0.1) on \code{(0,\infty)}.
#'
#' @return A list with components:
#' \describe{
#'   \item{delta}{Numeric vector of sampled \code{delta} values (length \code{n_iter}).}
#'   \item{lambda}{Numeric vector of sampled \code{lambda} values.}
#'   \item{likelihood}{Numeric vector of log-likelihood values at each iteration.}
#'   \item{p_first5}{Matrix (\code{n_iter} × \code{min(5,K)}) of the first five entries of \code{p} at each iteration.}
#' }
#'
#' @examples
#' \dontrun{
#' K <- length(p_init)
#' out <- draw_gibbs(
#'   p_init     = rep(1/K, K),
#'   D          = D_mat,
#'   M          = M_mat,
#'   n_obs      = counts,
#'   n_iter     = 2000
#' )
#' head(out$p_first5)
#' }
#'
#' @export
draw_gibbs<- function(
    p_init,
    D, M, n_obs,
    n_iter       = 10000,
    delta_init   = 0.1,
    lambda_init  = 1.0,
    sd_logit     = 0.1,
    sd_loglam    = 0.1,
    alpha_dir    = 1,
    prior_delta  = function(d) dbeta(d, 9, 1,  log = TRUE),
    prior_lambda = function(l) dgamma(l, 1, 0.1, log = TRUE)
) {
  K <- length(p_init)
  if (length(alpha_dir) == 1) alpha_dir <- rep(alpha_dir, K)

  # storage for first 5 entries of p only
  p5_chain   <- matrix(NA_real_, n_iter, min(5, K))
  delta_chain  <- numeric(n_iter)
  lambda_chain <- numeric(n_iter)
  ll_chain     <- numeric(n_iter)

  # initialise state
  p_curr      <- p_init / sum(p_init)
  delta_curr  <- delta_init
  lambda_curr <- lambda_init

  pb <- utils::txtProgressBar(min = 0, max = n_iter, style = 3)

  logit  <- function(x) log(x / (1 - x))
  ilogit <- function(x) exp(x) / (1 + exp(x))

  for (t in seq_len(n_iter)) {
    # (1) collapsed Dirichlet update for p via expected counts
    Kmat      <- make_kernel(D, M, delta_curr, lambda_curr)
    denom     <- as.numeric(Kmat %*% p_curr)
    tmp       <- n_obs / denom
    exp_counts <- p_curr * as.numeric(crossprod(Kmat, tmp))
    p_curr     <- rdirichlet(alpha_dir + exp_counts)

    # store only first 5 entries of p
    p5_chain[t, ] <- p_curr[1:ncol(p5_chain)]

    # (2) joint MH step for (delta, lambda)
    logit_prop <- rnorm(1, logit(delta_curr), sd_logit)
    d_prop     <- ilogit(logit_prop)
    logl_prop  <- rnorm(1, log(lambda_curr), sd_loglam)
    lam_prop   <- exp(logl_prop)

    ll_prop <- loglikelihood(p_curr, D, M, d_prop, lam_prop, n_obs)
    ll_curr <- loglikelihood(p_curr, D, M, delta_curr, lambda_curr, n_obs)

    log_post_curr <- ll_curr + prior_delta(delta_curr) + prior_lambda(lambda_curr)
    log_post_prop <- ll_prop  + prior_delta(d_prop)    + prior_lambda(lam_prop)

    jac_delta  <- log(d_prop * (1 - d_prop)) - log(delta_curr * (1 - delta_curr))
    jac_lambda <- log(lam_prop) - log(lambda_curr)

    log_r <- (log_post_prop - log_post_curr) + jac_delta + jac_lambda

    if (log(runif(1)) < log_r) {
      delta_curr <- d_prop
      lambda_curr <- lam_prop
      ll_curr <- ll_prop
    }

    delta_chain[t]  <- delta_curr
    lambda_chain[t] <- lambda_curr
    ll_chain[t]     <- ll_curr
    utils::setTxtProgressBar(pb, t)
  }

  close(pb)

  list(
    delta       = delta_chain,
    lambda      = lambda_chain,
    likelihood  = ll_chain,
    p_first5    = p5_chain
  )
}

