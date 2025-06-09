#' Run Gibbs sampler for *name* mixture model
#'
#' @param data          Data frame with columns
#'                      – **name**  : candidate name (unique identifier)
#'                      – **count** : observed integer count
#' @param neighbor_list Length‑K list; each element contains
#'                      $j$ : integer vector of neighbour indices (length mᵢ)
#'                      $d$ : numeric vector of edit distances (same length)
#' @param lambda        Initial value for the error‑kernel scale parameter (default 0.1)
#' @param priors        Optional list of prior hyper‑parameters.
#' @param init          Optional list of initial values (excludes λ).
#' @param n_iter        Total MCMC iterations (default 10000).
#'
#' @return A list with
#'   * **delta_samples**  : numeric vector of δ draws
#'   * **lambda_samples** : numeric vector of λ draws
#'   * **x_avg**         : posterior mean inclusion probability for each name
#'   * **p_avg**         : posterior mean of mixture weights p
#'
#' @useDynLib nicknamer, .registration = TRUE
#' @importFrom Matrix sparseMatrix rowSums diag
#' @import Rcpp
#' @export
#'
#' @examples
#' df <- data.frame(name = c("a","b","c"), count = c(10,5,1))
#' nbrs <- list(list(j = 2,      d = 1),
#'              list(j = c(1,3), d = c(1,2)),
#'              list(j = 2,      d = 2))
#' out <- draw_gibbs(df, nbrs, lambda = 0.1, n_iter = 100)
draw_gibbs <- function(data,
                       neighbor_list,
                       lambda  = 0.1,
                       priors  = list(),
                       init    = list(),
                       n_iter  = 2e4) {
  if(!is.numeric(lambda) || lambda <= 0){
    stop("`lambda` must be positive.")
  }


  n <- data$count
  K <- nrow(data)

  # ----- Priors ----------------------------------------------------------------------
  prior_def <- list(presence_alpha  = 1,
                    presence_beta   = 99,
                    dirichlet_alpha = 1,
                    spike_epsilon   = 0.1,
                    error_alpha     = 1,
                    error_beta      = 9)
  priors <- modifyList(prior_def, priors)
  list2env(priors, environment())

  # ----- Initial values --------------------------------------------------------------
  init_def <- list(x     = as.integer(n > mean(n)),
                   p     = n / sum(n),
                   delta = 0.1)
  init <- modifyList(init_def, init)
  list2env(init, environment())

  # Pre‑extract neighbour index list for quick access in R (still needed by C++)
  j_list <- lapply(neighbor_list, `[[`, "j")

  # ----- Storage ---------------------------------------------------------------------
  delta_samples  <- numeric(n_iter)
  lambda_samples <- numeric(n_iter)
  p_sum          <- numeric(K)
  x_sum          <- numeric(K)
  post_ctr       <- 0L

  # ----- MCMC control ----------------------------------------------------------------
  log_sd <- 0.1                     # RW‑proposal σ on log‑λ scale

  pb <- txtProgressBar(min = 0, max = n_iter, style = 3)
  for(iter in seq_len(n_iter)) {

    # 1. Allocate counts  Z | p,δ,λ  (C++) -------------------------------------------
    zz <- sampleZ_cpp(
      n        = as.integer(n),
      p        = as.numeric(p),
      delta    = delta,
      lambda   = lambda,
      neighbor = neighbor_list
    )
    Z <- sparseMatrix(i = zz$i, j = zz$j, x = zz$x, dims = c(K, K))

    # 2. Sufficient statistics ---------------------------------------------------------
    T_i <- rowSums(Z)
    S   <- sum(diag(Z))       # self‑matches
    E   <- sum(Z) - S         # garbled matches

    # 3. λ | rest  (Metropolis–Hastings) ---------------------------------------------
    lambda_prop <- lambda * exp(rnorm(1, 0, log_sd))
    if(lambda_prop > 0) {
      loglik_curr <- compute_loglik_cpp(Z, lambda,      neighbor_list)
      loglik_prop <- compute_loglik_cpp(Z, lambda_prop, neighbor_list)

      # Exp(10) prior(mean ≈ 0.1)
      logprior_curr <- dexp(lambda,      rate = 10, log = TRUE)
      logprior_prop <- dexp(lambda_prop, rate = 10, log = TRUE)

      # Symmetric on log‑scale ⇒ Hastings adjustment log(λ_prop/λ)
      logA <- (loglik_prop + logprior_prop) -
        (loglik_curr + logprior_curr) +
        log(lambda_prop / lambda)

      cat(paste0("lambda = ", round(lambda,3), " lambda prop. = ", round(lambda_prop,3),
                 " delta = ", round(delta,3), " log proposal odds = ", round(logA,0), "\n"))

      if(log(runif(1)) < logA)
        lambda <- lambda_prop
    }

    # 4. p | Z,x ----------------------------------------------------------------------
    p <- rdirichlet(dirichlet_alpha * x + spike_epsilon + T_i)

    # 5. x | Z ------------------------------------------------------------------------
    lbf <- lgamma(dirichlet_alpha + spike_epsilon + T_i) -
      lgamma(dirichlet_alpha + spike_epsilon) -
      (lgamma(spike_epsilon + T_i)            - lgamma(spike_epsilon))
    log_prior_odds <- log(presence_alpha) - log(presence_beta)
    x <- rbinom(K, 1, plogis(log_prior_odds + lbf))

    # 6. δ | Z ------------------------------------------------------------------------
    delta <- rbeta(1, error_alpha + E, error_beta + S)

    # 7. Book‑keeping ------------------------------------------------------------------
    setTxtProgressBar(pb, iter)
    delta_samples [iter] <- delta
    lambda_samples[iter] <- lambda

    if(iter > min(n_iter / 2, 1e4)) {           # Hard-coded 50% burn‑in, max 10k length
      p_sum  <- p_sum + p
      x_sum  <- x_sum + x
      post_ctr <- post_ctr + 1L
    }
  }
  close(pb)

  list(delta_samples  = delta_samples,
       lambda_samples = lambda_samples,
       x_avg          = x_sum / post_ctr,
       p_avg          = p_sum / post_ctr)
}

