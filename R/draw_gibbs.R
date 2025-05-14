#' Run Gibbs sampler for string mixture model, progress bar,
#' flexible priors and data-driven defaults for init.
#'
#' @param data          Data frame with columns:
#'                      - string: unique identifiers
#'                      - count: integer counts
#' @param neighbor_list List of length K with elements:
#'                      $j: integer vector of neighbor indices
#'                      $d: numeric vector of edit distances
#' @param priors        Optional named list of priors.
#' @param init          Optional named list of initial values.
#' @param n_iter        Total MCMC iterations
#' @param burnin        Burn-in iterations
#' @return Posterior results: delta_samples, lambda_samples, x_avg, p_avg
#' @importFrom Matrix sparseMatrix
#' @import Rcpp
#' @export
draw_gibbs <- function(data,
                       neighbor_list,
                       priors = list(),
                       init   = list(),
                       n_iter = 10000,
                       burnin = 1000) {


  library(Matrix)
  n <- data$count
  K <- nrow(data)

  # Default priors
  default_priors <- list(
    presence_alpha  = 1,
    presence_beta   = 9,
    dirichlet_alpha = 1,
    spike_epsilon   = 1e-4,
    error_alpha     = 1,
    error_beta      = 9,
    lambda_rate     = 2
  )
  priors <- modifyList(default_priors, priors)
  list2env(priors, environment())

  # Init defaults
  default_init <- list(
    x      = as.integer(n > mean(n)),
    p      = n / sum(n),
    delta  = 0.1,
    lambda = 1
  )
  init <- modifyList(default_init, init)
  list2env(init, environment())

  # Build distance matrix once
  dist_mat <- Matrix(0, K, K, sparse = TRUE)
  for(i in seq_len(K)) {
    js <- neighbor_list[[i]]$j
    ds <- neighbor_list[[i]]$d
    dist_mat[i, js] <- ds
  }

  # Storage
  n_save         <- max(n_iter - burnin, 0)
  delta_samples  <- numeric(n_save)
  lambda_samples <- numeric(n_save)
  x_sum          <- numeric(K)
  p_sum          <- numeric(K)

  rdirichlet <- function(alpha) {
    y <- rgamma(length(alpha), alpha, 1)
    y / sum(y)
  }

  save_idx <- 0
  pb <- txtProgressBar(1, n_iter, style = 3)
  for(iter in seq_len(n_iter)) {
    setTxtProgressBar(pb, iter)

    # Z update
    zz <- sampleZ_cpp(as.integer(n), as.numeric(p), delta,
                      neighbor_list, as(dist_mat, "matrix"), lambda)
    Z  <- sparseMatrix(i = zz$i, j = zz$j, x = zz$x, dims = c(K,K))

    # Sufficient stats
    T_i <- rowSums(Z)
    S   <- sum(diag(Z)); E <- sum(Z) - S

    # p update
    alpha_vec <- dirichlet_alpha * x + spike_epsilon
    p         <- rdirichlet(alpha_vec + T_i)

    # x update
    lbf <- lgamma(dirichlet_alpha + spike_epsilon + T_i) - lgamma(dirichlet_alpha + spike_epsilon) -
      (lgamma(spike_epsilon + T_i)             - lgamma(spike_epsilon))
    odds <- exp(log(presence_alpha) - log(presence_beta) + lbf)
    x    <- rbinom(K, 1, odds / (1+odds))

    # lambda MH
    zz2   <- summary(Z)
    off   <- which(zz2$i != zz2$j)
    d_off <- dist_mat[cbind(zz2$i[off], zz2$j[off])]
    z_off <- zz2$x[off]
    lambda <- sampleLambda_cpp(d_off, z_off, lambda, lambda_rate)

    # delta update
    delta <- rbeta(1, error_alpha + E, error_beta + S)

    if(iter > burnin) {
      save_idx               <- save_idx + 1
      delta_samples[save_idx]  <- delta
      lambda_samples[save_idx] <- lambda
      x_sum <- x_sum + x
      p_sum <- p_sum + p
    }
  }
  close(pb)

  list(
    delta_samples  = delta_samples,
    lambda_samples = lambda_samples,
    x_avg          = x_sum / n_save,
    p_avg          = p_sum / n_save
  )
}

