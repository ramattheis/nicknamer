#' Run Gibbs sampler for string mixture model (fixed λ)
#'
#' @param data          Data frame with columns:
#'                      - string: unique identifiers
#'                      - count: integer counts
#' @param neighbor_list List of length K with elements:
#'                      $j: integer vector of neighbor indices
#'                      $d: numeric vector of edit distances
#' @param lambda        Numeric; fixed scale parameter for error kernel (default 1)
#' @param priors        Optional named list of priors.
#' @param init          Optional named list of initial values (excludes λ).
#' @param n_iter        Total MCMC iterations (default 10000)
#' @param burnin        Burn-in iterations (default 1000)
#'
#' @return A list with:
#'   - `delta_samples`: numeric vector of sampled error rates post-burnin
#'   - `x_avg`: posterior mean of inclusion indicator `x`
#'   - `p_avg`: posterior mean of mixture weights `p`
#'
#' @useDynLib nicknamer, .registration=TRUE
#' @importFrom Matrix sparseMatrix rowSums diag
#' @import Rcpp
#' @export
#' @examples
#' # Assuming sampleZ_cpp is compiled and available
#' df <- data.frame(string = c("a","b","c"), count = c(10,5,1))
#' nbrs <- list(list(j=c(2),d=c(1)), list(j=c(1,3),d=c(1,2)), list(j=2,d=2))
#' out <- draw_gibbs(df, nbrs, lambda=1, n_iter=100, burnin=10)
draw_gibbs <- function(data,
                       neighbor_list,
                       lambda        = 1,
                       priors        = list(),
                       init          = list(),
                       n_iter        = 10000,
                       burnin        = 1000) {
  n <- data$count
  K <- nrow(data)

  # Default priors
  default_priors <- list(
    presence_alpha  = 1,
    presence_beta   = 99,
    dirichlet_alpha = 1,
    spike_epsilon   = 1e-8,
    error_alpha     = 1,
    error_beta      = 9
  )
  priors <- modifyList(default_priors, priors)
  list2env(priors, environment())

  # Init defaults (λ fixed externally)
  default_init <- list(
    x     = as.integer(n > mean(n)),
    p     = n / sum(n),
    delta = 0.1
  )
  init <- modifyList(default_init, init)
  list2env(init, environment())

  # Precompute G kernel lists: normalized exp(-d/λ)
  G_list <- vector("list", K)
  j_list <- vector("list", K)
  for(i in seq_len(K)) {
    js <- neighbor_list[[i]]$j
    ds <- neighbor_list[[i]]$d
    j_list[[i]] <- js
    w  <- exp(-ds / lambda)
    G_list[[i]] <- w / sum(w)
  }

  # Storage for post-burnin
  n_save        <- max(n_iter - burnin, 0)
  delta_samples <- numeric(n_save)
  x_sum         <- numeric(K)
  p_sum         <- numeric(K)

  # Dirichlet sampler
  rdirichlet <- function(alpha) {
    y <- rgamma(length(alpha), shape = alpha, rate = 1)
    y / sum(y)
  }

  save_idx <- 0
  pb <- txtProgressBar(min = 1, max = n_iter, style = 3)
  for(iter in seq_len(n_iter)) {
    setTxtProgressBar(pb, iter)

    # 1. Sample Z via C++
    zz <- sampleZ_cpp(
      n             = as.integer(n),
      p             = as.numeric(p),
      delta         = delta,
      neighbor_list = j_list,
      G_list        = G_list
    )
    Z <- sparseMatrix(i = zz$i, j = zz$j, x = zz$x, dims = c(K, K))

    # 2. Sufficient stats
    T_i <- rowSums(Z)
    S   <- sum(diag(Z)); E <- sum(Z) - S

    # 3. Update p | Z, x
    alpha_vec <- dirichlet_alpha * x + spike_epsilon
    p         <- rdirichlet(alpha_vec + T_i)

    # 4. Update x | Z
    lbf_vec <- lgamma(dirichlet_alpha + spike_epsilon + T_i) - lgamma(dirichlet_alpha + spike_epsilon) -
      (lgamma(spike_epsilon + T_i)             - lgamma(spike_epsilon))
    log_prior <- log(presence_alpha) - log(presence_beta)
    post_odds <- exp(log_prior + lbf_vec)
    x         <- rbinom(K, 1, post_odds / (1 + post_odds))

    # 5. Update delta | Z
    delta <- rbeta(1, error_alpha + E, error_beta + S)

    # Store
    if(iter > burnin) {
      save_idx               <- save_idx + 1
      delta_samples[save_idx] <- delta
      x_sum <- x_sum + x
      p_sum <- p_sum + p
    }
  }
  close(pb)

  list(
    delta_samples = delta_samples,
    x_avg         = x_sum / n_save,
    p_avg         = p_sum / n_save
  )
}


