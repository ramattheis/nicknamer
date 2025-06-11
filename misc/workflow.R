library(nicknamer)        # for clean_surnames(), build_neighbor_matrices(), draw_gibbs()
library(data.table)       # fast I/O and aggregation
library(Matrix)           # sparse‐matrix support

# 1) Load & clean raw name counts
dt <- fread("misc/chunk.csv")
dt[, namelast := clean_surnames(namelast)]
setnames(dt, "namelast", "name")
dt_counts <- dt[, .(count = .N), by = name][order(-count, name)]

# 2) Build sparse distance (D) and mask (M) matrices
nmats <- find_neighbors(
  names  = dt_counts$name,
  method   = "jw",
  max_dist = 0.2,
  ncores   = 10
)
D <- nmats$D
M <- nmats$M

# 3) Fix p exogenously as slightly squished empirical frequencies
p_vec <- dt_counts$count / sum(dt_counts$count)

# 4) Observed counts vector
n_obs <- dt_counts$count

library(Matrix)
p_init      = p_vec
D           = D
M           = M
n_obs       = n_obs
n_iter      = 2000
delta_init   = 0.1
lambda_init  = 1.0
sd_logit     = 0.1
sd_loglam    = 0.1
alpha_dir    = 1
prior_delta  = function(d) dbeta(d, 9, 1,  log = TRUE)
prior_lambda = function(l) dgamma(l, 1, 0.1, log = TRUE)


# 5) Run the MH-within-Gibbs sampler
out <- draw_gibbs(
  p_init      = p_vec,
  D           = D,
  M           = M,
  n_obs       = n_obs,
  n_iter      = 200
)

# 6) Quick convergence diagnostics
plot(out$delta,  type = "l", main = "Trace of δ",     ylab = "delta")
plot(out$lambda, type = "l", main = "Trace of λ", ylab = "lambda")
plot(out$delta, out$lambda, xlab = "delta", ylab = "lambda", pch = ".")
mean(diff(out$lambda) != 0)

cat(sprintf("Posterior means: δ ≈ %.5f, λ ≈ %.4f\n",
            mean(out$delta), mean(out$lambda)))
