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
p_vec <- p_vec^1.05/(sum(p_vec^1.05))

# 4) Observed counts vector
n_obs <- dt_counts$count

# 5) Run the MH-within-Gibbs sampler
out <- draw_mcmc(
  p           = p_vec,
  D           = D,
  M           = M,
  n_obs       = n_obs,
  n_iter      = 2000
)

# 6) Quick convergence diagnostics
plot(out$delta[1000:2000],  type = "l", main = "Trace of δ",     ylab = "delta")
plot(out$lambda[1000:2000], type = "l", main = "Trace of λ", ylab = "lambda")
plot(out$delta[1000:2000], out$lambda[1000:2000], xlab = "delta", ylab = "lambda", pch = ".")
mean(diff(out$lambda[1000:2000]) != 0)

cat(sprintf("Posterior means: δ ≈ %.5f, λ ≈ %.4f\n",
            mean(out$delta[1000:2000]), mean(out$lambda[1000:2000])))
