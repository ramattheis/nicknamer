library(nicknamer)        # for clean_surnames(), build_neighbor_matrices(), draw_gibbs()
library(data.table)       # fast I/O and aggregation

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

# 3) Run the MH-within-Gibbs sampler
out <- draw_gibbs(
  D           = D,
  M           = M,
  n_obs       = dt_counts$count,
  n_iter      = 20
)

# 4) Quick convergence diagnostics
plot(out$delta,  type = "l", main = "Trace of δ",     ylab = "delta")
plot(out$lambda, type = "l", main = "Trace of λ", ylab = "lambda")
plot(out$delta, out$lambda, xlab = "delta", ylab = "lambda", pch = ".")
mean(diff(out$lambda) != 0)

cat(sprintf("Posterior means: δ ≈ %.5f, λ ≈ %.4f\n",
            mean(out$delta), mean(out$lambda)))
