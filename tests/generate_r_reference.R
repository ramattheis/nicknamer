#!/usr/bin/env Rscript
# Generates reference outputs from the R nicknamer package.
# Saved as JSON / CSV files in tests/r_reference/ for comparison with Python.

library(nicknamer)
library(Matrix)
library(jsonlite)

outdir <- file.path(dirname(normalizePath("tests/generate_r_reference.R")), "r_reference")
if (!dir.exists(outdir)) dir.create(outdir)

cat("=== Generating R reference data ===\n")

# ---- 1. clean_surnames -------------------------------------------------------
cat("1. clean_surnames\n")
raw <- c(
  "SMITH", "O'Brien", "McDonald", "von Neumann",
  "jones jr.", "WILLIAMS SR", "D4vis", "??unreadable??",
  "ng", "Xu", "Lee", "ab",
  "S?m?th", "????", "", NA,
  "Jo-Ann", "St. Claire", "de la Cruz"
)
raw[is.na(raw)] <- NA_character_
cleaned <- clean_surnames(raw)
writeLines(toJSON(list(input = raw, output = cleaned), na = "null"),
           file.path(outdir, "clean_surnames.json"))

# ---- 2. rdirichlet -----------------------------------------------------------
cat("2. rdirichlet\n")
set.seed(42)
alpha3  <- rdirichlet(c(1, 1, 1))
alpha10 <- rdirichlet(rep(2, 10))
writeLines(toJSON(list(alpha3 = alpha3, alpha10 = alpha10)),
           file.path(outdir, "rdirichlet.json"))

# ---- 3. make_kernel ----------------------------------------------------------
cat("3. make_kernel\n")
set.seed(0)
K <- 5
names5 <- c("smith", "smyth", "smit", "jones", "john")
D5 <- sparseMatrix(
  i = c(1,1,2,2,3),
  j = c(2,3,1,3,1),
  x = c(0.1, 0.2, 0.1, 0.15, 0.2),
  dims = c(K, K)
)
M5 <- sparseMatrix(
  i = c(1,1,2,2,3),
  j = c(2,3,1,3,1),
  x = c(1,1,1,1,1),
  dims = c(K, K)
)
Kmat <- make_kernel(D5, M5, delta = 0.2, lambda = 2.0)
writeLines(toJSON(list(
  K     = K,
  D_i   = c(1,1,2,2,3) - 1L,   # 0-based for Python
  D_j   = c(2,3,1,3,1) - 1L,
  D_x   = c(0.1, 0.2, 0.1, 0.15, 0.2),
  delta = 0.2,
  lambda = 2.0,
  Kmat  = as.matrix(Kmat)
), digits = 15), file.path(outdir, "make_kernel.json"))

# ---- 4. loglikelihood --------------------------------------------------------
cat("4. loglikelihood\n")
p5   <- c(0.5, 0.2, 0.15, 0.1, 0.05)
nobs5 <- c(50L, 20L, 15L, 10L, 5L)
ll <- loglikelihood(p5, D5, M5, delta = 0.2, lambda = 2.0, nobs5)
writeLines(toJSON(list(
  p     = p5,
  n_obs = nobs5,
  delta = 0.2,
  lambda = 2.0,
  ll    = ll
)), file.path(outdir, "loglikelihood.json"))

# ---- 5. find_neighbors -------------------------------------------------------
cat("5. find_neighbors\n")
names8 <- c("smith", "smyth", "smit", "jones", "john", "johnson", "brown", "browne")
nm <- find_neighbors(names8, method = "jw", max_dist = 0.2, ncores = 1)
# Extract sparse matrix components
Dm <- summary(nm$D)
Mm <- summary(nm$M)
writeLines(toJSON(list(
  names   = names8,
  method  = "jw",
  max_dist = 0.2,
  D_i     = Dm$i - 1L,
  D_j     = Dm$j - 1L,
  D_x     = Dm$x,
  M_i     = Mm$i - 1L,
  M_j     = Mm$j - 1L,
  M_x     = Mm$x
), digits = 15), file.path(outdir, "find_neighbors.json"))

# ---- 6. make_bayes_choice_dictionary -----------------------------------------
cat("6. make_bayes_choice_dictionary\n")
p8 <- c(0.30, 0.15, 0.10, 0.20, 0.10, 0.08, 0.05, 0.02)
nm8 <- find_neighbors(names8, method = "jw", max_dist = 0.2, ncores = 1)
bcd <- make_bayes_choice_dictionary(names8, nm8$D, p8, delta = 0.2, lambda = 2.0)
writeLines(toJSON(list(
  names      = names8,
  p          = p8,
  delta      = 0.2,
  lambda     = 2.0,
  observed   = bcd$observed,
  standard   = bcd$standard,
  p_standard = bcd$p_standard
), na = "null"), file.path(outdir, "make_bayes_choice_dictionary.json"))

# ---- 7. standardize_missing_name --------------------------------------------
cat("7. standardize_missing_name\n")
# Build a small standard_names table (deduped on 'standard')
std_df <- subset(bcd, !duplicated(standard))
std_df$observed <- NULL
# Use it to standardize a name not in the dictionary
test_name <- "smth"
# Manually replicate the env the function expects in R
standard_names <- std_df
lambda <- 2.0
delta  <- 0.2
method <- "jw"
res <- standardize_missing_name(test_name)
writeLines(toJSON(list(
  name         = test_name,
  result       = res,
  standard     = std_df$standard,
  p_standard   = std_df$p_standard,
  lambda       = lambda,
  delta        = delta,
  method       = method
), na = "null"), file.path(outdir, "standardize_missing_name.json"))

# ---- 8. synthetic_name_counts ------------------------------------------------
cat("8. synthetic_name_counts\n")
set.seed(42)
snc <- synthetic_name_counts()
# Save just the first 10 rows (random; used only to check schema/types)
writeLines(toJSON(list(
  colnames = colnames(snc),
  nrow     = nrow(snc),
  head     = head(snc, 10)
)), file.path(outdir, "synthetic_name_counts.json"))

cat("\nDone. Reference data written to", outdir, "\n")
