#!/usr/bin/env Rscript
# tests/test_workflow.R
#
# End-to-end smoke test for the nicknamer R package.
# Walks through every major step of the workflow and checks that results
# look sensible.  Run from the package root:
#
#   Rscript tests/test_workflow.R
#
# The US surname dictionary download is tested explicitly; it requires an
# internet connection and ~30 MB of bandwidth.

library(nicknamer)

pass <- 0L
fail <- 0L

check <- function(label, expr) {
  result <- tryCatch(
    { val <- expr; list(ok = isTRUE(val), val = val, err = NULL) },
    error = function(e) list(ok = FALSE, val = NULL, err = conditionMessage(e))
  )
  if (result$ok) {
    cat(sprintf("  PASS  %s\n", label))
    pass <<- pass + 1L
  } else {
    msg <- if (!is.null(result$err)) result$err else paste("got:", deparse(result$val))
    cat(sprintf("  FAIL  %s  [%s]\n", label, msg))
    fail <<- fail + 1L
  }
}

# =============================================================================
cat("\n=== 1. clean_surnames ===\n")
# =============================================================================

raw     <- c("SMITH", "O'Brien", "jones jr.", "WILLIAMS SR", "D4vis",
             "??unreadable??", "ng", "Xu", "Jo-Ann", "de la Cruz", NA)
cleaned <- clean_surnames(raw)

check("returns same length as input",   length(cleaned) == length(raw))
check("SMITH → smith",                  cleaned[1] == "smith")
check("O'Brien → obrien",               cleaned[2] == "obrien")
check("jones jr. → jones",             cleaned[3] == "jones")
check("WILLIAMS SR → williams",         cleaned[4] == "williams")
check("D4vis has digit removed",        cleaned[5] == "dvis")
check("unreadable → blank",             cleaned[6] == "")
check("ng kept (valid 2-char)",         cleaned[7] == "ng")
check("Xu blanked (invalid 2-char)",    cleaned[8] == "")
check("Jo-Ann → jo?ann",               cleaned[9] == "jo?ann")
check("de la Cruz → delacruz",          cleaned[10] == "delacruz")
check("NA → NA",                        is.na(cleaned[11]))

# =============================================================================
cat("\n=== 2. synthetic_name_counts ===\n")
# =============================================================================

set.seed(1)
snc <- synthetic_name_counts()

check("returns a data.frame",           is.data.frame(snc))
check("has 'name' column",              "name"  %in% colnames(snc))
check("has 'count' column",             "count" %in% colnames(snc))
check("counts are positive integers",   all(snc$count > 0))
check("sorted descending by count",     !is.unsorted(-snc$count))
check("no duplicate names",             !any(duplicated(snc$name)))
check("at least 50 rows",               nrow(snc) >= 50)

# =============================================================================
cat("\n=== 3. find_neighbors ===\n")
# =============================================================================

# Use a small subset for speed
top_names <- head(snc$name[nchar(snc$name) > 0], 40)
nm <- find_neighbors(top_names, method = "jw", max_dist = 0.2, ncores = 1)

check("returns list with D and M",      is.list(nm) && all(c("D","M") %in% names(nm)))
check("D is a sparse matrix",          inherits(nm$D, "sparseMatrix"))
check("M is a sparse matrix",          inherits(nm$M, "sparseMatrix"))
check("D and M are square",            all(dim(nm$D) == c(length(top_names), length(top_names))))
check("D is symmetric",                Matrix::norm(nm$D - Matrix::t(nm$D), "1") < 1e-10)
check("M is symmetric",                Matrix::norm(nm$M - Matrix::t(nm$M), "1") < 1e-10)
check("D values are non-negative",     all(nm$D@x >= 0))
check("M values are 0 or 1",          all(nm$M@x %in% c(0, 1)))
check("D and M have same sparsity",    Matrix::nnzero(nm$D) == Matrix::nnzero(nm$M))

D <- nm$D
M <- nm$M

# =============================================================================
cat("\n=== 4. make_kernel ===\n")
# =============================================================================

Kmat <- make_kernel(D, M, delta = 0.15, lambda = 1.5)
K    <- length(top_names)

check("returns a matrix-like object",  !is.null(dim(Kmat)))
check("correct dimensions",            all(dim(Kmat) == c(K, K)))
check("all values in [0, 1]",         all(Kmat@x >= 0) && all(Kmat@x <= 1))

# Columns for names with neighbours should sum to 1
has_nbr      <- Matrix::colSums(M) > 0
col_sums     <- Matrix::colSums(Kmat)
check("connected columns sum to 1",   all(abs(col_sums[has_nbr] - 1) < 1e-9))
check("isolated columns sum to 1-delta", all(abs(col_sums[!has_nbr] - 0.85) < 1e-9))

# =============================================================================
cat("\n=== 5. loglikelihood ===\n")
# =============================================================================

top_counts <- head(snc$count[nchar(snc$name) > 0], 40)
p_init     <- top_counts / sum(top_counts)
ll         <- loglikelihood(p_init, D, M, delta = 0.15, lambda = 1.5, n_obs = top_counts)

check("returns a single finite number", length(ll) == 1 && is.finite(ll))
check("log-likelihood is negative",    ll < 0)

# =============================================================================
cat("\n=== 6. draw_gibbs ===\n")
# =============================================================================

cat("  (running 300 iterations — this may take a moment)\n")
set.seed(42)
post <- draw_gibbs(
  D           = D,
  M           = M,
  n_obs       = top_counts,
  n_iter      = 300,
  delta_init  = 0.1,
  lambda_init = 1.0
)

burnin <- 150

check("returns list with expected keys",
      all(c("delta","lambda","likelihood","p_first5","p_mean") %in% names(post)))
check("delta chain length == n_iter",   length(post$delta)  == 300)
check("lambda chain length == n_iter",  length(post$lambda) == 300)
check("no NaN in delta chain",          !any(is.nan(post$delta)))
check("no NaN in lambda chain",         !any(is.nan(post$lambda)))
check("no NaN in p_mean",               !any(is.nan(post$p_mean)))
check("p_mean sums to 1",               abs(sum(post$p_mean) - 1) < 1e-10)
check("delta values in (0, 1)",         all(post$delta > 0) && all(post$delta < 1))
check("lambda values positive",         all(post$lambda > 0))
check("MH moves (delta not constant)",  length(unique(post$delta))  > 1)
check("MH moves (lambda not constant)", length(unique(post$lambda)) > 1)

accept_rate <- mean(diff(post$delta) != 0)
check("MH acceptance rate > 1%",        accept_rate > 0.01)
cat(sprintf("  (acceptance rate: %.1f%%)\n", accept_rate * 100))

delta_post  <- post$delta[burnin:300]
lambda_post <- post$lambda[burnin:300]
cat(sprintf("  Posterior means: delta=%.4f  lambda=%.4f\n",
            mean(delta_post), mean(lambda_post)))

# =============================================================================
cat("\n=== 7. make_bayes_choice_dictionary ===\n")
# =============================================================================

bcd <- make_bayes_choice_dictionary(
  names  = top_names,
  D      = D,
  p      = post$p_mean,
  delta  = mean(delta_post),
  lambda = mean(lambda_post)
)

check("returns a data.frame",            is.data.frame(bcd))
check("has 'observed' column",           "observed"   %in% colnames(bcd))
check("has 'standard' column",           "standard"   %in% colnames(bcd))
check("has 'p_standard' column",         "p_standard" %in% colnames(bcd))
check("observed matches input names",    all(bcd$observed == top_names))
check("standard is character",           is.character(bcd$standard))
check("p_standard is non-negative",
      all(bcd$p_standard[!is.na(bcd$p_standard)] > 0))

# =============================================================================
cat("\n=== 8. standardize_names ===\n")
# =============================================================================

std <- standardize_names(
  names      = top_names,
  dictionary = bcd,
  lambda     = mean(lambda_post),
  delta      = mean(delta_post)
)

check("returns same length as input",    length(std) == length(top_names))
check("output is character",             is.character(std))

# =============================================================================
cat("\n=== 9. load_us_dictionary (requires internet) ===\n")
# =============================================================================

cat("  Downloading US surname dictionary...\n")
us_dict <- tryCatch(
  load_us_dictionary(),
  error = function(e) {
    cat(sprintf("  FAIL  download failed: %s\n", conditionMessage(e)))
    fail <<- fail + 1L
    NULL
  }
)

if (!is.null(us_dict)) {
  check("returns a data.frame",          is.data.frame(us_dict))
  check("has 'observed' column",         "observed" %in% colnames(us_dict))
  check("has 'standard' column",         "standard" %in% colnames(us_dict))
  check("at least 1000 rows",            nrow(us_dict) >= 1000)
  check("no completely empty observed",  !any(us_dict$observed == "" & !is.na(us_dict$observed)))

  # Second call should use the cache (no re-download)
  us_dict2 <- load_us_dictionary()
  check("second call returns same object (cached)",
        identical(us_dict, us_dict2))

  # =============================================================================
  cat("\n=== 10. standardize_us_surnames ===\n")
  # =============================================================================

  test_names <- c("smith", "jones", "williams", "browne", "")
  std_us     <- standardize_us_surnames(test_names)

  check("returns same length as input",  length(std_us) == length(test_names))
  check("'smith' gets a standard form",  !is.na(std_us[1]))
  check("empty string returns NA",       is.na(std_us[5]))
  check("standardized values are in dictionary$standard",
        all(std_us[!is.na(std_us)] %in% us_dict$standard))
}

# =============================================================================
cat("\n=== 11. clean_firstnames ===\n")
# =============================================================================

raw_first <- c("JOHN Edward", "Wm", "Thos Jr.", "J", "Mary Ann",
               "jno", "Elizabeth", "??unreadable??", NA)
cf <- clean_firstnames(raw_first)

check("returns same length as input",    length(cf) == length(raw_first))
check("JOHN Edward → john (drops middle)", cf[1] == "john")
check("Wm → william (abbreviation)",     cf[2] == "william")
check("Thos Jr. → thomas",              cf[3] == "thomas")
check("J is kept (not blanked)",         cf[4] == "j")
check("Mary Ann → mary (drops middle)", cf[5] == "mary")
check("jno → john (abbreviation)",      cf[6] == "john")
check("Elizabeth → elizabeth",           cf[7] == "elizabeth")
check("unreadable → blank",              cf[8] == "")
check("NA → NA",                         is.na(cf[9]))

# drop_middle = FALSE keeps full first name
cf2 <- clean_firstnames(c("John Edward"), drop_middle = FALSE)
check("drop_middle=FALSE keeps full name", cf2[1] == "john?edward")

# expand_abbreviations = FALSE preserves abbreviation
cf3 <- clean_firstnames(c("Wm"), expand_abbreviations = FALSE)
check("expand_abbreviations=FALSE keeps wm", cf3[1] == "wm")

# =============================================================================
cat("\n=== 12. grab_middle_initial ===\n")
# =============================================================================

raw_mi <- c("John Edward", "Mary Ann", "John", "J E", "William Jr.", NA)
mi <- grab_middle_initial(raw_mi)

check("returns same length as input",    length(mi) == length(raw_mi))
check("John Edward → E",                mi[1] == "E")
check("Mary Ann → A",                   mi[2] == "A")
check("John (no middle) → NA",          is.na(mi[3]))
check("J E → E",                        mi[4] == "E")
check("Jr. stripped before split",      is.na(mi[5]))
check("NA → NA",                         is.na(mi[6]))

# =============================================================================
cat("\n=== 13. load_nickname_dictionary ===\n")
# =============================================================================

cat("  Downloading nickname dictionary...\n")
nick_dict <- tryCatch(
  load_nickname_dictionary(),
  error = function(e) {
    cat(sprintf("  FAIL  download failed: %s\n", conditionMessage(e)))
    fail <<- fail + 1L
    NULL
  }
)

if (!is.null(nick_dict)) {
  check("returns a data.frame",              is.data.frame(nick_dict))
  check("has 'nickname' column",             "nickname"  %in% colnames(nick_dict))
  check("has 'canonical' column",            "canonical" %in% colnames(nick_dict))
  check("has 'sex' column",                  "sex"       %in% colnames(nick_dict))
  check("at least 100 rows",                 nrow(nick_dict) >= 100)
  check("bob maps to robert",
        any(nick_dict$nickname == "bob" & nick_dict$canonical == "robert"))
  check("second call uses cache",
        identical(nick_dict, load_nickname_dictionary()))

  # =============================================================================
  cat("\n=== 14. standardize_nicknames ===\n")
  # =============================================================================

  test_nicks <- c("bob", "betty", "alex", "john", "notanickname")
  std_nicks  <- standardize_nicknames(test_nicks, dictionary = nick_dict)

  check("returns same length as input",      length(std_nicks) == length(test_nicks))
  check("bob → robert",                      std_nicks[1] == "robert")
  check("betty → elizabeth",                 std_nicks[2] == "elizabeth")
  check("alex maps to something",            !is.na(std_nicks[3]))
  check("john (not a nickname) unchanged",   std_nicks[4] == "john")
  check("unknown name unchanged",            std_nicks[5] == "notanickname")

  # Sex filtering
  std_male   <- standardize_nicknames("alex", sex = "male",   dictionary = nick_dict)
  std_female <- standardize_nicknames("alex", sex = "female", dictionary = nick_dict)
  check("alex male → alexander",             std_male[1]   == "alexander")
  check("alex female → alexandra",           std_female[1] == "alexandra")
}

# =============================================================================
cat("\n=== Summary ===\n")
# =============================================================================

cat(sprintf("\n%d passed, %d failed\n", pass, fail))
if (fail > 0) quit(status = 1L)
