#' Helper function for `standardize_names()`
#'
#' @description
#' Takes a character vector of “clean” name strings (outside the dictionary) and
#' returns the best‐guess standardized names, or `NA` when no candidate passes
#' the posterior‐probability cutoff of 0.75. Relies on the output of
#' `draw_gibbs()` and `make_bayes_choice_dictionary()`, which populate
#' `standard_names$standard`, `standard_names$p_standard`, as well as the
#' globals `method`, `lambda`, and `delta`.
#'
#' @param names_vec A character vector of names to standardize.
#' @return A character vector of the same length as `names_vec`, containing
#'   the standardized name or `NA` for each entry.
#' @importFrom stringdist stringdistmatrix
#' @export
standardize_missing_name <- function(names_vec) {

  # 1) compute n × m distance matrix
  D <- stringdist::stringdistmatrix(
    a = names_vec,
    b = standard_names$standard,
    method = method
  )

  # 2) threshold by method
  thresh <- if (identical(method, "jw")) 0.15 else 3
  mask  <- D <= thresh

  # 3) raw weights (zero out beyond threshold)
  W <- exp(-D * lambda)
  W[!mask] <- 0

  # 4) unnormalized posterior mass φ_ij = δ * p_j * W_ij
  phi <- sweep(
    W,
    MARGIN = 2,
    STATS  = delta * standard_names$p_standard,
    FUN    = `*`
  )

  # 5) normalize across each row
  row_sums  <- rowSums(phi)
  phi_norm  <- phi / row_sums
  phi_norm[row_sums == 0, ] <- NA_real_

  # 6) extract both index and value in one go
  res <- t(
    apply(phi_norm, 1, function(x) {
      if (all(is.na(x))) {
        # if the entire row is NA, return (idx = NA, val = NA)
        c(idx = NA_integer_, val = NA_real_)
      } else {
        i <- which.max(x)
        c(idx = i,        # index of the max
          val = x[i])     # the max itself
      }
    })
  )

  best_idx <- res[, "idx"]
  max_post <- res[, "val"]

  # 7) penalty
  penalty     <- 1 - exp(-0.8 * (nchar(standard_names$standard[best_idx]) - 1))
  max_post[!is.na(max_post)] <- max_post[!is.na(max_post)] * penalty[!is.na(max_post)]

  # 8) cutoff
  bad         <- is.na(max_post) | max_post < 0.5
  best_idx[bad] <- NA_integer_

  # 9) map indices back to standardized names
  cbind(names_vec,standard_names$standard[unlist(best_idx)])
}
