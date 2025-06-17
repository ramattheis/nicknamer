#' Helper function for `standardize_names()`, one name at a time
#'
#' @description
#' Takes a single “clean” name string (outside the dictionary) and
#' returns the best‐guess standardized name, or `NA` when no candidate
#' passes the posterior‐probability cutoff of 0.75. Relies on the output of
#' `draw_gibbs()` and `make_bayes_choice_dictionary()`, which populate
#' `standard_names$standard`, `standard_names$p_standard`, as well as the
#' globals `method`, `lambda`, and `delta`.
#'
#' @param name A single character string to standardize.
#' @return A single character (the standardized name) or `NA`.
#' @importFrom stringdist stringdist
#' @export
standardize_missing_name <- function(name) {
  # globals → locals
  std     <- standard_names$standard
  p_std   <- standard_names$p_standard
  dp      <- delta * p_std
  penalty <- 1 - exp(-0.8 * (nchar(std) - 1))

  # 1) distances & threshold
  D <- stringdist::stringdist(name, std, method = method)
  thresh <- if (identical(method, "jw")) 0.15 else 3
  D[D > thresh] <- Inf

  # 2) weights & φ
  W   <- exp(-D * lambda)
  W[is.infinite(D)] <- 0
  φ   <- W * dp    # element‐wise

  # 3) posterior max
  total <- sum(φ)
  if (total == 0) return(NA_character_)
  best_idx <- which.max(φ)
  max_post <- (φ[best_idx] / total) * penalty[best_idx]

  # 4) cutoff
  if (is.na(max_post) || max_post < 0.5) return(NA_character_)

  # 5) map back
  std[best_idx]
}

