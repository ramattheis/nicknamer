#' Helper function for `standardize_names()`
#'
#' @description takes a vector of clean name strings, outside the dictionary!,
#' and returns standardized names. Relies on the output of `draw_gibbs()` and `make_bayes_choice_dictionary`.
#'
#' @param name A character string to standardize.
#'
#' @return A character string of the standardized name or NA.
standardize_missing_name <- function(
    name
) {
  # Compute string distances between `name` and each canonical name
  dists <- stringdist(name, standard_names$standard, method = method)

  # Threshold based on method
  if (identical(method, "jw")) {
    thresh <- 0.2
  } else {
    thresh <- 3
  }
  keep_idx <- which(dists <= thresh)

  # If no candidate within threshold, return NA
  if (length(keep_idx) == 0) {
    return(NA_character_)
  }

  # Extract distances and priors for kept candidates
  d_keep <- dists[keep_idx]
  p_keep <- standard_names$p_standard[keep_idx]

  # Compute weights: w_i = exp(-d_i / lambda)
  ws <- exp(-d_keep / lambda)
  if (max(ws) > 0) {
    ws <- ws / sum(ws)
  }

  # Compute unnormalized posterior mass: phi_i = delta * p_i * w_i
  phi_i <- delta * p_keep * ws

  # If all phi_i are zero, return NA
  if (max(phi_i) == 0) {
    return(NA_character_)
  }

  # Normalize to get posterior probabilities
  phi_norm <- phi_i / sum(phi_i)

  # If the max posterior is below 0.75, return NA
  if (max(phi_norm) < 0.75) {
    return(NA_character_)
  }

  # Otherwise, select the index of the best candidate
  best_idx_within <- which(phi_norm == max(phi_norm))[1]
  best_global_idx  <- keep_idx[best_idx_within]

  # Return the best standardized name
  return(unname(standard_names$standard[best_global_idx]))
}
