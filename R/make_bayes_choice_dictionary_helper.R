#'  Helper function for `make_bayes_choice_dictionary()`
#'
#' @param pack List of elements prepared by
#' @return A data.frame with the `observed` name, id of the `standard` name,
#'        `posterior` probability of the classification, and `bayes_choice`
#'        indicator of the posterior mode.
make_bayes_choice_dictionary_helper = function(pack){
  # Unpack
  observed <- pack$name
  nbs      <- pack$nb
  id       <- pack$id
  x        <- as.numeric(pack$x > 0.99)
  p        <- pack$p
  xs       <- as.numeric(pack$xs > 0.99)
  ps       <- pack$ps

  # Posterior for self
  phi_self <- (1 - delta) * p * x

  # Posterior for neighbors
  ws <- xs * exp(-nbs$d / lambda)
  ws <- if (max(ws) > 0) ws / sum(ws) else ws
  phis <- delta * ps * ws

  # Combine into one vector
  all_ids <- c(id, nbs$j)
  all_phis <- c(phi_self, phis)

  # Drop if everything is zero
  if (max(all_phis) == 0) {
    return(data.frame(observed = observed, candidate_id = NA_integer_, posterior = NA_real_, bayes_choice = NA_integer_))
  }

  # Normalizing phis
  all_phis <- all_phis / sum(all_phis)

  # Keep only those at least 10% as large as the max
  max_phi <- max(all_phis)
  keep <- which(all_phis >= max_phi / 10)
  max_pos <- which(all_phis == max_phi)[1]
  bc <- numeric(length(all_phis))
  bc[max_pos] <- 1

  out <- data.frame(
    observed     = observed,
    p_observed = p,
    candidate_id = all_ids[keep],
    posterior    = all_phis[keep],
    bayes_choice = bc[keep]
  )

  out[order(-out$posterior), ]

}
