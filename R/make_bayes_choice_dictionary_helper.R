#'  Helper function for `make_bayes_choice_dictionary()`
#'
#' @param pack List of elements prepared by
#' @return A one-row data.frame with the `observed` name and id of the `standard` name.
make_bayes_choice_dictionary_helper = function(pack){
  # unpacking
  observed = pack$name
  nbs = pack$nb
  id = pack$id
  x = as.numeric(pack$x > 0.99)
  p = pack$p
  xs = as.numeric(pack$xs > 0.99)
  ps = pack$ps

  # Computing posterior
  phi_self = (1-delta)*p*x

  # Computing off-diagonal posterior
  ws = xs*exp(-nbs$d/lambda)
  ws = if(max(ws) > 0){ws/sum(ws)} else {ws}
  phis = delta*ps*ws

  # Picking posterior mode
  if(max(c(phi_self, phis)) == 0){
    bayes_choice_id = NA
  } else {
    if(phi_self > max(phis)){
      bayes_choice_id = id
    } else {
      bayes_choice_id = nbs$j[which(phis == max(phis))][1]
    }
  }

  # Returning self + bayes choice
  as.data.frame(cbind(observed, bayes_choice_id))
}
