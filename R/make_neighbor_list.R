# Function to generate neighbor list 
make_neighbor_list = function(
    strings,
    min_dist = 1L,
    max_dist = 3L,
    show_progress = TRUE) {
  if (!is.character(strings)) stop("`strings` must be a character vector.")
  K   <- length(strings)
  lens <- nchar(strings)
  
  neighbor_list <- vector("list", K)
  names(neighbor_list) <- strings
  
  if (show_progress) {
    pb <- txtProgressBar(0, K, style = 3)
    on.exit(close(pb), add = TRUE)
  }
  
  for (i in seq_len(K)) {
    # 1) Pre‐filter by length difference to cut down comparisons
    cand  <- which(abs(lens - lens[i]) <= max_dist)
    # 2) Compute Levenshtein distances only on those candidates
    dvec  <- adist(strings[i], strings[cand])
    # 3) Select those within [min_dist, max_dist]
    sel <- which(dvec >= min_dist & dvec <= max_dist)
    # 4) Removing self from list 
    sel = sel[sel!=i]
    
    # Throwing an error if an element doesn't have neighbors
    if(length(sel) == 0){
      stop("Element has no neighbors; consider widening max distance.")
    }
    
    neighbor_list[[i]] <- list(
      j = cand[sel],
      d = dvec[sel]
    )
    
    if (show_progress) setTxtProgressBar(pb, i)
  }
  
  return(neighbor_list)
}
