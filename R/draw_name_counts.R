## generating a fake list of names 

draw_name_counts = function(){
  
  # Starting with the set of state names 
  names = state.name
  
  # Defining a function to garble names
  garble = function(x) {
    
    # split to chars
    s = strsplit(x, "", TRUE)[[1]]
    L = length(s)
    
    # how many substitutions (capped by string length)
    n = sample(1:3, 1, prob = c(0.6,0.3,0.1))
    
    # pick n distinct positions and substitute
    pos = sample.int(L, n)
    s[pos] <- sample(c(letters, rep("",30)), n, replace = TRUE)
    
    return(paste(s,collapse=""))
  }
  
  # Generating the list 
  clean_list = unlist(lapply(seq_along(names), function(i) rep(names[i], floor(2000 /(i+i))) ))
  garbled_list = unlist(lapply(clean_list, garble))
  full_list = c(rep(clean_list,10), garbled_list)
  
  # Collapsing to character counts 
  full_list = as.data.frame(full_list)
  colnames(full_list) = "name"
  full_list |> 
    dplyr::group_by(name) |>
    dplyr::summarise(count = dplyr::n()) |>
    dplyr::arrange(-count) |>
    as.data.frame() -> name_counts
  
  return(name_counts)
  
}



