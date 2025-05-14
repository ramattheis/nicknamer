#' Generate a fake dataset of name counts with “garbled” variants
#'
#' This function builds a synthetic name‐count dataset by starting from the
#' built-in `state.name` vector, replicating each name, applying random
#' character substitutions (“garbling”), and then counting the frequencies of
#' both clean and garbled names.
#'
#' @return A data frame with two columns:
#'   - `name`: character, the (possibly garbled) name
#'   - `count`: integer, the number of times it appears
#'
#' @examples
#' # Generate a toy dataset
#' df <- synthetic_name_counts()
#' head(df)
#'
#' @export
synthetic_name_counts <- function(){

  # Starting with the set of state names
  names = state.name

  # Defining a function to garble names
  garble <- function(x) {
    # split to chars
    s <- strsplit(x, "", TRUE)[[1]]
    L <- length(s)

    # how many substitutions (capped by string length)
    n <- sample(1:3, 1, prob = c(0.6,0.3,0.1))

    # pick n distinct positions and substitute
    pos <- sample.int(L, n)
    s[pos] <- sample(c(letters, rep("",30)), n, replace = TRUE)

    paste(s, collapse = "")
  }

  # Generating the list of clean names
  clean_list <- unlist(lapply(seq_along(names), function(i) {
    rep(names[i], floor(2000 / (i + i)))
  }))

  # Garble each clean name once
  garbled_list <- unlist(lapply(clean_list, garble))

  # Combine clean (10×) and garbled once
  full_list <- c(rep(clean_list, 10), garbled_list)

  # Collapsing to character counts
  name_counts <- full_list |>
    as.data.frame(stringsAsFactors = FALSE) |>
    setNames("name") |>
    dplyr::group_by(name) |>
    dplyr::summarise(count = dplyr::n(), .groups = "drop") |>
    dplyr::arrange(-count) |>
    as.data.frame()

  return(name_counts)

}



