#' Return Bayes choices for surnames in US historical censuses
#'
#' @description takes a vector of cleaned name strings and returns standardized names.
#' Jumps to the final dictionary for
#'
#' @param names A vector of character strings to standardize. It's assumed that
#'               name strings have already been "cleaned" e.g. using
#'               `clean_surnames()` for English-language surnames.
#'
#' @return A data.frame with two columns:
#'   - `observed`: string vector of observed names.
#'   - `standard`: string vector of standardized names.
#'
#' @export
standardize_us_surnames <- function(names) {

  # Checking -- does it seem like names are cleaned?
  if(any(grepl("[A-Z0-9[:punct:]]",names))){
    stop("`names` has upper case letters, numbers, or punctuation. Apply `clean_surnames()` before `standardize_us_surnames()`.")
    }

  # Downloading US surname dictionary
  us_dictionary <- load_us_dictionary()

  # Merging and returning
  us_dictionary[match(names, us_dictionary$observed),"standard"]

}
