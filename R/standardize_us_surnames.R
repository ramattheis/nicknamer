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

  # Downloading US surname dictionary


}
