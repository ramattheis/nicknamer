#' Extract the middle initial from a raw first-name string
#'
#' @description
#' `grab_middle_initial()` inspects a raw first-name string (which may include
#' a middle name or initial) and returns the middle initial as a single
#' upper-case character, or `NA` if no middle name / initial is present.
#'
#' The function strips common trailing suffixes (Jr, Sr, I, II, III, IV) before
#' splitting, so they are not mistaken for middle names.
#'
#' @param raw_names Character vector of raw first-name strings.
#' @return A character vector of upper-case middle initials (single letters),
#'   or `NA` where no middle name is present.  Same length as `raw_names`.
#' @export
grab_middle_initial <- function(raw_names) {

  # Note NA positions
  is_na   <- is.na(raw_names)
  cleaned <- ifelse(is_na, "", raw_names)

  # Lowercase and strip trailing suffixes
  cleaned <- tolower(cleaned)
  cleaned <- gsub("\\s+(jr\\.?|sr\\.?|i{1,3}|iv)\\s*$", "", cleaned)
  cleaned <- trimws(cleaned)

  # Split on whitespace; second token → middle initial
  tokens <- strsplit(cleaned, "\\s+")

  initials <- vapply(tokens, function(t) {
    if (length(t) >= 2L && nchar(t[2L]) > 0L) {
      toupper(substr(t[2L], 1L, 1L))
    } else {
      NA_character_
    }
  }, character(1L))

  # Restore NAs
  initials[is_na] <- NA_character_

  return(initials)
}
