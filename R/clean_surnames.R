#' Clean a vector of (English-language) surnames
#'
#' @description
#' `clean_surnames()` takes a character vector of raw surname strings and
#' returns a vector of "cleaned" or empty strings if the input is too ambiguous.
#' The rules are designed for handling surnames in historical US census data,
#' but may work well for English-language surnames more broadly.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Convert all strings to lowercase.
#'   \item Remove common suffixes “jr” and “sr”.
#'   \item Strip spaces and apostrophes.
#'   \item Standardize “missing‐letter” markers:
#'   \item Remove all digits.
#'   \item Blank out remaining names still containing non-letter/space characters.
#'   \item Blank out known placeholders: “unreadable”, “unknown”, etc.
#'   \item Blank out any name of length ≤1.
#'   \item Blank out uncommon names of length 2.
#'   \item Blank out names with >25% missing characters.
#' }
#'
#' @param raw_names Character vector of raw surname strings to be cleaned.
#' @return A character vector of cleaned or blank names the same length as `raw_names`
#' @export
clean_surnames <- function(raw_names) {

  # 1) lowercase
  cleaned <- tolower(raw_names)

  # 2) removes jr / sr
  cleaned <- gsub("\\s+(jr|sr)\\.?$", "", cleaned)

  # 3) strip spaces/apostrophes
  cleaned <- gsub("[ ']", "", cleaned)

  # 4) standardizing "missing letter" symbols
  cleaned <- gsub( "(?<=[a-z]{2})[^a-z](?=[a-z])", " ", cleaned, perl = TRUE)
  cleaned <- gsub( "(?<=[a-z])[^a-z](?=[a-z]{2})", " ", cleaned, perl = TRUE)
  cleaned <- gsub( "(?<=[a-z]{3})[^a-z]{2}(?=[a-z])", " ", cleaned, perl = TRUE)
  cleaned <- gsub( "(?<=[a-z])[^a-z]{2}(?=[a-z]{3})", " ", cleaned, perl = TRUE)
  cleaned <- gsub( "(^[^a-z]{1,2}(?=[a-z]{4}))|((?<=[a-z]{4})[^a-z]{1,2}$)", " ", cleaned, perl = TRUE)

  # 5) removing numbers
  cleaned <- gsub("[0-9]","",cleaned)

  # 6) blank out names with any remaining non-letters
  cleaned[ grepl("[^a-z ]","",cleaned)] = ""

  # 7) Replacing spaces with ?
  cleaned = gsub(" ","?",cleaned)

  # 8) blank out common issues
  cleaned[grepl("unreadable",cleaned)] = ""
  cleaned[grepl("unknown",cleaned)] = ""
  cleaned[grepl("?blank?",cleaned)] = ""
  cleaned[grepl("alias",cleaned)] = ""

  # 9) blank out any 1-char names or uncommon 2-char names
  cleaned[ nchar(cleaned) <= 1 ] = ""
  twochars = c( "ah", "ng", "ho", "ma", "ha", "lu", "la", "ba", "on", "wo",
                "ba", "an", "le", "un", "lo", "mo", "ca", "bo", "wa", "li",
                "go", "co", "he", "wu", "su", "da", "jo", "yu", "bu", "hi",
                "ko", "me", "ek", "ax", "re", "sy", "ey", "ox")
  cleaned[ nchar(cleaned) <= 2 & !(cleaned %in% twochars )] = ""

  # 10) blank out names with too many missing characters
  cleaned[(nchar(gsub("\\s+","",cleaned)) / nchar(cleaned)) < 0.75] = ""

  # Return the vector of clean surnames
  return(cleaned)

}
