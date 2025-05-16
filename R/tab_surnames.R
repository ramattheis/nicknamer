#' Clean and collapse a vector of (English-language) surnames
#'
#' Takes raw character strings, lower-cases them, removes spaces and apostrophes,
#' then blanks out any single-letter result or any string that is >50% non-alphanumeric.
#' Finally tabulates the cleaned names and returns a data.frame of unique names and their counts.
#'
#' @param raw_names Character vector of raw surname strings.
#' @return A data.frame with columns:
#'   * `name` — cleaned, unique surnames
#'   * `count` — number of occurrences
#' @export
tab_surnames <- function(raw_names) {

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

  # 6) removing numbers
  cleaned <- gsub("[0-9]","",cleaned)

  # 7) blank out names with any remaining non-letters
  cleaned[ grepl("[^a-z ]","",cleaned)] = ""

  # 8) blank out common issues
  cleaned[grepl("unreadable",cleaned)] = ""
  cleaned[grepl("unknown",cleaned)] = ""
  cleaned[grepl(" blank ",cleaned)] = ""
  cleaned[grepl("alias",cleaned)] = ""

  # 8) blank out any 1-char names or uncommon 2-char names
  cleaned[ nchar(cleaned) <= 1 ] = ""
  twochars = c( "ah", "ng", "ho", "ma", "ha", "lu", "la", "ba", "on", "wo",
                "ba", "an", "le", "un", "lo", "mo", "ca", "bo", "wa", "li",
                "go", "co", "he", "wu", "su", "da", "jo", "yu", "bu", "hi",
                "ko", "me", "ek", "ax", "re", "sy", "ey", "ox")
  cleaned[ nchar(cleaned) <= 2 & !(cleaned %in% twochars )] = ""

  # blank out names with too many missing characters
  cleaned[(nchar(gsub("\\s+","",cleaned)) / nchar(cleaned)) < 0.75] = ""

  # 9) tabulate and return sorted data.frame
  tbl <- table(cleaned, useNA = "no")
  df  <- data.frame(
    name  = names(tbl),
    count = as.integer(tbl),
    stringsAsFactors = FALSE
  )
  df <- df[order(-df$count), , drop = FALSE]
  rownames(df) <- NULL
  df
}
