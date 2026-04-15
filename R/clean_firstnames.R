#' Clean a vector of (English-language) first names
#'
#' @description
#' `clean_firstnames()` takes a character vector of raw first-name strings and
#' returns a vector of cleaned strings suitable for string-distance comparisons.
#' The rules follow the approach of Abramitzky & Boustan / Yildirim for
#' historical US census first names.
#'
#' Unlike `clean_surnames()`, single-character names are **not** blanked out,
#' because abbreviated first names such as "J" or "E" are common and
#' informative in historical census records.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Convert to lowercase.
#'   \item Remove common trailing suffixes (Jr, Sr, I, II, III, IV).
#'   \item If `drop_middle = TRUE`, retain only the first whitespace-delimited
#'         token (i.e., drop the middle name or initial).
#'   \item Strip apostrophes.
#'   \item Remove all digits.
#'   \item Standardize "missing-letter" markers (same convention as
#'         `clean_surnames()`).
#'   \item Blank out names with remaining non-letter characters.
#'   \item Replace spaces with `?`.
#'   \item Blank out known placeholder strings.
#'   \item Blank out names where > 25% of characters are missing markers.
#'   \item If `expand_abbreviations = TRUE`, replace known census
#'         abbreviations with their full forms (e.g. "wm" → "william").
#' }
#'
#' @section Abbreviation expansion table:
#' Only unambiguous single-name abbreviations are expanded. "J" is not expanded
#' because it could be John, James, Joseph, etc.
#'
#' | Abbreviation | Expansion     |
#' |---|---|
#' | wm           | william       |
#' | thos         | thomas        |
#' | jno          | john          |
#' | jhn          | john          |
#' | jas          | james         |
#' | chas         | charles       |
#' | geo          | george        |
#' | benj         | benjamin      |
#' | robt         | robert        |
#' | richd        | richard       |
#' | edwd         | edward        |
#' | saml         | samuel        |
#' | danl         | daniel        |
#' | michl        | michael       |
#' | nathl        | nathaniel     |
#' | alexr        | alexander     |
#' | abm          | abraham       |
#' | christr      | christopher   |
#' | nichs        | nicholas      |
#' | andr         | andrew        |
#' | lawr         | lawrence      |
#' | archd        | archibald     |
#' | eliz         | elizabeth     |
#'
#' @param raw_names Character vector of raw first-name strings.
#' @param expand_abbreviations Logical. If `TRUE` (the default), apply the
#'   conservative census abbreviation expansion table to whole-name matches.
#' @param drop_middle Logical. If `TRUE` (the default), drop any middle name
#'   or middle initial, retaining only the first whitespace-delimited token.
#' @return A character vector of cleaned first names the same length as
#'   `raw_names`. `NA` inputs return `NA`.
#' @export
clean_firstnames <- function(raw_names, expand_abbreviations = TRUE, drop_middle = TRUE) {

  # Conservative census abbreviation expansion table (whole-name matches only)
  abbr_table <- c(
    wm      = "william",     thos    = "thomas",
    jno     = "john",        jhn     = "john",
    jas     = "james",       chas    = "charles",
    geo     = "george",      benj    = "benjamin",
    robt    = "robert",      richd   = "richard",
    edwd    = "edward",      saml    = "samuel",
    danl    = "daniel",      michl   = "michael",
    nathl   = "nathaniel",   alexr   = "alexander",
    abm     = "abraham",     christr = "christopher",
    nichs   = "nicholas",    andr    = "andrew",
    lawr    = "lawrence",    archd   = "archibald",
    eliz    = "elizabeth"
  )

  # 1) Note NA positions; treat NA as "" throughout
  is_na   <- is.na(raw_names)
  cleaned <- ifelse(is_na, "", raw_names)

  # 2) Unicode normalization (accented chars → ASCII)
  cleaned <- iconv(cleaned, from = "UTF-8", to = "ASCII//TRANSLIT")
  cleaned <- ifelse(is.na(cleaned), "", cleaned)

  # 3) Lowercase
  cleaned <- tolower(cleaned)

  # 4) Remove common trailing suffixes (jr, sr, i, ii, iii, iv)
  cleaned <- gsub("\\s+(jr\\.?|sr\\.?|i{1,3}|iv)\\s*$", "", cleaned)

  # 5) Strip leading/trailing whitespace
  cleaned <- trimws(cleaned)

  # 6) Optionally drop middle name/initial (keep first token only)
  if (drop_middle) {
    cleaned <- sub("\\s+.*$", "", cleaned)
  }

  # 7) Strip apostrophes
  cleaned <- gsub("'", "", cleaned)

  # 8) Remove digits
  cleaned <- gsub("[0-9]", "", cleaned)

  # 9) Standardize "missing-letter" markers (same rules as clean_surnames)
  cleaned <- gsub("(?<=[a-z]{2})[^a-z](?=[a-z])",         " ", cleaned, perl = TRUE)
  cleaned <- gsub("(?<=[a-z])[^a-z](?=[a-z]{2})",         " ", cleaned, perl = TRUE)
  cleaned <- gsub("(?<=[a-z]{3})[^a-z]{2}(?=[a-z])",      " ", cleaned, perl = TRUE)
  cleaned <- gsub("(?<=[a-z])[^a-z]{2}(?=[a-z]{3})",      " ", cleaned, perl = TRUE)
  cleaned <- gsub("(^[^a-z]{1,2}(?=[a-z]{4}))|((?<=[a-z]{4})[^a-z]{1,2}$)",
                  " ", cleaned, perl = TRUE)

  # 10) Blank out names with any remaining non-letter/non-space chars
  cleaned[grepl("[^a-z ]", cleaned)] <- ""

  # 11) Replace spaces with ?
  cleaned <- gsub(" ", "?", cleaned)

  # 12) Blank out known placeholder strings
  cleaned[grepl("unreadable|unknown|\\?blank\\?|alias", cleaned)] <- ""

  # 13) Blank out names with too many missing characters (> 25%)
  #     (Unlike clean_surnames, single-char names are NOT blanked here)
  has_chars <- nchar(cleaned) > 0
  frac_letters <- nchar(gsub("\\?", "", cleaned)) / pmax(nchar(cleaned), 1L)
  cleaned[has_chars & frac_letters < 0.75] <- ""

  # 14) Expand abbreviations (whole-name matches only)
  if (expand_abbreviations) {
    idx <- match(cleaned, names(abbr_table))
    cleaned <- ifelse(!is.na(idx), abbr_table[idx], cleaned)
  }

  # Restore NAs
  cleaned[is_na] <- NA_character_

  return(cleaned)
}
