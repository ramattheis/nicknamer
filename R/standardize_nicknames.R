#' Map first names to their canonical forms via a nickname dictionary
#'
#' @description
#' `standardize_nicknames()` is an **optional post-processing step** that maps
#' already spelling-standardized first names to their canonical forms using a
#' nickname dictionary.  For example, "bob" → "robert", "betty" → "elizabeth".
#'
#' This function is intended to be applied \emph{after} Bayesian spelling
#' standardization (via `draw_gibbs()` / `make_bayes_choice_dictionary()`), not
#' as a substitute for it.
#'
#' When `sex` is supplied, only canonical forms of the matching sex are
#' considered for ambiguous nicknames (e.g. "alex" → "alexander" for males,
#' "alexandra" for females).  If no sex-specific match exists, the function
#' falls back to all available matches.
#'
#' Names that do not appear in the dictionary are returned unchanged.
#'
#' @param names Character vector of (cleaned, spelling-standardized) first
#'   names.
#' @param sex `NULL` (default), a single string (`"male"` or `"female"`), or a
#'   character vector the same length as `names`.  Values may be `NA` to skip
#'   sex filtering for individual records.
#' @param dictionary A data frame produced by `load_nickname_dictionary()` with
#'   columns `nickname`, `canonical`, and `sex`.  Defaults to downloading /
#'   returning the cached dictionary.
#' @return Character vector the same length as `names`.
#' @export
standardize_nicknames <- function(names,
                                  sex        = NULL,
                                  dictionary = load_nickname_dictionary()) {

  # Validate / expand sex argument
  if (!is.null(sex)) {
    sex <- tolower(as.character(sex))
    if (length(sex) == 1L) sex <- rep(sex, length(names))
    if (length(sex) != length(names)) {
      stop("`sex` must be NULL, a single value, or the same length as `names`.")
    }
    bad <- !is.na(sex) & !(sex %in% c("male", "female"))
    if (any(bad)) {
      stop('`sex` values must be "male", "female", or NA.')
    }
  }

  result <- names

  for (i in seq_along(names)) {
    nm <- names[i]
    if (is.na(nm) || nm == "") next

    # Rows in the dictionary matching this nickname
    rows <- dictionary[dictionary$nickname == nm, , drop = FALSE]
    if (nrow(rows) == 0L) next   # Not a known nickname — leave unchanged

    # If sex filter requested, prefer sex-matching rows
    if (!is.null(sex) && !is.na(sex[i])) {
      sex_rows <- rows[!is.na(rows$sex) & rows$sex == sex[i], , drop = FALSE]
      if (nrow(sex_rows) > 0L) rows <- sex_rows
      # If no sex-specific match, fall back to all rows
    }

    result[i] <- rows$canonical[1L]
  }

  return(result)
}
