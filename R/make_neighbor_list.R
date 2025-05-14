#' Build neighbor list for `draw_gibbs()` via full loop
#'
#' `make_neighbor_list()` computes, for each string, distances to all others
#' using `stringdist::stringdist()`, thresholds by `max_dist`, and returns
#' lists of neighbors suitable for `draw_gibbs()`.
#'
#' @param strings       Character vector of unique strings to compare.
#' @param method        Character; distance method for `stringdist::stringdist()`, e.g. "lv", "jw".
#' @param max_dist      Numeric; maximum distance to consider neighbors (default 3 for edit distance, or 0.3 for similarity methods).
#' @param show_progress Logical; if TRUE, display a progress bar (default TRUE).
#'
#' @return A list of length `length(strings)` where each element is a list with:
#'   - `j`: integer vector of neighbor indices (in the original `strings`).
#'   - `d`: numeric vector of corresponding distances.
#'
#' @examples
#' s <- c("apple","apply","appla","banana","banane")
#' make_neighbor_list(s, method = "lv", max_dist = 3)
#'
#' @importFrom stringdist stringdist
#' @export
make_neighbor_list <- function(
    strings,
    method        = "lv",
    max_dist      = ifelse(method %in% c("jw","cosine","jaccard"), 0.3, 3),
    show_progress = TRUE
) {
  if (!is.character(strings)) stop("`strings` must be a character vector.")
  K <- length(strings)
  neighbor_list <- vector("list", K)
  names(neighbor_list) <- strings

  if (show_progress) pb <- txtProgressBar(min = 0, max = K, style = 3)

  for (i in seq_len(K)) {
    # compute full distances for string i
    dists <- stringdist::stringdist(strings[i], strings, method = method)
    # select neighbors within max_dist, exclude self
    sel <- which(dists <= max_dist & seq_len(K) != i)
    neighbor_list[[i]] <- list(
      j = sel,
      d = dists[sel]
    )
    if (show_progress) setTxtProgressBar(pb, i)
  }

  if (show_progress) close(pb)
  neighbor_list
}

