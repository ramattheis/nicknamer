#' Build neighbor list for `draw_gibbs()` in parallel
#'
#' `find_neighbors()` identifies, for each string in `strings`, all other
#' strings within a specified `max_dist` under the chosen `method` (e.g. edit
#' distance `"lv"` or Jaro–Winkler `"jw"`).  It dispatches the work across
#' multiple cores, showing progress as each chunk completes.
#'
#' @param strings  A non‐empty character vector of **unique** strings to compare.
#'                 Duplicates will trigger an error.
#' @param method   Character scalar; distance metric passed to
#'                 `stringdist::stringdist()`.  Common choices:
#'                 - `"lv"`: Levenshtein edit distance
#'                 - `"jw"`: Jaro–Winkler distance (1 − similarity)
#'                 - `"cosine"`, `"jaccard"`, etc.
#' @param max_dist Numeric ≥ 0; only those pairs whose distance ≤ `max_dist`
#'                 are returned as neighbors.  Defaults to 3 for edit
#'                 distances and 0.3 for similarity‐based metrics.
#' @param ncores   Integer ≥ 1; number of parallel worker processes to launch.
#'                 Defaults to 1.
#'
#' @return A **named** list of length `length(strings)`.  Each element is a
#'   two‐element list containing:
#'   - `j`: integer vector of 1-based positions in `strings` that are
#'           within `max_dist` of the query string,
#'   - `d`: numeric vector of the corresponding distances.
#'
#' @details
#' Internally, `find_neighbors()` proceeds as follows:
#' 1. **Input validation** — ensures `strings` is non‐empty, character, and
#'    contains no duplicates.
#' 2. **Worker function** — for a single index `i`, computes
#'    `stringdist(strings[i], strings, method = method)`, filters
#'    those ≤ `max_dist`, and returns `j` (indices) and `d` (distances).
#' 3. **Cluster setup** — starts a PSOCK cluster with `ncores` workers.
#' 4. **Parallel mapping** — uses `pbapply::pblapply()` to apply the worker
#'    across all `i ∈ 1:K`, showing a progress bar as chunks finish.
#' 5. **Cleanup & naming** — stops the cluster on exit, assigns the original
#'    strings as names of the output list, and returns.
#'
#' @examples
#' s <- c("apple","apply","appla","banana","banane")
#' # Find all within Levenshtein distance 2, on 2 cores:
#' find_neighbors(s, method = "lv", max_dist = 2, ncores = 2)
#' # Find all within Jaro–Winkler distance 0.15, on all cores:
#' find_neighbors(s, method = "jw", max_dist = 0.15)
#'
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom pbapply pblapply
#' @importFrom stringdist stringdist
#' @export
find_neighbors <- function(
    strings,
    method    = "jw",
    max_dist  = ifelse(method %in% c("jw","cosine","jaccard"), 0.3, 3),
    ncores    = 1
) {
  # 1) Input validation
  if (!is.character(strings) || length(strings) < 1) {
    stop("`strings` must be a non‐empty character vector.")
  }
  if (anyDuplicated(strings)) {
    stop("`strings` must not contain duplicates.")
  }
  K <- length(strings)

  # 2) Define the worker: computes distances for one string
  worker <- function(i) {
    dists <- stringdist::stringdist(strings[i], strings, method = method)
    sel   <- which(dists <= max_dist & seq_len(K) != i)
    list(j = sel, d = dists[sel])
  }

  # 3) Launch a PSOCK cluster for parallel execution
  cl <- parallel::makeCluster(ncores)
  on.exit(parallel::stopCluster(cl), add = TRUE)

  # 4) Map the worker across all indices with a progress bar
  nb_list <- pbapply::pblapply(seq_len(K), worker, cl = cl)

  # 5) Assign names and return
  names(nb_list) <- strings
  nb_list
}


