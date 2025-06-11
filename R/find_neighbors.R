#' Build sparse distance (D) and neighbor‐mask (M) matrices for names
#'
#' `find_neighbors()` computes, for a vector of unique names,
#' (1) a sparse matrix `D` of pairwise distances (only for neighbors within
#' `max_dist`), and (2) a sparse binary mask `M` indicating which pairs are
#' “neighbors.”  It parallelizes the distance computations and packs the
#' results into two symmetric `Matrix::sparseMatrix` objects.
#'
#' @param names  A non‐empty character vector of **unique** names.
#' @param method   Distance metric passed to `stringdist::stringdist()`.
#'                 E.g. `"lv"`, `"jw"`, `"cosine"`, `"jaccard"`.
#' @param max_dist Numeric ≥ 0; include only pairs with distance ≤ `max_dist`.
#'                 Defaults to 3 for edit distances, 0.3 for similarity‐based.
#' @param ncores   Integer ≥ 1; number of parallel workers.  Defaults to 1.
#'
#' @return A list with two elements:
#'   - `D`: symmetric sparse matrix of distances (`d_{ij}`) for neighbor pairs.
#'   - `M`: symmetric sparse 0/1 mask indicating neighbor edges.
#'
#' @importFrom parallel makeCluster stopCluster
#' @importFrom pbapply pblapply
#' @importFrom stringdist stringdist
#' @importFrom Matrix sparseMatrix
#' @export
find_neighbors <- function(
    names,
    method   = "jw",
    max_dist = ifelse(method %in% c("jw","cosine","jaccard"), 0.3, 3),
    ncores   = 1
) {
  # 1) validation
  if (!is.character(names) || length(names) < 1) {
    stop("`names` must be a non‐empty character vector.")
  }
  if (anyDuplicated(names)) {
    stop("`names` must not contain duplicates.")
  }
  K <- length(names)

  # 2) worker: compute dists for one string
  worker <- function(i) {
    d <- stringdist::stringdist(names[i], names, method = method)
    sel <- which(d <= max_dist & seq_len(K) != i)
    list(i = rep(i, length(sel)),
         j = sel,
         d = d[sel])
  }

  # 3) parallel map with progress bar
  cl <- parallel::makeCluster(ncores)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::clusterExport(cl = cl,
        varlist = c("names","method","max_dist","K"),
        envir = environment())
  results <- pbapply::pblapply(seq_len(K), worker, cl = cl)
  is <- unlist(lapply(results, function(r) r$i))
  js <- unlist(lapply(results, function(r) r$j))
  ds <- unlist(lapply(results, function(r) r$d))

  # 4) build sparse matrices
  D <- Matrix::sparseMatrix(
    i = is,
    j = js,
    x = ds,
    dims = c(K, K),
    dimnames = list(names, names)
  )
  M <- Matrix::sparseMatrix(
    i = is,
    j = js,
    x = rep(1L, length(is)),
    dims = c(K, K),
    dimnames = list(names, names)
  )

  list(D = D, M = M)
}
