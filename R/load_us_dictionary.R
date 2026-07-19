#' Download, cache, and load a large list of "standardized" surnames from US censuses
#'
#' @description Loads the US census surname dictionary used by
#' `standardize_us_surnames()`. The dictionary is a large (~104 MB) file, so it
#' is cached both in memory (for the current session) and on disk (across
#' sessions) to avoid repeated downloads. If the automatic download is
#' unreliable (e.g. on an HPC node with a slow or firewalled connection), you
#' can download the file manually and pass its location via `path`.
#'
#' @param path Optional path to a locally downloaded `us_dictionary.rds` file.
#'             If supplied, the file is read directly and no download is
#'             attempted. Useful when network access is unreliable.
#' @param cache_dir Directory used for the persistent on-disk cache. Defaults to
#'             a per-user cache directory (`tools::R_user_dir("nicknamer",
#'             "cache")`). The downloaded dictionary is stored here so that
#'             subsequent sessions load it from disk instead of re-downloading.
#' @param refresh If `TRUE`, ignore any cached copy (in memory or on disk) and
#'             download a fresh copy. Defaults to `FALSE`.
#' @param tries Number of times to attempt the download before giving up.
#'             Defaults to `3`.
#'
#' @return A data frame containing a dictionary of observed and "standard" surnames.
#' @export
load_us_dictionary <- function(path = NULL,
                               cache_dir = tools::R_user_dir("nicknamer", "cache"),
                               refresh = FALSE,
                               tries = 3) {

  # Hard coding the URL
  us_dictionary_url <- "https://github.com/ramattheis/nicknamer/releases/download/v1.0.0/us_dictionary.rds"

  # 1. If the user supplied a local file, use it directly.
  if (!is.null(path)) {
    if (!file.exists(path)) {
      stop("`path` was supplied but no file exists at: ", path)
    }
    us_dictionary <- readRDS(path)
    .nicknamerenv$us_surnames_dictionary_cache <- us_dictionary
    return(us_dictionary)
  }

  # 2. In-memory cache for the current session.
  if (!refresh && !is.null(.nicknamerenv$us_surnames_dictionary_cache)) {
    return(.nicknamerenv$us_surnames_dictionary_cache)
  }

  # 3. Persistent on-disk cache across sessions.
  cache_file <- file.path(cache_dir, "us_dictionary.rds")
  if (!refresh && file.exists(cache_file)) {
    us_dictionary <- readRDS(cache_file)
    .nicknamerenv$us_surnames_dictionary_cache <- us_dictionary
    return(us_dictionary)
  }

  # 4. Download to the persistent cache, with retries.
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }

  message("Downloading US surname dictionary (~104 MB)... this might take a while...")

  old_timeout <- getOption("timeout")   # Store the current timeout
  options(timeout = 1800)               # allowing up to 30 min to download the list
  on.exit(options(timeout = old_timeout), add = TRUE)

  # Download to a temp file first, then move into place, so an interrupted
  # download never leaves a truncated file in the cache.
  temp_file <- tempfile(fileext = ".rds")

  last_error <- NULL
  for (attempt in seq_len(tries)) {
    result <- tryCatch({
      download.file(us_dictionary_url, destfile = temp_file, mode = "wb", quiet = FALSE)
      us_dictionary <- readRDS(temp_file)
      TRUE
    }, error = function(e) {
      last_error <<- e
      FALSE
    })

    if (isTRUE(result)) {
      # Move the completed download into the persistent cache.
      ok <- file.copy(temp_file, cache_file, overwrite = TRUE)
      if (file.exists(temp_file)) file.remove(temp_file)
      if (!ok) {
        warning("Downloaded dictionary but could not write it to the cache at ",
                cache_file, "; it will be re-downloaded next session.")
      }
      .nicknamerenv$us_surnames_dictionary_cache <- us_dictionary
      return(us_dictionary)
    }

    if (attempt < tries) {
      message("Download attempt ", attempt, " of ", tries, " failed; retrying...")
    }
  }

  # All attempts failed -- clean up and give an actionable error.
  if (file.exists(temp_file)) file.remove(temp_file)
  stop("Failed to download the US surname dictionary after ", tries, " attempts: ",
       conditionMessage(last_error), "\n",
       "You can download it manually and pass it to `load_us_dictionary(path = ...)`:\n",
       "  ", us_dictionary_url)
}
