#' Download, load, and delete a large list of "standardized" surnames from US censuses
#'
#' @return A data frame containing a dictionary of observed and "standard" surnames.
#' @export
load_us_dictionary <- function() {

  # Hard coding the URL
  us_dictionary_url <- "https://github.com/ramattheis/nicknamer/releases/download/v1.0.0/us_dictionary.rds"

  # Create a temporary file path
  temp_file <- tempfile(fileext = ".rds")

  # Checking if us_surnames is already in the local cache
  if (!is.null(.nicknamerenv$us_surnames_dictionary_cache)) {
    return(.nicknamerenv$us_surnames_dictionary_cache)
  }

  message("Downloading US surname dictionary... this might take a while...")

  tryCatch({

    # Download the file to the temporary location
    old_timeout <- getOption("timeout") # Store the current timeout
    options(timeout = 1800) # allowing up to 30 min to download the list
    download.file(us_dictionary_url, destfile = temp_file, mode = "wb", quiet = FALSE)

    # Load the object from the temporary file
    us_dictionary <- readRDS(temp_file)

    # Store in the package's internal cache for this session
    .nicknamerenv$us_surnames_dictionary_cache <- us_dictionary

    return(us_dictionary)

  }, error = function(e) {
    # Clean up temp file on error too
    if (file.exists(temp_file)) {
      file.remove(temp_file)
    }
    stop("Failed to download or load dictionary: ", e$message)
  }, finally = {
    # Ensure temporary file is removed
    if (file.exists(temp_file)) {
      file.remove(temp_file)
    }
    # Restore the old timeout
    options(timeout = old_timeout)
  })
}
