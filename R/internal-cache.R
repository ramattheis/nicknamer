# This environment will hold all internal cached data for the package.
.nicknamerenv <- new.env(parent = emptyenv())

# Initialize the cache slot for the dictionary within this environment
.nicknamerenv$us_surnames_dictionary_cache <- NULL
