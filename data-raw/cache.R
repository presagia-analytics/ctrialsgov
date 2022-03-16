http_cache_get <- function(url, cache_dir)
{
  # create cache directory if it does not yet exist
  dir.create(cache_dir, showWarnings = FALSE)
  
  # create a cache of the query
  cache_file <- file.path(cache_dir, paste0(rlang::hash(url), ".rds"))

  # check if file exists and either load or query and save
  if (file.exists(cache_file))
  {
    res <- readRDS(cache_file)
  } else {
    res <- httr::GET(url)
    saveRDS(res, cache_file)
  }
  
  return(res)
}

http_cache_clear <- function(cache_dir)
{
  # create cache directory if it does not yet exist
  dir.create(cache_dir, showWarnings = FALSE)

  # what are the cache files
  fpaths <- dir(cache_dir, full.names = TRUE, pattern = "rds$")
  sapply(fpaths, file.remove)
  
  invisible()
}

null_to_na <- function(x) { ifelse(is.null(x), NA, x) }
