#' Load raw run data
#'
#' This function loads all the raw data files and caches the raw data if \code{cache} is set.
#'
#' @param folder the folder that contains the run data files (for now only isodat .dxf files are supported, functionality is provided by the \link{isoread} package).
#' @param cache a cache folder if set (can be "." for current folder), caches the raw data at the specified location, by default set to "cache", set to NULL to avoid caching
#' @param quiet whether the function should output information messages or be quiet (default is to output)
#' @export
load_run_folder <- function(folder, cache = "cache", quiet = F) {
  if (!dir.exists(folder))
    stop("The provided data folder '", folder,
         "' does not seem to exist (current working directory: ", getwd(), ")")

  if (!is.null(cache) && !dir.exists(cache))
    dir.create(cache)

  cache_file <- file.path(cache, make.names(folder) %>% paste0(".RData"))
  if (!is.null(cache) && file.exists(cache_file)) {
    if (!quiet) message("Loading data from cached file at ", cache_file)
    load(cache_file, env = .GlobalEnv)
  } else {
    if (!quiet) message("Loading data from isodat files in ", folder)
    raw_data <-
      isoread_folder(folder, type = "CFLOW", ext = ".dxf", quiet = quiet)
    if (!is.null(cache)) {
      if (!quiet) message("Caching data at ", cache_file)
      save(raw_data, file = cache_file)
    }
  }

  return(raw_data)
}
