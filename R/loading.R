#' Load raw run data
#'
#' This function loads all the raw data files and caches the raw data if \code{cache} is set.
#'
#' @param folder the folder that contains the run data files (for now only isodat .dxf files are supported, functionality is provided by the \link{isoread} package).
#' @param cache a cache folder if set (can be "." for current folder), caches the raw data at the specified location, by default set to the folder where the data is, set to NULL to avoid caching
#' @param quiet whether the function should output information messages or be quiet (default is to output)
#' @param ... parameters passed on to \code{\link{isoread}}
#' @export
load_run_folder <- function(folder, cache = folder, quiet = F, ...) {

  # deprecate
  deprecate_for_switch_to_isoverse("load_run_folder()", "isoreader::iso_read_continuous_flow()")

  if (!dir.exists(folder))
    stop("The provided data folder '", folder,
         "' does not seem to exist (current working directory: ", getwd(), ")")

  if (!is.null(cache) && !dir.exists(cache))
    dir.create(cache)

  cache_file <- default_cache_file(cache, basename(folder))
  if (!is.null(cache) && file.exists(cache_file)) {
    raw_data <- load_cache(cache_file, quiet = quiet)
  } else {
    if (!quiet) message("Loading data from isodat files in ", folder)
    raw_data <-
      isoread::isoread_folder(folder, type = "CFLOW", ext = ".dxf", quiet = quiet, ...)
    if (!is.null(cache)) {
      save_cache(raw_data, cache_file, quiet = quiet)
    }
  }

  return(raw_data)
}

#' default cache file name
default_cache_file <- function(path, name) {
  file.path(path, make.names(name) %>% paste0(".RData"))
}

#' store cache file
save_cache <- function(data, cache_file, quiet = F) {
  cache <- data
  if (!quiet) message("Caching data at ", cache_file)
  save(cache, file = cache_file)
}

#' store cach file
load_cache <- function(cache_file, quiet = F) {
  if (!quiet) message("Loading data from cached file at ", cache_file)
  load(cache_file)
  return(cache)
}

