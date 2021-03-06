% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loading.R
\name{load_run_folder}
\alias{load_run_folder}
\title{Load raw run data}
\usage{
load_run_folder(folder, cache = folder, quiet = F, ...)
}
\arguments{
\item{folder}{the folder that contains the run data files (for now only isodat .dxf files are supported, functionality is provided by the \link{isoread} package).}

\item{cache}{a cache folder if set (can be "." for current folder), caches the raw data at the specified location, by default set to the folder where the data is, set to NULL to avoid caching}

\item{quiet}{whether the function should output information messages or be quiet (default is to output)}

\item{...}{parameters passed on to \code{\link{isoread}}}
}
\description{
This function loads all the raw data files and caches the raw data if \code{cache} is set.
}
