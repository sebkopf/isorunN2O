
#' Run the N2O data viewer
#'
#' To run this function stand-alone as a script from any folder on your system, copy the
#' run (Unix systems) or run.bat (Windows) systems from the GitHub repository
#' (\link{http://www.github.com/sebkopf/isorunN2O/inst/shiny-apps/data_viewer/}
#' and modify depending on your R executable.
#'
#' @param base_dir the base data directory where the data viewer should start
#' @param ... passed on to the \link{shiny::runApp} call and could include paramters such as host or port
#' @export
run_data_viewer <- function(base_dir = ".", ...) {

  app_dir <- system.file("shiny-apps", "data_viewer", package = "isorunN2O")
  if (app_dir == "")
    stop("Could not find data viewer directory. Try re-installing `isorunN2O`.", call. = FALSE)

  isorunN2O:::run_data_viewer_dev(base_dir = base_dir, app_dir = app_dir, ...)
}


#' for development of data viewer
#' not exported to namespace but helpful for active development of the shiny app
run_data_viewer_dev <- function(base_dir = ".", app_dir = file.path("inst", "shiny-apps", "data_viewer"), ...) {

  if (!file.exists(app_dir))
    stop("App directory does not exist, something might be wrong with the `isorunN2O` installation: ", app_dir)

  if (!file.exists(base_dir))
    stop("Could not find base directory '", base_dir, " from the current working directory: ", getwd())

  # store base_dir for the shiny
  if (R.utils::isAbsolutePath(base_dir))
    .GlobalEnv$.base_dir <- base_dir
  else
    .GlobalEnv$.base_dir <- R.utils::filePath(getwd(), base_dir)
  on.exit(rm(.base_dir, envir=.GlobalEnv))

  # start app
  shiny::runApp(app_dir, display.mode = "normal", ...)
}

