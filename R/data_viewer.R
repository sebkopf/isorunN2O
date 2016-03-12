
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

  if (!file.exists(base_dir))
    stop("Could not find base directory '", base_dir, " from the current working directory: ", getwd())

  # store base_dir for the shiny app
  .GlobalEnv$.base_dir <- file.path(getwd(), base_dir)
  on.exit(rm(.base_dir, envir=.GlobalEnv))

  # start app
  shiny::runApp(app_dir, display.mode = "normal", ...)
}