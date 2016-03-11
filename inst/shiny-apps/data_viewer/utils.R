#' Utility functions for the data viewer

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

# SETTINGS =========

#' Functions for the settings tab

#' Construct the UI for changing the settings
make_settings_UI <- function(settings) {
  settings <- subset(settings, Editable == "yes")
  r <- list()
  for (i in 1:nrow(settings)) {
    r <- c(r,
           with(settings[i,], {
             list(
               if (Type == "numeric")
                 numericInput(paste0("setting_", Variable),
                              paste0(Label, " [", Units, "]"),
                              value = Value)
               else textInput(paste0("setting_", Variable), Label, value = Value)
             )
           })
    )
  }

  return(r)
}

#' Save the settings
#' @param the settings data frame
#' @param the input variables
#' @param file the name where to save the settings
#' @return a list with the current settings and an error/success message
save_settings <- function(settings, input, file = "settings.csv") {
  error <<- c()
  for (i in 1:nrow(settings)) {
    settings[i, "Value"] <-
      with(settings[i,], {
        if (Editable == "yes") {
          new_val <- input[[paste0("setting_", Variable)]]
          if (Type == "numeric" && is.na(new_val)) {
            error <<- c(error, paste0(Label, " is not a valid number"))
            new_val <- Value # go back to old value
          }
          return(new_val)
        } else
          return (Value)
      })
  }

  if (length(error) > 0) {
    msg <- paste(c("Settings could not be saved, the following errors occured:", error), collapse = "<br/>")
  } else {
    message("INFO: Saving settings...")
    write.csv(settings, file = file, row.names = FALSE)
    msg <- "INFO: Settings saved succesfully."
  }
  return(list(settings = settings, msg = msg))
}



# DATA ===============

#' Load isodat files
#' @param files file paths
#' @param progress callback function, will be called with filename and total number of files as parameters
load_isodat_files <- function(files, progress = NULL, quiet = T) {
  iso_files <- list()
  for (file in files) {
    if (!is.null(progress)) progress(basename(file), length(files))
    iso_files <- c(iso_files, isoread::isoread(file, type = "CFLOW", quiet = quiet))
  }
  names(iso_files) <- sapply(iso_files, function(i) i$filename)
  return(iso_files)
}

#' get a subset of the data table and prepare it for plotting by assigning x and y
#' @param data_table the whole data table
#' @param pattern regexp pattern to find which files to include
#' @param x name of the x column
#' @param y name of the y column
get_plot_data_table <- function(data_table, pattern, x, y) {
  df <- subset(data_table, grepl(pattern, file))
  df$x <- df[[x]]
  df$y <- df[[y]]
  return(df)
}

#' Combine isodat files' mass traces
#' @param isodat_files isodat objects
get_isodat_mass_traces <- function(isodat_files) {
  do.call(rbind, lapply(isodat_files, function(i) mutate(i$get_mass_data(melt = T)[c("time", "signal", "variable")], file = i$filename)))
}

# PLOTTING ===========

#' make isodat file mass plot
#' @param isodat_files the isodat files with the data
#' @param show_files filenames of the ones to plot
plot_masses <- function(isodat_files, show_files, show_masses) {
  if (length(show_files) > 0 && show_files[1] != "0") {
    message("INFO: Showing traces ", paste(show_masses, collapse=", "),
            " for ", paste(show_files, collapse = ", "))
    isolate({
      mass_traces <- get_isodat_mass_traces(isodat_files[show_files])
      ggplot(
        subset(mass_traces, variable %in% show_masses),
        aes(time, signal, linetype = variable, colour = file)) +
        geom_line() +
        scale_x_continuous(expand = c(0,0)) +
        labs(x = "Time [s]", y = "Signal [mV]", linetype = "Trace", colour = "File") +
        theme_bw() + theme(
          legend.position = "bottom", legend.direction = "vertical",
          text = element_text(size = 18)) +
        guides(
          color = guide_legend(ncol = 2, byrow = TRUE, title.hjust = 0.5),
          linetype = guide_legend(nrow = 1, title.hjust = 0.5)
          )
    })
  } else
    plot.new()
}

# GENERIC UI ELEMENTS ===========

#' make a trace selector box
make_trace_selector <- function(id, isodat_files, label = "Traces", size = 4) {
  selectInput(
    id, label,
    if (length(isodat_files) > 0) names(isodat_files[[1]]$plotOptions$masses) else c("Nothing loaded yet"="0"),
    selected = if (length(isodat_files) > 0) names(isodat_files[[1]]$plotOptions$masses) else c(),
    multiple=TRUE, selectize=FALSE, size = 4)
}

#' make a file selector box
make_file_selector <- function(id, isodat_files, label = "Files", selected = NULL, size = 7) {
  files <- names(isodat_files)
  if (is.null(selected) || length(selected <- grep("linearity", files, value = T)) == 0)
    selected <- files[1] # if none selected, select first one
  selectInput(
    id, label,
    if (length(files) > 0) files else c("Nothing loaded yet"="0"),
    selected = selected, multiple=TRUE, selectize=FALSE, size = size)
}
