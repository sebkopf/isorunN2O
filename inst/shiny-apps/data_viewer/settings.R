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
