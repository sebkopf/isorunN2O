library(shiny)
library(isorunN2O)
library(ggplot2)
library(plotly)

# make sure base directory is set
if (!exists(".base_dir", env = .GlobalEnv))
  .GlobalEnv$.base_dir <- getwd()

# functions
source("utils.R")
source("linearity.R")
source("folder_browser.R")

# SERVER =====
server <- shinyServer(function(input, output, session) {

  # STARTUP =======
  data_dir <- .GlobalEnv$.base_dir
  data_root <- c(Data = data_dir)
  settings_file <- file.path(data_dir, "settings.csv")
  message("\n***************************************************************",
          "\nINFO: Launching N2O Data Viewer ...",
          "\nINFO: App directory: ", getwd(),
          "\nINFO: Data directory: ", data_dir)

  # SETTINGS =======
  if (!file.exists(settings_file)) {
    message("INFO: No settings file exists in this workspace yet. Creating default settings.csv")
    default_settings_file <- system.file("shiny-apps", "data_viewer", "default_settings.csv", package = "isorunN2O")
    file.copy(default_settings_file, settings_file)
  }
  settings <- read.csv(settings_file, stringsAsFactors = FALSE)

  # TODO: fixme - the settings would be done much better with reactive values, currently the info message is problematic
  get_settings <- reactive({
    sets <- settings
    msg <- ""
    if (input$save_settings > 0) {
      saved <- isolate(save_settings(settings, input))
      sets <- saved$settings
      msg <- saved$msg
    }
    return (c(plyr::dlply(sets, plyr::.(Variable), function(df) df$Value), list(msg = msg)))
  })
  output$settings <- renderUI(make_settings_UI(settings))
  output$settings_msg <- renderUI(HTML(get_settings()$msg))

  # LINEARITY ============
  linearity_folder <- callModule(folderSelector, "linearity_folder", root = data_dir,
                            folders_sort_desc = TRUE,
                            files_pattern = "\\.dxf", size = 10)

  # load
  is_linearity_loaded <- reactive(length(get_linearity_folder()) > 0)
  get_linearity_folder <- reactive({
    validate(need(input[["linearity_folder-open"]] > 0, message = FALSE))
    return(isolate(linearity_folder()))
  })
  get_linearity_files <- reactive({
    if ( is_linearity_loaded() ) {
      message("INFO: Loading linearity data from folder ", get_linearity_folder())
      return(
        withProgress(message = 'Loading data...', value = 0, {
            load_isodat_files (
              list.files(get_linearity_folder(), pattern = "\\.dxf$", full.names = TRUE),
              function(file, n) incProgress(1/n, detail = paste0("Reading ", file, " ...")))
          })
      )
    } else
      return(list())
  })
  get_linearity_data_table <- reactive(get_isodat_data_tables(get_linearity_files()))

  # show linearity traces
  output$loaded_masses <- renderUI(make_trace_selector("selected_mass", get_linearity_files()))
  output$loaded_files <- renderUI(make_file_selector("selected_file", get_linearity_files(), selected = "linearity"))
  make_linearity_traces_plot <- reactive(
    if (is_linearity_loaded()) {
      withProgress(message = 'Rendering plot', detail = "for raw mass traces...", value = 0.5,
                   plot_masses(get_linearity_files(), input$selected_file, input$selected_mass))
    }
  )
  output$linearity_traces_plot <- renderPlot(make_linearity_traces_plot())


  # linearity evaluation
  get_linearity_data_O <- reactive(
    if ( is_linearity_loaded() ) {
      get_linearity_plot_data(get_linearity_data_table(), " 18O/16O")
    } else {
      stop("ERROR: no linearity folder loaded.")
    })
  get_linearity_data_N <- reactive(
    if ( is_linearity_loaded() ) {
      get_linearity_plot_data(get_linearity_data_table(), " 15N/14N")
    } else {
      stop("ERROR: no linearity folder loaded.")
    }
  )

  # cutoffs
  output$slider_O_min <- renderUI(make_cutoff_slider("cutoff_O_min", get_settings(), max = ceiling(max(get_linearity_data_O()$x))))
  output$slider_O_max <- renderUI(make_cutoff_slider("cutoff_O_max", get_settings(), max = ceiling(max(get_linearity_data_O()$x))))
  output$slider_N_min <- renderUI(make_cutoff_slider("cutoff_N_min", get_settings(), max = ceiling(max(get_linearity_data_N()$x))))
  output$slider_N_max <- renderUI(make_cutoff_slider("cutoff_N_max", get_settings(), max = ceiling(max(get_linearity_data_N()$x))))
  get_xrange_O <- reactive(c(input$cutoff_O_min, input$cutoff_O_max))
  get_xrange_N <- reactive(c(input$cutoff_N_min, input$cutoff_N_max))

  # regressions
  get_regression_O <- reactive(get_linearity_reg("d18O", get_linearity_data_O(), get_xrange_O()))
  output$regression_O <- renderText(if (is_linearity_loaded() && length(get_xrange_O()) > 0) isolate(get_regression_O()$msg))
  get_regression_N <- reactive(get_linearity_reg("d15N", get_linearity_data_N(), get_xrange_N()))
  output$regression_N <- renderText(if (is_linearity_loaded() && length(get_xrange_N()) > 0) isolate(get_regression_N()$msg))

  # plots
  make_linearity_plot_O <- reactive(make_linearity_plot("d18O [permil]", get_linearity_data_O(), get_xrange_O()))
  output$linearity_plot_O <- renderPlot(if (is_linearity_loaded() && length(get_xrange_O()) > 0)
    isolate(print(make_linearity_plot_O())))
  make_linearity_plot_N <- reactive(make_linearity_plot("d15N [permil]", get_linearity_data_N(), get_xrange_N()))
  output$linearity_plot_N <- renderPlot(if (is_linearity_loaded() && length(get_xrange_N()) > 0)
    isolate(print(make_linearity_plot_N())))

  # summary for linearity and ON/OFFs
  output$summarize <- downloadHandler(
    filename = function() {paste0(basename(get_linearity_folder()), "_summary.pdf")},
    content = function(file) {
      withProgress(message = 'Generating summary', detail = "for linearity and ON/OFF data...", value = 0.5,
                   generate_linearity_summary (
                     file.path(get_linearity_folder()),
                     get_linearity_data_table(),
                     get_regression_O(), get_regression_N(),
                     make_linearity_plot_O(), make_linearity_plot_N(),
                     save_download_file = file, summary_dir = data_dir)
      )
    })

  # history
  get_linearity_history <- reactive({
    get_linearity_folder() # make sure to trigger whenever there is a new folder loaded or tabs are changed

    if (input$linearity_tabs == "linearity_history_tab") {
      # makes sure to trigger on tab change but only retrieve data if it's actually the right tab

      summary_file <- file.path(data_dir, linearity_record_csv)
      if (file.exists(summary_file)) {
        data <- read.csv(file.path(data_dir, linearity_record_csv), check.names = F, stringsAsFactors = F)
        data <- mutate(data, datetime = as.POSIXct(`Run date & time`), date = as.Date(datetime))

        # remove duplicates and sort
        data.nodup <- subset(data[rev(order(data$Timestamp)),], !duplicated(datetime))
        data.nodup <- data.nodup[order(data.nodup$datetime),] # sort by date time

        if (nrow(data.nodup) < nrow(data)) {
          # some duplicates removed --> store again
          message("Removing duplicates from history...")
          write.table(data.nodup[!names(data.nodup) %in% c("datetime", "date")], file = summary_file, row.names = FALSE, sep = ",", col.names = TRUE)
        }

        return(data.nodup)
      } else
        stop("No linearity history file yet stored at '", summary_file, "'")
    } else
      return(data.frame())
  })

  output$linhis_date_range_widget <- renderUI({
    if (nrow(data <- get_linearity_history()) > 0) {
      dateRangeInput("linhis_date_range", "",
                     start = min(data$date)[1], end = max(data$date)[1],
                     min = min(data$date)[1], max = max(data$date)[1],
                     format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                     language = "en", separator = " to ")
    }
  })

  make_linearity_history_plot <- reactive({

    # show if any data selected in the date range
    # doing the sequential && to trigger the right reactivity
    if (nrow(data <- get_linearity_history()) > 0 &&
          nrow(data <- subset(data,
            date >= input$linhis_date_range[1] & date <= input$linhis_date_range[2])) > 0) {

      message("INFO: Plotting linearity history from ", input$linhis_date_range[1], " to ", input$linhis_date_range[2])
      withProgress(message = 'Rendering plot', detail = "for linearity history...", value = 0.5, {
        data.melt <- tidyr::gather(data[c("date", "Linearity d15N slope [permil/V]", "Linearity d18O slope [permil/V]")],
                            variable, value, -date)
        ggplot(data.melt, aes(date, value, fill = variable)) +
          geom_point(shape = 21, size = 4) +
          scale_x_date("Date", labels = scales::date_format("%b %d\n%Y")) +
          labs(y = "linearity slope [permil/V]", fill = "") +
          theme_bw() +
          theme(text = element_text(size = 18),
                legend.position = "bottom", legend.direction = "vertical")
      })
    } else
      plot.new()
  })
  output$linearity_history <- renderPlot(make_linearity_history_plot())


  # DATA ==================
  devmode <- TRUE # FIXME: for testing purposes only
  devrun <- "testing.RData"

  # upload
  observe({
    if (is.null(input$upload)) return()
    input$upload$datapath %>% unzip(exdir = data_dir)
  })

  data_folder <- callModule(folderSelector, "data_folder", root = data_dir,
                            folders_sort_desc = TRUE,
                            files_pattern = "\\.dxf", size = 10)

  data <- reactiveValues(
    files = list(), # stores the isodat files
    n2o_rt = NULL, # stores the n2o retention time settings
    n2o = NULL, # stores the selection of n2o groups
    std1 = NULL, # stores the selection of standard1 groups
    std2 = NULL, # stores the selection of standard2 groups
    exclude = NULL # stores the selection excluded samples
  )

  # load data
  is_data_loaded <- reactive(length(get_data_folder()) > 0)
  get_data_folder <- reactive({
    if (devmode && file.exists(devrun)) return("testing")
    validate(need(input[["data_folder-open"]] > 0, message = FALSE))
    isolate({
      data$files <- list() # reset data files everytime the input folder changes
      data$n2o_rt <- NULL
      data$n2o <- NULL
      data$std1 <- NULL
      data$std2 <- NULL
      data$exclude <- NULL
    })

    return(isolate(data_folder()))
  })

  get_data_files <- reactive({

    if ( is_data_loaded() ) {

      # testing
      if (devmode && file.exists(devrun)) {
        load(devrun) # FIXME for testing only
        return(testing)
      }

      if (input$data_refresh > 0 && isolate(length(data$files)) > 0)
        message("INFO: Checking for newly added files in folder ", basename(get_data_folder()))

      # load all files that are not loaded yet
      isolate({
        files <- list.files(get_data_folder(), pattern = "\\.dxf$", full.names = TRUE)
        not_loaded_yet <- setdiff(basename(files), names(data$files)) # check which files have not been loaded yet

        if ( length(not_loaded_yet) > 0) {
          data$files <- c(
            data$files,
            withProgress(message = 'Loading data...', value = 0, {
              load_isodat_files (files[basename(files) %in% not_loaded_yet], function(file, n) incProgress(1/n, detail = paste0("Reading ", file, " ...")))
            }))
        }
      })
    }

    # testing
    if (devmode && !file.exists(devrun)) {
      testing <- data$files
      save(testing, file = devrun)
    }

    return(data$files)
  })

  # show data traces
  output$data_loaded_masses <- renderUI(make_trace_selector("data_selected_mass", get_data_files()))
  output$data_loaded_files <- renderUI(make_file_selector("data_selected_file", get_data_files(), size = 11))
  make_data_traces_plot <- reactive(
    if (is_data_loaded()) {
      withProgress(message = 'Rendering plot', detail = "for raw mass traces...", value = 0.5,
                   plot_masses(get_data_files(), input$data_selected_file, input$data_selected_mass))
    }
  )
  output$data_traces_plot <- renderPlot(make_data_traces_plot())

  # Data folder and N2O peak selection
  output$loaded_data_folder <- renderText(paste("Loaded folder:", basename(get_data_folder())))
  output$rt_selector_widget <- renderUI({
    if (is_data_loaded()) {
      max_rt <- ceiling(max(get_data_files()[[1]]$get_mass_data()$time)/10)*10
      value <- isolate(data$n2o_rt %||% c(get_settings()$n2o_rt_start, get_settings()$n2o_rt_end))
      sliderInput("n2o_rt", "Retention time of N2O peaks",
                  min = 0, max = max_rt, step = 1, value = value, post = " s")
    }
  })

  # Get data tables with parsed file names for categorization / grouping
  get_data_table <- reactive(
    get_data_files() %>%
      get_isodat_data_tables() %>%
      mutate(folder = get_data_folder() %>% basename()) %>%
      parse_file_names()
  )

  # Group Selection widgets
  output$group_selector_widgets <- renderUI({
    if (is_data_loaded()) {

      # find available categories
      categories <- get_data_table() %>%
        dplyr::select(file, category) %>% unique() %>%
        dplyr::count(category) %>%
        dplyr::arrange(desc(n)) %>%
        dplyr::mutate(label = paste0(category, "... (", n, "x)"))

      # define options for group drop downs and make dropdowns based on
      # existing categories and default selection settings
      isolate({

        options <- setNames(categories$category, categories$label)
        files <- get_data_table()[c("file", "run_number")] %>% unique()
        files <- setNames(files$file, paste0(files$file, " (#", files$run_number, ")"))

        n2o <- isolate(data$n2o %||% grep(get_settings()$lab_ref, options, value = T))
        std1 <- isolate(data$std1 %||% grep(get_settings()$std1, options, value = T))
        std2 <- isolate(data$std2 %||% grep(get_settings()$std2, options, value = T))
        exclude <- isolate(data$exclude %||% grep(get_settings()$exclude, files, value = T))

        # MAYBE IMPLEMENT -- chromatogram load upon double click
        # for how to implement, check: http://stackoverflow.com/questions/26208677/double-click-in-r-shiny

        # generate UI
        list(
          selectInput("n2o_select", "Lab reference standard",
                      options, multiple=TRUE, selectize=FALSE, size = 3, selected = n2o),
          selectInput("std1_select", "Isotope standard #1",
                      options, multiple=TRUE, selectize=FALSE, size = 3, selected = std1),
          selectInput("std2_select", "Isotope standard #2",
                      options, multiple=TRUE, selectize=FALSE, size = 3, selected = std2),
          selectInput("exclude_select", "Exclude from analysis",
                      files, selected = exclude, multiple=TRUE, selectize=FALSE, size = 5)
        )
      })
    }
  })

  # get overview data
  get_overview_data <- reactive({
    if ( nrow(get_data_table()) > 0 && !is.null(input$n2o_rt)) {

      message("INFO: Compiling overview data")
      data$n2o_rt <- input$n2o_rt
      data$n2o <- input$n2o_select
      data$std1 <- input$std1_select
      data$std2 <- input$std2_select
      data$exclude <- input$exclude_select
      input$data_drift_correction

      isolate({

        # N2O peak selection
        dt <- get_data_table() %>%
            select_N2O_peak(input$n2o_rt)
        if (nrow(dt) == 0)
          stop("No peaks found at this retention time. Please check where the N2O peaks are.")

        # determine grouping (for panels)
        dt <- dt %>%
          mutate(
            group =
              ifelse(category %in% data$n2o, "Lab ref",
                     ifelse(category %in% data$std1, "Standard 1",
                            ifelse(category %in% data$std2, "Standard 2",
                                   "Samples"))),
            color = ifelse(group == "Samples", "Samples", name))

        # remove excluded
        if (length(data$exclude) > 0)
          dt <- mutate(dt, group = ifelse(file %in% data$exclude, "Excluded", group))

        # factor groups for right order
        dt <- mutate(dt, group = factor(group,
                levels = c("Lab ref", "Standard 1", "Standard 2", "Samples", "Excluded")))
        dt <- dt[with(dt, order(group, run_number)),]

        # data frame simplification and processing
        dt <- dt %>%
          dplyr::rename(d45 = `d 45N2O/44N2O`, d46 = `d 46N2O/44N2O`, area = `Intensity All`, intensity = `Ampl 44`) %>%
          select_columns(folder, file, date, analysis,
                         run_number, name, category, group,
                         volume, intensity, area, d45, d46, color) %>%
          evaluate_drift(d45, d46, correct = input$data_drift_correction != "none", plot = FALSE,
                         span = as.numeric(input$data_drift_loess),
                         correct_with = group %in% c("Lab ref", "Standard 1", "Standard 2"),
                         method = if (input$data_drift_correction == "loess") "loess" else "lm") %>%
          correct_N2O_for_17O(d45.drift, d46.drift)

        return(dt)
      })
    } else
      return(data.frame())
  })

  # make the overview plot
  make_overview_plot <- reactive({

    withProgress(detail = "for data overview...", value = 0, {
      setProgress(0.2, "Compiling data")

      dt <- get_overview_data()

      if (nrow(dt) > 0) {
        message("INFO: Plotting data overview")
        dt <- mutate(dt, size = ifelse(group == "Excluded", median(area), area))
        dt$y <- dt[[input$data_type_selector]]

        setProgress(0.5, "Constructing plot")
        p <- dt %>%
          plot_overview(
            y, size = size,
            text = make_itext(paste0(name, " (#", x, ")"),
                              d15 = round(d15.raw, 2), d18 = round(d18.raw, 2),
                              area = round(area,1)),
            color = color, panel = group) +
          labs(y = input$data_type_selector, size = "Area All [Vs]")
        setProgress(0.8, "Rendering plot")
        return(p)
      } else
        plot.new()
    })
  })
  output$data_overview_plot <- renderPlot(make_overview_plot())

  output$data_overview_download <- downloadHandler(
    filename = function() {paste0(basename(get_data_folder()), "_", input$data_type_selector, "_overview.pdf")},
    content = function(file) {
      device <- function(..., version="1.4") grDevices::pdf(..., version=version)
      ggsave(file = file, plot = make_overview_plot(), width = 12, height = 8, device = device)
    })

  output$data_csv_download <- downloadHandler(
    filename = function() {paste0(basename(get_data_folder()), "_data.csv")},
    content = function(file) {
      write.csv(
        get_overview_data() %>% dplyr::select(-color) %>% dplyr::arrange(group, name, run_number),
        file = file, row.names = FALSE)
    })

  # interactive plot =========

  output$data_overview_iplot <- renderPlotly({
    make_overview_plot() %>% make_interactive()
  })

  # drift correction plot ========
  output$data_drift_correct_plot <- renderPlot ({
    if (input$data_drift_correction == "none") {
      plot.new()
      ggplot() + theme_bw() + annotate("text", 0.5, 0.5, label = "no drift correction", size = 20) +
        theme(text = element_blank())
    } else {
      get_overview_data() %>%
        mutate(bla = as.character(group)) %>%
        evaluate_drift(d45, d46, correct = TRUE, plot = TRUE, span = as.numeric(input$data_drift_loess),
                       correct_with = bla %in% c("Lab ref", "Standard 1", "Standard 2"),
                       method = if (input$data_drift_correction == "loess") "loess" else "lm")
    }
  })

})
